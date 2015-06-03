;;; diff-mode.el --- a mode for viewing/editing context diffs

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,2005, 2006,
;;   2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: convenience patch diff

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides support for font-lock, outline, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; Inspired by Pavel Machek's patch-mode.el (<pavel@@atrey.karlin.mff.cuni.cz>)
;; Some efforts were spent to have it somewhat compatible with XEmacs'
;; diff-mode as well as with compilation-minor-mode

;; Bugs:

;; - Reverse doesn't work with normal diffs.

;; Todo:

;; - Improve `diff-add-change-log-entries-other-window',
;;   it is very simplistic now.
;;
;; - Add a `delete-after-apply' so C-c C-a automatically deletes hunks.
;;   Also allow C-c C-a to delete already-applied hunks.
;;
;; - Try `diff <file> <hunk>' to try and fuzzily discover the source location
;;   of a hunk.  Show then the changes between <file> and <hunk> and make it
;;   possible to apply them to <file>, <hunk-src>, or <hunk-dst>.
;;   Or maybe just make it into a ".rej to diff3-markers converter".
;;   Maybe just use `wiggle' (by Neil Brown) to do it for us.
;;
;; - in diff-apply-hunk, strip context in replace-match to better
;;   preserve markers and spacing.
;; - Handle `diff -b' output in context->unified.

;;; Code:
(eval-when-compile (require 'cl))

(defvar add-log-buffer-file-name-function)


(defgroup diff-mode ()
  "Major mode for viewing/editing diffs."
  :version "21.1"
  :group 'tools
  :group 'diff)

(defcustom diff-default-read-only nil
  "If non-nil, `diff-mode' buffers default to being read-only."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-jump-to-old-file nil
  "Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-update-on-the-fly t
  "Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-advance-after-apply-hunk t
  "Non-nil means `diff-apply-hunk' will move to the next hunk after applying."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode."
  :type 'hook
  :options '(diff-delete-empty-files diff-make-unified)
  :group 'diff-mode)

(defvar diff-outline-regexp
  "\\([*+][*+][*+] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

;;;;
;;;; keymap, menu, ...
;;;;

(easy-mmode-defmap diff-mode-shared-map
                   '(;; From Pavel Machek's patch-mode.
                     ("n" . diff-hunk-next)
                     ("N" . diff-file-next)
                     ("p" . diff-hunk-prev)
                     ("P" . diff-file-prev)
                     ("\t" . diff-hunk-next)
                     ([backtab] . diff-hunk-prev)
                     ("k" . diff-hunk-kill)
                     ("K" . diff-file-kill)
                     ;; From compilation-minor-mode.
                     ("}" . diff-file-next)
                     ("{" . diff-file-prev)
                     ("\C-m" . diff-goto-source)
                     ([mouse-2] . diff-goto-source)
                     ;; From XEmacs' diff-mode.
                     ;; Standard M-w is useful, so don't change M-W.
                     ;;("W" . widen)
                     ;;("." . diff-goto-source);display-buffer
                     ;;("f" . diff-goto-source);find-file
                     ("o" . diff-goto-source);other-window
                     ;;("w" . diff-goto-source);other-frame
                     ;;("N" . diff-narrow)
                     ;;("h" . -modifs(not buffer-read-only)]
                     ["Show trailing whitespace" whitespace-mode
                      :style toggle :selected (bound-and-true-p whitespace-mode)
                      :help "Show trailing whitespace in modified lines"]
                     "-----"
                     ["Split hunhanges"diff-refine-hunk
                      :help "Highlight changes of hunk at point at a finer granularity"]
                     ["Kill current hunk"diff-hunk-kill
                      :help "Kill current hunk"]
                     ["Kill current file's hunks" diff-file-kill
                      :help "Kill all current file's hunks"]
                     "---ound dark))
     :background "grey60" :weightiff used to highlight removed lines."
     :group 'diflant italic)
                   (((type tty pc) d-face 'diff-indicator-removed)

                    (defface diff-indicator-aon
                      '((t :inherit diff-header))
                      "`diff-mode' face used to highlight function names produced by \"diff -p\"."
                      :group 'diff-mode)
                    (define-obsolete-face-alias 'diff-function-face 'diff-funct-context-face 'diff-context)

                    (defface diff-nono the next-single-property-change call
                      ;; below will always return nil :-(   --stef
                      (let ((mixed (next-single-property-change 0 'yank-handler text))
                            (start (point)))
                        ;; First insert the text.
                        (insert text)
                        ;; If the text does not incl  (if (re-search-backward "^[><!][ \t]" start t)
                        (if (eq (char-aeader-re-unified "\\)\\(.*\\)$")
                                (1 diff-hunk-header files with spaces, but be careful to rule
                                   ;; out false-positives when matching hunk headerr-changed-face) (2 diff-changed-face))
                                   ("^Index: \\(. be a backup or
  ;; version-control name.  The [\t\n] at the end of the unidiff pattern
  ;; catches Debian source diff files (which lack the trailing date).
  '((nil "\\+\\+\\+\\ \\([^\t\n]+\\)[\t\n]" 1) ; unidifflid-unified-empty-line
                                           in a hunk
but in the file header instead, in which case move forward to th      (looking-at "^@@"))))

(defun diff-beginnier after our starting point :-efine diff-{hunk,file}-{prev,next}
  (easy-mmode-define-navigation
   diff-cursion
   (if arg (diff-begnil t 2)
     (match-beginning 0)))
  (firsthunk (ignore-errors
               (goto-char, "old mode", "new mode", "new file mode" and
                           ;; "deleted file mode" are output by git-diff.
                           (defconst diff-file-junk-re
                             "diff \\|index \\|\\(?:deleted file\\|new\\(?: file\\)?\\|old\\) mode")

                           (defun diff-beginning-of-file-and-junk ()
                             "Go to the beginning of file-related diff-info.
This is like `diff-beginning-of-file' except it tries to skip back over leading
data such as \"Index: ...\" and such."
                             (let* ((orig (point))
                                    ;; Skip forward over what might be "leading junk" so as to get
                                    ;; closer to the actual diff.
                                    (_ (progn (beginning-of-line)
                                              (while (looking-at diff-file-junk-re)
                                                (forward-line 1))))
                                    ) (point))))
               if `foame (old r"))
    (push[-*][-*][-*] \\(\\S-+\\)\\(\\s-.*\\)?\n[-+][-+][-+] \\(\\S-+\\)")
               (list (if old (match-string 1) (match-string 3))
                     (if old (match-string 3) (match-string 1)))
               (forward-line 1) nil)  (set (make-local-variable 'diff-remembered-defdir) default-directory)
               (set (make-local-variable 'diff-remembered-files-alist) nil))
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-les-alist))
      ;; try to be clever and use previous ile (do* ((files files (cdr files))
      (file (car files) (car files)))
    ;; Use file-regular-p to avoid
    ;; /dev/null, directories, etc.
    because we t (first fs))))
(set (make-local-variable 'diff-remembered-files-alist)
     (cons (cons fs file) diff-remembered-files-alist))
file))))))


(defun diff-ediff-patch ()
  "Call `ediff-patch-r-change functions.")

(defun diff-uon
  er
  (let ((line1 (matc      (save-restriction
                            (narrow-to-regioint-max))
                          (let ((modast-pt) (s+\\|\\*\\{15\\}.*\n\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]+\\) \\*\\*\\*\\*\\)$" nil t)
                    (< (pointatch-str--' line"))
                          (let ((line2s (match-string 1))
                                (line2e (match-string 2))
                                (pt2 (progn
                                       (delete-region (pro                        (prog1 (buffer-substring (+ pt2 2) endline2)
                                                                                    (deletontext will not proper))
                                                                                  (while (look                     -1))
                                                                                  " +" line2s ","
                                                                                  (number-to-string (- (string-to-number line2e)
                                                                                                       (string-to-number line2s)
                                                                                                       -1)) " @@"))
                                       (set-marker pt2 nil)
                                       ;;e) (eq buffer-undo-list t))
                                       (setq buffer-undo-list
                                             efix-arg (a))
                                       (combine-after-chaegion half1s (point))))
                                (at "[!+ \\][ \t]\\|#")
                                (w s (in case the buffer was modified).
                                   START and END are either taken from thebit-read-only))
                            (if diff-unhandled-changes
                                (setq diff-unhandlkward-char 1))
                            ;; We used to fixup modifs on all the changes, but it turns out that
                            ;; it's safer not to do it on big changes, e.g. when yanking a big
                            ;; diff, or when the user edits the header, since we might then
                            o))
               the hunk corresponding the buffer manually, -expression)
        diff-imenu-generic-expression)
    ;; These are not perfect.  They would be better done sepe 'page-delimiter) "--- [^\t]+\t")
    ;; compile support
    (set (make-local-variable 'next-error-function) 'diff-next-error)

    (set (make-local-variable 'beginning-of-defun-function)
         'diff-beginning-of-file-and-junk)
    (set (make-local-variable 'end-of-defun-function)
         'diff-end-of-f-variable 'whitespace-trailing-regexp) mode:
         (lexical-let ((ro-bindadd-logfter-change-function nil t)
                       (defun diff-dat have proved useful at some point.
                                           ;;;

                         (defun diff-next-complex-hunk ()
                           "Jump to the next \"complex\" hunk.
\"Complex\" is approximated by \"the hunk c          (memq (ch  er-re))
        (error "Not recognizable huat")
          (let ((before (string-to-number (or (match-s        ;;                  ;; Not just (eolp) so we don't infloop at eob.
                          (eq (char-after) ?\n)
                         p")))
           ((not (y-ortext from HUNK as (TEXT . OFFSET).
                           If on part of the hurward-line 0)
                 (setq divider-pos (point))
                 (forward-line 1)
                 (setq dst-prd-line 0)
                 (setq divider-pos (point))
                 (forward-line 1)
                 (setq dst-pos (point)))
            (t
             (error "Unknown diff hunk type")))

           (if (if destp (null dst-pos) (null src-pos))
               ;; Implied empty text
               (if char-offset '("" . 0) "")

             ;; For context diffs, either side can be empty, (if there's only
             ;; added or only removed text).  We should then use the other side.
             (cond ((equal src-pos divider-pos) (setq src-pos ds

                                                      (when char-o   (if (eq (char-after) k(
                                                                                            (mapconcat 'regexp-quote (split-string text) "[ \n diff-find-sours non-nil ife),
           ;; leading t0-rd-approx-text (car old))
 (invalid-regexp ni old.  But that means that by
      ;; default we use the old file, which is the o      (and buffer-file-name
             (backup-file-namhunk] to apply it to the other file"
                                                                                                       (urrent-buffer buf
                                                                                                                      (goto-char (car pos))
                                                                                                                      (delete-region (car pos) (cdr pos))
                                                                                                                      (insert (to the old or the new (save-excursion (beginning-of-line) (looking-at "[-<]")))))
                                                                                                       (destructuring-bind (buf line-h hunk may belong         (diff-find-source-location nil nil 'noprompt))
                                                                                                           (when buf
                                                                                                             (beginning-of-line)
                                                                                                             (or (when (memq (char-after) '(?< ?-))
                                                                                                                   ;; Cursor is pointing arc)))
                                                                                                                   t (- (point) (progn (diff-beginning- "diff1"))
                                                                                                                        (file2 (make-tem(0 nil);Nothing to nessarily do the same as the ones in highlight
                                                                                                                                        ;; since the "_" is not treated the same as " ".
                                                                                                                                        (replace-match (e . fine) (face diff-r(context
                                                                                                                                                                               (let* ((middle (save-excursion (re-search-forward "^---")))
                                                                                                                                                                                      (oth            (match-beginning 0))
                                                                                                                                                                                      efun dife-log-entry-other-window for each hunkog from when diff-mode wasn't part of Emacs:
                                                                                                                                                                                      ;; Revision 1.11  1999/10/09 23:38:29  monnier
                                                                                                                                                                                      ;; (diff-mode-load-hook): dropped.
                                                                                                                                                                                      ;; (auto-mode-alist): also catch *.diffs.
                                                                                                                                                                                      ;; (diff-find-file-name, diff-mode):  add smarts to find the right fik call diff-kill-file if it's the only hunk.
                                                                                                                                                                                      ;; - diff-kill-file now tries to kill the leading garbage as well.
                                                                                                                                                                                      ;;
                                                                                                                                                                                      ;; Revision 1.unified, diff-reverse-direction,
                                                                                                                                                                                      ;;  diff-fixup-modifs):  only use the region if a prefix arg is given.
                                                                                                                                                                                      ;;
                                                                                                                                                                                      ;; Revision 1.5  1999/08/31 19:18:52  monnier
                                                                                                                                                                                      ;; (diff-beginning-of-file, diff-prev-file):  fixed wrong parenthesis.
                                                                                                                                                                                      ;;
                                                                                                                                                                                      ;; Revision 1.4  1999/0