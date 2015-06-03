;;; html-helper-mode.el --- Major mode for composing html files.

;; Author: Nelson Minar <nelson@reed.edu>
;; Maintainer: Nelson Minar <nelson@reed.edu>
;; Created: 01 Feb 1994
;; Version: $Revision: 2.0 $
;; Keywords: HTML major-mode

;; LCD Archive Entry:
;; html-helper-mode|Nelson Minar|nelson@reed.edu|
;; Major mode for editing HTML.|
;; 16-Mar-94|Version 2.0|ftp://ftp.reed.edu/pub/src/html-helper-mode.tar.Z

;; Copyright (C) 1994 Nelson Minar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;{{{ 

;; Installation:
;;   add this line in your .emacs:
;;     (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;   to invoke html-helper-mode automatically on .html files, do this:
;;     (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;   NOTE - this mode requires another lisp file, tempo.el. This can be
;;          retrieved from ftp://ftp.lysator.liu.se/pub/emacs/tempo.el

;; Configuration:
;;   see the "user variables" section, or the documentation on configuration
;;   in http://www.reed.edu/~nelson/tools/. There are variables you want to
;;   configure, particularly html-helper-do-write-file-hooks,
;;   html-helper-build-new-buffer, and html-helper-address-string

;; Description:
;;   html-helper-mode makes it easier to write HTML documents. This mode
;;   handles inserting HTML codes in a variety of ways (keybindings,
;;   completion in the buffer). It also supports indentation, timestamps,
;;   skeletons for new documents, hilit19 patterns, and a variety of other
;;   things. For the full skinny, see the HTML documentation that comes
;;   with the package or is at http://www.reed.edu/~nelson/tools/

;; Thank yous:
;;   David Kågedal <davidk@lysator.liu.se> for the tempo code which
;;   forms the core of the HTML insertion, as well as the HTML+
;;   cookies.

;;   Magnus Homann <d0asta@dtek.chalmers.se> for suggestions and code
;;   for the timestamp insertion

;;   Marc Andreessen <marca@ncsa.uiuc.edu> for writing the original html-mode
;;   that inspired this one

;; To do:
;;   some general way of building menus (easymenu.el)?
;;   font-lock patterns
;;   a way to send the right "load URL" signal to xmosaic for the current file?

;; The newest version of html-helper-mode should always be available from
;;   http://www.reed.edu/~nelson/tools/
;;   ftp://ftp.reed.edu/pub/src/html-helper-mode.tar.Z

;; This code was writting using folding.el, a wonderful folding editor
;; minor mode for emacs. That's what the strange {{{ comments are for.

;;}}}

;;; Code:

;;{{{ user variables

;; features. I recommend you turn these on.

(defvar html-helper-do-write-file-hooks nil
  "*If not nil, then html-helper-mode will modify the local-write-file-hooks
to do timestamps.")

(defvar html-helper-build-new-buffer nil
  "*If not nil, then html-helper will insert html-helper-new-buffer-strings
when new buffers are generated")

;; (see also tempo.el)

;; variables to configure

(defvar html-helper-basic-offset 2
  "*basic indentation size used for list indentation")

(defvar html-helper-item-continue-indent 5
  "*Indentation of lines that follow a <li> item. Default is 5, the length
of things like \"<li> \" and \"<dd> \".")

(defvar html-helper-never-indent nil
  "*If t, the indentation code for html-helper is turned off.")


;; hooks (see also tempo.el)

(defvar html-helper-mode-hook nil
  "*Hook run when html-helper-mode is started.")

(defvar html-helper-load-hook nil
  "*Hook run when html-helper-mode is loaded.")

(defvar html-helper-timestamp-hook 'html-helper-default-insert-timestamp
  "*Hook called for timestamp insertion. Override this for your own
timestamp styles.")


;; strings you might want to change

(defvar html-helper-address-string ""
  "*The default author string of each file.")

(defvar html-helper-new-buffer-template
  '("<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" html-helper-address-string "</address>\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body> </html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if
html-helper-build-new-buffer is set to t")

(defvar html-helper-timestamp-start "<!-- hhmts start -->\n"
  "*Delimiter for timestamps. Everything between html-helper-timestamp-start
and html-helper-timestamp-end will be deleted and replaced with the output of
the function html-helper-insert-timestamp if html-helper-do-write-file-hooks
is t")

(defvar html-helper-timestamp-end "<!-- hhmts end -->"
  "*Delimiter for timestamps. Everything between html-helper-timestamp-start
and html-helper-timestamp-end will be deleted and replaced with the output of
the function html-helper-insert-timestamp if html-helper-do-write-file-hooks
is t")

;; this is what the byte compiler does to see if its emacs18. You probably
;; don't need to change this.

(defvar html-helper-emacs18
  (and (boundp 'emacs-version)
       (or (and (boundp 'epoch::version) epoch::version)
	   (string-lessp emacs-version "19")))
  "I'll do minimal emacs18 support, grumble.")

;;}}}

(require 'tempo)

;;{{{ html-helper-mode-syntax-table

;; emacs doesn't really seem to be general enough to handle SGML like
;; syntax. In particular, comments are a loss. We do try this, though:
;;   give < and > matching semantics

(defvar html-helper-mode-syntax-table nil
  "Syntax table for html-helper.")

(if html-helper-mode-syntax-table
    ()
  (setq html-helper-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " html-helper-mode-syntax-table))

;;}}}
;;{{{ keymap variable and function setup

(defvar html-helper-keymap-list
  '(html-helper-head-map html-helper-header-map html-helper-anchor-map
    html-helper-logical-map html-helper-phys-map html-helper-list-map
    html-helper-note-map html-helper-form-map html-helper-image-map)
  "list of all the subkeymaps html-helper uses")

(defvar html-helper-keymap-alist
  '((head . html-helper-head-map)
    (header . html-helper-header-map)
    (anchor . html-helper-anchor-map)
    (logical . html-helper-logical-map)
    (phys . html-helper-phys-map)
    (list . html-helper-list-map)
    (note . html-helper-note-map)
    (form . html-helper-form-map)
    (image . html-helper-image-map))
  "alist associating cookie types with keymaps")

;; basic keymap variables (not easy to mapcar a macro)
(defvar html-helper-mode-map (make-sparse-keymap)
  "Keymap for html-helper")
(defvar html-helper-head-map nil
  "Keymap used for head info.")
(defvar html-helper-header-map nil
  "Keymap used for headers.")
(defvar html-helper-anchor-map nil
  "Keymap used for anchors.")
(defvar html-helper-logical-map nil
  "Keymap used for logical styles.")
(defvar html-helper-phys-map nil
  "Keymap used for physical styles.")
(defvar html-helper-list-map nil
  "Keymap used for lists.")
(defvar html-helper-note-map nil
  "Keymap used for notes.")
(defvar html-helper-form-map nil
  "Keymap used for forms.")
(defvar html-helper-image-map nil
  "Keymap used for images.")

;; make keymaps into prefix commands (does this do anything useful in 18?)
(mapcar 'define-prefix-command html-helper-keymap-list)

;; if we're emacs18, we have to build the prefix maps by hand
(if html-helper-emacs18
    (mapcar (function (lambda (v) (set v (make-sparse-keymap))))
	    html-helper-keymap-list))

;; now build the mode keymap.
;; special mode keys
(mapcar
 (function (lambda (l) (define-key html-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)
   
   ("\M-\C-t" html-helper-insert-timestamp-delimiter-at-point)))
 
;; indentation keys - only rebind these if the user wants indentation
(if html-helper-never-indent
    ()
  (define-key html-helper-mode-map "\t" 'html-helper-indent-command)
  (define-key html-helper-mode-map "\C-m" 'newline-and-indent))

;; special keybindings in the prefix maps (not in the list of cookies)
(define-key html-helper-list-map "i" 'html-helper-smart-insert-item)

;; install the prefix maps themselves into the mode map
;; eval the keymap in 18 so we get the value, not the symbol
(defun html-helper-install-prefix (l)
  "Install a prefix key into the map. Special code for emacs18"
  (if html-helper-emacs18
      (define-key html-helper-mode-map (car l) (eval (nth 1 l)))
    (define-key html-helper-mode-map (car l) (nth 1 l))))

(mapcar
 'html-helper-install-prefix
 '(("\C-c\C-b" html-helper-head-map)
   ("\C-c\C-t" html-helper-header-map)
   ("\C-c\C-a" html-helper-anchor-map)
   ("\C-c\C-s" html-helper-logical-map)
   ("\C-c\C-p" html-helper-phys-map)
   ("\C-c\C-l" html-helper-list-map)
   ("\C-c\C-n" html-helper-note-map)
   ("\C-c\C-f" html-helper-form-map)
   ("\C-c\C-i" html-helper-image-map)))

;;}}}
;;{{{ html-helper-mode-abbrev-table

(defvar html-helper-mode-abbrev-table nil
  "Abbrev table used while in html-helper-mode.")
(define-abbrev-table 'html-helper-mode-abbrev-table ())

;;}}}

;;{{{ html-helper-add-cookie function for building basic cookies

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

(defun html-helper-add-cookie (l)
  "Add a new cookie to html-helper-mode. Builds a tempo-template for the
cookie and puts it into the appropriate keymap if a key is
requested."
  (let* ((type (car l))
	 (keymap (cdr-safe (assq type html-helper-keymap-alist)))
	 (key (nth 1 l))
	 (tag (nth 2 l))
	 (name (nth 3 l))
	 (cookie (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template name cookie tag doc 'html-helper-tempo-tags)))

    (if (stringp key)			                  ;bind at all?
	(if keymap			                  ;special keymap?
	    (define-key (eval keymap) key command)        ;bind to prefix
	  (define-key html-helper-mode-map key command))  ;bind to global
      )))

;;}}}

;;{{{ html-helper-smart-insert-item

;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dict