(column-number-mode t);显示列号

;;设置tab为4个空格的宽度
(setq default-tab-width 4)
(setq c-basic-offset 4)
;;去掉Emacs和gnus启动时的引导界面
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)
            (setq default-tab-width 4)
            (setq indent-tabs-mode nil)))

;;自动补全功能，这事从王垠的网站直接Copy过来的，引用一些他对此的说明
;;你可以设置以下 hippie-expand 的补全方式。它是一个优先列表， hippie-expand 会优先使用表最前面的函数来补全
;;这是说，首先使用当前的buffer补全，如果找不到，就到别的可见的窗口里寻找，如果还找不到，那么到所有打开的buffer去找，如果还……那么到kill-ring里，到文件名，到简称列表里，到list，…… 当前使用的匹配方式会在 echo 区域显示。
;;特别有意思的是 try-expand-line，它可以帮你补全整整一行文字。我很多时后有两行文字大致相同，只有几个字不一样，但是我懒得去拷贝粘贴以下。那么我就输入这行文字的前面几个字。然后多按几下 M-/ 就能得到那一行。
(setq hippie-expand-try-functions-list
'(try-expand-line
try-expand-line-all-buffers
try-expand-list
try-expand-list-all-buffers
try-expand-dabbrev
try-expand-dabbrev-visible
try-expand-dabbrev-all-buffers
try-expand-dabbrev-from-kill
try-complete-file-name
try-complete-file-name-partially
try-complete-lisp-symbol
try-complete-lisp-symbol-partially
try-expand-whole-kill))

;;所有的问题用y/n方式，不用yes/no方式。有点懒，只想输入一个字母
(fset 'yes-or-no-p 'y-or-n-p)

;;当寻找一个同名的文件，自动关联上那个文件？
(setq uniquify-buffer-name-style 'forward)

;;鼠标自动避开指针，如当你输入的时候，指针到了鼠标的位置，鼠标有点挡住视线了
(mouse-avoidance-mode 'animate)

;;当指针到一个括号时，自动显示所匹配的另一个括号
(show-paren-mode 1)

;;滚动页面时比较舒服，不要整页的滚动
(setq scroll-step 1
scroll-margin 3
scroll-conservatively 10000)

;;设定句子结尾，主要是针对中文设置
(setq sentence-end "\\([&#161;&#163;&#163;&#161;&#163;&#191;]\\|&#161;&#161;\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;;允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)



(global-font-lock-mode 1)               ; 开启语法高亮。

;; 包自动加载
;; more package
(require 'package)
(add-to-list 'package-archives'
             ("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives'
             ("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives'
             ("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/emacs/lisp_ext")
(autoload 'css-mode "css-mode" "CSS editing mode" t)
(setq auto-mode-alist
      ;; 将文件模式和文件后缀关联起来。
      (append '((".py'" . python-mode)
                (".s?html?'" . html-helper-mode)
                (".dwt'" . html-helper-mode)
                (".lbi'" . html-mode)
                (".asp'" . html-helper-mode)
                (".phtml'" . html-helper-mode)
                (".css'" . css-mode))
              auto-mode-alist))


;; add js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)


;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)


;;---------- php
(require 'php-mode)

(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; 默认不加载indent-tabs-mode
(setq-default indent-tabs-mode  nil)

;; 保存文件前执行一次whitespace-cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)

;; 如果是打开makefile文件，则开启indent-tabs-mode，因为whitespace-cleanup中会用到这个
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;;(setq php-manual-path "/usr/share/doc/php-manual/en/html/")

;;mustache
;;(add-to-list 'load-path "~/.emacs.d/vendor/mustache-mode.el")
;;(require 'mustache)

;;default add by js2-mode
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-cleanup-whitespace t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(prefer-coding-system 'utf-8)

;; If you use the default mail user agent.
(setq send-mail-function 'smtpmail-send-it)
;; If you use Message or Gnus.
(setq message-send-mail-function 'smtpmail-send-it)

;; Send mail using SMTP via mail.example.org.
(setq smtpmail-smtp-server "mail.163.com")

(setq smtpmail-smtp-service 25)

;; Authenticate using this username and password against my server.
;; Note that if port is an integer, you must not quote it as a
;; string.  Normally port should be the integer 25, and the example
;; become:
(setq smtpmail-auth-credentials
      '(("smtp.163.com" 25 "flj10" "teartear")))


;;lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

;;使用Emacs-w3m浏览网页
;; (add-to-list 'load-path "~/emacs/lisp_ext/w3m")
;; (require 'w3m-load)
;; (require 'w3m-e21)
;; (provide 'w3m-e23)
;; (setq w3m-use-favicon nil)
;; (setq w3m-command-arguments '("-cookie" "-F"))
;; (setq w3m-use-cookies t)
;; (setq w3m-home-page "http://www.google.com")

;; (setq w3m-display-inline-image t)

;;启动和初始化w3m.el
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
;;使用w3m作为默认的浏览器
(setq browse-url-browser-function 'w3m-browse-url)
;;使用mule-ucs，只有在你安装mule-ucs elisp扩展包时这个才有用，可以看Unicode解码的网页
;(setq w3m-use-mule-ucs t)
;;使用工具包
(setq w3m-use-toolbar t)
;;使用info的快件键绑定
;(set-default 'w3m-key-binding 'info)
;;启用cookie
(setq w3m-use-cookies t)
;;这个是作什么的?
(setq w3m-use-tab-menubar t)
;;设定w3m图标所在文件夹
;(setq w3m-icon-directory "/home/jerry/software/xemacs/w3m/emacs-w3m-1.4.4/icons")
;;显示图标
(setq w3m-show-graphic-icons-in-header-line t)
(setq w3m-show-graphic-icons-in-mode-line t)
;;设定w3m运行的参数，分别为使用cookie和使用框架
(setq w3m-command-arguments '("-cookie" "-F"))
;;用w3m浏览网页时也显示图片
(setq w3m-display-inline-image t)
;; ;;设定w3m的语言设置，以便方便使用和阅读中文-用了就乱码
;; ;;书签解码设置
;; (setq w3m-bookmark-file-coding-system 'chinese-iso-8bit)
;; ;;w3m的解码设置，后面最好都有，我也不详解了
;; (setq w3m-coding-system 'chinese-iso-8bit)
;; (setq w3m-default-coding-system 'chinese-iso-8bit)
;; (setq w3m-file-coding-system 'chinese-iso-8bit)
;; (setq w3m-file-name-coding-system 'chinese-iso-8bit)
;; (setq w3m-terminal-coding-system 'chinese-iso-8bit)
;; (setq w3m-input-coding-system 'chinese-iso-8bit)
;; (setq w3m-output-coding-system 'chinese-iso-8bit)
;;w3m是使用tab的，设定Tab的宽度
(setq w3m-tab-width 4)
;;设定w3m的主页
(setq w3m-home-page "http://www.google.cn")
;;当用 shift+RET 打开新链接时将不自动跳转到新的页面，等提示已经完全打开，才用 C-c C-n ，
;;C-c C-p 打开，这个好用
(setq w3m-view-this-url-new-session-in-background t)
(add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
;;好像是有利于中文搜索的
(defun remove-w3m-output-garbages ()
"去掉w3m输出的垃圾."
(interactive)
(let ((buffer-read-only))
(setf (point) (point-min))
(while (re-search-forward "[\200-\240]" nil t)
(replace-match " "))
(set-buffer-multibyte t))
(set-buffer-modified-p nil))
;;颜色设置
;(setq w3m-
;;;;;;;;;;;;;;;;;;;;;
;;语言设置
;;这个不知道有用没，好像在下一版emacs对unicode支持好了就可以了
;;当然这个是用emacs-cvs
;;;;;;;;;;;;;;;;;;;;;
(when (boundp 'utf-translate-cjk)
(setq utf-translate-cjk t)
(custom-set-variables
'(utf-translate-cjk t)))
(if (fboundp 'utf-translate-cjk-mode)
(utf-translate-cjk-mode 1))
;;配置handle text/html part with emacs-w3m under SEMI MUAs such as Wanderlust
;;(require `mime-w3m)

(add-to-list 'load-path "~/emacs/site-lisp/magit-1.0.0")
;;(require 'magit)
;;(global-set-key "\C-xg" 'magit-status)

;;不要生成临时文件
(setq-default make-backup-files nil)

;;写文件的编码方式
;;(set-buffer-file-coding-system 'gb2312)
(set-buffer-file-coding-system 'utf-8)

;;新建文件的编码方式
;;(setq default-buffer-file-coding-system 'gb2312)
(setq default-buffer-file-coding-system 'utf-8)

;;终端方式的编码方式
(set-terminal-coding-system 'utf-8)

;;读取或写入文件名的编码方式
(setq file-name-coding-system 'utf-8)

;;去掉菜单栏
(menu-bar-mode nil)


;;代码折叠
(require 'hideshow)

(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
;;(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;热键设置
(global-set-key [f5] 'complile)
    (setq-default compile-command "make")
(global-set-key [f3] 'query-replace-regexp)


(add-hook 'php-mode-hook
(lambda()
  (c-set-offset 'arglist-intro '+ )
(c-set-offset 'arglist-close 'endp )
(c-set-offset 'arglist-cont-nonempty '+ )
(require 'php-completion)
(php-completion-mode t)
(define-key php-mode-map (kbd "C-c p") 'phpcmp-complete)
(when (require 'auto-complete nol t)
(make-variable-buffer-local 'ac-sources)
(add-to-list 'ac-sources 'ac-source-php-completion)
(auto-complete-mode t))))

;;======================            自动补全功能        =====================
;;自动补全功能，这事从王垠的网站直接Copy过来的，引用一些他对此的说明
;;设置以下 hippie-expand 的补全方式。它是一个优先列表， hippie-expand 会优先使用表最前面
;;的函数来补全这是说，首先使用当前的buffer补全，如果找不到，就到别的可见的窗口里寻找，如
;;还找不到，那么到所有打开的buffer去找，如果还……那么到kill-ring里，到文件名，到简称列表
;;里，到list，当前使用的匹配方式会在 echo 区域显示。
;;特别有意思的是 try-expand-line，它可以帮你补全整整一行文字。我很多时后有两行文字大致相
;;同，只有几个字不一样，但是我懒得去拷贝粘贴以下。那么我就输入这行文字的前面几个字。然后
;;多按几下 M-/ 就能得到那一行。
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
         '(try-expand-line
              try-expand-line-all-buffers
              try-expand-list
              try-expand-list-all-buffers
              try-expand-dabbrev
              try-expand-dabbrev-visible
              try-expand-dabbrev-all-buffers
              try-expand-dabbrev-from-kill
              try-complete-file-name
              try-complete-file-name-partially
              try-complete-lisp-symbol
              try-complete-lisp-symbol-partially
              try-expand-whole-kill))

;;hippie的自动补齐策略，优先调用了senator的分析结果：
(autoload 'senator-try-expand-semantic "senator")
;;----------------------            End 自动补全        ---------------------


(add-to-list 'load-path "~/emacs/lisp_ext/php/")
(require 'install-elisp)
(setq install-elisp-repository-directory "~/emacs/lisp_ext/php/")

(locate-library "auto-complete.el")

(which-function-mode t)

;; temp mark
;(global-set-key (kbd "C-x C-m") 'ska-point-to-register)
;(global-set-key (kbd "C-c <right>") 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))
;; end

;; geben
;;(add-to-list 'load-path "~/emacs/lisp_ext/geben/")
;;(require 'geben)
;;(require 'dbgp)
;;(autoload 'geben "geben" "PHP Debugger on Emacs" t)
;; end geben

(put 'set-goal-column 'disabled nil)

;; tabbar
;; (require 'tabbar)
;; (tabbar-mode)
;; (global-set-key (kbd "") 'tabbar-backward-group)
;; (global-set-key (kbd "") 'tabbar-forward-group)
;; (global-set-key (kbd "C-x 8") 'tabbar-backward)
;; (global-set-key (kbd "C-x 9") 'tabbar-forward)

(require 'less-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-mode))

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(global-set-key (kbd "C-t") 'set-mark-command)

;; key bindings
(setq mac-option-modifier 'alt)
(setq mac-command-modifier 'meta)

;; https://github.com/fgallina/multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; rainbow-mode
(require 'rainbow-mode)

(defun concat-symbol (&rest lst)
  (intern (apply 'concat (mapcar (lambda(x)(if (symbolp x) (symbol-name x) x)) lst))))

(dolist (hook
         '(css
           html
           sass
           ))
  (add-hook
   (concat-symbol hook '-mode-hook)
   'rainbow-turn-on))
;; (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
;; (add-hook hook 'rainbow-turn-on))
