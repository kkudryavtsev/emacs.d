;; this is my emacs initialization script
;; init.el is symlined to ~/.emacs

;(setq debug-on-error t)

(defvar my-config-dir "~/emacs.d/" 
  "Directory for individual elisp configuration files")
(defun set-load-path ()
  (interactive)
  (mapc (lambda (dir)
          (let* ((lisp-dir (concat dir "/lisp"))
                 (add-path (if (file-directory-p lisp-dir)
                               lisp-dir
                             dir)))
            (unless (member add-path load-path)
              (add-to-list 'load-path add-path))))
        (directory-files (concat my-config-dir "site-lisp") t "[^.]+")))
(set-load-path)
(add-to-list 'load-path (concat my-config-dir "site-lisp/nxhtml/related"))

;(require 'pcvs)
;(require 'psvn)
(require 'imenu)
;(require 'speedbar)
(require 'uniquify)                     ;make buffer names unique using path
(require 'ibuffer)                      ;improved buffer listing
(require 'comint)
;(eval-after-load "icomplete" '(progn (require 'icomplete+)))
;(require 'icomplete)
;(require 'iswitchb)
(require 'pastie)
(require 'linum)
(require 'yasnippet)
(require 'find-file-in-tags)

(require 'ido)(ido-mode t)
(setq ido-enable-flex-matching t)
;(icomplete-mode 99)
;(iswitchb-default-keybindings)

;(load-file (concat my-config-dir "site-lisp/" "cedet-1.0pre3/common/cedet.el"))
;(load-file (concat my-config-dir "site-lisp/" "elunit/elunit.el"))
(load-file (concat my-config-dir "dka.el"))
;(load-file (concat my-config-dir "bbdb-config.el"))
(load-file (concat my-config-dir "fkeys.el"))
;(load-file (concat my-config-dir "wiki-config.el"))
;(load-file (concat my-config-dir "twitter-config.el"))
;(load-file (concat my-config-dir "w3m-config.el"))
;(load-file (concat my-config-dir "message-config.el"))
;(load-file (concat my-config-dir "auto-encryption.el"))
;(load-file (concat my-config-dir "mmm-mode-conf.el"))
(load-file (concat my-config-dir "todo-config.el"))
;;;(load-file (concat my-config-dir "planner-config.el"))
;(load-file (concat my-config-dir "site-lisp/" "jw-testing.el"))
(load-file (concat my-config-dir "site-lisp/" "codol.el"))
(load-file (concat my-config-dir "ruby-conf.el"))
;(load-file (concat my-config-dir "display-time-config.el"))
;(load-file (concat my-config-dir "elscreen-config.el"))
(load-file (concat my-config-dir "coffeescript-config.el"))
(add-hook 'eshell-mode-hook '(lambda () (load-file (concat my-config-dir "eshell-funcs.el"))))
(load-file (concat my-config-dir "readpassword-remap.el"))
(load-file (concat my-config-dir "keys.el"))
(load-file (concat my-config-dir "site-lisp/misc/" "color-theme-blackboard.el"))
(load-file (concat my-config-dir "nxml-config.el"))
(load-file (concat my-config-dir "nxhtml-config.el"))
(load-file (concat my-config-dir "yas-config.el"))
(load-file (concat my-config-dir "ecb-config.el"))

;; Add color to a shell running in emacs 'M-x shell'
;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(if (eq system-type 'darwin)
    (load-file (concat my-config-dir "mac-setup.el")))

;(if (or (string-match "scooby" (system-name))
;	(string-match "trstone" (system-name)))
;    (normal-erase-is-backspace-mode t))

(setq orig-auto-mode-alist auto-mode-alist)
;(setq auto-mode-alist orig-auto-mode-alist)
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode)
                ("\\.json$" . js2-mode)
                 ("\\.ss$" . shell-script-mode)
                 ("\\.mk$" . makefile-mode)
		 ("diary" . auto-fill-mode)
                 ("t/.*\\.t$" . perl-mode)
		 ("\\.w3m/" . auto-fill-mode)
		 ("\\.pgp$" . mc-decrypt)
		 ("\\.htaccess$" . apache-mode)
		 ("httpd\\.conf$" . apache-mode)
		 ("srm\\.conf$" . apache-mode)
                 ("\\.mas$" . html-mode)
                 ("dhandler" . html-mode)
                 ("autohandler" . html-mode)
                 ("\\.yml$" . yaml-mode)
                 ("\\.markdown$" . markdown-mode)
		 ("access\\.conf$" . apache-mode))   auto-mode-alist))


;; Misc global settings
(abbrev-mode 1)
(auto-compression-mode 1)
(setq svn-status-verbose nil)
(setq default-tab-width 2)
;;(menu-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq dired-recursive-deletes 'top)
(setq transient-mark-mode t)
(mwheel-install)
(setq mouse-wheel-scroll-amount nil)
(setq mail-user-agent 'gnus-user-agent)
(setq mouse-wheel-follow-mouse t)
(global-font-lock-mode 1)

(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1)
(setq truncate-partial-width-windows nil)
(setq progress-feedback-use-echo-area t) ;diable progress meter
(setq indent-tabs-mode nil)             ;all tabs are spaces
(setq require-final-newline t)          ;always make sure the file ends in a newline
(setq next-line-add-newlines nil)       ;beep at end of file instead of adding new lines
(setq focus-follows-mouse nil) ;the mac doesn't have ffm
(line-number-mode t)                    ;add line number to mode-line
(setq list-matching-lines-whole-buffer t) ;occur matches whole buffer
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq mode-compile-save-all-p t)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq imenu-auto-rescan t)

(setq tramp-default-method "scp"
      tramp-terminal-type "vt100"
      ange-ftp-ftp-program-args '("-i" "-n" "-g" "-v" "-e"))

(setq vc-follow-symlinks t)             ;always follow symlinks in vc controlled stuff
(setq cvs-update-optional-flags nil)

(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'find-file-in-project "arorem" nil t)
(autoload 'camelCase-mode "camelCase-mode" nil t)
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(autoload 'dict-lookup-words "dict" "client interface for dict.org" t)
(autoload 'cvs-examine "pcl-cvs" "examine directory via cvs" t)
(autoload 'switch-to-erc (concat my-config-dir "erc-config.el") "Major mode to waste time" t)
(autoload 'erc-select (concat my-config-dir "erc-config.el") "Major mode to waste time" t)
(autoload 'erc-start (concat my-config-dir "erc-config.el") "Major mode to waste time" t)
(autoload 'ssh "ssh" "Open a network login connection via `ssh'" t nil)
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(autoload 'yaml-mode "yaml-mode")
(autoload 'css-mode "css-mode")
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(autoload 'magit-status "magit" nil t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(autoload 'js2-mode "js2-mode" "an improved JavaScript editing mode" t)

(put 'eval-expression 'disabled nil)

;(require 'show-wspace)
;(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-search-directory-list (quote ("~/info" "/usr/share/info")))
 '(Info-restoring-point nil)
 '(ansi-color-for-comint-mode (quote filter))
 '(column-number-mode t)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-ask-about-save nil)
 '(compilation-mouse-motion-initiate-parsing nil)
 '(compilation-scroll-output t)
 '(complex-buffers-menu-p nil)
 '(default-frame-plist (quote (height 55 width 150)))
 '(delete-key-deletes-forward t)
 '(desktop-save-mode t)
 '(display-time-day-and-date t)
 '(ecb-compile-window-height 0.3)
 '(ecb-compile-window-prevent-shrink-below-height nil)
 '(ecb-history-make-buckets (quote never))
 '(ecb-history-sort-method nil)
 '(ecb-layout-name "left14")
 '(ecb-layout-window-sizes (quote (("left8" (0.17857142857142858 . 0.27941176470588236) (0.17857142857142858 . 0.23529411764705882) (0.17857142857142858 . 0.29411764705882354) (0.17857142857142858 . 0.17647058823529413)) ("left15" (0.1865079365079365 . 0.5) (0.1865079365079365 . 0.4852941176470588)) ("left14" (0.21428571428571427 . 0.7352941176470589) (0.21428571428571427 . 0.25)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("~/devel")))
 '(ecb-toggle-layout-sequence (quote ("left8" "left14" "left15")))
 '(ecb-windows-width 0.1)
 '(elscreen-display-tab nil)
 '(font-lock-mode t t (font-lock))
 '(get-frame-for-buffer-default-instance-limit nil)
 '(global-auto-revert-mode t)
 '(grep-find-ignored-directories (quote ("CVS" ".svn" ".git")))
 '(grep-find-ignored-files (quote (".#*" "*~" "*.elc" "*.idx" "*.orig")))
 '(gutter-buffers-tab-enabled nil)
 '(gutter-buffers-tab-visible-p nil)
 '(indent-tabs-mode nil)
 '(init-face-from-resources nil)
 '(initial-frame-alist (quote ((width . 200))))
 '(ispell-program-name "aspell")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-highlight-level 3)
 '(js2-mirror-mode t)
 '(js2-strict-missing-semi-warning nil)
 '(large-file-warning-threshold nil)
 '(make-backup-files nil)
 '(make-tags-files-invisible t)
 '(modeline-scrolling-method t)
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(mwheel-follow-mouse t)
 '(mwheel-scroll-amount (quote (nil . 5)))
 '(nxhtml-default-validation-header "body-utf-8")
 '(nxhtml-validation-header-if-mumamo t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(paren-backwards-message t)
 '(paren-mode (quote sexp) nil (paren))
 '(php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.module\\'")))
 '(query-user-mail-address nil)
 '(resize-minibuffer-window-exactly nil)
 '(resize-minibuffer-window-max-height -1)
 '(safe-local-variable-values (quote ((encoding . utf-8) (todo-categories "Emacs" "Todo"))))
 '(save-place t t)
 '(scroll-step 1)
 '(shifted-motion-keys-select-region nil)
 '(speedbar-mode-specific-contents-flag nil)
 '(speedbar-supported-extension-expressions (quote (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".rb" ".html" ".js" ".rake" ".erb" ".css" ".haml")))
 '(speedbar-use-imenu-flag nil)
 '(tab-width 8)
 '(tags-always-exact t)
 '(tags-auto-read-changed-tag-files t)
 '(tags-build-completion-table t)
 '(temp-buffer-max-height 0.3)
 '(temp-buffer-shrink-to-fit t)
 '(toolbar-visible-p nil)
 '(tooltip-mode nil)
 '(unshifted-motion-keys-deselect-region nil)
 '(woman-use-own-frame nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gainsboro" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "apple" :family "Andale_Mono"))))
 '(bold-italic ((t (:bold t :italic t))))
 '(compilation-warning ((((class color) (min-colors 16)) (:foreground "Orange"))))
 '(cursor ((t (:background "#faac13" :box (:line-width 2 :color "red" :style released-button)))))
 '(diff-added ((t (:inherit diff-changed :foreground "darkgreen"))))
 '(diff-file-header-face ((t (:bold t))) t)
 '(diff-index-face ((t (:bold t))) t)
 '(diff-nonexistent-face ((t (:bold t))) t)
 '(diff-removed ((t (:inherit diff-changed :foreground "darkred"))))
 '(dired-face-directory ((t (:foreground "darkblue" :bold t))))
 '(ecb-default-general-face ((((class color) (background light)) (:background "#e9e9e9" :height 1.0))))
 '(eshell-ls-archive ((t (:foreground "IndianRed" :bold t))))
 '(eshell-ls-archive-face ((t (:foreground "IndianRed" :bold t))))
 '(eshell-ls-directory ((t (:foreground "MediumSlateBlue" :bold t))))
 '(eshell-ls-directory-face ((t (:foreground "MediumSlateBlue" :bold t))))
 '(eshell-ls-todo-face ((t (:foreground "aquamarine" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "OliveDrab" :weight bold))))
 '(font-lock-doc-string-face ((((class color) (background light)) (:foreground "green4"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "brown4" :bold t))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "cadetblue4" :bold t))))
 '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "red3"))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "darkslategrey"))))
 '(font-lock-string-face ((t (:foreground "forestgreen"))))
 '(font-lock-type-face ((t (:foreground "saddlebrown"))))
 '(font-lock-variable-name-face ((t (:foreground "steelblue" :bold t))))
 '(font-lock-warning-face ((t (:foreground "IndianRed" :bold t))))
 '(hyper-apropos-hyperlink ((t (:foreground "DodgerBlue1" :bold t))))
 '(info-node ((t (:foreground "DodgerBlue1" :bold t :underline t))))
 '(info-xref ((t (:foreground "DodgerBlue1" :bold t))))
 '(italic ((t (:background "khaki3"))))
 '(mode-line ((t (:background "#a8ccdc" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((default (:inherit mode-line :background "#d3d3d3")) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) (:background "grey70"))))
 '(paren-match ((t (:background "darkseagreen"))))
 '(primary-selection ((t (:background "seagreen"))) t)
 '(show-paren-mismatch ((t (:background "red" :foreground "white"))))
 '(widget-button ((t (:bold t)))))

(require 'color-theme)(color-theme-initialize)
(load-file (concat my-config-dir "color-theme-dka.el"))
(if window-system
    (color-theme-dka)
(load-file (concat my-config-dir "tty-config.el")))

(server-start)
(if window-system-version
    (maximize-frame))
