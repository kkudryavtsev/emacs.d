;; Interchange
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-group
 'interchange
 '(
   (itl-perl
	:submode cperl-mode
	:face mmm-code-submode-face
	:front "\\\[perl.*?\\\]"
	:back "\\\[/perl\\\]")
   
   (itl-calc
	:submode cperl-mode
	:face mmm-code-submode-face
	:front "\\\[calc\\\]"
	:back "\\\[/calc\\\]")
   (itl-heredoc
	:submode cperl-mode
	:face mmm-code-submode-face
	:front "<<\\([a-zA-Z0-9_-]+\\)"
    :back "^~1$"
	:save-matches 1
   )))

(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil interchange))
;;;(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil mason))
(add-to-list 'mmm-mode-ext-classes-alist '(fundamental-mode nil interchange))

;; Javascript
;;(add-to-list 'mmm-mode-ext-classes-alist '(nxml-mode nil javascript))

;;; see ruby-conf.el for html/erb mmm-mode config
