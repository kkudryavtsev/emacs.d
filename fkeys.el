(global-set-key [f1] '(lambda () (interactive) (find-file "~/emacs.d/fkeys.el")))
(global-set-key [(control f1)] '(lambda () (interactive) (load-file "~/emacs.d/fkeys.el")))
(global-set-key [(shift f1)] '(lambda () (interactive) (find-file "~/emacs.d/keys.el")))
(global-set-key (kbd "C-S-<f1>") '(lambda () (interactive) (load-file "~/emacs.d/keys.el")))

(global-set-key [f2] 'shell)
(global-set-key (kbd "M-<f2>") '(lambda () (interactive) (eshell t)))
(global-set-key [(control f2)] 'shell-command)
(global-set-key (kbd "M-r") 'ruby-rake)
(global-set-key (kbd "M-<f8>") 'compile) ; this is for "re-compile"
(global-set-key (kbd "<f8>") 'ruby-test-file)
(global-set-key (kbd "A-<f8>") 'ruby-test-function)
(global-set-key [f7] 'toggle-ruby-compilation-mode)

(global-set-key [f9] 'rgrep)
(global-set-key [f10] 'magit-status)

(global-set-key (kbd "A-n") 'find-file-in-tags)
(global-set-key (kbd "A-M-l") 'indent-region)
