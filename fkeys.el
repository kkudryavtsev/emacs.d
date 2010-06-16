(global-set-key [f1] '(lambda () (interactive) (find-file "~/emacs.d/fkeys.el")))
(global-set-key [(control f1)] '(lambda () (interactive) (load-file "~/emacs.d/fkeys.el")))
(global-set-key [(shift f1)] '(lambda () (interactive) (find-file "~/emacs.d/keys.el")))
(global-set-key (kbd "C-S-<f1>") '(lambda () (interactive) (load-file "~/emacs.d/keys.el")))

(global-set-key [f2] 'eshell)
(global-set-key (kbd "M-<f2>") '(lambda () (interactive) (eshell t)))
(global-set-key [(control f2)] 'shell-command)
(global-set-key [f5] 'compile)
(global-set-key (kbd "M-<f5>") 'ruby-rake)
(global-set-key [f6] 'ruby-test-file)
(global-set-key (kbd "M-<f6>") 'ruby-test-function)
(global-set-key [f7] 'toggle-ruby-compilation-mode)

(global-set-key [f9] 'compile)
(global-set-key [f10] 'svn-examine)
