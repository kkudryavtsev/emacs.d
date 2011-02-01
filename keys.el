;(define-key function-key-map [(escape)] [?\C-c]) ; make escape equivalent to C-c
;(global-set-key (kbd "C-x C-b") 'isearchb-activate)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-x C-d") 'find-dired)
(global-set-key (kbd "C-$") 'dict-lookup-words)
(global-set-key (kbd "C-c [") 'occur)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c 4 y") 'bury-buffer-other-window)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key [(control tab)] 'comint-dynamic-complete)
(defalias 'dka-prev-window
  (read-kbd-macro "C-u - 1 C-x o"))
(global-set-key (kbd "C-x p") 'dka-prev-window)
(global-set-key (kbd "C-x j") 'dka-jump-to-window)
;(global-set-key (kbd "C-h C-a") 'apropos-command)
(global-set-key (kbd "C-h a") 'apropos)
;(global-set-key [(control h) a] 'apropos)
(global-set-key (kbd "M-g") 'goto-line)


(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c g") 'imenu)
(global-set-key [(control x) k] 'context-kill)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x t") 'find-file-in-project)
(global-set-key (kbd "C-x 4 t") 'find-file-in-project-other-window)
(global-set-key (kbd "C-x C-d") 'find-grep-dired)
(global-set-key (kbd "M-.") 'find-tag)
