(require 'message)
(require 'message-x)

(setq message-x-body-function '(lambda () (interactive)(hippie-expand nil)))

;; message mode stuff
(setq message-default-charset (quote iso-8859-1)
      message-dont-reply-to-names "\\(doug@lathi\.net\\)\\|\\(dougalcorn@gmail.com\\)"
      message-kill-buffer-on-exit t)

(defun dka-insert-xface ()
  "Insert an X-Face header to outgoing mail.
  Hang it off the appropriate message send hook."
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator)
    (beginning-of-line)
    (insert \"X-Face:\")
    (insert-file-contents \"~/etc/XEmacs/xface\")))

(add-hook 'message-mode-hook 'mc-install-write-mode)
;(add-hook 'message-send-hook 'dka-insert-xface)
(add-hook 'message-send-hook 'ispell-message)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
