(defun dka-nxml-fix-dabbrev ()
  (interactive)
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[<>=\"\/]")
  (setq dabbrev-abbrev-char-regexp "\\sw"))

(add-hook 'nxml-mode-hook 'dka-nxml-fix-dabbrev)
