(require 'nxml-mode)

(defun dka-nxml-fix-dabbrev ()
  (interactive)
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[<>=\"\/]")
  (setq dabbrev-abbrev-char-regexp "\\sw"))

(add-hook 'nxml-mode-hook 'dka-nxml-fix-dabbrev)

(require 'rng-loc)

(add-to-list 'rng-schema-locating-files (concat my-config-dir "schemas/schemas.xml"))

(mapcar '(lambda (binding) (add-to-list 'auto-mode-alist binding))
        '(("\\.xml" . nxml-mode)
          ("\\.xsd" . nxml-mode)))
