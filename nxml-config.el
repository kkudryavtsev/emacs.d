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

(defun my-nxml-mode-hook ()
  "Functions to run when in nxml mode."
  (setq nxml-sexp-element-flag t)
  (hs-minor-mode 1)) 

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(eval-after-load "hideshow.el"
  (let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1) 
                                       "^\\s-*</.*>\\s-*$")))
    (when (not (member nxml-mode-hs-info hs-special-modes-alist))
      (setq hs-special-modes-alist
            (cons nxml-mode-hs-info hs-special-modes-alist)))))
