(load (concat my-config-dir "site-lisp/nxhtml/autostart.el"))

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))
