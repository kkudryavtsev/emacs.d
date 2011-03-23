(load (concat my-config-dir "site-lisp/nxhtml/nxhtml/autostart.el"))

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))

(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

(require 'whattf-dt)
