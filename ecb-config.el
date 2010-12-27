(load-file (concat my-config-dir "site-lisp/" "cedet-1.0pre7/common/cedet.elc"))
(global-ede-mode 1)
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(require 'ecb)

;(ecb-tip-of-the-day nil)

(add-hook 'ecb-activate-hook
          (lambda ()
            (let ((compwin-buffer (ecb-get-compile-window-buffer)))
              (if (not (and compwin-buffer
                            (ecb-compilation-buffer-p compwin-buffer)))
                  (ecb-toggle-compile-window -1)))))

(ecb-activate)
