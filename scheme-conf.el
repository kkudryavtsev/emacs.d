(setq scheme-program-name "/opt/local/bin/scheme")

(require 'xscheme)
(defun jw-scheme-send-expression ()
    (interactive)
    (move-end-of-line 1)
    (advertised-xscheme-send-previous-expression))

(add-hook 'scheme-interaction-mode-hook
            '(lambda ()
              (define-key scheme-interaction-mode-map [S-return]
                 'jw-scheme-send-expression)))
