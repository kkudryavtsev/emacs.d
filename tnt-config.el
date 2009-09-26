(setq tnt-load-path "~/lisp/tnt-cvs")
(add-to-list 'load-path tnt-load-path)

(require 'tnt)
(setq tnt-recenter-windows nil)
(setq tnt-default-username "lathinet")
(setq tnt-default-password "super-secret")
(setq tnt-archive-conversations t)
(setq tnt-directory "~/data/instant-messaging")
;(setq tnt-beep-on-incoming-message (expand-file-name "~/.xemacs/clink.au"))
;(setq tnt-beep-on-visible-incoming-message (expand-file-name "~/.xemacs/clink.au"))
(setq tnt-sound-exec nil)
;(setq tnt-sound-exec nil)
(setq tnt-separator "\n")
(setq tnt-use-timestamps t)
(setq tnt-group-idle-buddies t)
(setq tnt-very-idle-minimum 7200)

;(setq tnt-html-tags-to-strip "<HTML>\\|</HTML>\\|<BODY[^>]*>\\|</BODY>\\|<FONT[^>]*>\\|</FONT>\\|<PRE>\\|</PRE>")

(defun dka-tnt-set-width ()
  "defun to be run in tnt-im-mode-hook to set the fill-column to be the widnow's width -5"
  (let ((width (- (window-width) 5)))
    (if (> width (current-fill-column))
	(set-fill-column width))))
(add-hook 'tnt-im-mode-hook 'dka-tnt-set-width)

(defun dka-tnt-make-href (string)
  (if (string-match "\\(http://[^ \t\n]+\\)" string)
      (replace-in-string string (regexp-quote (match-string 1 string)) (concat "<a href=\"" (match-string 1 string) "\">" (match-string 1 string) "</a>"))
    string))

(add-hook 'tnt-send-pre-hook 'dka-tnt-make-href)
(add-hook 'tnt-send-pre-hook 'dka-expand-wiki-in-string)
(add-hook 'tnt-send-pre-hook #'(lambda (string) 
				 (replace-in-string string "\\*\\([^*]+\\)\\*"
						    "<b>\\1</b>")))
(add-hook 'tnt-send-pre-hook #'(lambda (string) 
				 (replace-in-string string " _\\([^_]+\\)_ "
						    " <i>\\1</i> ")))
(add-hook 'tnt-send-pre-hook #'(lambda (string) (replace-in-string string "<" "&lt;")))
(add-hook 'tnt-send-pre-hook #'(lambda (string) (replace-in-string string ">" "&gt;")))
;(setq tnt-send-pre-hook nil)
