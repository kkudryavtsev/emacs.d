(setq mac-option-modifier 'meta
      mac-command-modifier 'alt
      mac-pass-command-to-system t
      mac-pass-control-to-system nil
      focus-follows-mouse nil)
(require 'mac-key-mode)
(mac-key-mode)

(define-key mac-key-mode-map (kbd "A-`") 'other-frame)
(define-key mac-key-mode-map (kbd "A-n") 'make-frame-command)
(define-key mac-key-mode-map (kbd "A-w") 'delete-frame)
(define-key mac-key-mode-map (kbd "A-p") 'pdf-preview-buffer)
(define-key mac-key-mode-map (kbd "M-A-p") 'pdf-preview-buffer-with-faces)
(define-key mac-key-mode-map (kbd "A-t") 'peepopen-goto-file-gui)
;(define-key mac-key-mode-map (kbd "A-t") 'textmate-goto-file)
(define-key mac-key-mode-map (kbd "A-l") 'goto-line)
(define-key mac-key-mode-map (kbd "A-F") 'project-grep)

(require 'osx-plist)
;;(osx-plist-update-environment)
(let ((path "/Users/dalcorn/.rvm/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/binxs"))
  (mapc (lambda (dir)
	  (add-to-list 'exec-path dir))
	(parse-colon-path path)))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil)) ; use pipe
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
