;;(require 'rails)
(require 'compile)
;; from JW 
;; (add-to-list 'compilation-error-regexp-alist
;;              '("^\\(Failure\\|Error\\) occurred in .*\\[\\([^:]+\\):\\([0-9]+\\)\\]" 2 3))
;(add-to-list 'compilation-error-regexp-alist 
;             '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
;; after much consternation, something is getting a bad regexp into this list that breaks matching on ruby tests.  
(setq compilation-error-regexp-alist nil)
(add-to-list 'compilation-error-regexp-alist
             '("\\([^ \t:\[]+\\):\\([0-9]+\\):in" 1 2))
(setq compilation-mode-font-lock-keywords nil)
(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(test[^\(]+\\)" (1 font-lock-function-name-face)))
(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(test[^\(]+\\)(\\([^\)]+\\))" (2 font-lock-constant-face)))
(add-to-list 'compilation-mode-font-lock-keywords
             '("Started\n[.EF]*\\([EF]\\)[.EF]*\nFinished" (1 compilation-error-face)))
(add-to-list 'compilation-mode-font-lock-keywords 
             '("\\([1-9][0-9]* \\(failures\\|errors\\)\\)" (1 compilation-error-face)))
(add-to-list 'compilation-mode-font-lock-keywords 
             '("^[ \t]*\\(.*, 0 failures, 0 errors\\)" (1 compilation-info-face)))

 
(defun toggle-ruby-compilation-mode ()
     "Toggle between inferior-ruby-mode and compilation-mode"
     (interactive)
     (cond
      ((string-equal major-mode "compilation-mode") 
       (progn (goto-char (point-max)) (toggle-read-only -1) (inferior-ruby-mode) ))
      ((string-equal major-mode "inferior-ruby-mode") (compilation-mode))))

(defun snippet-insert-and-indent (this-snippet-string)
  (let ((start (point)))
    (snippet-insert this-snippet-string)
    (indent-region start (+ (point) 5 (length this-snippet-string)))))

;(setq erb-background "grey70") ; must do before rhtml-mode is loaded, i think

(require 'rinari)
(load-file (concat my-config-dir "dka-ruby-snippets.el"))
(require 'find-file-in-project)
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))
(require 'ri-ruby)
(require 'rails-shoulda)

(setq which-func-functions '(rails-shoulda:current-context))
(setq auto-mode-alist
      (append '(("\\.erb" . rhtml-mode) 
                ("\\.builder" . ruby-mode)
                ("\\.rjs" . ruby-mode)) auto-mode-alist))
(setq find-file-in-project-excludes '())
;(set-face-background 'erb-face "grey18")
;(set-face-background 'erb-delim-face "grey18")

;(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-ruby-mode-hook () nil)
;  (ruby-electric-mode)
;  (hs-minor-mode)
;  (if (= emacs-major-version 22) (reveal-mode))
;  (define-key ruby-mode-map "\C-\M-h" 'backward-kill-word) ; ruby-mode redefines this badly
;  (local-set-key (kbd "RET") 'ruby-reindent-then-newline-and-indent))


(when (= emacs-major-version 22)
;  (require 'ido)
;  (ido-mode t)
;  (ido-toggle-prefix)
  (file-name-shadow-mode t)
  (add-to-list 'hs-special-modes-alist
	       (list 'ruby-mode
		     (concat ruby-block-beg-re "\|{")
		     (concat ruby-block-end-re "\|}")
		     "#"
		     'ruby-forward-sexp nil)))

;; Thanks PragDave:

(defun ruby-xmp-region (reg-start reg-end)
  "Pipe the region through Ruby's xmp utility and replace
     the region with the result."
  (interactive "r")
  (shell-command-on-region reg-start reg-end
			   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

(defun ruby-autotest-run-all ()
  (interactive)
  (save-excursion
    (rinari-find-config-file "config/environment.rb")
    (set-buffer-modified-p t)
    (save-buffer)))

(defun ruby-autotest-run-file ()
  (interactive)
  (save-window-excursion
    (switch-to-buffer (get-buffer (ido-completing-read 
                                   "Enter buffer to run tests for: " 
                                   (mapcar (lambda (buffer) (buffer-name buffer)) (buffer-list))
                                   nil t nil nil (buffer-name (current-buffer)))))
    (set-buffer-modified-p t)
    (save-buffer)))

(define-key ruby-mode-map (kbd "A-t") 'file-file-in-project)

;; (define-key ruby-mode-map (kbd "C-c t r") 'ruby-autotest-run-all)
;; (define-key ruby-mode-map (kbd "C-c t u") 
;; 	(lambda () (interactive) (compile "rake test:units")))
;; (define-key ruby-mode-map (kbd "C-c t f") 'ruby-autotest-run-file)
;; (define-key ruby-mode-map (kbd "C-c t m") 'ruby-test-function)

(setq jw-test-warnings nil)

(defun ansi-colorize-buffer
  "Apply ansi colors to the entire buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun toggle-buffer-other-window (&optional mappings)
  "Opens a related file to the current buffer using matching
rules in the `other-window'. Matches the current buffer against
rules in toggle-mappings. If a match is found, switches to that
buffer."
  (interactive)
  (if (symbolp mappings) ; for toggling according to non-default mappings
      (setq mappings (toggle-style mappings)))
  (let ((mapping (toggle-filename (buffer-file-name) (or mappings toggle-mappings))))
    (if (stringp mapping)
				(find-file-other-window mapping)
      (if (functionp mapping)
					(funcall mapping (buffer-file-name))
				(message (concat "Match not found for " (buffer-file-name)))))))


(define-key ruby-mode-map (kbd "C-c 4 t") 'toggle-buffer-other-window)
(define-key ruby-mode-map (kbd "C-c t") 'toggle-buffer)

(defun ruby-reindent-then-newline-and-indent ()
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (indent-according-to-mode))
