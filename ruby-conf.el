;;(require 'rails)
(require 'compile)
;; from JW 
;; (add-to-list 'compilation-error-regexp-alist
;;              '("^\\(Failure\\|Error\\) occurred in .*\\[\\([^:]+\\):\\([0-9]+\\)\\]" 2 3))
;(add-to-list 'compilation-error-regexp-alist 
;             '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
;; after much consternation, something is getting a bad regexp into this list that breaks matching on ruby tests.  

(defun toggle-ruby-compilation-mode ()
     "Toggle between inferior-ruby-mode and compilation-mode"
     (interactive)
     (cond
      ((string-equal major-mode "compilation-mode") 
       (progn (goto-char (point-max)) (toggle-read-only -1) (inferior-ruby-mode) ))
      ((string-equal major-mode "inferior-ruby-mode") (compilation-mode))))

(require 'rinari)
(require 'ruby-test)
;(load-file (concat my-config-dir "dka-ruby-snippets.el"))

(require 'rails-shoulda)
(setq which-func-functions '(rails-shoulda:current-context))

;; Thanks PragDave:
(defun ruby-xmp-region (reg-start reg-end)
  "Pipe the region through Ruby's xmp utility and replace
     the region with the result."
  (interactive "r")
  (shell-command-on-region reg-start reg-end
			   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

;; touch the right files to make autotest re-run the right stuff
(defun ruby-autotest-run-all ()
  (interactive)
  (save-excursion
    (rinari-find-config-file "config/environment.rb")
    (set-buffer-modified-p t)
    (save-buffer)))

(defun ruby-autotest-run-file ()
  (interactive)
  (save-window-excursion
    (switch-to-buffer 
     (get-buffer (ido-completing-read 
                  "Enter buffer to run tests for: " 
                  (mapcar (lambda (buffer) (buffer-name buffer)) (buffer-list))
                  nil t nil nil (buffer-name (current-buffer)))))
    (set-buffer-modified-p t)
    (save-buffer)))

(setq jw-test-warnings nil)

(defun ansi-colorize-buffer()
  "Apply ansi colors to the entire buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun ansi-filter-buffer()
  "Filter all ansi color sequences from the buffer"
  (interactive)
  (ansi-color-filter-region (point-min)(point-max)))

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

;; from an older ruby-mode that's no longer used?
(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)

(mapcar '(lambda (binding) (add-to-list 'auto-mode-alist binding))
        '(("\\.rb$" . ruby-mode)
          ;; ("\\.rhtml$" . rhtml-mode)
          ;; ("\\.erb" . rhtml-mode)
          ("\\.rake$" . ruby-mode)
          ("Gemfile$" . ruby-mode)
          ("\\.builder" . ruby-mode)
          ("\\.rjs" . ruby-mode)))

