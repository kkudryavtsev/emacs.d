;; (defun my-ruby-compile-hook ()
;;   (add-to-list 'compilation-error-regexp-alist 
;; 							 '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
;;   (add-to-list 'compilation-error-regexp-alist 
;; 							 '("^ *\\[?\\([^:\\n\\r]+\\):\\([0-9]+\\):in" 1 2))
;;   (setq compile-command "rake"))

;;(add-hook 'ruby-mode-hook 'my-ruby-compile-hook)

;; run the current test function

(defun ruby-test-function ()
  "Test the current ruby function (must be runable via ruby <buffer> --name <test>)."
  (interactive)
  (if (ruby-is-spec)
      (ruby-run-spec (concat " --line " (number-to-string (line-number-at-pos))))
    (if (ruby-is-cucumber)
        (ruby-run-cucumber (concat ":" (number-to-string (line-number-at-pos))))
    (let* ((funname (which-function))
           (fn (or (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)) funname)))
      (ruby-run-test (concat " --name \"/" fn "/\""))))))

(defun ruby-is-spec() (string-match "_spec.rb$" buffer-file-name))
(defun ruby-is-test() (string-match "_test.rb$" buffer-file-name))
(defun ruby-is-cucumber() (string-match ".feature$" buffer-file-name))
(defun ruby-test-file ()
  (interactive)
  (if (ruby-is-test)
      (ruby-run-test)
    (if (ruby-is-spec)
        (ruby-run-spec)
      (if (ruby-is-cucumber)
          (ruby-run-cucumber)
        (toggle-buffer)
        (ruby-run-test)
        (toggle-buffer)))))

(defun ruby-run-spec(&optional args)
  "The actual compile command to run an individual rspec (either file or function)"
  (let ((ruby-compile-command 
         (concat "rm log/test.log; bundle exec rspec  --no-color -Ispec " (buffer-file-name) args))
        (current-buffer (current-buffer)))
    (compile (ruby-rvm-compile ruby-compile-command))
    ))

(defun ruby-run-cucumber(&optional args)
  "The actual compile command to run an individual cucumber (either file or function)"
  (let ((ruby-compile-command 
         (concat "rm log/cucumber.log; bundle exec cucumber  --no-color " (buffer-file-name) args))
        (current-buffer (current-buffer)))
    (compile (ruby-rvm-compile ruby-compile-command))
    ))

(defun ruby-run-test(&optional args)
  "The actual compile command to run an individual rails test (either file or function)"
  (let ((ruby-compile-command (concat "rm log/test.log; ruby -Itest " (buffer-file-name) args))
        (current-buffer (current-buffer)))
  (save-window-excursion
    (save-excursion
      (save-some-buffers (not compilation-ask-about-save) nil)
      (dired (rails-root))
      (compile (ruby-rvm-compile ruby-compile-command))
      (dired (rails-root))
      (bury-buffer)))))

(defun ruby-rvm-compile(command)
  (concat "cd " (rails-root) ";"
          (if (file-exists-p (concat (rails-root) ".rvmrc"))
              (concat " source .rvmrc;"))
          command))

(defun autotest ()
  (interactive)
  (let ((buffer (shell (concat "cd " (rails-root) ";autotest -rails"))))
    (compilation-shell-minor-mode)
    (define-key shell-mode-map "\C-c\C-a" 'autotest-switch)
    (comint-send-string buffer "autotest\n")))

(provide 'ruby-test)
