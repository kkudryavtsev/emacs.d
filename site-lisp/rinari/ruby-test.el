(require 'which-func)
(require 'project-local-variables)

(defun ruby-is-spec() (string-match "_spec.rb$" buffer-file-name))
(defun ruby-is-test() (string-match "_test.rb$" buffer-file-name))
(defun ruby-is-cucumber() (string-match ".feature$" buffer-file-name))

(defun ruby-test-file (&optional args)
  "Run the entire file with the appropriate test runner"
  (interactive)
  (compile (ruby-rvm-compile (concat (buffer-file-name) args))))

(defun ruby-test-function(&optional args)
  "Try to determine the current context and just test that piece of this file"
  (interactive)
  (let* ((function-name (which-function))
         (func-spec
         (cond
          ((ruby-is-spec) (concat " --line " (number-to-string (line-number-at-pos))))
          ((ruby-is-cucumber) (concat ":" (number-to-string (line-number-at-pos))))
          (t (concat " --name \"/"
                     (or
                      ;; look for Class#method and just return the function name part
                      (and (string-match "#\\(.*\\)" function-name) (match-string 1 function-name))
                      ;; rely on which-function/imenu to find the test function name
                      function-name) "/\"")))))
    (compile (ruby-rvm-compile (concat (buffer-file-name) func-spec)))))

(defun ruby-run-spec(&optional args)
  "The actual compile command to run an individual rspec (either file or function)"
  (let ((ruby-compile-command (concat (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-run-cucumber(&optional args)
  "The actual compile command to run an individual cucumber (either file or function)"
  (let ((ruby-compile-command (concat (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-run-test(&optional args)
  "The actual compile command to run an individual rails test (either file or function)"
  (let ((ruby-compile-command (concat (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-rvm-compile(command)
  (let ((log-file (if (ruby-is-cucumber) "cucumber.log" "test.log"))
        (runner (cond
                 ((ruby-is-spec) "rspec --no-color -Ispec ")
                 ((ruby-is-cucumber) "cucumber --no-color ")
                 (t "ruby -Itest ")))
        (bundle (if (file-exists-p (concat (rails-root) "Gemfile")) "bundle exec "))
        (rvm (if (file-exists-p (concat (rails-root) ".rvmrc")) "source .rvmrc; ")))
  (concat "cd " (rails-root) "; " rvm "rm -f log/" log-file "; " rvm bundle runner command)))

(provide 'ruby-test)

;;; All this below here is to dynamically rename the *complation* buffer with each run
;;; I found this a little annoying and so have turned it off
(defun comp-mult-name (mjr-mode)
  "Suitable for assignment to compilation-buffer-name-function"
  (concat "*" (downcase mjr-mode)
          " " (file-name-nondirectory
               (substring (if (buffer-file-name)
                              (file-name-directory (buffer-file-name))
                            (expand-file-name default-directory))
                          0 -1)) "*"))

(setq
 ;; Usually compile the same way so don't ask unless I give prefix arg
 compilation-read-command nil
 compilation-buffer-name-function	nil)
