(defun ruby-is-spec() (string-match "_spec.rb$" buffer-file-name))
(defun ruby-is-test() (string-match "_test.rb$" buffer-file-name))
(defun ruby-is-cucumber() (string-match ".feature$" buffer-file-name))

(defun ruby-test-file ()
  "Run the entire file with the appropriate test runner"
  (interactive)
  (cond
   (ruby-is-spec (ruby-run-spec))
   (ruby-is-cucumber (ruby-run-cucumber))
   (t (ruby-run-test))))

(defun ruby-test-function()
  "Try to determine the current context and just test that piece of this file"
  (interactive)
  (cond
   (ruby-is-spec (ruby-run-spec (concat " --line " (number-to-string (line-number-at-pos)))))
   (ruby-is-cucumber (ruby-run-cucumber (concat ":" (number-to-string (line-number-at-pos)))))
   (t (let* ((funname (which-function))
             (fn (or (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)) funname)))
        (ruby-run-test (concat " --name \"/" fn "/\"")))

(defun ruby-run-spec(&optional args)
  "The actual compile command to run an individual rspec (either file or function)"
  (let ((ruby-compile-command 
         (concat "rm -f log/test.log; bundle exec rspec  --no-color -Ispec " (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-run-cucumber(&optional args)
  "The actual compile command to run an individual cucumber (either file or function)"
  (let ((ruby-compile-command 
         (concat "rm -f log/cucumber.log; bundle exec cucumber  --no-color " (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-run-test(&optional args)
  "The actual compile command to run an individual rails test (either file or function)"
  (let ((ruby-compile-command (concat "rm -f log/test.log; ruby -Itest " (buffer-file-name) args)))
    (compile (ruby-rvm-compile ruby-compile-command))))

(defun ruby-rvm-compile(command)
  (let ((log-file (if (ruby-is-cucumber) "cucumber.log" "test.log"))
        (runner (cond
                 ((ruby-is-spec) "rspec --no-color -Ispec ")
                 ((ruby-is-cucumber) "cucumber --no-color ")
                 (t "ruby -Itest")))
        (bundle (if (file-exists-p (concat (rails-root) "Gemfile")) "bundle exec"))
        (rvm (if (file-exists-p (concat (rails-root) ".rvmrc")) "source .rvmrc; ")))
  (concat "cd " (rails-root) ";" rvm "rm -f log/" log-file ";" rvm bundle runner command)))

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
