;;;
;;;  Rinari
;;;
;;;  Rinari Is Not A Rails IDE
;;;  (c) 2006 Phil Hagelberg, Forrest Chang, Ryan Davis, Paul Stickne, and others
;;;  
;;; http://rinari.rubyforge.org
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ruby-mode)
(require 'inf-ruby)
(require 'rhtml-mode)

(require 'cl)
(require 'toggle)
(require 'which-func)
;(require 'snippet)

;(require 'rinari-abbrevs)
;(require 'ruby-test)

;;; helpers for rinari-find-view
(defun rinari-name-components (name)
  (let ((case-fold-search nil))
	(labels ((rnc (in)
			(let ((ind (string-match "\\([A-Z][a-z0-9]+\\)\\([A-Z:]\\)" name in)))
			  (if (eq ind nil)
			      nil
			    (cons (concat
                                   (downcase (match-string 1 name))
                                   (and (equal (match-string 2 name) ":") "/")
                                                                 ) (rnc (match-end 1)))))))
          (rnc 0))))

(defun rinari-make-dirname (comps)
  (reduce #'(lambda (str next) (concat str (if (string-match "/$" str)
                                               next
                                           (concat "_" next)))
                                                             ) comps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view toggling

(defun rinari-find-view ()
  (interactive)
  (let* ((funname (which-function))
 	 (cls (rinari-make-dirname (rinari-name-components funname)))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (railsdir (rails-root (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat railsdir "/app/views/" cls "/" fn ".rhtml"))))

(defvar rinari-config-files
  '("config/environment.rb"
    "config/database.yml"
    "config/routes.rb"
    "config/deploy.rb"
    "db/schema.rb"))

(defun rinari-find-config-file (file)
  (interactive (list (if (functionp 'ido-completing-read)
			 (ido-completing-read "Find config file: " rinari-config-files)
			 (completing-read "Find config file: " rinari-config-files))))
  (find-file (concat (rails-root) file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rake

(defvar rake-task-list nil "List of the rake tasks for a given project")
(defun rake-reset-tasks()
  (interactive)
  (setq rake-task-list nil))
(defun rake-tasks()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (if rake-task-list
       rake-task-list
     (setq rake-task-list (delq nil (mapcar '(lambda(line)
			(if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
		     (split-string (shell-command-to-string (ruby-rvm-compile "rake -T")) "[\n]"))))))

(defun ruby-rake (task)
  "Run a rake command for the current project using compilation mode"
  (interactive (list (ido-completing-read "Run rake task: " (rake-tasks))))
  (compile (ruby-rvm-compile (concat "rake " task))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; show-schema

(defun rinari-show-schema ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "set_table_name\\s['\"]\\([^'\"]+\\)" nil t))
  (let ((table-name (or (match-string 1)
			(substring (buffer-name) 0 -3))))
    (find-file (concat (rails-root) "/db/schema.rb"))
    (beginning-of-buffer)
    (search-forward-regexp (concat "create_table \"" table-name))))

;; rinari-find-by-context passes (action) or (controller action) so if the
;; second argument is present, it is the action, otherwise the first argument
;; is the action
(defun rinari-find-action (action-or-controller &optional action)
  (if action
      (progn
        (find-file (concat (rails-root) "/app/controllers/" action-or-controller "_controller.rb"))
        (setq action-or-controller action)
        ))
  (beginning-of-buffer)
  (search-forward-regexp (concat "def *" action-or-controller))
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (if (equal dir  "/")
	nil
      (rails-root (expand-file-name (concat dir "../"))))))

(defun rinari-console ()
  (interactive)
  (run-ruby (concat (rails-root) "/script/console")))

(define-key ruby-mode-map "\C-c\C-s" 'rinari-console)
(define-key ruby-mode-map "\C-c\C-v" (lambda () (interactive) (toggle-buffer 'rails-view)))
(define-key ruby-mode-map "\C-c\C-t" 'toggle-buffer)
(define-key ruby-mode-map "\C-c\C-r" 'ruby-rake)
(define-key ruby-mode-map "\C-c\C-g" 'rinari-get-path)
(define-key ruby-mode-map "\C-c\C-f" 'rinari-find-config-file)
(define-key ruby-mode-map "\C-c\C-b" 'rinari-find-by-context)
(define-key ruby-mode-map "\C-x\C-\M-F" 'find-file-in-project)
(define-key ruby-mode-map "\C-c\C-\M-t" 'ruby-test-file)
(define-key ruby-mode-map (kbd "C-c C-S-t") 'ruby-test-function)
(define-key ruby-mode-map (kbd "C-c C-M-t") (lambda nil (interactive) (compile (concat "ruby " (file-name-nondirectory buffer-file-name)))))

(defun rinari-get-path (path)
  (interactive "MPath: ")
  (switch-to-buffer (concat "rails-" path))
  (insert (shell-command-to-string 
           (concat (rails-root) 
                   "/script/runner \"app = ActionController::Integration::Session.new; app.get '"
                   path 
                   "'; puts app.response.body\"")))
  (html-mode)
  ;; work around the fact that it inserts some random testing output
  (kill-region (search-backward "Loaded suite") (point-max)))

(provide 'rinari)
