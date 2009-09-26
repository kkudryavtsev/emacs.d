;; require this first so diary isn't loaded without support defuns
(require 'cal-desk-calendar)

(mapc 'require '(appt
                 emacs-wiki
                 planner
                 planner-id
                 planner-bbdb
                 planner-cyclic
                 planner-diary
                 planner-gnus
                 planner-w3m
                 planner-multi
                 remember
                 remember-planner))

(planner-install-extra-task-keybindings)

;; insinuations
(planner-insinuate-calendar)
(planner-insinuate-diary)
(planner-gnus-insinuate)

(setq
 appt-display-diary nil
 appt-display-duration 3600
 appt-display-format 'window

 diary-default-schedule-start-time 900
 diary-default-schedule-stop-time 1700
 diary-schedule-interval-time 60

 list-diary-entries-hook 'sort-diary-entries

 planner-carry-tasks-forward t
 planner-cyclic-diary-file "~/diary.tasks"
 planner-diary-string "* Schedule"
 planner-diary-use-diary t
 planner-use-other-window nil
 planner-tasks-file-behavior 'save
 planner-sort-tasks-key-function 'planner-sort-tasks-by-description

 planner-id-update-automatically t
 remember-handler-functions '(remember-planner-append)
 remember-annotation-functions planner-annotation-functions
 remember-save-after-remembering t
 remember-append-to-planner-hook '(remember-planner-add-timestamp
                                   remember-planner-add-xref)
 )

(defun planner-sort-tasks-by-description ()
  "Sort tasks by status, priority and link."
  (let ((info (planner-current-task-info)))
    (concat ;(read (current-buffer))
     (cond
      ((string= (planner-task-status info) "P") "1")
      ((string= (planner-task-status info) ">") "2")
      ((string= (planner-task-status info) "X") "3")
      ((string= (planner-task-status info) "C") "4")
      (t "0"))
     (planner-task-priority info)
     (or (planner-task-description info) ""))))

;(custom-set-faces
; (my-face 'planner-high-priority-task-face "red")
; (my-face 'planner-medium-priority-task-face "dark orange")
; (my-face 'planner-low-priority-task-face "orange"))

;(global-set-key (kbd "<f9>") 'planner-create-task-from-buffer)
;(global-set-key (kbd "<f10>") 'remember-buffer)
;(global-set-key (kbd "<f11>") 'remember-region)

(add-hook 'diary-display-hook 'fancy-schedule-display-desk-calendar t)
(add-hook 'diary-display-hook 'fancy-diary-display t)
(add-hook 'planner-mode-hook 'emacs-wiki-use-font-lock)

(defun my-appt-disp-planner (min-to-app new-time appt-msg)
  "display today's planner page with the upcoming appointment highlighted"
  ;; sometimes appt-msg will have newlines; if so, remove them
 (if (string-match "\\(.+\\)[\r\n]" appt-msg)
      (setq appt-msg (match-string 1 appt-msg)))
  (planner-goto-today)
  (save-excursion
    (when (search-forward-regexp (format "^.*%s" appt-msg))
      (let ((overlay (make-overlay (match-beginning 0) (1+ (match-end 0)))))
        (overlay-put overlay 'face '((:background "bisque")))
        (run-at-time 600 nil 'funcall 'delete-overlay overlay)))))
(setq appt-disp-window-function 'my-appt-disp-planner)
(setq appt-delete-window-function nil)

;(plan)

(appt-activate 1)
