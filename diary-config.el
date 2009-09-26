;; Diary and Calendar stuff
;(setq view-diary-entries-initially t)
;(setq view-calendar-holidys-initially t)
(if (not (featurep 'calendar))(require 'calendar))
(setq holiday-hebrew nil holiday-islamic nil 
      holiday-rosh-hashanah-etc nil holiday-advent nil
      holiday-hanukkah nil holiday-tisha-b-av-etc nil)
(setq number-of-diary-entries [7 5 4 3 2 4 1])
(setq mark-diary-entries-in-calendar nil)
(setq mark-holidays-in-calendar t)
(setq calendar-today-marker 'calendar-today-face)
(defun calendar-week-number ()
  (interactive)
  (if (not (and (featurep 'calendar) (member (current-buffer) (calendar-buffer-list))))
      (error "must be in a calendar buffer"))
  (let* ((day-num (calendar-day-number (calendar-cursor-to-date)))
	   (week-num (/  day-num 7)))
      (when (interactive-p) (message "cursor is on week number %d" week-num))
      week-num))
(define-key calendar-mode-map [p w] 'calendar-week-number)
(setq diary-file "~/.emacs.d/diary")
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(add-hook 'diary-display-hook 'fancy-diary-display)
(remove-hook 'diary-display-hook 'fancy-diary-display-week-graph)
(add-hook 'diary-display-hook 'sort-diary-entries)
;;; Appointments
;(if (not (featurep 'appt))(require 'appt))
;(when (featurep 'xemacs)
;    (appt-initialize))
;; (setq appt-msg-countdown-list '(10 5 1)
;;       appt-audible (cons 3 .5)
;;       appt-check-time-syntax nil
;;       appt-announce-method 'appt-persistent-message-announce
;;       appt-display-duration 25)
;; (defun diary-save-hook ()
;; "Stuff to do when saving the diary files."
;;   (appt-initialize))
;; (defun add-diary-save-hook ()
;;   "find-file-hooks hook to add the diary-save-hook when appropriate"
;;   (when (string-match "diary" (buffer-name))
;; 	 (add-hook 'after-save-hook 'diary-save-hook)))
;(add-hook 'find-file-hooks 'add-diary-save-hook)


