;; display-time stuff
(setq display-time-day-and-date t)
(setq display-time-mail-file "/var/spool/mail/dalcorn")
(if (featurep 'xemacs)
    (progn
      ;;(setq display-time-mail-balloon-show-gnus-group t) 
      ;;(setq display-time-mail-balloon-suppress-gnus-group '("^junk"))
      ;;(balloon-help-mode)
      (setq display-time-ignore-read-mail t)
      (setq display-time-show-icons-maybe t)
      (setq display-time-form-list '(month "/" day " " 12-hours ":" minutes " " am-pm mail)))
  (setq display-time-format "%m/%d %l:%M%p")
  (setq display-time-use-mail-icon t)
  )
      
(display-time)
(line-number-mode 1)
(column-number-mode 1)

(setq display-time-string-forms
      ;; need to override this to remove load meter
      '((format-time-string display-time-format now)))

(defvar gnus-important-level 1
  "When checking for \"important\" mail, what levels are
  considered important?")

(defun gnus-new-important-mail (&optional level)
  "Return t there is any new mail/news in groups at or below
  level LEVEL (defaults to gnus-important-group-level)."
  (if (featurep 'gnus)
      (progn
        (or level (setq level gnus-important-level))
        (let ((newmail nil))
          (save-window-excursion
            (if (and (file-exists-p display-time-mail-file)
                     (> (nth 7 (file-attributes display-time-mail-file)) 0)
                     (> (- (float-time (current-time))
                           (float-time 
                            (nth 5 (file-attributes display-time-mail-file)))) 60))
                (gnus-group-get-new-news level))
            (switch-to-buffer gnus-group-buffer)
            (gnus-group-list-groups level)
            (let ((groups (gnus-group-listed-groups)) group)
              (while (setq group (pop groups))
                (unless newmail
                  (gnus-group-goto-group group)
                  (setq newmail (> (gnus-group-group-unread) 0)))))
            (gnus-group-list-groups gnus-level-subscribed))
          newmail))))

;(setq display-time-mail-function 'gnus-new-important-mail)
;(setq display-time-mail-function (lambda () nil))
