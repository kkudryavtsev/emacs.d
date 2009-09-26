(require 'executable)

(let ((w3mmee (executable-find "w3mmee"))
      (mbconv (executable-find "mbconv"))
      (w3m (executable-find "w3m")))
  (if (and w3mmee mbconv)
      (setq w3m-command w3mmee)
    (setq w3m-command w3m)))

(require 'w3m)
(require 'w3m-search)
      
(setq w3m-use-cookies t)

(setq w3m-home-page "file:///Users/dalcorn/.w3m/bookmark.html")
(setq w3m-search-default-engine "google")
(setq w3m-form-use-fancy-faces nil)
(setq w3m-use-favicon t)
(setq w3m-icon-directory "~/.emacs.d/site-lisp/w3m-cvs/icons")

(add-to-list 'w3m-search-engine-alist
             '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s" nil))
(add-to-list 'w3m-search-engine-alist
             '("google-groups" "http://www.google.com/groups?q=%s" nil))
(add-to-list 'w3m-search-engine-alist
	     '("worldclock" "http://www.timeanddate.com/worldclock/results.html?query=%s"))
(add-to-list 'w3m-search-engine-alist
	     '("acronym" "http://www.acronymfinder.com/af-query.asp?acronym=%s&string=exact"))
(add-to-list 'w3m-search-engine-alist
	     '("rfc" "http://www.faqs.org/rfcs/rfc%s.html"))
(add-to-list 'w3m-search-engine-alist 
	     '("syndic8" "http://www.syndic8.com/feedlist.php?ShowStatus=all&ShowMatch=%s"))
(add-to-list 'w3m-search-engine-alist
	     '("weather" "http://www.weather.com/search/search?where=%s&what=WeatherLocalUndeclared"))

(add-to-list 'w3m-search-engine-alist
             '("cpan" "http://search.cpan.org/search?query=%s"))

(add-to-list 'w3m-search-engine-alist
             '("ebay" "http://search.ebay.com/search/search.dll?query=%s"))

;; Make the previous search engine the default for the next
;; search.

(defadvice w3m-search (after mph activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (member engine
                  (mapcar (lambda (elt)
                            (car elt))
                          w3m-search-engine-alist))
      (setq w3m-search-default-engine engine))))

(defun dka-w3m-goto-wiki ()
  (interactive)
  (let ((wiki-topic (read-input "Wiki:Topic? "))
        topic wiki wiki-list)
    (if (string-match "\\(.*\\):\\(.*\\)" wiki-topic)
        (setq wiki (match-string 1 wiki-topic) 
              topic (match-string 2 wiki-topic)
              wiki-list (assoc wiki dka-wikis))
      (error "Must specify a wiki name and topic (Wiki:TopicName)"))
    (unless wiki-list
      (error (format "\"%s\" is not a defined wiki" wiki)))
    (unless topic
      (setq topic (nth 2 wiki-list)))
    (w3m-goto-url (concat (nth 1 wiki-list) topic))))

(defun w3m-download-with-wget (loc)
  (interactive "DSave to: ")
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
        (let ((proc (start-process "wget" (format "*wget %s*" url)
                                   "wget" "--passive-ftp" "-nv" 
                                   "-P" (expand-file-name loc) url)))
          (with-current-buffer (process-buffer proc)
            (erase-buffer))
          (set-process-sentinel proc (lambda (proc str)
                                       (message "wget download done"))))
      (message "Nothing to get"))))

(defun w3m-feh-this-url ()
  (interactive)
  "Display the image at point in the w3m buffer using feh"
  (let ((url (w3m-image)))
    (if url
	(let ((proc (start-process "feh" (format "*feh %s*" url)
				   "feh" url)))
	      (with-current-buffer (process-buffer proc)
		(erase-buffer))
	      (set-process-sentinel proc (lambda (proc str)
					   (message "feh display done"))))
      (message "Nothing to view"))))


(let ((map (make-keymap)))
  (suppress-keymap map)
  (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
  (define-key map [delete] 'w3m-scroll-down-or-previous-url)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-left-tab)] 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [(shift return)] 'w3m-view-this-url-new-session)
    (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
    (define-key map [(button2)] 'w3m-mouse-view-this-url)
    (define-key map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
    (define-key map " " 'scroll-up)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "A" 'w3m-antenna)
    (define-key map "b" 'w3m-switch-buffer)
    (define-key map "c" 'w3m-print-this-url)
    (define-key map "C" 'w3m-print-current-url)
    (define-key map "d" 'w3m-download)
    (define-key map "D" 'w3m-download-this-url)
    (define-key map "f" 'w3m-feh-this-url)
    (define-key map "g" 'w3m-goto-url)
    (define-key map "G" 'w3m-goto-url-new-session)
    (define-key map "h" 'describe-mode)
    (define-key map "H" 'w3m-gohome)
    (define-key map "I" 'w3m-toggle-inline-images)
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "M" 'w3m-view-url-with-external-browser)
    (define-key map "n" 'w3m-view-next-page)
    (define-key map "N" 'w3m-namazu)
    (define-key map "o" 'w3m-history)
    (define-key map "O" 'w3m-db-history)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "s" 'w3m-search)
    (define-key map "S" (lambda ()
                          (interactive)
                          (let ((current-prefix-arg t))
                            (call-interactively 'w3m-search))))
    (define-key map "u" 'w3m-view-parent-page)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "W" 'w3m-weather)
    (define-key map "=" 'w3m-view-header)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "?" 'describe-mode)
    (define-key map ">" 'scroll-left)
    (define-key map "<" 'scroll-right)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
    (define-key map "\C-c\C-n" 'w3m-next-buffer)
    (define-key map "\C-c\C-p" 'w3m-previous-buffer)
    (setq dka-w3m-map map))

(defun dka-w3m-textarea-hook()
  (save-excursion
    (while (re-search-forward "\r\n" nil t)
      (replace-match "\n" nil nil))
    (delete-other-windows)))

(add-hook 'w3m-mode-hook '(lambda () (use-local-map dka-w3m-map)))
(add-hook 'w3m-form-input-textarea-mode-hook 'dka-w3m-textarea-hook)

;(add-hook 'w3m-form-input-textarea-set-hook
;          '(lambda () 
;             (save-excursion
;               (while (re-search-forward "\n" nil t)
;                 (replace-match "\r\n" nil nil)))))

(fset 'mailman-discard-next
   [?\C-s ?D ?i ?s ?c ?a ?r ?d ?\C-m down down left left left return])

(defun w3m-cookie-remove-version ()
  (interactive)
  (mapcar 
   (lambda (cookie) 
     (if (string= (w3m-cookie-name cookie) "Version") 
	 (w3m-cookie-remove (w3m-cookie-domain cookie)
			    (w3m-cookie-path cookie)
			    (w3m-cookie-name cookie))))
   w3m-cookies))


(defun w3m-erase-authinfo-root (root)
  (setq w3m-process-authinfo-alist
        (assq-delete-all 
         nil (mapcar 
              (lambda (elem) (if (not (equal root (car elem))) elem)) 
              w3m-process-authinfo-alist))))

(defun w3m-forget-authinfo ()
  (interactive)
  (let* ((roots (mapcar
                 (lambda (elem) (list (car elem) (car elem)))
                 w3m-process-authinfo-alist))
         (root (completing-read "URL: " roots nil t)))
    (w3m-erase-authinfo-root root)))
    
