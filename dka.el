(defun dka-delta-time (start delta &optional format)
  "Returns the time/date stamp of the START date plus the DELTA"
  (interactive "sStart date (current)\nsDelta (HH:MM:SS)")
  (let* ((start-date (if (equal start "")
                         (current-time)
                       (date-to-time start)))
        (delta-list (split-string delta ":"))
        (delta-date (+ (* (string-to-int (car delta-list)) 3600) 
                       (* (string-to-int (cadr delta-list)) 60)
                       (string-to-int (caddr delta-list))))
        (format-str (or format "%d %b %y %r")))
    (message (format "%s plus %s is %s" (current-time-string start-date) delta (format-time-string format-str (time-add start-date (seconds-to-time delta-date)))))))


(defun outlook-invitation-to-diary ()
  (interactive)
  (let ((buffer (or (get-buffer "*Article*") (current-buffer)))
	date time subject diary-buffer)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (when (re-search-forward "^Subject: \\(.*\\)$")
	(setq subject (match-string 1))
	(when (re-search-forward "^When: \\w+, \\(\\w+ [0-9]\\{1,2\\}, [0-9]\\{4\\}\\) \\(.*?\\)(")
	  (setq date (match-string 1) time (match-string 2))
	  (setq diary-buffer (find-file-noselect diary-file))
	  (set-buffer diary-buffer)
	  (goto-char (point-max))
	  (insert (format "\n%s\n %s %s\n" date time subject))
	  (save-buffer)
	  (bury-buffer diary-buffer))))
      (message "%s %s %s" date time subject)))


(defun context-kill (arg)
  "Kill buffer, taking gnuclient into account."
  (interactive "p")

  (when (and (buffer-modified-p)
             (not (string-match "\\*.*\\*" (buffer-name)))
             (= 1 arg))
    (if (y-or-n-p (format "<%s> is modified, save " (buffer-name)))
        (save-buffer)))
  (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
      (gnuserv-edit)
    (let ((buffer-modified-p nil))
      (kill-buffer (current-buffer)))))

(defun dka-insert-time ()
  (interactive)
  (insert (format-time-string "%d %b %y - %I:%M%p")))

(defun dka-fix-comments ()
"Move through the entire buffer searching for comments that begin with
\"//\" and fill them appropriately.  That means if comments start too
close to the end of line (20 less than the fill-column) move the
entire comment on a line by itself."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "//")
      (lisp-indent-for-comment)
      ;; when the comment is deemed to be too close to the end of the
      ;; line, yank it and put it on the previous line before filling
      (while (< (- fill-column 20) (- (current-column) 3))
            (search-backward "//")
            (kill-line)
            (beginning-of-line)
            (yank)
            (insert "\n"))
      ;; now fill the lines that are too long
      (if (and (not (end-of-line))
               (< fill-column (current-column)))
          (c-fill-paragraph)))))

(defun gbvb-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)))

(defmacro make-double-command (name args doc-string interactive
                                    first-form second-form)
  (let ((int-form (if (not interactive)
                      '(interactive)
                    (list 'interactive interactive))))
    `(progn
       (defun ,name ,args ,doc-string
         ,int-form
         (if (eq last-command this-command)
             ,(if (and (listp second-form) (> (length second-form) 1))
                  (cons 'progn second-form)
                second-form)
           ,first-form)))))
(put 'make-double-command 'lisp-indent-function 2)

(make-double-command my-home ()
  "Go to beginning of line, or beginning of buffer." 
  nil
  (beginning-of-line)
  (beginning-of-buffer))

(defun dka-lpr-buffer (arg)
  "Print ARG copies of just the region if it is active.
Print ARG copies of text bewteen point-min and point-max is it is not.
Originially developed by t901353@goodyear.com (Tom Wurgler) and posted 
to comp.emacs.sources."
  (interactive "p")
  (let* ((sys_type (if (string-match "lpr" lpr-command) "-#" "-n"))
         (lpr-switches
          (if (> arg 1)
              (cons (concat sys_type (int-to-string arg)) lpr-switches)
            lpr-switches)))
    (if mark-active
        (lpr-region (mark) (point))
      (lpr-region (point-min) (point-max)))))

(defun dired-xmms-play-mp3 (&optional arg)
  "A little function that plays marked files in a dired buffer with xmms.
Written by brakjoller@hotmail.com (Barman Brakjoller) and submitted to gnu.emacs.sources
on Dec 2 2001"
  (interactive)
  (let ((files nil))
    (progn
      (mapcar
       '(lambda (x) 
          (setq files
                (concat files " \"" x  "\"")))
       (dired-map-over-marks     
        (dired-get-filename) arg))
      (start-process-shell-command 
       "dired-play-mp3" "*dired-play-mp3*" "xmms" files))))

;; Example of how to bind this to M-m when opening dired:

(add-hook 'dired-load-hook
          (function (lambda ()
                      (define-key dired-mode-map "\M-m" 'dired-xmms-play-mp3))))

(defun dka-get-c++-function-name ()
  "This function should insert the function's name quoted.
 Note that this defun assumes a c++ syntax (i.e. function
 names have CLASS::FUNCTION form)"
  (interactive)
  (let (start)
    (save-excursion
      (beginning-of-defun)
      (search-backward "::")
      (search-backward-regexp "\\s-\\|\\*")(forward-char 1)
      (setq start (point))
      (search-forward-regexp "\\s-\\|(")(backward-char 1)
      (buffer-substring start (point)))))

(defun dka-insert-c++-function-name ()
  "This defun will insert the c++ function name that the point is
in at point"
  (interactive)
  (insert-string (dka-get-c++-function-name)))

(defun dka-loan-payment-calculator (amount rate years)
  "Calculate what the payments for a loan of AMOUNT dollars when
annual percentage rate is RATE and the term of the loan is
YEARS years.  The RATE should expressed in terms of the percentage 
\(i.e. \'8.9\' instead of \'.089\'\).  The total amount of
interest charged over the life of the loan is also given."
  (interactive "nLoan Amount: \nnAPR: \nnTerm (years): ")
  (let ((payment (/ (* amount (/ (/ rate 1.0) 1200)) (- 1 (expt (+ 1 (/ (/ rate 1.0) 1200)) (* years -12.0))))))
    (if (interactive-p)
        (message "%s payments of $%.2f. Total interest $%.2f" 
             (* years 12) payment (- (* payment years 12) amount))
      payment)))

(defun dka-loan-refinance-calculator (amount curr-rate curr-term new-rate new-term cost points)
  (interactive "nLoan Amount: \nnCurrent APR: \nnCurrent Term (years): \nnNew APR: \nnNew Term (years): \nnCost (fees): \nnPoints (percentage): ")
  (let* ((curr-payment (dka-loan-payment-calculator amount curr-rate curr-term))
         (new-payment (dka-loan-payment-calculator amount new-rate new-term))
         (total-cost (+ cost (* amount (/ points 100.0))))
         (payback (/ total-cost (- curr-payment new-payment))))
    (if (interactive-p)
        (if (> payback 0)
            (message "New payment is %s.  The payback is %s months." 
                     (dka-round-to-decimal new-payment 2)
                     (dka-round-to-decimal payback 1)))
      payback)))

(defun dka-round-to-decimal (value place)
  (let ((multiplier (* 1.0 (expt 10 place))))
    (/ (round (* multiplier value)) multiplier)))


(defun kill-whole-line (count)
  "Kill an entire line, or ARG lines if prefix specified"
  (interactive "*p")
  (beginning-of-line)
  (kill-line count))


(defun whack-whitespace (arg) (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "\\s-+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(global-set-key (kbd "C-c d") 'whack-whitespace)

(defun whack-typedefs ()
  (interactive)
  (let ((typedefs-buffer (or (get-buffer "typedefs.h")
			     (find-file-noselect "typedefs.h")))
	name typedefs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^typedef \\(struct\\|enum\\) ?\\(\\(?:\\w\\|\\s_\\)+\\)?[ \t\n]*{" nil t)
	(setq keyword (match-string 1))
	(setq name (match-string 2))
	(if name
	    (progn			;must be of the format "typedef struct foo_ {\n ... \n} foo;"
	     (when (equal (substring name -1) "_")
	       (setq name (substring name 0 -1)))
	     (replace-match (concat keyword " " (downcase name) " {") t nil)
	     (goto-char (- (point) 1))
	     (forward-sexp)
	     (kill-line)
	     (insert ";"))
	  ; must be of the format "typedef struct\n{ ... }\nfoo;
	  (goto-char (- (point) 1))
	  (forward-sexp)
	  (when (re-search-forward "\\(\\(?:\\w\\|\\s_\\)+\\);" nil t)
	    (setq name (match-string 1))
	    (kill-entire-line)
	    (goto-char (- (point) 1))
	    (insert ";")
	    (backward-sexp)
	    (delete-indentation)
	    (insert (concat " " (downcase name)))))
	(when name
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward (concat "\\([^_A-Za-z]\\)" name "\\([^_A-Za-z]\\)") nil t)
	      (replace-match 
	       (concat (match-string 1)
		       (when keyword
			 (concat keyword " "))
		       (downcase name) (match-string 2)) t nil)))
	  (with-current-buffer typedefs-buffer 
	    (insert (concat "typedef " 
			    (when (and keyword (equal keyword "struct"))
			      (concat "struct "))
			    (downcase name) " " name "\n")))
	  (setq name nil))))))

(defun celsius-to-fahrenheit (degrees)
  "Convert DEGREES from Celsius to Fahrenheit"
  (interactive "n")
  (let ((fahrenheit (+ (* degrees (/ 9.0 5.0)) 32.0)))
    (when (interactive-p)
      (message "%.1f degrees Celsius is %.1f degrees Fahrenheit" degrees fahrenheit))
    fahrenheit))

(defun fahrenheit-to-celsius (degrees)
  "Convert DEGREES from Fahrenheit to Celsius"
  (interactive "n")
  (let ((celsius (* (- degrees 32) (/ 5.0 9.0))))
    (when (interactive-p)
      (message "%.1f degrees Fahrenheit is %.1f degrees Celsius" degrees celsius))
    celsius))

(defun other-column (prefix)
  "Switch to the buffer in the verticle split to the right whose top
of window is closest to the top of the selected window.  With PREFIX
arg move to the left."
  (interactive "p")
  (let ((curr-col (car (window-pixel-edges)))
	(curr-top (cadr (window-pixel-edges)))
	(slop 25)
	(dir (if (not (eq prefix 1)) -1 1))
	next-col)
    (setq next-col
	  (cond
	   ((and (eq dir 1) (window-rightmost-p (selected-window))) 0)
	   ((and (eq dir -1) (window-leftmost-p (selected-window)))
	    (car (window-pixel-edges (frame-rightmost-window))))
	   (t
	    (while (or (and (eq dir 1) (<= (car (window-pixel-edges)) curr-col))
		       (and (eq dir -1) (>= (car (window-pixel-edges)) curr-col)))
	      (other-window dir))
	    (car (window-pixel-edges)))))
    (let ((smallest-delta (frame-pixel-height))
	  closest-window)
      (walk-windows 
       '(lambda (window)
	  (if (eq (car (window-pixel-edges window)) next-col)
	      (let ((this-delta (abs (- curr-top (cadr (window-pixel-edges window))))))
		(if (< this-delta smallest-delta)
		    (setq smallest-delta this-delta closest-window window))))))
      (select-window closest-window))))


(defun other-window-this-column (prefix)
  (interactive "p")
  (let ((window-list (get-column-window-list)))
    (select-window 
     (cond
      ((and (eq prefix 1)
	    (> (length window-list) 1))
       (cadr (get-column-window-list)))
      ((eq prefix 1) (car window-list))
      (t
       (car (last (get-column-window-list))))))))

(defun dka-sort-by-other-list (to-sort-list other-list)
  (let* ((index 0)
         (other-alist (mapcar (lambda (buffer) 
                                (setq index (+ index 1))
                                (cons buffer index))
                              other-list))
         (swartz (mapcar (lambda (item) 
                           (cons (cdr (assoc item other-alist)) item))
                         to-sort-list))
         (sorted-list (sort swartz (lambda (a b) (< (car a) (car b))))))
    (mapcar 'cdr sorted-list)))

(defun dka-jump-to-window ()
  "Interactively jump to another visible window based on it's `buffer-name' using `ido-completing-read'"
  (interactive)
  (let* ((visible-buffers (mapcar '(lambda (window) (window-buffer window)) (window-list)))
         (sorted-visible-buffers (dka-sort-by-other-list visible-buffers (buffer-list)))
         (rotated-buffer-list (rotate-list sorted-visible-buffers 1))
         (visible-buffer-names (mapcar (lambda (buffer) (buffer-name buffer)) rotated-buffer-list))
         (buffer-name (ido-completing-read "Enter buffer to jump to: " 
                                           visible-buffer-names
                                           nil t))
         (window-of-buffer
          (delq nil 
                (mapcar '(lambda (window) 
                           (if (equal buffer-name (buffer-name (window-buffer window)))
        window nil)) (window-list)))))
    (select-window (car window-of-buffer)))
)

(defun get-column-window-list (&optional side)
  (let ((column (if (eq side 'right) (caddr (window-pixel-edges))
		  (car (window-pixel-edges))))
	(current-window (selected-window))
	column-window-list)
    (mapc '(lambda (window)
	     (when (eq (car (window-pixel-edges window)) column)
	       (add-to-list 'column-window-list window)))
	  (window-list))
    (setq column-window-list 
	  (sort column-window-list
		'(lambda (a b) 
		   (< (cadr (window-pixel-edges a))
		      (cadr (window-pixel-edges b))))))
    (while (not (eq (car column-window-list) current-window))
      (setq column-window-list (rotate-list column-window-list 1)))
    column-window-list))

(defun rotate-list (list count)
  "Rotate the LIST by COUNT elements"
  (cond
   ((= count 0) list)
   ((not list) list)
   (t
    (rotate-list (nconc  (cdr list) (list (car list)) '()) (1- count)))))


(defvar winnav-prefix-char (kbd "C-c o")
  "Prefix character for using winnav commands")

(defvar winnav-map (make-sparse-keymap)
  "*Keymap for winnav commands.")
(global-set-key winnav-prefix-char winnav-map)
(cond
 ((define-key winnav-map "v"    'other-window-this-column)
  (define-key winnav-map "h"    'other-column)
  (define-key winnav-map "j"    'jump-to-window)
  (define-key winnav-map "o"    'other-window)
  (define-key winnav-map "t"    'transpose-windows)))

(defun dka-nnmail-split-history () "Generate an overview of where the
last mail split put articles.  \"Re-written\" to make the
*nnmail-split-history* buffer more temporary like XEmacs help
windows."
     (interactive)
     (gnus-group-get-new-news)
     ;; had to do my own implementation of nnmail-split-history
     ;; because of bad window configuration mangling
     (if (not nnmail-split-history)
	 (message "No current split history")
       (let* ((winconfig (current-window-configuration))
	      (was-one-window (one-window-p))
	      (buffer-name "*nnmail split history*")
	      (split-not-visible
	       (not (and (windows-of-buffer buffer-name)
			 (memq (selected-frame)
			       (mapcar 'window-frame (windows-of-buffer buffer-name))))))
	      (history nnmail-split-history)
	      elem)
	 (prog1
	     (with-output-to-temp-buffer buffer-name
	       (prog1
		   (while (setq elem (pop history))
		     (princ (mapconcat (lambda (ga)
					 (concat (car ga) ":" (int-to-string (cdr ga))))
				       elem
				       ", "))
		     (princ "\n"))
		 (save-excursion
		   (set-buffer standard-output)
		   (funcall mode-for-help)))))
	 (let ((splitwin (get-buffer-window buffer-name)))
	   (when splitwin
	     (when split-not-visible
	       (with-current-buffer (window-buffer splitwin)
		 (setq help-window-config winconfig)))
	     (when help-selects-help-window
	       (select-window splitwin)))))))

(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


(defvar no-word-coding-systems '(("greek-iso-8bit" . "8859-7.txt")
                                     ("iso-8859-7" . "8859-7.txt")
                                     ("iso-8859-1" . "8859-1.txt"))
      "Alist mapping coding system to antiword map file.")

(defun no-word ()
   "Run antiword on the entire buffer."
  (interactive)
  (let* ((no-word-coding (completing-read
			  "Select coding: (default iso-8859-1) "
			  no-word-coding-systems nil t nil nil "iso-8859-1"))
	 (map-file (cdr (assoc no-word-coding no-word-coding-systems)))
	 (coding-system-for-read (intern no-word-coding))
	 (new_name (concat "*" (buffer-name) "*")))
    (save-window-excursion
      (shell-command-on-region (point-min) (point-max) (format "antiword -m %s -"  map-file) new_name)  
      (kill-buffer (current-buffer)))
    (switch-to-buffer new_name)))

(defun gnus-summary-next-page-wrapper ()
  "If the current article has been replaced with a w3m buffer, scroll
that buffer one page.  Otherwise, call `gnus-summary-next-page' as
normal."
  (interactive)
  (if (not gnus-article-browse-urls-showing)
      (gnus-summary-next-page)
    (let ((this-window (selected-window))
	  (w3m-window (get-buffer-window "*w3m*")))
      (save-excursion
	(select-window w3m-window)
	(scroll-up))
      (select-window this-window))))

(defvar gnus-article-browse-urls-showing nil
  "State variable indicating whether the current `gnus-article-buffer'
has been replaced with a w3m buffer.")

(defun gnus-article-copy-urls ()
  "Copy a URL from the `gnus-article-buffer' by prompting from a list
of available URLs."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
	(urls (gnus-article-get-current-urls))
	(this-window (selected-window))
	(browse-window (or (get-buffer-window gnus-article-buffer)
			   ))
	(count 0))
    (save-excursion
      (save-window-excursion
	(set-buffer temp-buffer)
	(mapcar
	 (lambda (string)
	   (insert (format "\t%d: %s\n" count string))
	   (setq count (1+ count))) urls)
	(not-modified)
	(pop-to-buffer temp-buffer)
	(setq count (string-to-number (read-input "Copy which URL: ")))
	(kill-buffer temp-buffer))
      (kill-new (nth count urls)))
    (select-window this-window)))

(defun gnus-article-browse-urls ()
  "Visit a URL from the `gnus-article-buffer' by prompting via a
poping up a buffer showing the list of URLs found with the
`gnus-button-url-regexp'."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
	(urls (gnus-article-get-current-urls))
	(this-window (selected-window))
	(browse-window (or (get-buffer-window gnus-article-buffer)
			   ))
	(count 0))
    (save-excursion
      (save-window-excursion
	(set-buffer temp-buffer)
	(mapcar
	 (lambda (string)
	   (insert (format "\t%d: %s\n" count string))
	   (setq count (1+ count))) urls)
	(not-modified)
	(pop-to-buffer temp-buffer)
	(setq count (string-to-number (read-input "Browse which URL: ")))
	(kill-buffer temp-buffer))
      (if browse-window
	  (progn (select-window browse-window)
		 (and (setq gnus-article-browse-urls-showing t)
		      (browse-url (nth count urls))))))
    (select-window this-window)))
		      
(defun gnus-article-browse-urls-reset ()
  (setq gnus-article-browse-urls-showing nil))

(add-hook 'gnus-article-mode-hook 'gnus-article-browse-urls-reset)
(add-hook 'gnus-summary-exit-hook 'gnus-article-browse-urls-reset)

(defun gnus-article-get-current-urls ()
  "Return a list of the urls found in the current `gnus-article-buffer'"
  (let (url-list)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (setq url-list (gnus-article-get-urls-region (point-min) (point-max))))
    url-list))

(defun gnus-article-get-urls-region (min max)
  "Return a list of urls found in the region between MIN and MAX"
  (let (url-list)
    (save-excursion
      (save-restriction
	(narrow-to-region min max)
	(goto-char (point-min))
	(while (re-search-forward gnus-button-url-regexp nil t)
	  (let ((match-string (match-string-no-properties 0)))
	    (if (and (not (equal (substring match-string 0 4) "file"))
		     (not (member match-string url-list)))
		(setq url-list (cons match-string url-list)))))))
    url-list))

(defun gnus-article-html-renderer ()
  "Render article buffer with `htmlr-render' and list references at
end of message"
  (let ((url-list (gnus-article-get-urls-region (point-min) (point-max))))
    (goto-char (point-min))
    (htmlr-render)
    (mapcar (lambda (string)
	      (insert (format " * %s\n" string))) url-list)))

(defun dka-make-frame-by-name (position)
  "Makes a new frame with the X window property name POSITION."
  (interactive)
  (make-frame (list (cons name position))))

(defvar myframe-prefix-char (kbd "C-c 5")
  "Prefix character for using winnav commands")

(defvar myframe-map (make-sparse-keymap)
  "*Keymap for myframe commands.")
(global-set-key myframe-prefix-char myframe-map)
(cond
 ((define-key myframe-map "l"    '(lambda () (interactive) (make-frame '(name "left"))))
  (define-key myframe-map "r"    '(lambda () (interactive) (make-frame '(name "right"))))
  (define-key myframe-map "b"    '(lambda () (interactive) (make-frame '(name "bottom"))))))

(defun make-repeat-command (symbol command-list)
  "Command changes with each repetition.
SYMBOL is a symbol unique to this command.
Snagged from xemacs-beta mailing list from Adrian Kubala."
  (if (eq last-command symbol)
      (set symbol (+ (eval symbol) 1))
    (set symbol 0))
  (if (>= (eval symbol) (length command-list))
      (set symbol 0))
  (call-interactively (nth (eval symbol) command-list))
  (setq this-command symbol))


;(defun my-home ()
;  (interactive "_")
;  (make-repeat-command 'my-home '(beginning-of-line-text
;                                  beginning-of-line
;                                  beginning-of-buffer)))


(defun radioparadise-now-playing ()
  "Scrape from the http://www.radioparadise.com/nowplay_frame.php page the current song"
  (replace-in-string  
   (exec-to-string ". ~/bin/setproxy.sh && /sw/bin/lynx -dump http://radioparadise.com/")
   "\\(.\\|
\\)*Recently Played:.*?\\]\\([^\[\n]+\\)\\(.\\|
\\)*"
   "\\2"))

(defun radioparadise-insert-now-playing ()
  "Insert at point the song now playing on radioparadise.com"
  (interactive)
  (insert (concat (radioparadise-now-playing) " <http://www.radioparadise.com>")))

(defun radioparadise-message-now-playing ()
  (interactive)
  (message (radioparadise-now-playing)))

(defun http-get (url)
      "Get URL."
      (interactive "sURL: ")
      (let* (host dir file port buf command)
        (unless (string-match "http://\\([^/]+\\)/\\(.*/\\)?\\([^:]*\\)\\(:\\([0-9]+\\)\\)?" url)
          (error "Cannot parse URL %s" url))
        (setq host (match-string 1 url)
              dir  (match-string 2 url)
              file (match-string 3 url)
              port (or (match-string 5 url) 80)
              buf (get-buffer-create (format "*HTTP GET %s *" url)))
        (open-network-stream (concat "HTTP GET " url) buf host port)
        (switch-to-buffer buf)
        (erase-buffer)
        (setq command (format "GET /%s%s HTTP/1.1\nHost: %s\n\n" dir file host))
        (insert command)
        (process-send-string buf command)))

(defun dka-host-lives-p (hostname)
  (interactive "sHost: ")
  "Determine if HOSTNAME is \"alive\" by attempting to obtain a ssh connection"
  (let ((ping (shell-command-to-string (format "ssh %s echo -n 1" hostname))))
    (if (string= "1" ping)
	(if (interactive-p)
	    (message "%s is alive" hostname)
	  t)
      (if (interactive-p)
	  (message "cannot connect to %s" hostname)
	nil))))

(defun dka-get-hosts-alive-string ()
  "Return a string (bolded) of hosts from the dka-watch-hosts-list
that are not responding"
  (interactive)
  (let ((not-found 
	 (delq nil 
	       (mapcar (lambda (host)
			 (unless (dka-host-lives-p host)
			   host))
		       dka-watch-hosts-list))))
    (setq not-found
	  (mapcar 
	   (lambda (string)
	     (put-text-property 0 (length string) 'face 
				'((foreground . "red") bold) string)
	     string)
	   not-found))
    (message "%S" not-found)
    (setq dka-hosts-dead-string (mapconcat 'identity not-found ","))))

(defvar dka-hosts-dead-string "" 
  "List of hosts (with possible fontification) that are down")

(put 'dka-hosts-dead-string 'risky-local-variable t)
(or (memq 'dka-hosts-dead-string global-mode-string)
    (setq global-mode-string (append global-mode-string '(dka-hosts-dead-string))))

(defvar dka-watch-hosts-list '("daphne.lathi.net" "foo")
  "List of hostnames to monitor with ssh connections")

(if (not (fboundp 'replace-in-string))
    (defun replace-in-string (str regexp newtext &optional literal)
      "Replace all matches in STR for REGEXP with NEWTEXT string, and
returns the new string.  Optional LITERAL non-nil means do a literal
replacement."
      (replace-regexp-in-string regexp newtext str nil literal)))

(if (not (fboundp 'exec-to-string))
    (defun exec-to-string (command)
      "Execute shell COMMAND and return its output as a string."
      (shell-command-to-string command)))

(defun replace-regexp-in-region (start end) (interactive "*r")
  "Interactively replace regexp on region"
  (save-excursion
    (save-restriction
      (let ((regexp (read-string "Regexp: "))
            (to-string (read-string "Replacement: ")))
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match to-string nil nil))))))

(defun make-password (requested-length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
  "Return a string of LENGTH random characters.  If UPPER is non-nil,
use uppercase letters.  If lower is non-nil, use lowercase letters.
If NUMBER is non-nil, use numbers.  If SYMBOL is non-nil, use one of
\"!\"#$%&'()*+'-./:;<=>?@`{}|~\".  If AMBIGUOUS is nil, avoid
characters like \"l\" and \"1\", \"O\" and \"0\"."
  (interactive "nlength: ")
  (let ((char-list (make-password-char-list upper lower number symbol ambiguous))
	 positon password)
;    (random t)
  (loop for i from 1 to requested-length 
	do (setq position (random (length char-list))
		 password (concat password (string (nth position char-list)))))
;;   (if (interactive-p)
;;       (let* ((strength (make-password-strength length upper lower number symbol ambiguous))
;; 	     (bits (car strength))
;; 	     (number (cadr strength)))
;; 	(message "The password \"%s\" is one of 10^%d possible an has a bit equivalence of %d" 
;; 		 password (round number) (round bits)))
    password))

(defun make-password-char-list (upper lower number symbol ambiguous)
  (let* ((upper-chars-ambiguous '(?I ?O ?G))
	 (upper-chars (loop for i from ?A to ?Z unless 
			    (member i upper-chars-ambiguous)
			    collect i))
	 (lower-chars-ambiguous '(?l ?o))
	 (lower-chars (loop for i from ?a to ?z unless 
			    (member i lower-chars-ambiguous)
			    collect i))
	 (number-chars-ambiguous '(?0 ?1 ?6))
	 (number-chars (loop for i from ?0 to ?9 unless
			     (member i number-chars-ambiguous)
			     collect i))
	 (symbol-chars '(?! ?@ ?# ?$ ?% ?& ?* ?( ?) ?+ ?= ?/ 
			    ?{ ?} ?[ ?] ?: ?\; ?< ?>))
	 (symbol-chars-ambiguous '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))
	 char-list)
  (if upper
      (setq char-list (append char-list upper-chars)))
  (if lower
      (setq char-list (append char-list lower-chars)))
  (if number
      (setq char-list (append char-list number-chars)))
  (if symbol
      (setq char-list (append char-list symbol-chars)))
  (if ambiguous
      (setq char-list (append char-list
			      upper-chars-ambiguous 
			      lower-chars-ambiguous
			      number-chars-ambiguous
			      symbol-chars-ambiguous)))
  char-list))

(defun make-password-prompt-for-args ()
  (interactive)
  (list
   (string-to-number (read-from-minibuffer "Number of Characters: "))
   (y-or-n-p "User uppercase: ")
   (y-or-n-p "User lowercase: ")
   (y-or-n-p "User numbers: ")
   (y-or-n-p "User symbols: ")
   (y-or-n-p "User ambiguous characters: ")))

(defun make-password-strength (length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
  "Calculate the number of possible passwords that could be generated
given the criteria of LENGTH and use of UPPER, LOWER, NUMBER, SYMBOL,
and AMBIGUOUS characters"
  (interactive (make-password-prompt-for-args))
  (let* ((char-list (make-password-char-list upper lower number symbol ambiguous))
	 (bits (/ (* length (log (length char-list))) (log 2)))
	 (number (/ (* bits (log 2)) (log 10))))
    (if (interactive-p)
	(message "number of combinations is 10^%d with a bit equivalence of %d" (round number) (round bits))
      (list bits number))))

(defun dired-kill-ring-save-timestamp ()
  "In dired, find the timestamp of the file or directory named on this
line and add it to the kill-ring as a string in the format used by
`touch' (ie YYYYMMDDhhmmss)."
  (interactive)
  (let* ((file-name (dired-get-filename))
	 (time-stamp (nth 5 (file-attributes file-name)))
	 (time-string (format-time-string "%Y%m%d%H%M.%S" time-stamp)))
    (kill-new time-string)))

(defun dired-kill-ring-save-filename ()
  "In dired, add the absolute path and filename of the file a point to
the kill-ring as a string."
  (interactive)
  (kill-new (dired-get-filename)))

(defun save-buffer-same-timestamp (&optional args)
  "Save the current buffer in visited file if modified.  Versions are
controlled with ARGS as with `save-buffer'.  The difference between
this command and save-timestamp is that this command saves the
modification time of the file from disk and resets it after the file
is saved."
  (interactive)
  (if (string-match blosxom-file-regexp (buffer-file-name))
      (let* ((buffer (current-buffer))
             (file-name (if (tramp-tramp-file-p (buffer-file-name))
                            (tramp-file-name-path 
                             (tramp-dissect-file-name 
                              (tramp-completion-handle-expand-file-name (buffer-file-name))))
                          (expand-file-name (buffer-file-name))))
             (file-attributes (file-attributes (buffer-file-name)))
             (time-string (if file-attributes 
                              (format-time-string "%Y%m%d%H%M.%S" 
                                                  (nth 5 file-attributes))))
             (shell-cmd (if (tramp-tramp-file-p (buffer-file-name)) 'tramp-handle-shell-command 'shell-command)))
        
        (save-buffer args)
        (if (and (stringp time-string)
                 (= 0 (funcall shell-cmd 
                               (format "touch -t %s %s" 
                                       time-string file-name)))
                 (set-buffer buffer)
                 (revert-buffer nil t))
            (message "Wrote %s, set timestamp to %s" file-name time-string)))
    (save-buffer args)))

(defun foreach-line-in-file-do (file function)
  "Open FILE and iterate across each line passing it as a string to
FUNCTION"
  (save-window-excursion
    (let* ((buffer (get-buffer-create
		    (generate-new-buffer-name file)))) 
      (set-buffer buffer)
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (search-forward-regexp "^.+$" nil t)
	(funcall functionv (match-string 0)))
      (kill-buffer buffer))))


(defun dired-file-contents (file)
  (interactive "fFilename: ")
  (let ((buffer (get-buffer-create 
		 (concat "*Dired virtual:" file "*"))))
    (set-buffer buffer)
    (kill-region (point-min) (point-max))
    (foreach-line-in-file-do
     file
     (lambda (string)
       (if (file-exists-p string)
	   (save-excursion
	     (set-buffer buffer)
	     (insert (shell-command-to-string (format "ls -l %s" string)))))))
    (dired-virtual "file")))

(defvar blosxom-file-regexp "blosxom.*txt"
  "Regular expression matching files that are stories for a blosxom blog")

(defun blosxom-find-file-hook ()
  "A function to be bound as a `find-file-hooks' that will rebind
\"C-x C-s\" to `save-buffer-same-timestamp'."
  (if (string-match blosxom-file-regexp (buffer-file-name))
      (local-set-key (kbd "C-x C-s") 'save-buffer-same-timestamp)))

(add-hook 'find-file-hooks 'blosxom-find-file-hook)

(defun scrmable-word (word)
  "Aoccdrnig to rscheearch at Cmabrigde Uinervtisy, it deosn't mttaer
in waht oredr the ltteers in a wrod are, the olny iprmoetnt tihng is
taht the frist and lsat ltteer be at the rghit pclae. The rset can be
a total mses and you can sitll raed it wouthit a porbelm. Tihs is
bcuseae the huamn mnid deos not raed ervey lteter by istlef, but the
wrod as a wlohe.

  This duefn will slrabcme the word peassd in."
  (let* ((letters (save-match-data (split-string word "")))
         (first-letter (if (length letters) (pop letters)))
         (last-letter (last letters))
         order)
    (if last-letter
        (progn
          (setq letters (butlast letters))
          (mapcar (lambda (letter)
                    (let ((rand (random* (* 2 (length letters))))) 
                      (setq order (append order (list (cons letter rand)))))) letters)))
    (mapcar (lambda (letter)
              (setq first-letter (concat first-letter letter)))
            (sort letters (lambda (a b)
                            (< (cdr (assoc a order)) (cdr (assoc b order))))))
    (setq first-letter (concat first-letter (car last-letter)))))

(defun scrmable-region (start end)
  "Aoccdrnig to rscheearch at Cmabrigde Uinervtisy, it deosn't mttaer
in waht oredr the ltteers in a wrod are, the olny iprmoetnt tihng is
taht the frist and lsat ltteer be at the rghit pclae. The rset can be
a total mses and you can sitll raed it wouthit a porbelm. Tihs is
bcuseae the huamn mnid deos not raed ervey lteter by istlef, but the
wrod as a wlohe.

  Tihs duefn wlil sbmralce and ertnie region, word by word."
  (interactive "r")
  (message "%d %d" start end)
  (goto-char start)
  (while (re-search-forward "\\(\\([^[:space:][:punct:]]+\\)\\)[[:space:][:punct:]]*" end t)
    (replace-match (scrmable-word (match-string-no-properties 1)) nil nil nil 1)))

(defun scrmable-buffer ()
  (scrmable-region (point-min) (point-max)))

(defun bibble-babbly-region (start end)
  "Translates a word into \"bibble-babbly\".  Every vowel is followed
by the letter \"b\"."
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "\\([aeiou]+\\)[^b]" end t)
    (replace-match (concat (match-string 1) "b") nil nil nil 1)))
    

(defun bibble-babbly-word (word)
  (save-match-data
    (let ((start t))
      (while (string-match "\\([aeiou]+\\)[^b]" word (if (not start) (match-end 0)))
        (if start (setq start nil))
        (setq word 
              (replace-match (concat (match-string-no-properties 1) "b") nil nil word 1)))
      (if (string-match "[^aeiou]\\(e\\)$" word nil)
          (replace-match "eb" nil nil word 1))))
  word)


(defun chomp (string)
  (replace-regexp-in-string "[\r\n]+$" "" string))

(defun xmms-now-playing ()
  "Return as a string the artist and title of the song XMMS is
  now playing"
  (interactive)
  (if (= (string-to-int 
          (shell-command-to-string "xmmsctrl playing; echo $?"))
         0)
      (shell-command-to-string "xmmsctrl title")
    "nothing"))

(defun find-file-re-sudo () 
  (interactive)
  (let ((name (buffer-file-name)))
    (if (kill-buffer nil)
        (find-file (format "/sudo:localhost:%s" name)))))


(defun dka-using-proxy-server ()
  (interactive)
  (let ((location (exec-to-string "~/bin/getlocation.pl")))
    ;; The [Kk]roger location is the only place I have a proxy server
    (string-match "roger" location)))

 	

;;; Final version: while
(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

;;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Return one of the entries in index-alist or nil."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice
	(prepared-index-alist
	 (if (not imenu-space-replacement) index-alist
	   (mapcar
	    (lambda (item)
	      (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
					  (car item))
		    (cdr item)))
	    index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (cond (prompt)
	  ((and name (imenu--in-alist name prepared-index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (let ((minibuffer-setup-hook minibuffer-setup-hook))
      ;; Display the completion buffer.
      (if (not imenu-eager-completion-buffer)
	  (add-hook 'minibuffer-setup-hook 'minibuffer-completion-help))
      (setq name (ido-completing-read prompt
				  (mapcar (lambda (elem) (if (stringp (car elem))
							     (car elem)
							   (caar elem))) prepared-index-alist)
				  nil t nil 'imenu--history-list name)))

    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
	  (imenu--completion-buffer (cdr choice) prompt)
	choice))))

(defun git-parse-dirty ()
  (interactive)
  (if (not (string-match "nothing to commit.*clean" (exec-to-string "git status 2> /dev/null | tail -n1")))
      "*"))

(defun current-git-branch ()
  (interactive)
  (concat (chomp (exec-to-string "git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/\* *//'")) (git-parse-dirty)))

(defun fix-escape-quotes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp "\\?\\(\"\\|'\\)" "? \\1 ")))

(defun bury-buffer-other-window()
  (interactive)
  (let ((current-buffer (buffer-name))
        (current-point (point))
        (current-window (selected-window)))
    (other-window 1)
    (bury-buffer)
    (select-window current-window)))
;    (switch-to-buffer current-buffer)
;    (goto-char current-point)))

(defun maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (let* ((frame (selected-frame)) 
         (pixels-per-col (/ (float (frame-pixel-width)) (frame-width)))
         (pixels-per-row (/ (float (frame-pixel-height)) (frame-height))))
    (set-frame-size frame
                    ;; truncate or round?
                    (truncate (/ (x-display-pixel-width) pixels-per-col))
                    ;; reduce size to account for the toolbar
                    (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7))
    (set-frame-position frame 0 0)))

(defun eshell-handle-ansi-color ()
  "Handle ANSI color codes -- by filtering them out! DKA"
  (eval-and-compile (require 'ansi-color))
  (ansi-color-filter-region eshell-last-output-start
                              eshell-last-output-end))

(defun ido-find-tag ()
    "Find a tag using ido"
    (interactive)
    (tags-completion-table)
    (let (tag-names)
      (mapc (lambda (x)
              (unless (integerp x)
                (push (prin1-to-string x t) tag-names)))
            tags-completion-table)
      (find-tag (ido-completing-read "Tag: " tag-names))))
