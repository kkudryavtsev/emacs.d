
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
