(if (fboundp 'read-passwd-map)
    (defvar read-passwd-map
      (let ((i 0)
	    (s (make-string 1 0))
	    map)
	(cond ((fboundp 'set-keymap-parent)
	       (setq map (make-keymap))
	       (set-keymap-parent map minibuffer-local-map))
	      (t			; v18/FSFmacs compatibility
	       (setq map (copy-keymap minibuffer-local-map))))
	(if (fboundp 'set-keymap-name)
	    (set-keymap-name map 'read-passwd-map))

	(while (< i 127)
	  (aset s 0 i)
	  (or (and (boundp 'meta-prefix-char)
		   ;;(eq (int-char i) meta-prefix-char)
		   )
	      (define-key map s 'self-insert-command))
	  (setq i (1+ i)))

	(define-key map "\C-g" 'keyboard-quit)
	(define-key map "\C-h" 'delete-backward-char)
	(define-key map "\r" 'exit-minibuffer)
	(define-key map "\n" 'exit-minibuffer)
	(define-key map "\C-u" 'passwd-erase-buffer)
	(define-key map "\C-q" 'quoted-insert)
	(define-key map "\177" 'delete-backward-char)
	(define-key map "\M-n" 'passwd-next-history-element)
	(define-key map "\M-p" 'passwd-previous-history-element)
	(define-key map "\C-y" 'yank-clipboard-selection)
	map)
      "Keymap used for reading passwords in the minibuffer.
The \"bindings\" in this map are not real commands; only a limited
number of commands are understood.  The important bindings are:
\\<read-passwd-map>
	\\[passwd-erase-buffer]	Erase all input.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

All other characters insert themselves (but do not echo.)")
  (defun read-passwd (prompt &optional confirm default)
    "Read a password, prompting with PROMPT.  Echo `.' for each
character typed.  End with RET, LFD, or ESC.  DEL or C-h rubs out.
C-u kills line. C-y yanks from the X clipboard into the minibuffer.
Optional argument CONFIRM, if non-nil, then read it twice to make
sure.  Optional DEFAULT is a default password to use instead of empty
input."
    (if confirm
	(let (success)
	  (while (not success)
	    (let ((first (read-passwd prompt nil default))
		  (second (read-passwd "Confirm password: " nil default)))
	      (if (equal first second)
		  (progn
		    (and (arrayp second) (fillarray second ?\0))
		    (setq success first))
		(and (arrayp first) (fillarray first ?\0))
		(and (arrayp second) (fillarray second ?\0))
		(message "Password not repeated accurately; please start over")
		(sit-for 1))))
	  success)
      (let ((pass nil)
	    (c 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t))
	(while (progn (message "%s%s"
			       prompt
			       (make-string (length pass) ?.))
		      (setq c (read-char-exclusive nil t))
		      (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	  (clear-this-command-keys)
	  (if (= c ?\C-u)
	      (progn
		(and (arrayp pass) (fillarray pass ?\0))
		(setq pass ""))
	    (if (= c ?\C-y)
		;; allow C-y to yank into the minibuffer
		(setq pass (concat pass (current-kill 0)))
	      (if (and (/= c ?\b) (/= c ?\177))
		  (let* ((new-char (char-to-string c))
			 (new-pass (concat pass new-char)))
		    (and (arrayp pass) (fillarray pass ?\0))
		    (fillarray new-char ?\0)
		    (setq c ?\0)
		    (setq pass new-pass))
		(if (> (length pass) 0)
		    (let ((new-pass (substring pass 0 -1)))
		      (and (arrayp pass) (fillarray pass ?\0))
		      (setq pass new-pass)))))))
	  (message nil)
	  (or pass default "")))))

