(defun dka-remove-stdafx
  "Function to open file named on this line in dired and remove inclusion of
stdafx.h"
  (interactive)
  (dired-find-file)
  (while (search-forward "^#include \"stdafx\.h\"" nil t)
		 (beginning-of-line)
		 (kill-line)
		 (kill-line))
  (save-buffer)
  (kill-buffer nil))

(defun dka-add-assert-spoof
  "Function to open file named on this line in dired and insert spoofing of ASSERT if it's not already defined"
  (interactive)
  (dired-find-file)
  (if (not (and (search-forward-regexp "ASSERT[^A-Z_(]" nil t)
					 ((beginning-of-line (looking-at  "#define ASSERT ")))))
		(and 
		 (beginning-of-buffer)
		 (while (eq (char-to-string (following-char)) "/")
			(forward-line))
		 (insert-string "
#if defined LINUX && !defined ASSERT
#include <assert.h>
#define ASSERT assert   // spoof MS-Window's ASSERT function
#endif
")
		 (and buffer-read-only (toggle-read-only))
		 (save-buffer)))
  ;;(kill-buffer nil)
  )


(setq compilation-finish-function
		(lambda ()
		  (br-buffer-replace (br-regexp-quote "^[^ ]+/build/") (string " "))
		  ))

(defun dka-remove-build-dir
  "Function to parse the compilation buffer and remove any references
to the build dir.  After stripping out the build dir, the buffer is
left with the actual dir where the files are found"
  (interactive)
  (setq default-directory (replace-in-string (default-directory) 
															"TTServer/Linux/build" ""))
  (br-buffer-replace 
	"/home/dalcorn/devel/Projects/TTPro_Ver3/TTServer/Linux/build/" ""))

(add-hook 'compilation-mode-hook
			 (lambda ()
				(setq default-directory
						"/the/source/location/(not/in/build/dir)")))

(defun uk-gdb-complete-command ()
  "Perform completion on the GDB command preceding point.
This is implemented using the GDB `complete' command which isn't
available with older versions of GDB."
  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		    (beginning-of-line)
		    (and (looking-at comint-prompt-regexp)
			 (goto-char (match-end 0)))
		    (buffer-substring (point) end))))
    ;; Issue the command to GDB.
    (gdb-call (concat "complete " command)))
  ;; return t so that comint-dynamic-complete-functions is happy
  t)

(defun dka-search-upcase-word (regexp)
  "Prompt for regular expression, then search through current buffer
executing upcase-word after each regexp match."
  (interactive "sEnter REGEXP: ")
  (save-excursion
	 (beginning-of-buffer)
	 (while (search-forward-regexp regexp nil t)
		(forward-word -1)
		(upcase-word 1)))
  )