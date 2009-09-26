(defun eshell/pcvs-update (&rest args)
  "Invoke `M-x cvs-update RET' on the given directory."
  (cvs-update (pop args) nil))

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defun eshell/man (&rest args)
  "Invoke man"
  (manual-entry (apply 'eshell-flatten-and-stringify args)))
