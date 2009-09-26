(require 'etags)

(defun find-tag-tag (string)
  (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil))
				     tags-case-fold-search
				   case-fold-search))
	 (default (funcall (or find-tag-default-function
			       (get major-mode 'find-tag-default-function)
			       'find-tag-default)))
	 (spec (ido-completing-read (if default
				    (format "%s (default %s): "
					    (substring string 0 (string-match "[ :]+\\'" string))
					    default)
				  string)
				'tags-complete-tag
				nil nil nil nil default)))
    (if (equal spec "")
	(or default (error "There is no default tag"))
      spec)))
