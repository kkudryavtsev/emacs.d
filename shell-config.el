(defvar shell-scroll-to-bottom-on-input t
  "Scroll to the bottom of the *shell* buffer on new input")

(defun shell-output-filter-bottom-of-buffer
  (if shell-scroll-to-bottom-on-input
      (end-of-buffer)))
