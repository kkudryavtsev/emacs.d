(setq sgml-set-face t)

;; comment - comment declaration 
;; doctype - doctype declaration 
;; end-tag - end-tag 
;; ignored - ignored marked section 
;; ms-start - marked section end, if not ignored 
;; ms-end - marked section start, if not ignored 
;; pi - processing instruction 
;; sgml - SGML declaration 
;; start-tag - start-tag 
;; entity- entity reference 
;; shortref- short reference

(setq sgml-markup-faces
      '((start-tag . font-lock-function-name-face)
        (end-tag . font-lock-function-name-face)
        (comment . font-lock-comment-face)
        (pi . font-lock-keyword-face)
        (entity . font-lock-string-face)))
