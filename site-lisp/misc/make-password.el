;;;; make-password.el --- generate a very random password
;;;; Jim Blandy <jimb@red-bean.com> --- September 1997

;;; Generate passwords, using a random number generator seeded by
;;; keystroke timing.  The passwords use letters from alternating
;;; hands, to make them fast to type.
;;;
;;; I can't imagine anyone but a serious security freak caring about
;;; this, so I haven't bothered to give it a nice user interface, but:
;;;
;;; First, eval (fresh-password-seed).
;;; Then, run (make-password LAYOUT LENGTH),
;;; where: LAYOUT is a keyboard layout: mpw-querty-layout and
;;;        mpw-dvorak-layout are provided
;;;    and LENGTH is the length of the password you want, in characters.
;;; You may want to generate a bunch of them, to find one that suits
;;; your hands well.  Use C-u C-x C-e in the scratch buffer, so you
;;; can see them long enough to learn them.
;;;
;;; You should be sure to call fresh-password-seed after you generate
;;; a password you actually use, so that if people discover one
;;; password that you've used, it doesn't give them any information
;;; about any other passwords you've generated (i.e. they can't
;;; compute the random seed given an old password, and then discover
;;; subsequent passwords.)
;;;
;;; For example, I use an ordinary Unix system, which only handles
;;; eight-character passwords, and a dvorak keyboard layout, so I
;;; generated my password with the expressions:
;;; (fresh-password-seed)
;;; (make-password mpw-dvorak-layout 8)
;;;
;;; I've only tested the Dvorak layout stuff.

(require 'cl)

;; We'd like a password that alternates letters from the left and
;; right hand.  Furthermore, the center keys in the top row are hard
;; to reach.

;; These definitions are appropriate for the Dvorak keyboard layout on
;; a DEC keyboard.  Tweak as you please.
(defconst mpw-dvorak-layout
  (vector (concat "12345" "',.py" "aoeui" ";qjkx")
	  (concat "890[=" "fgcrl/]" "dhtns-\\" "bmwvz")
	  (concat "!@#$%" "\"<>PY" "AOEUI" ":QJKX")
	  (concat "*(){+" "FGCRL?}" "DHTNS_|" "BMWVZ")))

;; These definitions are appropriate for the QWERTY keyboard layout on
;; a DEC keyboard.
(defconst mpw-qwerty-layout
  (vector (concat "12345" "qwert" "asdfg" "zxcvb")
	  (concat "890-=" "yuiop[]" "hjkl;'\\" "nm,./")
	  (concat "!@#$%" "QWERT" "ASDFG" "ZXCVB")
	  (concat "*()_+" "YUIOP{}" "HJKL:\"|" "NM<>?")))

(defun mpw-gcd (x y)
  (if (> y x) (setq x (prog1 y (setq y x))))
  (let (m)
    (while (not (zerop (setq m (mod x y))))
      (setq x y y m))
    y))

;; On each step, we shift the current state up by a certain number of
;; bits, rotate the top eight bits down the bottom of the word (so
;; bits don't escape the system), and add in the millisecond portion
;; of the current time.  Given that we want to rotate eight bits down
;; each time, determine an appropriate right shift for the word size,
;; and a left shift that isn't a multiple of that.
(defconst mpw-right-shift
  (let ((mask 255)
	(shift 0))
    (while (and (natnump mask)
		(< shift 100))
      (setq mask (lsh mask 1)
	    shift (1+ shift)))
    shift))
(defconst mpw-left-shift
  (let ((shift 4))
    (while (not (= 1 (mpw-gcd shift mpw-right-shift)))
      (setq shift (+ shift 1)))
    shift))

(defun mpw-make-state ()
  (let ((state 0) (i (* mpw-left-shift mpw-right-shift)))
    (while (> i 0)
      (message "Please press the space bar %d times; take your time." i)
      (read-char)
      (setq state (+ (lsh state 7)
		     (lsh (logand state (lsh 127 21)) -21)
		     (elt (current-time) 2)))
      (setq i (1- i)))
    (make-random-state state)))

(defvar mpw-state nil)

(defun fresh-password-seed ()
  (setq mpw-state (mpw-make-state)))

(defun make-password (layout length)
  "Generate a random password, given a keyboard layout LAYOUT, and a LENGTH."
  (or length (setq length 8))
  (or mpw-state (error "make-password: call fresh-password-seed first"))
  (let ((word (make-string length ?_))
	(i 0)
	(left (zerop (random* 2 mpw-state))))
    (while (< i length)
      (let* ((shifted (and (not left)	; I use the left shift key
			   (zerop (random* 4 mpw-state))))
	     (group (aref layout (+ (if left 0 1) (if shifted 2 0)))))
	(aset word i (aref group (random* (length group) mpw-state)))
	(setq i (1+ i) left (not left))))
    word))
