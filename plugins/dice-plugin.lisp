;; This is a plugin that rolls dices

;; Supports expressions like: 2 * 2d12 + 4

;; Grammar of a dice expression:
;;
;; expression:
;;     expression - term
;;     expression + term
;;     term
;;
;; term:
;;     term / dice
;;     term * dice
;;     dice
;;
;; dice:
;;     primary D primary
;;     primary W primary
;;     D primary
;;     W primary
;;     primary
;;
;; primary:
;;     NUMBER
;;     ( expression )
;;     -primary

(defpackage :lispbot.dice
  (:use :cl :lispbot)
  (:export :dice-plugin))

(in-package :lispbot.dice)

(defun dice-emptyp (string)
  (string-equal string ""))

(defun dice-char-numberp (char)
  (cond
    ((or
      (char-equal char #\0)
      (char-equal char #\1)
      (char-equal char #\2)
      (char-equal char #\3)
      (char-equal char #\4)
      (char-equal char #\5)
      (char-equal char #\6)
      (char-equal char #\7)
      (char-equal char #\8)
      (char-equal char #\9))
     t)
    (t nil)))

(defun dice-first-non-whitespace (string)
  (do ((index 0 (1+ index)))
      ((or
	    (>= index (length string))
	    (not (let ((char (char string index)))
		   (or (char-equal #\Space char)
		       (char-equal #\Tab char)))))
       index)))

(defun dice-str-to-num (string index)
  (read-from-string (subseq string 0 index)))

(defun dice-extract-number (string)
  (do ((index 0 (1+ index)))
      ((or (>= index (length string))
	   (not (or
		 (dice-char-numberp (char string index))
		 (char-equal #\. (char string index)))))
       (values (dice-str-to-num string index) index))))

(define-condition dice-unknown-token (error)
  ((text :initarg :text :reader text)))

(defun dice-tokenize (string)
  (let ((string (subseq string (dice-first-non-whitespace string))))
    (if (dice-emptyp string)
	nil
	(let ((char (char string 0)) (next-char 1))
	  (cons (cond
		  ((char-equal char #\+) :+)
		  ((char-equal char #\-) :-)
		  ((char-equal char #\() :PAREN_O)
		  ((char-equal char #\)) :PAREN_C)
		  ((char-equal char #\w) :W)
		  ((char-equal char #\d) :W)
		  ((char-equal char #\*) :*)
		  ((char-equal char #\/) :/)
		  ((dice-char-numberp char)
		   (multiple-value-bind (number index) (dice-extract-number string)
		     (setf next-char index)
		     number))
		  (t (error 'dice-unknown-token :text (format nil "Unknown symbol ~A" char))))
		(dice-tokenize (subseq string next-char)))))))

(defparameter *dice-tokens* nil)
(defparameter *dice-curtok* nil)
(defparameter *dice-rolls* nil)

(defun dice-roll (times dice)
  (when (not (and (integerp times)
		  (> times 0)
		  (integerp dice)
		  (> dice 0)))
    (error "Wrong arguments for the dice"))
  (let ((sum 0))
    (dotimes (nix times sum)
      (let ((res (+ (random dice) 1)))
	(push res *dice-rolls*)
	(incf sum res)))))

(defun dice-primitive ()
  (let ((a (pop *dice-tokens*)))
    (cond
      ((equal a :-) (list '- (dice-primitive)))
      ((equal a :PAREN_O) 
       (let ((a (dice-expression)))
	 (setf *dice-curtok* (pop *dice-tokens*))
	 a))
      ((equal a :PAREN_C) (error "Hmm, what is this paren doing here? is this lisp?"))
      ((equal a :W)
       (progn
	 (setf *dice-curtok* :W)
	 1))
      ((numberp a)
       (progn
	 (setf *dice-curtok* (pop *dice-tokens*))
	 a))
      (t (error "I don't know... the syntax is fucked up")))))

(defun dice-dice ()
  (let ((a (dice-primitive)))
    (if (equal *dice-curtok* :W)
	(list 'dice-roll a (dice-primitive))
	a)))

(defun dice-term ()
  (let ((a (dice-dice)))
    (cond
      ((equal *dice-curtok* :*) (list '* a (dice-term)))
      ((equal *dice-curtok* :/) (list '/ a (dice-term)))
      (t a))))

(defun dice-expression ()
  (let ((a (dice-term)))
    (cond
      ((equal *dice-curtok* :+) (list '+ a (dice-expression)))
      ((equal *dice-curtok* :-) (list '- a (dice-expression)))
      (t a))))

(defun dice-parse (string)
  (let ((*dice-tokens* (dice-tokenize string)) (*dice-curtok* nil))
    (dice-expression)))

(defclass dice-plugin (plugin)
  ((name :initform "dices")))

(defmethod help ((plugin dice-plugin))
  (reply '("!roll <expr>: rolls dices for you"
	   "expr is a expression of the form: 3* 2w20 + 4")))

(defcommand roll ((plugin dice-plugin) &rest args)
  (declare (ignore plugin))
  (handler-case
      (let ((*dice-rolls* nil))
	(let ((res (eval (dice-parse (format nil "~{~a~^ ~}" args)))))
	  (reply (format nil "Result: ~A | dices: ~A" res *dice-rolls*) t)))
    (error (err) (reply (format nil "Uh Oh, an error: ~A" (text err)) t))))
