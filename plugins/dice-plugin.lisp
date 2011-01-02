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

(in-package :lispbot.plugins)

(defparameter *dice-max-rolls-default* 30
  "maximum number of rolls, the bot will perform in one XdY expression")

(defclass dice-plugin (plugin)
  ((max-rolls
    :initarg :max-rolls
    :initform *dice-max-rolls-default*
    :accessor dice-max-rolls))
  (:default-initargs :name "dice"))

(defmethod help ((self dice-plugin))
  (help-for-commands self))

(defcommand roll ((self dice-plugin) &rest expr)
  "rolls dices for you
`expr' is of the form: 3 * 2d20 + 4"
  (let ((*dice-max-rolls-default* (dice-max-rolls self)))
    (multiple-value-bind (res rolls)
        (dice-parse (reduce (curry #'concatenate 'string) expr))
      (reply (format nil "Result: ~a | dices: ~:[none~;~:*~{~a~^ ~}~]" res rolls) t))))

(defvar *dice-tokens* nil)
(defvar *dice-curtok* nil)
(defvar *dice-rolls* nil)

(defun dice-tokenize (string)
  (labels ((skip-number (string index)
             (position-if (lambda (c) (not (char<= #\0 c #\9)))
                          string :start index))

           (op-symbol (char) (make-keyword (string-upcase (string char)))))

    (do ((result)
         (index 0 (1+ index)))
        ((length= string index) (nreverse result))
      (let ((char (char string index)))
        (case char
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (let ((newindex (skip-number string index)))
             (push (parse-integer string :start index :end newindex)
                   result)
             (setf index (1- (or newindex (length string))))))
          ((#\( #\) #\+ #\- #\/ #\* #\w #\d)
           (push (op-symbol char) result))
          ((#\Tab #\Space) 'skip)
          (otherwise (error "Illegal token ~a" char)))))))

(defun dice-get-token ()
  (setf *dice-curtok* (pop *dice-tokens*)))

(defun dice-primitive ()
  (let ((token (dice-get-token)))
    (cond
      ((eq token :-)
       (let ((a (dice-get-token)))
         (if (numberp a)
             (prog1 (- a) (dice-get-token))
             (error "Number expected, ~a found" a))))
      ((eq token :|(|)
       (let ((a (dice-expression)))
         (if (eq *dice-curtok* :|)|)
             (progn (dice-get-token) a)
             (error "Missing closing paren"))))
      ((eq token :|)|) (error "One closing paren too much"))
      ((numberp token) (prog1 token (dice-get-token)))
      (t (error "Unexpected token ~a" token)))))

(defun dice-roll (times dice)
  (assert (and (> times 0) (<= times *dice-max-rolls-default*) (> dice 0))
          (times dice)
          "Arguments fo `dice' out of bounds: ~ad~a" times dice)
  (loop repeat times
        for roll = (1+ (random dice))
        do (push roll *dice-rolls*)
        sum roll))

(defun dice-dice ()
  (let ((a (dice-primitive)))
    (if (member *dice-curtok* '(:w :d))
        (dice-roll a (dice-primitive))
        a)))

(defun dice-term ()
  (let ((all (dice-dice)))
    (loop for token = *dice-curtok*
          while (member token '(:/ :*))
          do (setf all (if (eq *dice-curtok* :*)
                           (* all (dice-dice))
                           (/ all (dice-dice))))
          finally (return all))))

(defun dice-expression ()
  (let ((sum (dice-term)))
    (loop for token = *dice-curtok*
          while (member token '(:+ :-))
          do (setf sum (+ sum (if (eq *dice-curtok* :-)
                                  (- (dice-term))
                                  (dice-term))))
          finally (return sum))))

(defun dice-parse (string)
  (let ((*dice-tokens* (dice-tokenize string))
        (*dice-curtok* nil)
        (*dice-rolls* nil))
    (values (dice-expression) *dice-rolls*)))




