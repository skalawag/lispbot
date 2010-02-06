
(in-package :bot.plugins)

(defclass remember-plugin (plugin)
  ((things
    :initform (make-hash-table :test 'equal)
    :initarg :things
    :accessor remembered-things ;; exported
    :accessor things
    :type hash-table
    :documentation "hashtable of remembered things"))
  (:default-initargs :name "remember"))

(defcommand remember ((plugin remember-plugin) thing is &rest definition)
  (when (not (or (string= is "is") (string= is "=")))
    (error "bad syntax for remember. see help for details"))
  (push (format nil "~{~a~^ ~}" definition) (gethash thing (things plugin)))
  (reply "ok. remembered." t))

(defcommand tell ((plugin remember-plugin) whom about thing)
  (when (not (string= about "about"))
    (error "bad syntax for tell. see help for details"))
  (let ((found (gethash thing (things plugin))))
    (if found
	(reply (format nil "~@[~a: ~]~a is ~{~a~^ and ~}"
		       (cond
			 ((string= whom "us") nil)
			 ((string= whom "me") (nick (sender *last-message*)))
			 (t whom))
		       thing
		       found))
	(reply "i don't know about such a thing" t))))

(defun delete-nth (list n)
  (if (<= n 0)
      (cdr list)
      (let ((rest (nthcdr (1- n) list)))
	(pop (cdr rest))
	list)))

(defcommand forget ((plugin remember-plugin) thing &optional index)
  (let ((found (gethash thing (things plugin))))
    (if found
	(if index
	    (handler-case
		(setf (gethash thing (things plugin))
		      (delete-nth found (parse-integer index)))
	      (parse-error () (reply (format nil "sorry, ~a is no valid index." index) t))
	      (:no-error (&rest args)
		(declare (ignore args))
		(reply "done." t)))
	    (progn
	     (setf (gethash thing (things plugin)) nil)
	     (reply "done." t)))
	(reply (format nil "there is no such thing as ~a" thing)))))

(defmethod help ((plugin remember-plugin))
  (reply '("A plugin to remember facts."
	  "!remember thing {is|=} definition: learn a new definition for \"thing\""
	  "!tell {nick|me|us} about thing: tell someone what i learned about \"thing\""
	  "!forget thing [index]: forget what i learned about \"thing\". (optionally only the index's fact.)")))

;;; TODO: include some ability to save the remembered things to disc