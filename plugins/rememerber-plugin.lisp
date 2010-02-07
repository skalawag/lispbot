
(in-package :bot.plugins)

(defclass remember-plugin (plugin)
  ((things
    :initform (make-hash-table :test 'equal)
    :initarg :things
    :accessor remembered-things ;; exported
    :accessor things
    :type hash-table
    :documentation "hashtable of remembered things")
   (file
    :initform (make-pathname :name "remembered-things")
    :initarg :file
    :accessor file
    :type pathname
    :documentation "relative pathname of the file where the hashtable will be
saved (default is 'remembered-things')"))
  (:default-initargs :name "remember"))

(defgeneric save-remembered (plugin &key filename)
  (:documentation "save the all the remembered things"))

(defgeneric load-remembered (plugin &key filename)
  (:documentation "load the file of remembered things into plugin"))

(defcommand remember ((plugin remember-plugin) thing &rest definition)
  (push (format nil "~{~a~^ ~}" definition) (gethash thing (things plugin)))
  (reply "ok. remembered." t))

(defcommand tell ((plugin remember-plugin) whom about thing)
  (when (not (string= about "about"))
    (error "bad syntax for tell. see help for details"))
  (let ((found (gethash thing (things plugin))))
    (if found
	(reply (mapcar (lambda (x)
			 (format nil "~@[~a: ~]~a ~a"
				 (cond
				   ((string= whom "us") nil)
				   ((string= whom "me") (nick (sender *last-message*)))
				   (t whom))
				 thing
				 x))
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
	  "!remember thing definition: learn a new definition for \"thing\""
	  "!tell {nick|me|us} about thing: tell someone what i learned about \"thing\""
	  "!forget thing [i]: forget what i learned about \"thing\". (optionally only the ith fact.)")))

(defmethod save-remembered ((plugin remember-plugin) &key filename)
  (let ((filename (or filename
		      (merge-pathnames (file plugin)
				       (data-dir (bot plugin))))))
    (ensure-directories-exist filename)
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (pprint (hash-table-alist (things plugin)) stream))))

(defmethod load-remembered ((plugin remember-plugin) &key filename)
  (let ((filename (or filename
		      (merge-pathnames (file plugin)
				       (data-dir (bot plugin))))))
    (ensure-directories-exist filename)
    (with-open-file (stream filename)
      (setf (things plugin)
	    (alist-hash-table (read stream) :test 'equal)))))