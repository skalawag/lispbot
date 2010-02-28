(in-package :lispbot.http-plugins)

(defclass google-plugin (plugin)
  ()
  (:default-initargs :name "google"))

(defmethod help ((pl google-plugin))
  (declare (ignore pl))
  (reply "google {words*} : search google for words"))

(defparameter *google-url* "http://www.google.com/search")

(defun get-google-matches (search)
  (let* ((html (drakma:http-request *google-url*
				    :parameters `(("q" . ,search))))
	 (document (chtml:parse html (cxml-stp:make-builder)))
	 (res nil))
    (stp:do-recursively (a document)
      (when (and (typep a 'stp:element)
		 (equal (stp:local-name a) "a")
		 (equal (stp:attribute-value a "class") "l"))
	(push (cons (stp:string-value a)
		    (stp:attribute-value a "href"))
	      res)))
    (reverse res)))

(defcommand google ((pl google-plugin) arg1 &rest args)
  (declare (ignore pl))
  (let* ((str (format nil "\"~a\"~{ \"~a\"~}" arg1 args))
	 (res (get-google-matches str)))
    (if res
	(reply (format nil "~a - ~a"
		       (car (first res))
		       (cdr (first res))))
	(reply (format nil "nothing found for ~a" str)))))