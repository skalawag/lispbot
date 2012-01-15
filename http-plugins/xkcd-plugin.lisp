(in-package :lispbot.http-plugins)

(defclass xkcd-plugin (plugin)
  ((last-update :initform nil)
   (last-timestamp :initform nil))
  (:default-initargs :name "xkcd"))

(defmethod help ((self xkcd-plugin))
  (help-for-commands self))

(defun get-xkcds ()
  (handler-case
      (let* ((html (drakma:http-request "http://xkcd.com/archive/"))
             (document (chtml:parse html (cxml-stp:make-builder)))
             (res nil))
        (stp:do-recursively (a document)
          (when (and (typep a 'stp:element)
                     (equal (stp:local-name a) "a"))
            (cl-ppcre:register-groups-bind (num)
                ("^/([0-9]+)/$" (stp:attribute-value a "href"))
              (push (cons num (stp:string-value a)) res))))
        res)
    (condition () nil)))

(defun find-xkcd (pattern xkcds)
  (or (find pattern xkcds :test #'string= :key #'car)
      (let ((scanner (cl-ppcre:create-scanner pattern :case-insensitive-mode t)))
        (find-if (lambda (x) (cl-ppcre:scan scanner (cdr x))) xkcds))))

(defun string-join (delim list)
  (with-output-to-string (s)
    (when list (format s "~a" (first list)))
    (dolist (l (rest list))
      (format s "~a~a" delim l))))

(defun maybe-update (self)
  (with-slots (last-update last-timestamp) self
    (let ((now (get-universal-time)))
      (when (or (not last-timestamp) (< last-timestamp (- now (* 60))))
        (setf last-update (get-xkcds)
              last-timestamp now))
      last-update)))

(defcommand xkcd ((self xkcd-plugin) &rest args)
  "search xkcd comics by title"
  (if (endp args)
      (error "Too few arguments for xkcd")
      (let* ((xkcds (maybe-update self))
             (res (find-xkcd (string-join " " args) xkcds)))
        (if res (reply (format nil "http://xkcd.com/~a/ - ~a" (car res) (cdr res)) t)
            (reply "Sorry, nothing found" t)))))
