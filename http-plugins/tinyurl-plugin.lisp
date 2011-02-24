(in-package :lispbot.http-plugins)

(defclass tinyurl-plugin (plugin)
  ()
  (:default-initargs :name "tinyurl"))

(defmethod help ((self tinyurl-plugin))
  (help-for-commands self))

(defparameter *tinyurl-url* "http://www.tinyurl.com/api-create.php")

(defun tinyurl-get (url)
  (drakma:http-request *tinyurl-url* :parameters `(("url" . ,url))))

(defcommand tinyurl ((self tinyurl-plugin) url)
  "get a tiny url from tinyurl"
  (declare (ignore self))
  (reply (tinyurl-get url)))