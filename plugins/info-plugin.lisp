(in-package :lispbot.plugins)

(defclass info-plugin (plugin)
  ()
  (:default-initargs :name "info"))

(defmethod help ((plugin info-plugin))
  (help-for-commands plugin))

(defcommand lisp ((plugin info-plugin))
  "shows some info about the used lisp environment"
  (declare (ignore plugin))
  (reply (format nil "This is ~a Version ~a"
                 (lisp-implementation-type)
                 (lisp-implementation-version))))

