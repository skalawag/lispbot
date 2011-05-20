
(in-package :lispbot.plugins)

(defclass decide-plugin (plugin)
  ()
  (:default-initargs :name "decide"))

(defmethod help ((plugin decide-plugin))
  (help-for-commands plugin))

(defcommand decide ((plugin decide-plugin) arg1 &rest args)
  "decides something for you"
  (declare (ignore plugin))
  (if (null args)
      (if (= (random 2) 0) (reply "Yes" t) (reply "No" t))
      (reply (random-elt (cons arg1 args)) t)))