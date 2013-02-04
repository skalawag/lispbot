
(in-package :lispbot.plugins)

(defclass decide-plugin (plugin)
  ()
  (:default-initargs :name "decide"))

(defmethod help ((plugin decide-plugin))
  (help-for-commands plugin))

(defcommand decide ((plugin decide-plugin) &rest args)
  "decides something for you"
  (declare (ignore plugin))
  (if (<= (length args) 1)
      (reply (if (zerop (random 2)) "Yes" "No") t)
      (reply (random-elt args) t)))
