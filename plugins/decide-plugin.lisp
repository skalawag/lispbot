
(in-package :lispbot.plugins)

(defclass decide-plugin (plugin)
  ()
  (:default-initargs :name "decide"))

(defmethod help ((plugin decide-plugin))
  (reply "!decide <thing+>: decide something for you"))

(defcommand decide ((plugin decide-plugin) arg1 &rest args)
  (declare (ignore plugin))
  (if (null args)
      (if (= (random 2) 0) (reply "Yes" t) (reply "No" t))
      (reply (random-elt (cons arg1 args)) t)))