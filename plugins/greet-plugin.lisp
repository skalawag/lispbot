(in-package :lispbot.plugins)

(defclass greet-plugin (plugin)
  ((greet-new-users
    :initarg :greet-new-users
    :initform nil
    :accessor greet-new-users-p))
  (:default-initargs :name "greet"))

(defmethod help ((plugin greet-plugin))
  (reply "!greet [user|me]: greet someone"))

(defcommand greet ((plugin greet-plugin) &rest who)
  (declare (ignore plugin))
  (if who
      (reply (format nil "Hi ~{~a~^, ~}" who))
      (reply "Hi" t)))

(defmethod handle-event ((plugin greet-plugin) (event join-event))
  (when (greet-new-users-p plugin)
    (reply "Hi" t)))