(in-package :lispbot.plugins)

(defclass greet-plugin (plugin)
  ((name :initform "greet")
   (greet-new-users
    :initarg :greet-new-users
    :initform nil
    :accessor greet-new-users-p)))

(defmethod help ((plugin greet-plugin))
  (reply "!greet [user|me]: greet someone"))

(defcommand greet ((plugin greet-plugin) &optional (who "me"))
  (declare (ignore plugin))
  (if (string-equal who "me")
      (reply "Hi" t)
      (reply (format nil "Hi ~a" who))))

(defmethod handle-event ((plugin greet-plugin) (event join-event))
  (when (greet-new-users-p plugin)
    (reply "Hi" t)))