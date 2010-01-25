(in-package :lispbot.plugins)

(defclass greet-plugin (plugin)
  ((name :initform "greet")))

(defmethod help ((plugin greet-plugin))
  (reply "!greet [user|me]: greet someone"))

(defcommand greet ((plugin greet-plugin) &optional (who "me"))
  (declare (ignore plugin))
  (if (string-equal who "me")
      (reply "Hi" t)
      (reply (format nil "Hi ~a" who))))
