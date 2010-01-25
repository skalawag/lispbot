;; A Plugin that prints a short help about the bot and other plugins

(in-package :lispbot.plugins)

(defclass help-plugin (plugin)
  ((name :initform "hilfe")))

(defun helpfull-plugin-p (plugin)
  (let ((bot::*last-message* nil))
    (not (eq (help plugin) :unimplemented-help))))

(defcommand help ((p help-plugin) &optional what)
  (if what
      (loop for plugin in (plugins (bot p))
	 when (string= (name plugin) what)
	 do (help plugin))
      (reply (list (concatenate 'string "the following plugins are available: "
				(format nil "~{~a~^, ~}" (loop for x in (plugins (bot p))
							    when (helpfull-plugin-p x) collect (name x))))
		   "get more help with \"help <plugin>\""))))
