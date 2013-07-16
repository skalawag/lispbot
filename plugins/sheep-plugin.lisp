(in-package :lispbot.plugins)

(defclass sheep-plugin (plugin)
  ()
  (:default-initargs :name "sheep"))

(defmethod help ((plugin sheep-plugin))
  (reply "!sheep?: find out if you're a sheep or heathen."))

(defmethod handle-event ((plugin sheep-plugin) (event channel-message))
  (when (string= (text event) "sheep?")
    (if (= (random 2) 0)
        (reply (format nil "is a sheep!") t)
        (reply (format nil "is a heathen!") t))))


;;; these two commands get it started (after loading lispbot and lispbot-plugins
;; (setq wispy (make-instance 'bot :nick "wispbot" :channels
;; "#amarillolinux" :plugins (list 'bot.plugins:sheep-plugin)))

;; (bot:start wispy "irc.freenode.net")
