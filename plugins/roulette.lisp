(in-package :lispbot.plugins)

(defclass roulette-plugin (plugin)
  ()
  (:default-initargs :name "roulette"))

;; (defmethod help ((plugin roulette-plugin))
;;   (reply "!sheep?: find out if you're a sheep or heathen."))


(defparameter *standing-challenge* nil)

(defcommand r ((plugin roulette-plugin) nick)
  "challenges someone to a game of roulette if no other challenge has
been made."
  (declare (ignore plugin))
  nil)

(defcommand pff ((plugin roulette-plugin))
  "accepts a challenge of roulette if a challenge has been made."
  (declare (ignore plugin))
  nil)



;;; these two commands get it started (after loading lispbot and
;;; lispbot-plugins

;; (setq wispy (make-instance 'bot :nick "wispbot" :channels
;; "#amarillolinux" :plugins (list 'bot.plugins:sheep-plugin)))

;; (bot:start wispy "irc.freenode.net")
