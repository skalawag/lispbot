;;; This is a example configuration
;;;
;;; Usage:
;;;  - first load the bot and plugins via asdf
;;;  - then (load) this file into your lisp image to get the configuration
;;;  - type (start-my-bot)

(in-package :cl-user)

(use-package :bot)
(use-package :bot.plugins)

(defparameter *plugins*
  (list (make-instance 'help-plugin)
        (make-instance 'greet-plugin
                       :greet-new-users t)
        (make-instance 'dice-plugin)
        (make-instance 'decide-plugin)))

(defparameter *bot*
  (make-instance 'bot
                 :channels '("#chan1" "#chan2")
                 :plugins *plugins*
                 :nick "UberNick"))

(defparameter *server*
  "irc.example.com")

(defun start-my-bot ()
  (start *bot* *server*))
