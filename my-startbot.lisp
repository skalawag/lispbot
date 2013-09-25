(progn
    (ql:quickload "lispbot")
    (ql:quickload "lispbot-plugins")
    (in-package #:lispbot))

(setq wispy
      (make-instance 'bot :nick "euclid"
                     :channels (list "#amarillolinux" "#test--wispy")
                     :plugins (list 'bot.plugins:texas-holdem-plugin)))

(bot:start wispy "irc.freenode.net")
