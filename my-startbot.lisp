(progn
    (ql:quickload "lispbot")
    (ql:quickload "lispbot-plugins")
    (in-package #:lispbot))

(setq wispy
      (make-instance 'bot :nick "euclid"
                     :channels (list "#test-wispy")
                     :plugins (list 'bot.plugins:texas-holdem-plugin
                                    ;; 'bot.plugins:holdem-plugin
                                    ;; 'bot.plugins:roulette-plugin
                                    ;; 'bot.plugins:buzz-off-plugin
                                    ;; 'bot.plugins:wiktionary-plugin
                                    'bot.plugins:bb-plugin
                                    'bot.plugins:sleep-plugin
                                    'bot.plugins:fortune-plugin
                                    )))

(bot:start wispy "irc.freenode.net")
