(defpackage :lispbot.http-plugins
  (:nicknames :bot.http-plugins :http-plugins)
  (:use :lispbot :cl)
  (:export :google-plugin
           :tinyurl-plugin
           :bullshit-plugin
           :xkcd-plugin))
