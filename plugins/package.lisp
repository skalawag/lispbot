
(defpackage :lispbot.plugins
  (:use :cl :bot :alexandria)
  (:nicknames :bot.plugins)
  (:export :greet-plugin
	   :help-plugin
	   :decide-plugin
	   :poll-plugin
           :datetime-plugin
           :info-plugin
           :box-plugin
           :cake-plugin
           :texas-holdem-plugin

           ;; dice plugin
           :dice-plugin
           :dice-max-rolls
           :*dice-max-rolls-default*

           ;; remember plugin specific
	   :remember-plugin
	   :remembered-things
	   :file
	   :save-remembered
	   :load-remembered

           ;; poll plugin
           :track-users-p
           :polls
	   :save-polls
	   :load-polls
           :add-poll
           :delete-poll
           :poll
           :poll-name
           :poll-description
           :poll-options
           :poll-users
           :log-plugin)
  (:documentation "Some (not so) usefull plugins for the lispbot. These are the
plugins that come with the main distribution. More plugins may be found elsewhere"))
