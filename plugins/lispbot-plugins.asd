
(defsystem lispbot-plugins
  :name "lispbot-plugins"
  :version 0.1
  :maintainer "Hans-Peter Deifel"
  :licence "GPL"
  :description "Some common plugins for the lispbot"
  :components ((:file "buzz-off-plugin"
                      :depends-on ("package"))
               (:file "holdem-plugin"
                      :depends-on ("package"))
               (:file "roulette-plugin"
                      :depends-on ("package"))
               (:file "sheep-plugin"
                      :depends-on ("package"))
               (:file "dice-plugin"
		      :depends-on ("package"))
	       (:file "greet-plugin"
		      :depends-on ("package"))
	       (:file "datetime-plugin"
		      :depends-on ("package"))
	       (:file "help-plugin"
		      :depends-on ("package"))
	       (:file "decide-plugin"
		      :depends-on ("package"))
	       (:file "remember-plugin"
		      :depends-on ("package"))
               (:file "poll-plugin"
                      :depends-on ("package"))
               (:file "info-plugin"
                      :depends-on ("package"))
	       (:file "box-plugin"
		      :depends-on ("package"))
	       (:file "cake-plugin"
		      :depends-on ("package"))
	       (:file "log-plugin"
		      :depends-on ("package"))
	       (:file "package"))
  :depends-on (:lispbot :alexandria))
