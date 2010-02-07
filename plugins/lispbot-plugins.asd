
(defsystem lispbot-plugins
  :name "lispbot-plugins"
  :version 0.1
  :maintainer "Hans-Peter Deifel"
  :licence "GPL"
  :description "Some common plugins for the lispbot"
  :components ((:file "dice-plugin"
		      :depends-on ("package"))
	       (:file "greet-plugin"
		      :depends-on ("package"))
	       (:file "help-plugin"
		      :depends-on ("package"))
	       (:file "decide-plugin"
		      :depends-on ("package"))
	       (:file "rememerber-plugin"
		      :depends-on ("package"))
	       (:file "package"))
  :depends-on (:lispbot :alexandria))