
(defpackage :lispbot.plugins
  (:use :cl :bot :alexandria)
  (:nicknames :bot.plugins)
  (:export :greet-plugin
	   :help-plugin
	   :dice-plugin
	   :decide-plugin
	   
	   :remember-plugin
	   :remembered-things
	   :file
	   :save-remembered
	   :load-remembered)
  (:documentation "Some (not so) usefull plugins for the lispbot. These are the
plugins that come with the main distribution. More plugins may be found elsewhere"))
