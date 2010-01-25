
(defsystem lispbot
  :name "lispbot"
  :version 0.1
  :maintainer "Hans-Peter Deifel"
  :licence "GPL"
  :description "An IRC-bot framework"
  :components ((:file "events"
		      :depends-on ("package"))
	       (:file "bot"
		      :depends-on ("events"
				   "package"))
	       (:file "package"))
  :depends-on (:cl-irc
	       :cl-ppcre
	       :split-sequence))