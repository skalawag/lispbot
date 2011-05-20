
(defsystem lispbot
  :name "lispbot"
  :version 0.1
  :maintainer "Hans-Peter Deifel"
  :licence "GPL"
  :description "An IRC-bot framework"
  :components ((:file "events"
		      :depends-on ("package"))
               (:file "threads"
                      :depends-on ("package"))
	       (:file "bot"
		      :depends-on ("events"
				   "package"
                                   "threads"))
	       (:file "package")
               (:file "testbot"
                      :depends-on ("bot")))
  :depends-on (:cl-irc
	       :cl-ppcre
	       :split-sequence
	       :alexandria
               :bordeaux-threads))