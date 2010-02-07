(defpackage :lispbot
  (:use :cl :alexandria)
  (:nicknames :bot)
  (:export :*default-data-directory*
	   :bot
	   :make-bot
	   :add-plugins
	   :plugins
	   :channels
	   :nick
	   :data-dir

	   :connect
	   :disconnect
	   :read-loop
	   :reply
	   :action
	   :send

	   :plugin
	   :name
	   :defcommand
	   :help
	   :connected
	   :handle-event
	   :*last-message*

	   :event
	   :message
	   :time
	   :text
	   :sender
	   :channel-message
	   :channel
	   :query-message
	   :user-event
	   :join-event
	   :part-event

	   :user
	   :user-equal
	   :hostmask))
