(defpackage :lispbot
  (:use :cl :alexandria)
  (:nicknames :bot)
  (:export :*default-data-directory*
	   :*debug*

	   :bot
	   :make-bot
	   :add-plugins
	   :plugins
	   :channels
	   :nick
	   :data-dir

	   :reply
	   :action
	   :send
	   :start
	   :stop

	   :plugin
	   :name
	   :defcommand
	   :help
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
