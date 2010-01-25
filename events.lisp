
;; Events are organized as a class hirarchy. Plugins can specialize
;; the generic function HANDLE-EVENT on one of these classes to
;; receive the events it is interested in.

(in-package :bot)

(defclass event ()
  ((time
    :initarg :time
    :initform (get-universal-time)
    :reader timestamp
    :documentation "The time when this event was received")
   (bot
    :initarg :bot
    :reader bot
    :documentation "The bot, that received this event"))
  (:documentation "base class for all events"))

(defgeneric make-event (cl-irc-message bot)
  (:documentation "create the right event object, depending on the specified cl-irc-message class"))

(defclass message (event)
  ((text
    :initarg :text
    :accessor text
    :documentation "The message text")
   (sender
    :initarg :from
    :initarg :sender
    :reader sender
    :documentation "The guy who sent this message"))
  (:documentation "A message from another user that contain text"))

(defclass channel-message (message)
  ((original-text
    :initarg :original-text
    :accessor original-text
    :documentation "The text of this message as it was received by the bot.
note, that the bot is allowed to modify the TEXT-slot, e.g by stripping some
command prefix")
   (channel
    :initarg :channel
    :reader channel
    :documentation "The channel the message was received in"))
  (:documentation "Normal message exchanged in a channel"))

(defclass query-message (message)
  ()
  (:documentation "Message received as private message (read query)"))

(defun extract-user-from-irc-message (message)
  (make-instance 'user
		 :nick (irc:source message)
		 :host (irc:host message)
		 :username (irc:user message)))

(defmethod make-event ((message irc:irc-privmsg-message) bot)
  (let ((channel (find (elt (irc:arguments message) 0) (channels bot) :test #'string=))
	(text (car (last (irc:arguments message))))
	(sender (extract-user-from-irc-message message)))
    (if channel
	(make-instance 'channel-message
		       :text text
		       :from sender
		       :bot bot
		       :channel channel)
	(make-instance 'query-message
		       :text text
		       :from sender
		       :bot bot))))
