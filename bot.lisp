 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; __                               ____            __             ;;
;;/\ \       __                    /\  _`\         /\ \__          ;;
;;\ \ \     /\_\    ____  _____    \ \ \L\ \    ___\ \ ,_\         ;; 
;; \ \ \  __\/\ \  /',__\/\ '__`\   \ \  _ <'  / __`\ \ \/         ;;
;;  \ \ \L\ \\ \ \/\__, `\ \ \L\ \   \ \ \L\ \/\ \L\ \ \ \_        ;;
;;   \ \____/ \ \_\/\____/\ \ ,__/    \ \____/\ \____/\ \__\       ;;
;;    \/___/   \/_/\/___/  \ \ \/      \/___/  \/___/  \/__/       ;;
;;                          \ \_\                                  ;;
;;                           \/_/                                  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (debug 3) (speed 0)))

(defpackage :lispbot
  (:use :cl)
  (:nicknames :bot)
  (:export :bot
	   :make-bot
	   :add-plugin
	   :plugins
	   :channels
	   :nick

	   :connect
	   :disconnect
	   :read-loop
	   :reply
	   :action

	   :plugin
	   :name
	   :message
	   :defcommand
	   :help
	   :connected

	   :random-entry

	   :channel
	   :msg-type
	   :sender
	   
	   :user
	   :user-equal
	   :hostmask))

(in-package :lispbot)

 ;;;;;;;;;;;;;;
;;            ;;
;; Bot Class  ;;
;;            ;;
 ;;;;;;;;;;;;;;

(defclass bot ()
  ((connection
    :initform nil
    :type irc:connection
    :documentation "internal connection representation")
   (channels        ;; list of strings. TODO: add channel class, that for example keeps track of users
    :initform nil
    :initarg :channels
    :reader channels
    :documentation "list of channels the bot has joined")
   (plugins
    :initform nil
    :initarg :plugins
    :reader plugins
    :documentation "list of plugins for this bot")
   (nick
    :initform "lispbot"
    :initarg :nick
    :reader nick
    :documentation "the nickname of the bot"))
  (:documentation "a irc bot"))

;; TODO: implement
(defgeneric join (bot channels)
  (:documentation "join one or more chans"))

;; TODO: implement
(defgeneric leave (bot channels)
  (:documentation "leave one ore more chans"))

;; TODO: implement
(defgeneric (setf nick) (bot newnick)
  (:documentation "change the nick of the bot. return the nick on success and nil otherwise"))

(defun make-bot (nick channels &rest plugins)
  (let ((bot (make-instance 'bot
			    :channels (mklist channels)
			    :nick nick)))
    (setf (slot-value bot 'plugins) (mapcar (lambda (x) (make-instance x :bot bot)) plugins))
    bot))

(defun add-plugin (bot plugin-class)
  (unless (some (lambda (x) (eq (class-name (class-of x)) plugin-class)) (plugins bot))
    (push (make-instance plugin-class :bot bot) (slot-value bot 'plugins))))

(defgeneric connect (bot server &optional port)
  (:documentation "let the bot connect to irc server and join its chans"))

;; TODO: add threads, to make this asyncronous
(defgeneric read-loop (bot)
  (:documentation "enter the read loop"))

;; Proposed implementation of this asyncronous start
(defgeneric run-bot (bot server &optional port)
  (:documentation "connect to server, join channels and enter read-loop"))

(defgeneric disconnect (bot)
  (:documentation "disconnect the bot from server"))

;;;
;;;;; The funktions in this section should only be called from commands, or a reimplemented 'message' method
;;;

;; if message is not a string but a list of string, the bot will say multiple messages. each for every entry in the list
(defgeneric reply (message &optional to-user-p)
  (:documentation "can be used by plugins to let the bot say something. message can be a list of strings or a string"))

(defgeneric action (message)
  (:documentation "can be used by plugins write a /me message"))

;; TODO: implement some sort of translation system
(defun l10n (string)
  string)

;;;
;;;;;; End section
;;;


 ;;;;;;;;;;;;;;;;;
;;               ;;
;; Plugin Class  ;;
;;               ;;
 ;;;;;;;;;;;;;;;;;

(defclass plugin ()
  ((name :initform (error "Plugins need names") :reader name)
   (bot
    :initform nil
    :reader bot
    :initarg :bot))
  (:documentation "all plugins must derive from this class"))

(defgeneric connected (plugin)
  (:documentation "called when the bot is connected and the channel is joined"))

(defgeneric message (plugin message)
  (:documentation "called when someone said something to the bot, should not be used"))

(defgeneric help (plugin)
  (:documentation "called when the user requests help for a plugin"))

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (x) `(,x (gensym))) syms)
     ,@body))

(defmacro defcommand (name ((plvar plclass) &rest args) &body body)
  (with-gensyms (command closure)
   `(let ((,command (assoc ',name (get ',plclass :commands)))
	  (,closure (lambda (,plvar ,@args) ,@body)))
      (if ,command
	  (setf (cdr ,command) ,closure)
	  (push (cons ',name ,closure)
		(get ',plclass :commands))))))

 ;;;;;;;;;;;;;;;
;;             ;;
;; User Class  ;;
;;             ;;
 ;;;;;;;;;;;;;;;

;; 'user interface' is to be implemented

(defclass user ()
  ((nick
    :initarg :nick
    :reader nick)
   (username
    :initarg :username
    :reader name)
   (host
    :initarg :host
    :reader host)))

(defgeneric user-equal (user1 user2)
  (:documentation "compare users"))

(defgeneric hostmask (user)
  (:documentation "return a host mask of the form nick!username@host"))

 ;;;;;;;;;;;;;;;;;;;
;;                 ;;
;;  Message Class  ;;
;;                 ;;
 ;;;;;;;;;;;;;;;;;;;

(defclass message ()
  ((text
    :initform ""
    :reader text
    :initarg :text)
   (type
    :initform :public
    :reader msg-type
    :initarg :type
    :documentation "can be either :public for channel messages or :private for queries")
   (from
    :reader sender
    :initarg :from)
   (bot
    :reader bot
    :initarg :bot)
   (channel
    :reader channel
    :initarg :channel
    :initform nil
    :documentation "nil if query")))

 ;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;; Utility functions ;;
;;                   ;;
 ;;;;;;;;;;;;;;;;;;;;;

(defun random-entry (list)
  "return a randomly selected element from list"
  (nth (random (length list)) list))

(defun divisible (x y) (= (mod x y) 0))

(defun mklist (arg)
  (if (listp arg) arg (list arg)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Internal implementation ;;
;;                         ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *hooks* nil)

(defmacro defhook (name (bot message) &body body)
  `(let ((cons (assoc ',name *hooks*))
	 (hook (lambda (,bot ,message)
		  ,@body)))
     (if cons
	 (setf (cdr cons) hook)
	 (push (cons ',name hook) *hooks*))))

(defparameter *last-message* nil
  "this is bound to the last message addressing the bot during the exectution of commands
or the 'message' method of plugins")

;; FIXME: do something resonable here
(defun handle-errors-in-plugin (err plugin message)
  (declare (ignore plugin message))
  (format t "error: ~a~%" err))

(defun call-plugin-callbacks (message)
  (dolist (plugin (plugins (bot message)))
    (let ((*last-message* message))
      (dolist (command (get (class-name (class-of plugin)) :commands))
	(let ((name (car command)) (function (cdr command)))
	  (multiple-value-bind (match msg)
	      (ppcre:scan-to-strings (format nil "^~a(\\W+(.*))?" (if (symbolp name)
								      (string-downcase (symbol-name name))
								      name))
				     (text message))
	    (when match
	      (handler-case
		  (apply function plugin (partition:split-sequence #\Space (elt msg 1) :remove-empty-subseqs t))
		(error (err) (handle-errors-in-plugin err plugin message)))))))
      (message plugin *last-message*))))

 ;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; Internal Hooks  ;;
;;                 ;;
 ;;;;;;;;;;;;;;;;;;;

(defhook irc:irc-rpl_luserme-message (bot message)
  (declare (ignore message))
  (dolist (chan (channels bot))
   (irc:join (slot-value bot 'connection) chan))
  (dolist (plugin (plugins bot))
    (connected plugin)))

(defun handle-priv-message (message)
  (with-slots (bot text type) message
    (case type
      (:public
       (multiple-value-bind (match msg)
	   (ppcre:scan-to-strings (concatenate 'string "^(" (nick bot) "\\W+|!)(.*)") text)
	 (when match
	   (setf text (elt msg 1))
	   (call-plugin-callbacks message))))
      (:private
       (call-plugin-callbacks message)))))

(defhook irc:irc-privmsg-message (bot irc-message)
  (let ((message (message-to-message irc-message bot)))
    (handle-priv-message message)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Implementations of generics  ;;
;;                              ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro if-bind ((var expr) (&body true-form*) &optional false-form)
  (with-gensyms (variable)
    `(let ((,variable ,expr))
       (if ,variable
	   (let ((,var ,variable))
	     ,@true-form*)
	   ,false-form))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmethod connect ((bot bot) server &optional (port 6667))
  (if-bind (conn (irc:connect :server server :port port :nickname (nick bot) :logging-stream t))
	   ((setf (slot-value bot 'connection) conn)
	    (dolist (hook *hooks*)
	      (irc:add-hook conn (car hook) (lambda (message) (apply (cdr hook) (list bot message))))))
	   (error "could not connect to server")))

(defmethod read-loop ((bot bot)) 
  (irc:read-message-loop (slot-value bot 'connection)))


;; TODO do some more cleanup
(defmethod disconnect ((bot bot))
  (with-slots (connection) bot
   (irc:quit connection "god wrote in lisp code")
   (setf connection nil)))

(defmethod connected ((plugin plugin)) nil)
(defmethod message ((plugin plugin) message) nil)

(defmethod reply (texts &optional to-user-p)
  (when *last-message*
    (with-slots (bot type from) *last-message*
      (let ((destination (or (channel *last-message*)
			     (nick (sender *last-message*)))))
	(if (listp texts)
	    (loop for msg in texts
	       for i from 1
	       do (progn
		    (irc:privmsg (slot-value bot 'connection) destination
				 (if (and to-user-p (equal i 1))
				     (format nil "~a: ~a" (nick from) msg)
				     msg))
		    (when (divisible i 3) ;; avoid flooding
		      (sleep 1))))
	    (irc:privmsg (slot-value bot 'connection) destination texts))))))

(defmethod action ((message string))
  (let ((destination (or (channel *last-message*)
			 (nick (sender *last-message*)))))
    (irc:privmsg (slot-value (bot *last-message*) 'connection) destination
		 (format nil "~AACTION ~A~A" (code-char 1) message (code-char 1)))))

(defmethod help ((plugin plugin))
  (declare (ignore plugin))
  :unimplemented-help)

(defun extract-user-from-irc-message (message)
  (make-instance 'user
		 :nick (irc:source message)
		 :host (irc:host message)
		 :username (irc:user message)))

(defun message-to-message (message bot)
  (let ((channel (find (elt (irc:arguments message) 0) (channels bot) :test #'string=)))
    (make-instance 'message
		   :text (car (last (irc:arguments message)))
		   :from (extract-user-from-irc-message message)
		   :bot bot
		   :channel channel
		   :type (if channel :public :private))))

(defmethod user-equal ((user1 user) (user2 user))
  (and (string-equal (nick user1) (nick user2))
       (string-equal (host user1) (host user2))
       (string-equal (name user1) (name user2))))

(defmethod hostmask ((user user))
  (format nil
	  "~a!~a@~a"
	  (nick user)
	  (name user)
	  (host user)))

(defmethod print-object ((object bot) s)
  (print-unreadable-object (object s :type t)
    (format s "~a" (nick object))))
