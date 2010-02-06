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
    :reader connection
    :documentation "internal connection representation")
   (channels        ;; list of strings. TODO: add channel class, that for example keeps track of users
    :initform nil
    :initarg :channels
    :reader channels
    :documentation "list of channels the bot has joined")
   (plugins
    :initform nil
    :initarg :plugins
    :accessor plugins
    :documentation "list of plugins for this bot")
   (nick
    :initform "lispbot"
    :initarg :nick
    :reader nick
    :documentation "the nickname of the bot"))
  (:documentation "a irc bot"))

;; TODO: implement (suggestion: implement as (setf (channels bot)))
(defgeneric join (bot channels)
  (:documentation "join one or more chans"))

;; TODO: implement (same suggestion as above)
(defgeneric leave (bot channels)
  (:documentation "leave one ore more chans"))

;; TODO: implement
(defgeneric (setf nick) (bot newnick)
  (:documentation "change the nick of the bot."))

(defun make-bot (nick channels &rest plugins)
  "return a new bot with the nickname NICK witch joins the channels CHANNELS.
plugins can be instances of classes derived from PLUGIN, names of classes
derived from PLUGIN or lists of those including lists of lists of ..."
  (labels ((make-plugins (plugins bot)
	     (loop for p in plugins appending
		  (cond
		    ((listp p) (make-plugins p bot))
		    ((symbolp p) (mklist (make-instance p :bot bot)))
		    ((subtypep (type-of p) 'plugin) (progn
						      (setf (slot-value p 'bot) bot)
						      (mklist p)))
		    (t (error "strange plugin: ~a" p))))))
    (let ((bot (make-instance 'bot
			      :channels (mklist channels)
			      :nick nick)))
      (setf (slot-value bot 'plugins) (make-plugins plugins bot))
      bot)))

;; TODO: allow adding instances of the plugin class
(defun add-plugins (bot &rest plugins)
  (setf (plugins bot)
	(append (plugins bot)
		(mapcar (lambda (x)
			  (cond
			    ((symbolp x) (make-instance x :bot bot))
			    ((subtypep (type-of x) 'plugin) x)
			    (t (error "strange plugin: ~a" x))))
			plugins))))

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

(defgeneric send (lines to bot)
  (:documentation "send a privmsg to TO (which can be a chan or a user)"))

;; Convenience functions. Should only be called from commands, or some reimplemented handle-event methods

(defgeneric reply (texts &optional to-user-p)
  (:documentation "can be used by plugins to let the bot say something. message can be a list of strings or a string.
If to-user-p is t, address the user of the last received message directly"))

(defgeneric action (texts)
  (:documentation "can be used by plugins write a /me message"))

;; TODO: implement some sort of translation system
(defun l10n (string)
  string)

 ;;;;;;;;;;;;;;;;;
;;               ;;
;; Plugin Class  ;;
;;               ;;
 ;;;;;;;;;;;;;;;;;

(defclass plugin ()
  ((name
    :initform (error "Plugins need names")
    :accessor name
    :initarg :name)
   (bot
    :initform nil
    :reader bot
    :initarg :bot))
  (:documentation "all plugins must derive from this class"))

(defgeneric connected (plugin)
  (:documentation "called when the bot is connected and the channel is joined"))

(defgeneric handle-event (plugin event)
  (:documentation "plugins can implement this for the various events"))

(defgeneric help (plugin)
  (:documentation "called when the user requests help for a plugin"))

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (x) `(,x (gensym))) syms)
     ,@body))

(defmacro defcommand (name ((plvar plclass) &rest args) &body body)
  "define a new command for the plugin PLCLASS. NAME can be a string, or
a symbol (in witch case the command will be the lowercase symbolname. All
other parameters ARGS are matched against the command arguments to the
command, that was issued in a channel or a query. Note that this currently
has some limitations, such as not supporting multi-word arguments, or
keyword parameters."
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

(defun userp (object)
  (eq (type-of object) 'user))

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
    (let ((*last-message* message) (type (type-of message)))
      (when (or (eq type 'channel-message) (eq type 'query-message))
	(dolist (command (get (class-name (class-of plugin)) :commands))
	  (let ((name (car command)) (function (cdr command)))
	    (multiple-value-bind (match msg)
		(ppcre:scan-to-strings (format nil "^~a(\\W+(.*))?" (if (symbolp name)
									(string-downcase (symbol-name name))
									name))
				       (text message))
	      (when match
		(handler-case
		    ;; TODO: implement our own 'Argument String' -> 'Lambda list' function that handles quotes
		    ;;       and keyword parameters
		    (apply function plugin (partition:split-sequence #\Space (elt msg 1) :remove-empty-subseqs t))
		  (error (err) (handle-errors-in-plugin err plugin message))))))))
      (handle-event plugin *last-message*))))

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
  (with-slots (bot text) message
    (case (type-of message)
      (channel-message
       (multiple-value-bind (match msg)
	   (ppcre:scan-to-strings (concatenate 'string "^(" (nick bot) "\\W+|!)(.*)") text)
	 (when match
	   (setf (original-text message) text)
	   (setf text (elt msg 1))
	   (call-plugin-callbacks message))))
      (query-message
       (call-plugin-callbacks message)))))

(defhook irc:irc-privmsg-message (bot irc-message)
  (let ((message (make-event irc-message bot)))
    (handle-priv-message message)))

(defhook irc:irc-join-message (bot irc-message)
  (let ((*last-message* (make-event irc-message bot)))
    (when (not (string= (nick bot) (nick (user *last-message*))))
     (dolist (p (plugins bot))
       (handle-event p *last-message*)))))

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
(defmethod handle-event ((plugin plugin) (event event)) nil)

(defmethod send (lines to (bot bot))
  (let ((connection (connection bot))
	(to (if (userp to) (nick to) to)))
   (loop for msg in (mklist lines)
      for i from 1
      do (progn
	   (irc:privmsg connection to msg)
	   (when (divisible i 3)
	     (sleep 1))))))

(defgeneric reply-to-event (message lines &optional to-user-p))

(defun address-user (lines nick)
  (mapcar (lambda (x)
	    (format nil "~a: ~a" nick x))
	  (mklist lines)))

(defmethod reply-to-event ((message channel-message) lines &optional to-user-p)
  (send (if to-user-p
	    (address-user lines (nick (sender message)))
	    lines)
	(channel message) (bot message)))

(defmethod reply-to-event ((message query-message) lines &optional to-user-p)
  (declare (ignore to-user-p))
  (send lines (sender message) (bot message)))

(defmethod reply-to-event ((event join-event) lines &optional to-user-p)
  (send (if to-user-p
	    (address-user lines (nick (user event)))
	    lines)
	(channel event) (bot event)))

(defmethod reply-to-event ((event (eql nil)) lines &optional to-user-p)
  (declare (ignore event))
  (declare (ignore lines))
  (declare (ignore to-user-p)))

(defmethod reply (lines &optional to-user-p)
  (reply-to-event *last-message* lines to-user-p))

(defmethod action (lines)
  (let ((delimiter (code-char 1))) ;; CTCP delimiter for ACTION is ascii 0x1
   (reply-to-event *last-message*
		   (mapcar (lambda (x)
			     (format nil "~aACTION ~a~a" delimiter x delimiter))
			   (mklist lines)))))

(defmethod help ((plugin plugin))
  (declare (ignore plugin))
  :unimplemented-help)

(defmethod user-equal ((user1 user) (user2 user))
  (and (string-equal (nick user1) (nick user2))
       (string-equal (host user1) (host user2))
       (string-equal (name user1) (name user2))))

;; TODO: also specialize the hostmask function on the bot class
(defmethod hostmask ((user user))
  (format nil
	  "~a!~a@~a"
	  (nick user)
	  (name user)
	  (host user)))

(defmethod print-object ((object bot) s)
  (print-unreadable-object (object s :type t)
    (format s "~a" (nick object))))
