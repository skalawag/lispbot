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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Configuration variables ;;
;;                         ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-data-directory*
  (merge-pathnames ".lispbot/" (user-homedir-pathname))
  "the default place where the bot and plugins will create their files")

(defvar *debug* nil
  "print debug output?")

 ;;;;;;;;;;;;;;
;;            ;;
;; Bot Class  ;;
;;            ;;
 ;;;;;;;;;;;;;;

(defclass bot ()
  ((connection
    :initform nil
    :reader connection
    :type (or null irc:connection)
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
    :type string
    :documentation "the nickname of the bot")
   (data-dir
    :initform *default-data-directory*
    :initarg :data-dir
    :accessor data-dir
    :documentation "the directory where the bot and plugins will store there files"))
  (:documentation "a irc bot"))

(defun make-bot (nick channels &key plugins data-dir)
  "return a new bot with the nickname NICK witch joins the channels CHANNELS.
plugins can be instances of classes derived from PLUGIN, names of classes
derived from PLUGIN or lists of those including lists of lists of ..."
  (let ((bot (make-instance 'bot
			    :channels (ensure-list channels)
			    :nick nick)))
    (add-plugins bot plugins)
    (when data-dir (setf (data-dir bot) data-dir))
    bot))


(defgeneric start (bot server &optional port)
  (:documentation "connect to server and enter read loop"))

(defgeneric stop (bot)
  (:documentation "disconnect from server"))

(defgeneric send (lines to bot &key actionp)
  (:documentation "send a privmsg to `to' (which can be a chan or a user).
If `actionp' is true, use the ctcp action command"))

;; The next two functions rely on the context of *last-message*. They should only
;; be called from an implementation of handle-event or a command.

(defgeneric reply (texts &optional to-user-p)
  (:documentation "can be used by plugins to let the bot say something. `texts' can be a list of strings or a string.
If `to-user-p' is t, address the user of the last received message directly"))

(defgeneric action (texts)
  (:documentation "can be used by plugins write a /me message"))

 ;;;;;;;;;;;;;;;;;
;;               ;;
;; Plugin Class  ;;
;;               ;;
 ;;;;;;;;;;;;;;;;;

;; TODO: Make a special meta class for plugins.

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

(defgeneric handle-event (plugin event)
  (:documentation "plugins can implement this for the various events"))

(defgeneric help (plugin)
  (:documentation "called when the user requests help for a plugin"))

(defparameter *last-message* nil
  "this is bound to the last message to the bot during the exectution of commands
or the `handle-event' method of plugins")

(defmacro defcommand (name ((plvar plclass) &rest args) &body body)
  "define a new command for the plugin `plclass'. `Name' can be a string, or
a symbol (in witch case the command will be the lowercase symbolname). All
other parameters `args' are matched against the arguments to the
command, that was issued in a channel or a query."
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

(defun not-self-p (user bot)
  (not (string= (nick bot) (nick user))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Internal implementation ;;
;;                         ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-plugins (bot &rest plugins)
  "add plugins plugins to the bots plugin-list"
  (labels ((make-plugins (plugins bot)
	     (loop for p in plugins appending
		  (cond
		    ((listp p) (make-plugins p bot))
		    ((symbolp p) (ensure-list (make-instance p :bot bot)))
		    ((subtypep (type-of p) 'plugin) (progn
						      (setf (slot-value p 'bot) bot)
						      (ensure-list p)))
		    (t (error "strange plugin: ~a" p))))))
    (setf (plugins bot) (make-plugins plugins bot))))

(defparameter *hooks* nil)

(defmacro defhook (name (bot message) &body body)
  `(let ((cons (assoc ',name *hooks*))
	 (hook (lambda (,bot ,message)
		  ,@body)))
     (if cons
	 (setf (cdr cons) hook)
	 (push (cons ',name hook) *hooks*))))

(defhook irc:irc-rpl_luserme-message (bot message)
  (declare (ignore message))
  (dolist (chan (channels bot))
   (irc:join (slot-value bot 'connection) chan)))

(defhook irc:irc-privmsg-message (bot irc-message)
  (let ((message (make-event irc-message bot)))
    (handle-priv-message message)))

(defhook irc:irc-join-message (bot irc-message)
  (let ((*last-message* (make-event irc-message bot)))
    (when (not (string= (nick bot) (nick (user *last-message*))))
     (dolist (p (plugins bot))
       (handle-event p *last-message*)))))

(defun handle-errors-in-plugin (err plugin message)
  (declare (ignore plugin message))
  (reply (format nil "error: ~a~%" err)))

(defun command-regex (cmd)
  (format nil "^~a( (.*))?" (if (symbolp cmd)
				   (string-downcase (symbol-name cmd))
				   cmd)))

(defun string-splitter ()
  (let ((in-quotes nil)
	(escaped nil))
    (lambda (x)
      (cond
	(escaped (setf escaped nil))
	((char= x #\\) (setf escaped t) nil)
	((char= x #\") (setf in-quotes (not in-quotes)) t)
	((and (char= x #\Space)
	      (not in-quotes))
	 t)
	(t nil)))))

(defun split-string (string)
  (let ((list (partition:split-sequence-if (string-splitter)
					   string
					   :remove-empty-subseqs t)))
    list))

(defun call-commands (message)
  (dolist (plugin (plugins (bot message)))
    (let ((*last-message* message))
      (dolist (command (get (class-name (class-of plugin)) :commands))
	(destructuring-bind (name . function) command
	  (multiple-value-bind (match msg)
	      (ppcre:scan-to-strings (command-regex name) (text message))
	    (when match
	      (handler-case
		  (apply function plugin (split-string (elt msg 1)))
		(condition (err)
		  (handle-errors-in-plugin err plugin message))))))))))

(defun call-event-handlers (event)
  (dolist (p (plugins (bot event)))
    (handle-event p event)))

(defun handle-priv-message (message)
  (with-slots (bot text sender) message
    (when (not-self-p sender bot) ;; never respond to myself!!
      (let ((*last-message* message))
       (call-event-handlers message))
      (if (typep message 'channel-message)
	  (multiple-value-bind (match msg)
	      (ppcre:scan-to-strings (concatenate 'string "^(" (nick bot) "\\W+|!)(.*)") text)
	    (when match
	      (setf (original-text message) text)
	      (setf text (elt msg 1))
	      (call-commands message)))
	  (call-commands message)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Implementations of generics  ;;
;;                              ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-irc-port* 6667)

(defmethod start ((bot bot) server &optional (port *default-irc-port*))
  (if-let (conn (irc:connect
		 :server server
		 :port port
		 :nickname (nick bot)
		 :logging-stream *debug*))
    (progn
      (setf (slot-value bot 'connection) conn)
      (dolist (hook *hooks*)
	(irc:add-hook conn
		      (car hook)
		      (lambda (message)
			(funcall (cdr hook) bot message)))) 
      (unwind-protect
	   (irc:read-message-loop (connection bot))
	(let ((connection (connection bot)))
	  (setf (slot-value bot 'connection) nil)
	  (irc:quit connection "the lispbot: http://gitorious.org/lispbot"))))
    (error "could not connect to server")))

(defmethod send (lines to (bot bot) &key actionp)
  (let ((connection (connection bot))
	(to (if (userp to) (nick to) to))
	(lines (if actionp
		   (actionize-lines lines)
		   (ensure-list lines))))
   (loop for msg in lines
      for i from 1
      do (progn
	   (irc:privmsg connection to msg)
	   (when (= (mod i 3) 0)
	     (sleep 1))))))

(defmethod handle-event ((plugin plugin) (event event))
  (declare (ignore plugin event))
  nil)

(defgeneric reply-to-event (message lines &optional to-user-p))

(defun address-user (lines nick)
  (mapcar (lambda (x)
	    (format nil "~a: ~a" nick x))
	  (ensure-list lines)))

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
  (declare (ignore event lines to-user-p)))

(defmethod reply (lines &optional to-user-p)
  (reply-to-event *last-message* lines to-user-p))

(defparameter *ctcp-delimiter* (code-char 1))

(defun actionize-lines (lines)
  (mapcar (lambda (x)
	    (format nil "~aACTION ~a~@*~a~*" *ctcp-delimiter* x))
	  (ensure-list lines)))

(defmethod action (lines)
  (reply-to-event *last-message* (actionize-lines lines)))

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
    (princ (nick object) s)))

(defmethod print-object ((object plugin) s)
  (print-unreadable-object (object s :type t)
    (princ (name object) s)))
