(in-package :bot.plugins)

(defclass poll-plugin (plugin)
  ((track-users-p
    :initform t
    :initarg :track-users
    :accessor track-users-p
    :documentation "Allow a single user to vote more than once?")
   (polls
    :initform (make-hash-table :test 'equal)
    :initarg :polls
    :accessor polls
    :type hash-table
    :documentation "hashtable of polls")
   (file
    :initform (make-pathname :name "polls")
    :initarg :file
    :accessor file
    :type pathname
    :documentation "relative pathname of the file where the hashtable will be
saved (default is 'polls')"))
  (:default-initargs :name "poll"))

(defgeneric save-polls (plugin &key filename)
  (:documentation "save the all the polls things"))

(defgeneric load-polls (plugin &key filename)
  (:documentation "load the file of polls things into plugin"))

(defgeneric add-poll (plugin name description options)
  (:documentation "Add a new poll to the plugin or signal an error if a poll
with this name already exists"))

(defgeneric delete-poll (plugin name)
  (:documentation "Delete the poll with name name"))

(defclass poll ()
  ((name
    :initarg :name
    :reader poll-name
    :documentation "unique name of this poll")
   (description
    :initarg :description
    :reader poll-description
    :documentation "a description of this poll")
   (options
    :reader poll-options)
   (users
    :initform nil
    :reader poll-users
    :documentation "users who voted on this poll. If this is nil, users
are allowed to vote more than once.")))

(defcommand poll ((self poll-plugin) name description &rest options)
  (add-poll self name description options)
  (reply "Poll successfully created"))

(defcommand vote ((self poll-plugin) poll-name option-name)
  (if-let (poll (gethash poll-name (polls self)))
    (progn
     (when (and (poll-users poll)
                (gethash (user-indentificaton (sender *last-message*))
                         (poll-users poll)))
       (error "Sorry, you already voted on this poll"))
     (if-let (option (find option-name (poll-options poll) :key #'car :test #'string=))
       (incf (cdr option))
       (error "No such option in poll ~a: ~a" poll-name option-name))
     (reply (format nil "Voted for ~a on ~a" option-name poll-name))
     (setf (gethash (user-indentificaton (sender *last-message*)) (poll-users poll)) t))
    (error "There is no poll named ~a" poll-name)))

(defcommand result ((self poll-plugin) name)
  (if (gethash name (polls self))
      (reply (format nil "Stats for ~a: ~{~{~a: ~a~}~^, ~}"
                     name
                     (mapcar (lambda (x) (list (car x) (cdr x)))
                             (poll-options (gethash name (polls self)))))
             t)
      (error "No such poll: ~a" name)))

(defcommand polls ((self poll-plugin))
  (reply
   (mapcar
    (lambda (x)
      (format nil "~a: ~a" (poll-name x) (poll-description x)))
    (hash-table-values (polls self)))))

(defmethod help ((plugin poll-plugin))
  (reply '("A plugin to cast polls."
           "!poll name \"description\" options: Creates a poll"
           "!vote name option: Votes for \"option\" in \"poll\""
           "!result poll: Print current results for this poll"
           "!polls: list all active polls")))

(defun user-indentificaton (user)
  (concatenate 'string (name user) "@" (host user)))

(defmethod initialize-instance :after ((self poll) &key track-users options)
  (when track-users
    (setf (slot-value self 'users) (make-hash-table :test 'equal)))
  (setf (slot-value self 'options) (mapcar (rcurry #'cons 0) options)))

(defmethod add-poll ((self plugin) name description options)
  (if (gethash name (polls self))
      (error "A poll with the name ~a already exists!" name)
      (setf (gethash name (polls self))
            (make-instance 'poll
                           :name name
                           :description description
                           :options options
                           :track-users (track-users-p self)))))

(defmethod delete-poll ((self plugin) name)
  (if (gethash name (polls self))
      (remhash name (polls self))
      (error "No such poll: ~a" name)))

(defmethod save-polls ((plugin poll-plugin) &key filename)
  (let ((filename (or filename
		      (merge-pathnames (file plugin)
				       (data-dir (bot plugin))))))
    (ensure-directories-exist filename)
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (pprint (mapcar #'poll->list (hash-table-values (polls plugin))) stream))))

(defmethod load-polls ((plugin poll-plugin) &key filename)
  (let ((filename (or filename
		      (merge-pathnames (file plugin)
				       (data-dir (bot plugin))))))
    (ensure-directories-exist filename)
    (with-open-file (stream filename)
      (mapc (compose (lambda (x) (setf (gethash (poll-name x) (polls plugin)) x))
                     #'list->poll)
            (read stream)))))

(defun poll->list (poll)
  (with-slots (name description options users) poll
    `(,name ,description ,options ,(if users (hash-table-alist users) t))))

(defun list->poll (list)
  (let ((poll (make-instance 'poll
                             :name (first list)
                             :description (second list))))
    (setf (slot-value poll 'options) (third list))
    (when (listp (fourth list))
      (setf (slot-value poll 'users) (alist-hash-table (fourth list) :test 'equal)))
    poll))
