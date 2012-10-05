(in-package :lispbot.plugins)

(defclass log-plugin (plugin)
  ((log-stream
    :initarg :log-stream
    :initform nil
    :accessor log-plugin-stream))
  (:default-initargs :name "log"))

(defun log-print-date (stream)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format stream
            "~2,'0d.~2,'0d.~d ~2,'0d:~2,'0d:~2,'0d"
            date month year hour minute second)))

(defmethod handle-event ((self log-plugin) (event channel-message))
  (with-slots (log-stream) self
   (format log-stream
           "~a ~a ~a ~a~%"
           (log-print-date nil)
           (channel event)
           (nick (sender event))
           (text event)))
  (force-output (log-plugin-stream self)))
