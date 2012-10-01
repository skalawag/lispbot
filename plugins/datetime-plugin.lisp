(in-package :lispbot.plugins)

(defparameter *datetime-day-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday"
    "Friday" "Saturday" "Sunday"))

(defclass datetime-plugin (plugin)
  ((utc
    :initarg :utc
    :initform nil
    :accessor datetime-utc-p))
  (:default-initargs :name "datetime"))

(defmethod help ((plugin datetime-plugin))
  (help-for-commands plugin))

(defun print-time (&optional timezone)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time) timezone)
    (format nil
            "~2,'0d:~2,'0d:~2,'0d - ~a - ~d.~2,'0d.~d (UTC~@d)"
            hour
            minute
            second
            (elt *datetime-day-names* day-of-week)
            date
            month
            year
            (+ (if dst-p 1 0 ) (- tz)))))

(defcommand date ((plugin datetime-plugin) &optional timezone)
  "Print current date"
  (declare (ignore plugin))
  (let ((zone (and timezone (parse-integer timezone))))
    (reply (print-time zone))))

;;; TODO User print-time in !time

(defcommand time ((plugin datetime-plugin))
  "Print current time"
  (declare (ignore plugin))
  (multiple-value-bind
        (second minute hour)
      (get-decoded-time)
    (reply (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))))
