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

(defcommand date ((plugin datetime-plugin))
  "Print current date"
  (declare (ignore plugin))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p))
    (reply (format nil
                   "~2,'0d:~2,'0d:~2,'0d - ~a - ~d.~2,'0d.~d (UTC~@d)"
                   hour
                   minute
                   second
                   (elt *datetime-day-names* day-of-week)
                   date
                   month
                   year
                   (- tz)))))

(defcommand time ((plugin datetime-plugin))
  "Print current time"
  (declare (ignore plugin))
  (multiple-value-bind
        (second minute hour)
      (get-decoded-time)
    (reply (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))))
