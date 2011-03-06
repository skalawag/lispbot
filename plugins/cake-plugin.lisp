
(in-package :lispbot.plugins)

(defclass cake-plugin (plugin)
  ()
  (:default-initargs :name "cake"))

(defmethod help ((plugin cake-plugin))
  (help-for-commands plugin))

(defun make-cake-layer (size left-delimiter middle right-delimiter)
  (format nil "~{~A~^~}" (append (list  left-delimiter)
                                 (loop repeat size collecting middle)
                                 (list right-delimiter))))



(defcommand cake ((plugin cake-plugin) num)
  "!cake <number> : draws a crappy ASCII-art cake with <number> candles on top"
  (declare (ignore plugin))
  (reply (cons (make-cake-layer (parse-integer  num) " _" "i_" " ")
               (loop repeat 3 collect (make-cake-layer (parse-integer num) "|_" "__" "|")))))
