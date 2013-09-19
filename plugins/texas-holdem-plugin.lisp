(in-package :lispbot.plugins)

(defclass texas-holdem-plugin (plugin)
  ()
  (:default-initargs :name "texas-holdem"))

(defcommand holdem ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand start-holdem ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand join-holdem ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand fold ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand call ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand check ((plugin poker-plugin))
  (declare (ignore plugin))
  nil)

(defcommand bet ((plugin poker-plugin) amt)
  (declare (ignore plugin))
  nil)

(defcommand raise ((plugin poker-plugin) amt)
  (declare (ignore plugin))
  nil)
