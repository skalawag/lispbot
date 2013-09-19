(in-package :lispbot.plugins)

(defclass texas-holdem-plugin (plugin)
  ()
  (:default-initargs :name "texas-holdem"))

(defcommand holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand start-holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand join-holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand fold ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand call ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand check ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  nil)

(defcommand bet ((plugin texas-holdem-plugin) amt)
  (declare (ignore plugin))
  nil)

(defcommand raise ((plugin texas-holdem-plugin) amt)
  (declare (ignore plugin))
  nil)
