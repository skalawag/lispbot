(in-package :lispbot.http-plugins)

(defclass bullshit-plugin (plugin)
  ()
  (:default-initargs :name "bullshit"))

(defparameter *classnamer-url* "http://classnamer.com")
(defparameter *commit-msg-url* "http://whatthecommit.com")
(defparameter *saying-url* "http://sprichwortrekombinator.de")

(defun ->classname ()
  (let ((tree (chtml:parse (drakma:http-request *classnamer-url*) (cxml-stp:make-builder))))
    (stp:do-recursively (p tree)
      (when (and (typep p 'stp:element)
		 (equal (stp:local-name p) "p")
		 (equal (stp:attribute-value p "id") "classname"))
	(return-from ->classname (stp:string-value p))))))

(defun ->commit-msg ()
  (let ((tree (chtml:parse (drakma:http-request *commit-msg-url*) (cxml-stp:make-builder))))
    (stp:do-recursively(p tree)
      (when (and (typep p 'stp:element)
		 (equal(stp:local-name p) "div")
		 (equal(stp:attribute-value p "id") "content"))
        (return-from ->commit-msg (stp:string-value (second (stp:list-children p))))))))

(defun ->german-saying ()
  (let ((tree (chtml:parse (drakma:http-request *saying-url*) (cxml-stp:make-builder))))
    (stp:do-recursively (p tree)
      (when (and (typep p 'stp:element)
                 (equal (stp:local-name p) "div")
                 (equal (stp:attribute-value p "class") "spwort"))
        (return-from ->german-saying
          (stp:string-value p))))))

(defcommand classname ((plugin bullshit-plugin))
  (declare (ignore plugin))
  (reply (->classname)))

(defcommand commit ((plugin bullshit-plugin))
  (declare (ignore plugin))
  (reply (->commit-msg)))

(defcommand sprichwort ((plugin bullshit-plugin))
  (declare (ignore plugin))
  (reply (->german-saying)))
