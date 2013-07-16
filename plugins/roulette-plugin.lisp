(in-package :lispbot.plugins)

(defclass roulette-plugin (plugin)
  ()
  (:default-initargs :name "roulette"))

(defmethod help ((plugin roulette-plugin))
   (reply "!r <nick>: challenge <nick> to a game of Russian Roulette.")
   (reply "!pff: accept a challenge to play Russian Roulette.")
   (reply "!undo: undo your f*ckup."))

(defparameter *standing-challenge* nil)
(defparameter *challenger* nil)
(defparameter *challenged* nil)
;(defparameter *stats* '(("wag" . (0 0)) ("skalawag" . (0 0)))) ;; ((name . '(wins losses)) (name2 . '(wins losses))...)
(defparameter *stats* nil)

;; figure out how to get the nick of the person who issues this command.
(defcommand r ((plugin roulette-plugin) target)
  "offers a game of roulette if no other challenge has been made."
  (declare (ignore plugin))
  (if (null *standing-challenge*)
      (progn
        (setf *standing-challenge* t)
        (setf *challenger* (nick (sender *last-message*)))
        (setf *challenged*  target)
        (reply
         (format nil "~a has challenged ~a to Russian Roulette!" *challenger* *challenged*)))
      (reply (format nil "~a, there is a standing challenge." (nick (sender *last-message*))))))

(defun reset ()
  (setf *standing-challenge* nil)
  (setf *challenger* nil)
  (setf *challenged* nil))

(defcommand pff ((plugin roulette-plugin))
  "accepts a challenge of roulette if a challenge has been made."
  (declare (ignore plugin))
  (cond
    ((string= (nick (sender *last-message*)) *challenged*)
     (reply (format nil "~a accepts! The game begins!" *challenged*))
     (run-game))
    (t (reply (format nil "~a, this is none of your business!"
                      (nick (sender *last-message*)))))))

(defcommand undo ((plugin roulette-plugin))
  (declare (ignore plugin))
  (reply (format nil "~a rescinds the challenge." *challenger*))
  (setf *standing-challenge* nil
        *challenged* nil
        *challenger* nil))

(defcommand rstats ((plugin roulette-plugin))
  (declare (ignore plugin))
  (dolist (item *stats*)
    (reply (format nil "~a: ~a" (car item) (float (/ (car (cdr item)) (+ (car (cdr item)) (car (cdr (cdr item))))))))))

(defcommand stats ((plugin roulette-plugin))
  "this is just for diagnostics"
  (declare (ignore plugin))
  (reply (format nil "~a" *stats*)))

(defun log-stats (name win)
  (cond
    (win
     (setf (car (cdr (assoc name *stats* :test #'string=))) (1+ (car (cdr (assoc name *stats* :test #'string=))))))
    ((null win)
     (setf (car (cdr (cdr (assoc name *stats* :test #'string=)))) (1+ (car (cdr (cdr (assoc name *stats* :test #'string=)))))))))

(defun run-game ()
  (let ((run-length (1+ (random 6)))
        (players (if (= (random 2) 0)
                     (list *challenger* *challenged*)
                     (list *challenged* *challenger*))))
    (reply (format nil "~a has won the coin toss!" (first players)))
    (sleep 1)
    (if (null (assoc (first players) *stats* :test #'string=))
        (setf *stats* (cons `(,(first players) . ,(list 0 0)) *stats*)))
    (if (null (assoc (second players) *stats* :test #'string=))
        (setf *stats* (cons `(,(second players) . ,(list 0 0)) *stats*)))
    (dotimes (i run-length)
      (sleep 1)
      (reply (format nil "~a spins the cylinder!" (first players)))
      (sleep 1)
      (cond
        ((< i (- run-length 1))
         (reply "CLICK!"))
        ((= i (- run-length 1))
         (reply "BANG!")
         (reply (format nil "~a has blown his brains out again!" (first players)))
         (sleep 1)
         (reply (format nil "Congratulations, ~a, you have won!" (second players)))
         (log-stats (first players) nil)
         (log-stats (second players) t)
         (reset)))
      (setf players (list (second players) (first players))))))
