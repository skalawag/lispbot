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

(defcommand pff ((plugin roulette-plugin))
  "accepts a challenge of roulette if a challenge has been made."
  (declare (ignore plugin))
  (cond
    ((string= (nick (sender *last-message*)) *challenged*)
     (reply (format nil "~a accepts! The game begins!" *challenged*))
     (run-game)
     (setq *standing-challenge* nil
           *challenger* nil
           *challenged* nil))
    (t (reply (format nil "~a, this is none of your business!"
                      (nick (sender *last-message*)))))))

(defcommand undo ((plugin roulette-plugin))
  (declare (ignore plugin))
  (setf *standing-challenge* nil
        *challenged* nil
        *challenger* nil))

(defun run-game ()
  (let ((run-length (1+ (random 6)))
        (players (if (= (random 2) 0)
                     (list *challenger* *challenged*)
                     (list *challenged* *challenger*))))
    (reply (format nil "~a has won the coin toss!" (first players)))
    (sleep 1)
    (dotimes (i run-length)
      (sleep 1)
      (reply (format nil "~a spins the cylander!" (first players)))
      (sleep 1)
      (cond
        ((< i (- run-length 1))
         (reply "CLICK!"))
        ((= i (- run-length 1))
         (reply "BANG!")
         (reply (format nil "~a has blown his brains out again!" (first players)))
         (reply (format nil "Congratulations, ~a, you have won!" (second players)))))
      (setf players (list (second players) (first players))))))
