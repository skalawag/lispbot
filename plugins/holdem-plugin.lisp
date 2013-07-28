(in-package :lispbot.plugins)

(defclass holdem-plugin (plugin)
  ()
  (:default-initargs :name "holdem"))

(defcommand rd ((plugin holdem-plugin))
  (declare (ignore plugin))
  (display-game-state))

(defcommand holdem ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*))))
    (cond
      (*game-offered*
       (reply "A holdem game has already been started!"))
      (t
       (setf *players* nil)
       (setf *game-offered* t)
       (push (make-player player) *players*)
       (reply
        (format nil "~a has offered a game of Texas Holdem! '!join-holdem' to join the game." player))))))

(defcommand join-holdem ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*)))
        (already-joined (mapcar #'pname *players*)))
    (cond
      ((member player already-joined :test #'string=)
       (reply "You are already in the game." t))
      (*game-started*
       (reply "A game is in progress. You will join the next hand." t)
       (push (make-player player) *on-deck*))
      (t
       (seat-player (make-player player))
       (reply (format nil "~a has joined Texas Holdem!" player))
       (reply (format nil "There are ~a players in the game. Anyone else?" (length *players*)))))))

(defcommand sit-out ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*))))
    (dolist (p *players*)
      (when (string= (pname p) player)
        (push p *sitting-out*)
        (remove (first *sitting-out*) *players* :test #'equal)
        (reply "You are sitting out." t)))))

(defcommand sit-in ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*))))
    (dolist (p *sitting-out*)
      (when (string= (pname p) player)
        (push p *on-deck*)
        (remove (first *on-deck*) *sitting-out* :test #'equal)
        (reply "You are sitting in and will join the next hand." t)))))

(defun update ()
  (when (> (length *on-deck*) 0)
    (append *on-deck* *players*)
    (setf *on-deck* nil))
  (update-game-state)
  (display-game-state))

(defcommand reset-holdem ((plugin holdem-plugin))
  (declare (ignore plugin))
  (setf *game-offered* nil
        *game-over* nil
        *game-started* nil
        *on-deck* nil
        *sitting-out* nil
        *winners* nil
        *players* nil
        *hand-number* -1)
  (reply "Holdem reset."))

(defun rset-holdem ()
  (setf *game-offered* nil
        *game-over* nil
        *game-started* nil
        *on-deck* nil
        *sitting-out* nil
        *winners* nil
        *players* nil
        *hand-number* -1))

(defcommand start-holdem ((plugin holdem-plugin))
  (declare (ignore plugin))
  (setf *game-started* t)
  (reply "Let the game begin....")
  (holdem-reset)
  (display-game-state))

(defun verify-next (name)
  (let ((next-up (get-next-up)))
    (if next-up
        (string= name (pname next-up))
        nil)))

(defcommand fold ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-fold)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

(defcommand check ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-check)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

(defcommand call ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-call)))
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

(defcommand bet ((plugin holdem-plugin) amt)
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-bet amt)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

(defcommand raise ((plugin holdem-plugin) amt)
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-raise amt)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

(defcommand allin ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-allin)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (update))))
      (t (reply "It's not your turn!" t)))))

;; ;;;; cl-poker
(defparameter *game-offered* nil)
(defparameter *game-over* nil)
(defparameter *game-started* nil)
(defparameter *on-deck* nil)
(defparameter *sitting-out* nil)
(defparameter *player-chips* 2000)
(defvar *max-players* 12)
(defparameter *players* nil
  "Players involved in the game. A player is just popped when he goes
  broke.")
(defparameter *hand-number* -1)
(defparameter *board* nil
  "Community cards.")
(defparameter *folded-chips* 0)
(defparameter *bet* 0)
(defparameter *sb* 5)
(defparameter *bb* 10)
(defparameter *acts* 0
  "This variable is a counter. When a player bets or raises it is
  set (or reset) to 1 (plus the number of already folded players). All
  other actions increment it. When it equals the length of *players*,
  the betting round is ended.")
(defparameter *stage* 'preflop)
(defparameter *winners* nil)

;;;; Players and seating
(defun make-player (name)
  (list
   :name name
   :chips *player-chips*
   :position nil ; seat number at table
   :cards nil
   :hole-cards nil
   :allin nil
   :bet 0
   :chips-in-pot 0
   :act nil
   :next-up nil
   :folded nil
   :option nil))

(defun chips-in-pot (player &optional add subtract zero)
  (cond
    (add
     (setf (getf player :chips-in-pot)
	   (+ (getf player :chips-in-pot) add)))
    (subtract
     (setf (getf player :chips-in-pot)
	   (- (getf player :chips-in-pot) subtract)))
    (zero
     (setf (getf player :chips-in-pot) 0))
    (t (getf player :chips-in-pot))))

(defun next-up (player &optional set unset)
  (cond
    (set (setf (getf player :next-up) t))
    (unset (setf (getf player :next-up) nil))
    (t (getf player :next-up))))

(defun option (player &optional has-option zero)
  (cond
    (has-option
     (setf (getf player :option) t))
    (zero
     (setf (getf player :option) nil))
    (t (getf player :option))))

(defun folded (player &optional fold zero)
  (cond
    (fold
     (setf (getf player :folded) t))
    (zero
     (setf (getf player :folded) nil))
    (t
     (getf player :folded))))

(defun pname (player)
  (getf player :name))

(defun seat (player &optional num)
  (if num
      (setf (getf player :position) num)
      (getf player :position)))

(defun act (player &optional set zero)
  (cond
    ((and set (null zero))
     (setf (getf player :act) set))
    ((and (null set) (null zero))
     (getf player :act))
    ((and set zero)
     (setf (getf player :act) nil))))

(defun bet (player &optional add subtract zero allin)
  (cond
    (add
     (setf (getf player :bet) (+ (getf player :bet) add))
     (chips-in-pot player add)
     (setf (getf player :act) 'bet)
     (setf (getf player :chips) (- (getf player :chips) add))
     (if (<= (chips player) 0)
	 (allin player t)))
    (subtract
     (setf (getf player :bet) (- (getf player :bet) subtract)))
    (zero
     (setf (getf player :bet) 0))
    (allin
     (let ((bet (getf player :chips)))
       (setf (getf player :bet) (+ bet (chips-in-pot player)))
       (setf (getf player :chips) 0)
       (chips-in-pot player bet)
       (allin player t)))
    (t (getf player :bet))))

(defun allin (player &optional yes zero)
  (cond
    (yes
     (setf (getf player :allin) t))
    (zero
     (setf (getf player :allin) nil))
    (t
     (getf player :allin))))

(defun chips (player &optional add subtract)
  (cond
    ((and add (not (numberp add)))
     (print "In chips, add amt was not a number."))
    ((and subtract (not (numberp subtract)))
     (print "In chips, subtract amt was not a number."))
    (add
     (setf (getf player :chips) (+ (getf player :chips) add)))
    (subtract
     (setf (getf player :chips) (- (getf player :chips) subtract)))
    (t (getf player :chips))))

(defun cards (player &optional app replace zero)
  (cond
    (app
      (append app (getf player :cards)))
    (replace
     (setf (getf player :cards) replace))
    (zero
     (setf (getf player :cards) nil))
    (t
     (getf player :cards))))

(defun renumber-players ()
  (let ((tar (sort (get-unfolded) #'> :key #'(lambda (p) (seat p)))))
    (dolist (p tar)
      (seat p (- 12 (position p tar))))
    (setf *players* (reverse tar))))

(defun seat-player (player)
  (cond
    ((>= (length *players*) *max-players*)
     (print "Too many players"))
    ((listp player)
     (setf *players* (append (list player) *players*))
     (dolist (p *players*)
       (seat p (- 12 (position p *players*)))))
    (t
     (setf *players* (append (list (make-player player)) *players*))
     (dolist (p *players*)
       (seat p (- 12 (position p *players*)))))))

(defun seat-players (list-of-player-names)
  (cond
    ((> (length list-of-player-names) *max-players*)
     (format t "~a is greater than *max-players*~%"
	     (length list-of-player-names)))
    (t
     (dolist (n list-of-player-names)
       (seat-player (make-player n))))))

(defun rotate-players ()
  (let ((tar (sort (copy-tree *players*) #'< :key #'(lambda (p) (seat p)))))
    (seat (car tar) 12)
    (mapcar #'(lambda (p) (seat p (1- (seat p)))) (cdr tar))
    (setf *players* tar)))

(defun get-blind (&optional big)
  "By convention, blinds are always seats 11 and 12."
  (dolist (p *players*)
    (cond
      ((and big (= (seat p) 11)) (return p))
      ((and (null big) (= (seat p) 12)) (return p)))))

(defun get-player-n (n)
  (dolist (p *players*)
    (when (= n (seat p))
      (return p))))

(defun get-unfolded ()
  (remove-if #'folded *players*))

(defun clear-next-up ()
  (dolist (p *players*) (next-up p nil t)))

(defun get-next-up ()
  (let ((res (car (remove-if #'(lambda (p) (not (next-up p))) *players*))))
    (if res res nil)))

(defun set-next-up ()
  "When set-next-up returns nil, either all but one player is all-in
or folded."
  (let ((next-up (get-next-up)))
    (cond
      ((null next-up) nil)
      (t
       (move-next-up)
       (get-next-up)))))

(defun move-next-up ()
  (let* ((players (sort (copy-list *players*) #'> :key #'(lambda (p) (seat p))))
	 (pos-cur-next-up
	 (position
	  (car (remove-if #'(lambda (p) (not (next-up p))) players))
	  players :test #'equal))
	 (new-list (append (subseq players pos-cur-next-up)
			   (subseq players 0 pos-cur-next-up))))
    (next-up (car new-list) nil t)
    (dolist (p (cdr new-list))
      (cond
	((and
	  (not (allin p))
	  (not (folded p)))
	 (next-up p t)
	 (return))))))

(defun get-small-blind-or-next ()
    (cond
      ((get-blind) (get-blind))
      ((get-blind t) (get-blind t))
      (t
       (first
	(sort
	 (get-unfolded)
	 #'> :key #'(lambda (p) (seat p)))))))

(defun next-stage ()
  (set-active-players)
  (cond
    ((eql *stage* 'preflop)
     (setq *stage* 'flop)
     (next-up (get-small-blind-or-next) t))
    ((eql *stage* 'flop)
     (setq *stage* 'turn)
     (next-up (get-small-blind-or-next) t))
    ((eql *stage* 'turn)
     (setq *stage* 'river)
    (next-up (get-small-blind-or-next) t))
    ((eql *stage* 'river)
     (setq *stage* 'preflop))))

(defun make-deck ()
  (let ((res '()))
    (dolist (rank '("A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"))
      (dolist (suit '("h" "s" "c" "d"))
	(push (list rank suit) res)))
    (mapcar #'(lambda (x) (concatenate 'string (first x) (second x))) res)))

(defun shuff (deck)
  "adapted from something nicked from pastebin, author unknown"
  (do ((k (random (length deck) (make-random-state t))
          (random (length deck) (make-random-state t)))
       (result nil))
      ((null (rest deck)) (cons (first deck) result))
    (setq result (cons (nth k deck) result)
          deck (reverse (cdr (reverse
                              (subst (car (reverse deck))
                                     (nth k deck)
                                     deck)))))))

(defun deal-cards ()
  (let ((deck (shuff (make-deck))))
    (dolist (p *players*)
      (let ((cards nil))
	(push (pop deck) cards)
	(push (pop deck) cards)
	(setf (getf p :cards) cards)
        (setf (getf p :hole-cards) (copy-list cards))))
    (setf *board* (subseq deck 0 5))))

(defun post-blinds ()
  (let ((sb (get-blind))
	(bb (get-blind t)))
    (bet sb *sb*)
    (bet bb *bb*)
    (option bb t)
    (setf *bet* *bb*)))

(defun pot-total ()
  (let ((res 0))
    (dolist (p *players*)
      (setf res (+ res (chips-in-pot p))))
    res))

(defun clear-line-bets ()
  (dolist (p *players*)
    (bet p nil nil t)))

(defun clear-acts (&optional except-player)
  (let ((players
	 (remove-if #'(lambda (p) (equal p except-player)) *players*)))
    (if except-player
	(mapcar #'(lambda (p) (act p t t)) players)
	(mapcar #'(lambda (p) (act p t t)) *players*))))

(defun verify-player-act (player act &optional amt)
  (cond
    ((eq act 'fold)
     (cond
       ((and (= *bet* *bb*) (option player))
	"It is not rational to fold when you can check.")
       ((= *bet* 0) "Folding when you can check is irrational.")
       (t 1)))
    ((eq act 'check)
     (cond
       ((option player)
	(option player nil t)
	1)
       ((> *bet* 0)
	"You cannot check to a bet.")
       (t 1)))
    ((eq act 'call)
     (cond
       ((= *bet* 0)
	"There is no bet to call.")
       ((option player)
	(option player nil t)
	1)
       (t 1)))
    ((eq act 'bet)
     (cond
       ((> amt (chips player))
	"You don't have enough chips to do that.")
       ((and (< amt *bb*)
	     (> (chips player) *bb*))
	"Your bet is too small.")
       (t 1)))
    ((eq act 'raise)
     (cond
       ((= *bet* 0)  "There is no bet to raise.")
       ((< amt (* 2 *bet*))  "Your raise is too small.")
       ((> amt (chips player))  "You don't have enough chips.")
       ((option player)
	(option player nil t)
	1)
       (t 1)))
    ((eq act 'allin)
     (cond
       ((every
         #'(lambda (p) (allin p))
         (remove player (get-unfolded) :test #'equal))
       "Try calling.")
       (t 1)))))

(defun record-player-act (player action &optional amt)
  (let ((ver (verify-player-act player action amt)))
    (cond
      ((stringp ver)
       ver)
      ((or (eql action 'bet) (eql action 'raise))
       (setf *bet* (+ amt (bet player)))
       (setf *acts* 1)
       (clear-acts player)
       (bet player amt))
      ((eql action 'call)
       (bet player (- *bet* (bet player)))
       (act player 'call)
       (setf *acts* (+ 1 *acts*)))
      ((eql action 'fold)
       (act player 'fold)
       (folded player t)
       (set-active-players))
      ((eql action 'check)
       (setf *acts* (+ 1 *acts*))
       (act player action))
      ((eql action 'allin)
       (bet player nil nil nil t)
       (setf *bet* (chips-in-pot player))
       (setf *acts* 1)
       (act player action))
      (t (print "Problem in record-player-act")))))

(defparameter *active-players* 0)

(defun set-active-players ()
  (setf *active-players* (length (get-unfolded))))

(defun stage-over-p ()
   (when (>= *acts* *active-players*) t))

(defun remove-allin-or-folded ()
  (remove-if
   #'(lambda (p)
       (or (allin p) (folded p)))
   *players*))

(defun hand-over-p ()
  (when (or
	 (and (eq *stage* 'river) (stage-over-p))
	 (< (length (get-unfolded)) 2)
	 (null (get-next-up))
         ;; changed this from 0 to 1.
         (and (<= (length (remove-allin-or-folded)) 1)
              (stage-over-p)))
    t))

(defun game-over-p ()
  (< (length
      (remove-if
       #'(lambda (v) (<= v 0))
       *players*
       :key #'(lambda (p) (chips p))))
     2))

(defun find-winners ()
  (let ((players (remove-if
		  #'(lambda (p) (folded p))
		  (copy-list *players*)))
	(straight-flush nil) (quads nil) (house nil) (flush nil) (straight nil)
        (trips nil) (two-pair nil) (pair nil) (high-card nil))
    ;; eval each hand
    (dolist (p players)
      (cards p nil (get-best-5-card-hand
			     (append
			      (cards p)
			      *board*))))
    ;; sort the winners into hand-types
    (dolist (p *players*)
      (case (first (cards p))
	(straight-flush (push p straight-flush)) (quads (push p quads))
	(house (push p house)) (flush (push p flush))
        (straight (push p straight)) (trips (push p trips))
        (two-pair (push p two-pair)) (pair (push p pair))
	(high-card (push p high-card))))
    ;; sort each hand type by numeric value
    (let ((sorted-bins
           (mapcar
            #'(lambda (x)
                (sort x #'> :key #'(lambda (y) (third (cards y)))))
            (remove-if #'null
                       (list straight-flush quads house flush
                             straight trips two-pair pair high-card)))))
      (labels ((flatten-once (source)
		 (cond
		   ((null (cdr source)) (car source))
		   (t (append (car source) (flatten-once (cdr source)))))))
	(setf *winners* (add-dummy-folded (flatten-once sorted-bins))))))
  nil)

(defun add-dummy-folded (winners)
  "To make sure folded players pay out their chips when we call
payoff-players, we need to add them to the winners list. But we don't
want them to win any chips, so we'll put them at the end."
  (append winners (remove-if #'(lambda (p) (not (folded p))) *players*)))

(defun sort-by-chips-in-pot (alis)
  (sort alis #'< :key #'(lambda (p) (chips-in-pot p))))

(defun partition-best-and-rest (winners)
  (let* ((tied (remove-if
		#'(lambda (p)
		    (not (eql (third (cards p))
			      (third (cards (first winners))))))
		winners))
	 (remainder (remove-if
		     #'(lambda (p)
			 (member p tied :test #'equal)) winners)))
    (list (sort-by-chips-in-pot tied) remainder)))

(defun debit-losers (losers amt)
  (let ((res 0))
    (dolist (l losers)
      (if (< (chips-in-pot l) amt)
	  (progn
	    (setf res (+ res (chips-in-pot l)))
	    (chips-in-pot l nil nil t))
	  (progn
	    (setf res (+ res amt))
	    (chips-in-pot l nil amt))))
    res))

(defun payoff-players (winners)
  (let ((partitioned (partition-best-and-rest winners)))
    (let ((winners (first partitioned))
	  (losers (second partitioned)))
      (cond
	((null winners) nil)
	((and (= (length winners) 1) (null losers))
	 (chips (car winners) (chips-in-pot (car winners)))
	 (chips-in-pot (car winners) nil nil t))
	((= (length winners) 1)
	 (chips (car winners)
		(debit-losers
		 losers
		 (chips-in-pot (first winners))))
	 ;; pay back the player his own chips!
	 (chips (car winners) (chips-in-pot (car winners)))
	 (chips-in-pot (car winners) nil nil t)
	 (payoff-players losers))
	((> (length winners) 1)
	 (let ((target-amt (chips-in-pot (first winners))))
	   (divide-chips winners (debit-losers losers target-amt))
	   (dolist (w winners)
	     (chips w target-amt)
	     (chips-in-pot w nil target-amt))
	   (payoff-players (append (cdr winners) losers))))))))

(defun divide-chips (list-of-tied amt)
  (cond
    ((= (mod amt (length list-of-tied)) 0)
     (dolist (p list-of-tied)
       (chips p (/ amt (length list-of-tied)))))
    (t
     (let ((reduced (- amt (mod amt (length list-of-tied))))
	   (remainder (rem amt (length list-of-tied))))
       (dolist (p list-of-tied)
	 (chips p (/ reduced (length list-of-tied))))
       (dotimes (i remainder)
	 (chips (nth i list-of-tied) 1))))))

(defun holdem-reset ()
  (cond
    ((game-over-p)
     (reply "The Game is Over!")
     (reply (format nil "The winner is ~a with ~a chips! Congratulations!"
                    (name
                     (first
                      (remove-if #'(lambda (p) (<=(chips p) 0)) *players*)))
                    (chips
                     (first
                      (remove-if #'(lambda (p) (<=(chips p) 0)) *players*)))))
     (rset-holdem))
    (t
     (setf *hand-number* (1+ *hand-number*))
     (reply (format nil "** Hand ~a **" *hand-number*))
     (setf *stage* 'preflop)
     ;; reset players properties
     (dolist (p *players*)
       (act p t t)
       (cards p nil nil t)
       (allin p nil t)
       (folded p nil t)
       (bet p nil nil t)
       (option p nil t)
       (chips-in-pot p nil nil t))
     (clear-acts)
     (setf *acts* 0)
     (setf *winners* nil)
     (setf *bet* 0)
     ;; first we rotate the players, since some might get eliminated
     ;; before renumbering
     (rotate-players)
     ;; get rid of broke players
     (setf *players*
	   (remove-if
	    #'(lambda (v) (<= v 0))
	    *players* :key #'(lambda (p) (chips p))))
     ;; sort the remaining players so the renumbering comes out correct.
     (setf *players*
	   (sort *players* #'< :key #'(lambda (p) (seat p))))
     ;; now set the new numbering (have to reverse them for renumbering
     ;; to work properly)
     (renumber-players)
     (setf *players*
	   (sort *players* #'< :key #'(lambda (p) (seat p))))
     ;; (if (= (length *players*) 2)
     ;;     (next-up (get-player-n 11) t)
     ;;     (next-up (get-player-n 10) t))
     ;; sanity check
     ;; (format t "~%~%The number of chips in the game = ~a.~%~%"
     ;;         (reduce #'+ (mapcar #'(lambda (p) (chips p)) *players*)))
     (post-blinds)
     (deal-cards)
     (dolist (p *players*)
       (send (format nil "Your cards: ~a" (getf p :cards)) (pname p) (bot *last-message*)))
     (set-active-players)
     ;; set next-up
     (clear-next-up)
     (cond
       ((= (length *players*) 2)
	(next-up (get-player-n 12) t))
       (t (next-up (get-player-n 10) t))))))

(defun show-down ()
  (let ((players (get-unfolded)))
    (dolist (p players)
      (reply (format nil "~a shows: ~a" (pname p) (getf p :hole-cards))))
    (reply (format nil "Community cards: ~a" *board*))))

(defun show-called (called)
  (reply (format nil "~a has been called and shows: ~a"
                 (pname called) (getf called :hole-cards)))
  (reply (format nil "Community Cards: ~a" *board*)))

(defun display-winners (&optional show)
  (let ((winners
	 (remove-if
	  #'(lambda (p) (not (eql (third (cards p))
				  (third (cards (first *winners*))))))
	  (remove-if #'(lambda (p) (folded p)) *winners*))))
    (cond
      ((null show)
       (dolist (w winners)
	 (reply (format nil "*Winner*: ~a." (pname w)))))
      (t
       (dolist (w winners)
         (reply (format nil "*Winner*: ~a holding ~a (~a)."
		  (pname w)
		  (second (cards w))
		  (first (cards w)))))))))

(defun display-board ()
  "called by display-game-state"
  (cond
    ((eq *stage* 'preflop)
     (reply (format nil "Comm Cards: ")))
    ((eq *stage* 'flop)
     (reply (format nil "Comm Cards: ~a" (subseq *board* 0 3))))
    ((eq *stage* 'turn)
     (reply (format nil "Comm Cards: ~a" (subseq *board* 0 4))))
    ((eq *stage* 'river)
     (reply (format nil "Comm Cards: ~a" *board*)))))


;; ;;; display

(defun display-game-state ()
  (display-board)
  (reply (format nil "Pot: ~a" (pot-total)))
  (reply (format nil "~5a ~10a ~4a ~5a ~4a ~5a ~3a ~3a" "Seat" "Name" "Nxt" "Chips" "Bet" "Act" "Fld" "Opt"))
  (dolist (p *players*)
    (sleep .1)
    (reply (format nil "~5a ~10a ~4a ~5a ~4a ~5a ~3a ~3a"
	    (seat p)
            (pname p)
	    (if (next-up p) "*" "")
	    (chips p)
            (bet p)
	    (if (act p) (act p) "--")
	    (if (folded p) (folded p) "--")
	    (if (option p) (option p) "--")))))

(defun update-game-state ()
  (cond
    ((hand-over-p)
     (cond
       ((= (length (remove-if #'(lambda (p) (folded p)) *players*)) 1)
	;; all but one have folded
	(reply (format nil "The hand is complete."))
	(clear-line-bets)
	(let ((winner (car (get-unfolded))))
	  (chips winner
		 (reduce #'+
			 (mapcar #'(lambda (p)
				     (chips-in-pot p)) *players*))))
	(find-winners)
	(display-winners)
	(holdem-reset)
        (when *game-over*
          (rset-holdem)))
       ((every #'(lambda (p) (allin p)) (get-unfolded))
        (show-down)
        (clear-line-bets)
	(find-winners)
	(payoff-players *winners*)
	(display-winners t)
	(holdem-reset)
        (when *game-over*
          (rset-holdem)))
       (t
	(reply (format nil "The hand is complete."))
	(clear-line-bets)
        (let ((tar
               (car
                (remove-if
                 #'(lambda (p) (not
                                (or
                                 (eq (act p) 'allin)
                                 (eq (act p) 'raise)
                                 (eq (act p) 'bet))))
                 *players*))))
          (when tar
            (show-called tar)))
	(find-winners)
	(payoff-players *winners*)
	(display-winners t)
	(holdem-reset)
        (when *game-over*
          (rset-holdem)))))
    ((stage-over-p)
     (next-stage)
     (reply (format nil "*** ~a ***" *stage*))
     (clear-line-bets)
     (clear-acts)
     (setf *bet* 0)
     (setf *acts* 0)
     (clear-next-up)
     (let ((unfolded (sort (get-unfolded) #'>
			   :key (lambda (p) (seat p)))))
       (dolist (u unfolded)
	 (if (and
	      (not (allin u))
	      (> (chips u) 0))
	     (return (next-up u t))))))
    (t
     (set-next-up))))

;;; game actions (these are pretty much superflous in this context)

(defun player-call ()
  (let ((res (record-player-act (get-next-up) 'call *bet*)))
    (if (stringp res)
	(list res)
        t)))

(defun player-raise (amt)
  (let ((res (record-player-act (get-next-up) 'raise (read-from-string amt))))
    (if (stringp res)
        (list res)
        t)))

(defun player-fold ()
  (let ((res (record-player-act (get-next-up) 'fold)))
    (if (stringp res)
	(list res)
        t)))

(defun player-check ()
  (let ((res (record-player-act (get-next-up) 'check)))
    (if (stringp res)
	(list res)
        t)))

(defun player-bet (amt)
  (let ((res (record-player-act (get-next-up) 'bet (read-from-string amt))))
    (if (stringp res)
        (list res)
        t)))

(defun player-allin ()
  (let ((res (record-player-act (get-next-up) 'allin)))
    (if (stringp res)
	(list res)
        t)))

(defun get-player (name)
  (dolist (p *players*)
    (if (string= (pname p) name)
	(return p))))
