(in-package :lispbot.plugins)

;;; FIXED:
;; resent hand numbering. DONE

;; when p1 has fewer chips than p2 and moves allin, and p2 calls, the
;; star goes away. DONE (but watch this): (addressed in game-over-p)
;; commit: 890b260

;; once, when i was called by a pair of kings, and i had a pair of
;; aces, the kings won. i don't think this is the hand evaluator, but
;; run these through:
;;   <pythagoras> Comm Cards: (8d 5c Jh Kh As)
;;   <pythagoras> skalawag has been called and shows: (Ah Th)
;;   <pythagoras> *Winner*: fyv holding (As Kh Jh 8d Ks) (PAIR).

(defclass holdem-plugin (plugin)
  ()
  (:default-initargs :name "holdem"))

(defcommand redisplay ((plugin holdem-plugin))
  (declare (ignore plugin))
  (display-game-state))

(defcommand sends ((plugin holdem-plugin))
  (declare (ignore plugin))
  (send "hello" "skalawag" (bot *last-message*)))

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
             (progn
               (reply res)
               (update)))))
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
             (progn
               (reply res)
               (update)))))
      (t (reply "It's not your turn!" t)))))

(defcommand call ((plugin holdem-plugin))
  (declare (ignore plugin))
  (let ((nick (nick (sender *last-message*))))
    (cond
      ((null *game-started*)
       (reply "There is no holdem game in progress." t))
      ((verify-next nick)
       (let ((res (player-call)))
         ;; if res is a list then player input was defective in some
         ;; way and we should not update the game-state.
         (if (listp res)
             (reply (first res))
             (progn
               (reply (format nil "~a" res))
               (update)))))
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
             (progn
               (reply res)
               (update)))))
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
             (reply (format nil "~a" res))
             (progn
               (reply res)
               (update)))))
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
             (progn
               (reply res)
               (update)))))
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
(defparameter *stats* nil
  "Collect stats on players.")

(defun make-player-statistics (player-name)
  (list :name player-name
        :num-of-hands-dealt 0
        :showdowns 0
        :flops 0
        :turns 0
        :rivers 0
        :pre-flop-raise-perc 0 ; (/ pre-flop-raise pre-flop-X-not-fold)
        :limp-perc 0 ; (/ pre-flop-calls pre-flop-X-not-fold)
        :folds 0
        :pre-flop-folds 0
        :raise/bet-then-check-perc 0 ; (/ raise/bet-then-check raise/bet-X)
        :average-raise 0
        :average-bet 0
        :agressive/passive 0 ; what is an agressive player?
        :loose/tight 0
        :num-of-losing-calls 0
        :num-of-winning-calls 0
        :perc-of-hands-played 0
        :bluff-perc 0))

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
  "nicked from pastebin, author unknown"
  (do ((k (random (length deck) (make-random-state t)) (random (length deck) (make-random-state t)))
       (result nil))
      ((null (rest deck)) (cons (first deck) result))
    (setq result (cons (nth k deck) result)
          deck (reverse (cdr (reverse
                              (subst (car (reverse deck))
                                     (nth k deck)
                                     deck)))))))

(defun hshuffle ()
  (shuff (make-deck)))

(defun deal-cards ()
  (let ((deck (hshuffle)))
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
       ((> *bet* 0) "Try raising.")
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
         (<= (length (remove-allin-or-folded)) 1))
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
	(straight-flush nil)
	(quads nil)
	(house nil)
	(flush nil)
	(straight nil)
	(trips nil)
	(two-pair nil)
	(pair nil)
	(high-card nil))
    ;; eval each hand
    (dolist (p players)
      (cards p nil (get-best-5-card-hand
			     (append
			      (cards p)
			      *board*))))
    ;; sort the winners into hand-types
    (dolist (p *players*)
      (case (first (cards p))
	(straight-flush (push p straight-flush))
	(quads (push p quads))
	(house (push p house))
	(flush (push p flush))
	(straight (push p straight))
	(trips (push p trips))
	(two-pair (push p two-pair))
	(pair (push p pair))
	(high-card (push p high-card))))
    ;; sort each hand type by numeric value
    (let ((sorted-bins (mapcar #'(lambda (x) (sort x #'> :key #'(lambda (y) (third (cards y)))))
			       (remove-if #'null (list straight-flush quads house flush straight trips two-pair pair high-card)))))
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
     (setq *game-over* t))
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
  (reply (format nil "~a has been called and shows: ~a" (pname called) (getf called :hole-cards))))

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
          (reset-holdem)))
       ((every #'(lambda (p) (allin p)) (get-unfolded))
        (show-down)
        (clear-line-bets)
	(find-winners)
	(payoff-players *winners*)
	(display-winners t)
	(holdem-reset)
        (when *game-over*
          (reset-holdem)))
       (t
	(reply (format nil "The hand is complete."))
	(clear-line-bets)
        (show-called (car (remove-if #'(lambda (p) (not (or (eq (act p) 'raise) (eq (act p) 'bet)))) *players*)))
	(find-winners)
	(payoff-players *winners*)
	(display-winners t)
	(holdem-reset)
        (when *game-over*
          (reset-holdem))))
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

;; ;;; game actions

(defun player-call ()
  (let ((res (record-player-act (get-next-up) 'call *bet*)))
    (if (stringp res)
	(list res)
	(format nil "~a has called." (pname (get-next-up))))))

(defun player-raise (amt)
  (let ((res (record-player-act (get-next-up) 'raise (read-from-string amt))))
    (if (stringp res)
        (list res)
        (format nil "~a has raised the bet to ~a." (pname (get-next-up)) *bet*))))

(defun player-fold ()
  (let ((res (record-player-act (get-next-up) 'fold)))
    (if (stringp res)
	(list res)
	(format nil "~a has folded." (pname (get-next-up))))))

(defun player-check ()
  (let ((res (record-player-act (get-next-up) 'check)))
    (if (stringp res)
	(list res)
	(format nil "~a has checked." (pname (get-next-up))))))

(defun player-bet (amt)
  (let ((res (record-player-act (get-next-up) 'bet (read-from-string amt))))
    (if (stringp res)
        (list res)
        (format nil "~a has bet ~a." (pname (get-next-up)) amt))))

(defun player-allin ()
  (let ((res (record-player-act (get-next-up) 'allin)))
    (if (stringp res)
	(list res)
	(format nil "~a has moved all in for ~a!" (pname (get-next-up)) *bet*))))

(defun get-player (name)
  (dolist (p *players*)
    (if (string= (pname p) name)
	(return p))))

;;;; Hand evaluation
(defun card-value (card)
  (let ((vals '(("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5) ("6" . 6)
		("7" . 7) ("8" . 8) ("9" . 9) ("T" . 10) ("J" . 11)
		("Q" . 12) ("K" . 13) ("A" . 14))))
    (cdr (assoc (subseq card 0 1) vals :test #'string=))))

(defun hand-value (cards)
  (reduce #'+ (mapcar #'card-value cards)))

(defun make-pairs ()
  (let ((source '("A" "K" "Q" "J" "T" "9"
		  "8" "7" "6" "5" "4" "3" "2")))
    (reverse (mapcar #'(lambda (r) (list r r)) source))))

(defun make-trips ()
  (let ((source '("A" "K" "Q" "J" "T" "9"
		  "8" "7" "6" "5" "4" "3" "2")))
    (reverse (mapcar #'(lambda (r) (list r r r)) source))))

(defun make-quads ()
  (let ((source '("A" "K" "Q" "J" "T" "9"
		  "8" "7" "6" "5" "4" "3" "2")))
    (reverse (mapcar #'(lambda (r) (list r r r r)) source))))

;; FIXME: this is silly. why do i need all four of the quad cards, if
;; they're all the same? same goes for trips and pairs....
(defun eval-quads (quads)
  (let ((qu (first (remove-if-not #'listp quads)))
	(kicker (remove-if #'listp quads))
	(source (make-quads)))
    (+ (major-pair-factor
	(+ 1 (position
	      (list
	       (subseq (first qu) 0 1)
	       (subseq (second qu) 0 1)
	       (subseq (third qu) 0 1)
	       (subseq (fourth qu) 0 1))
	      source :test #'equal)))
       (position
	(subseq (first kicker) 0 1)
	'("2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A")
	:test #'string=))))

(defun eval-trips (trips)
  (let ((tr (first (remove-if-not #'listp trips)))
	(kickers (remove-if #'listp trips))
	(source (make-trips)))
    (+ (major-pair-factor
	(+ 1 (position
	      (list
	       (subseq (first tr) 0 1)
	       (subseq (second tr) 0 1)
	       (subseq (third tr) 0 1))
	      source :test #'equal)))
	(get-trips-kickers-value kickers))))

(defun eval-pair (pair)
  (let ((pr (first (remove-if-not #'listp pair)))
	(kickers (remove-if #'listp pair))
	(source (make-pairs)))
    (+ (major-pair-factor
	(+ 1 (position
	     (list
	      (subseq (first pr) 0 1)
	      (subseq (second pr) 0 1))
	     source :test #'equal)))
       (get-pair-kickers-value kickers))))

(defun eval-two-pair (two-pair-packed)
  (let* ((prs (remove-if-not #'listp two-pair-packed))
	 (pr1 (first prs))
	 (pr2 (second prs))
	 (kicker (remove-if #'listp two-pair-packed))
	 (source (make-pairs))
	 (pr1-val (1+ (position (list (subseq (first pr1) 0 1)
				      (subseq (second pr1) 0 1))
				source :test #'equal)))
	 (pr2-val (1+ (position (list (subseq (first pr2) 0 1)
				      (subseq (second pr2) 0 1))
				source :test #'equal)))
	 (kicker-val (cdr (assoc (subseq (car kicker) 0 1) *prime-vals* :test #'string=))))
    (cond
      ((> pr1-val pr2-val)
       (+ (major-pair-factor pr1-val) pr2-val kicker-val))
       (t
	(+ (major-pair-factor pr2-val) pr1-val kicker-val)))))

(defparameter *prime-vals* '(("2" . 2) ("3" . 3) ("4" . 5) ("5" . 7)
			     ("6" . 11) ("7" . 13) ("8" . 17) ("9" . 19)
			     ("T" . 23) ("J" . 29) ("Q" . 31) ("K" . 37)
			     ("A" . 41)))

(defun get-trips-kickers-value (kickers)
  (let* ((kicks (mapcar #'(lambda (x) (subseq x 0 1)) kickers))
	 (perms (permutations kicks)))
    (dolist (k perms)
      (if (member k *trips-kickers* :test #'equal)
	  (return (position k *trips-kickers* :test #'equal))))))

;; (mapcar #'(lambda (x) (sort (list (card-value (first x)) (card-value (second x))) #'>))  *trips-kickers*)
(defparameter *trips-kickers*
  '(("3" "2") ("4" "2") ("5" "2") ("6" "2") ("7" "2") ("8" "2") ("9" "2")
    ("T" "2") ("J" "2") ("Q" "2") ("K" "2") ("A" "2") ("4" "3") ("5" "3")
    ("6" "3") ("7" "3") ("8" "3") ("9" "3") ("T" "3") ("J" "3") ("Q" "3")
    ("K" "3") ("A" "3") ("5" "4") ("6" "4") ("7" "4") ("8" "4") ("9" "4")
    ("T" "4") ("J" "4") ("Q" "4") ("K" "4") ("A" "4") ("6" "5") ("7" "5")
    ("8" "5") ("9" "5") ("T" "5") ("J" "5") ("Q" "5") ("K" "5") ("A" "5")
    ("7" "6") ("8" "6") ("9" "6") ("T" "6") ("J" "6") ("Q" "6") ("K" "6")
    ("A" "6") ("8" "7") ("9" "7") ("T" "7") ("J" "7") ("Q" "7") ("K" "7")
    ("A" "7") ("9" "8") ("T" "8") ("J" "8") ("Q" "8") ("K" "8") ("A" "8")
    ("T" "9") ("J" "9") ("Q" "9") ("K" "9") ("A" "9") ("J" "T") ("Q" "T")
    ("K" "T") ("A" "T") ("Q" "J") ("K" "J") ("A" "J") ("K" "Q") ("A" "Q")
    ("A" "K")))

(defun get-pair-kickers-value (kickers)
  "This allows me to compare triples of kickers. The idea is to
combine this value with the value of eval-pair to get the true value
of a pair. FIXME: This is pretty inefficient because there is no easy way to
assure the kickers are in the correct order to be matched in *pair-kickers*."
  (let* ((kicks (mapcar #'(lambda (x) (subseq x 0 1)) kickers))
	 (perms (permutations kicks)))
    (dolist (k perms)
      (if (member k *pair-kickers* :test #'equal)
	  (return (position k *pair-kickers* :test #'equal))))))

(defparameter *pair-kickers*
  '(("4" "3" "2") ("5" "3" "2") ("5" "4" "2") ("5" "4" "3") ("6" "3" "2") ("6" "4" "2") ("6" "5" "2") ("6" "4" "3") ("6" "5" "3")
    ("6" "5" "4") ("7" "3" "2") ("7" "4" "2") ("7" "5" "2") ("7" "6" "2") ("7" "4" "3") ("7" "5" "3") ("7" "6" "3") ("7" "5" "4")
    ("7" "6" "4") ("7" "6" "5") ("8" "3" "2") ("8" "4" "2") ("8" "5" "2") ("8" "6" "2") ("8" "7" "2") ("8" "4" "3") ("8" "5" "3")
    ("8" "6" "3") ("8" "7" "3") ("8" "5" "4") ("8" "6" "4") ("8" "7" "4") ("8" "6" "5") ("8" "7" "5") ("8" "7" "6") ("9" "3" "2")
    ("9" "4" "2") ("9" "5" "2") ("9" "6" "2") ("9" "7" "2") ("9" "8" "2") ("9" "4" "3") ("9" "5" "3") ("9" "6" "3") ("9" "7" "3")
    ("9" "8" "3") ("9" "5" "4") ("9" "6" "4") ("9" "7" "4") ("9" "8" "4") ("9" "6" "5") ("9" "7" "5") ("9" "8" "5") ("9" "7" "6")
    ("9" "8" "6") ("9" "8" "7") ("T" "3" "2") ("T" "4" "2") ("T" "5" "2") ("T" "6" "2") ("T" "7" "2") ("T" "8" "2") ("T" "9" "2")
    ("T" "4" "3") ("T" "5" "3") ("T" "6" "3") ("T" "7" "3") ("T" "8" "3") ("T" "9" "3") ("T" "5" "4") ("T" "6" "4") ("T" "7" "4")
    ("T" "8" "4") ("T" "9" "4") ("T" "6" "5") ("T" "7" "5") ("T" "8" "5") ("T" "9" "5") ("T" "7" "6") ("T" "8" "6") ("T" "9" "6")
    ("T" "8" "7") ("T" "9" "7") ("T" "9" "8") ("J" "3" "2") ("J" "4" "2") ("J" "5" "2") ("J" "6" "2") ("J" "7" "2") ("J" "8" "2")
    ("J" "9" "2") ("J" "T" "2") ("J" "4" "3") ("J" "5" "3") ("J" "6" "3") ("J" "7" "3") ("J" "8" "3") ("J" "9" "3") ("J" "T" "3")
    ("J" "5" "4") ("J" "6" "4") ("J" "7" "4") ("J" "8" "4") ("J" "9" "4") ("J" "T" "4") ("J" "6" "5") ("J" "7" "5") ("J" "8" "5")
    ("J" "9" "5") ("J" "T" "5") ("J" "7" "6") ("J" "8" "6") ("J" "9" "6") ("J" "T" "6") ("J" "8" "7") ("J" "9" "7") ("J" "T" "7")
    ("J" "9" "8") ("J" "T" "8") ("J" "T" "9") ("Q" "3" "2") ("Q" "4" "2") ("Q" "5" "2") ("Q" "6" "2") ("Q" "7" "2") ("Q" "8" "2")
    ("Q" "9" "2") ("Q" "T" "2") ("Q" "J" "2") ("Q" "4" "3") ("Q" "5" "3") ("Q" "6" "3") ("Q" "7" "3") ("Q" "8" "3") ("Q" "9" "3")
    ("Q" "T" "3") ("Q" "J" "3") ("Q" "5" "4") ("Q" "6" "4") ("Q" "7" "4") ("Q" "8" "4") ("Q" "9" "4") ("Q" "T" "4") ("Q" "J" "4")
    ("Q" "6" "5") ("Q" "7" "5") ("Q" "8" "5") ("Q" "9" "5") ("Q" "T" "5") ("Q" "J" "5") ("Q" "7" "6") ("Q" "8" "6") ("Q" "9" "6")
    ("Q" "T" "6") ("Q" "J" "6") ("Q" "8" "7") ("Q" "9" "7") ("Q" "T" "7") ("Q" "J" "7") ("Q" "9" "8") ("Q" "T" "8") ("Q" "J" "8")
    ("Q" "T" "9") ("Q" "J" "9") ("Q" "J" "T") ("K" "3" "2") ("K" "4" "2") ("K" "5" "2") ("K" "6" "2") ("K" "7" "2") ("K" "8" "2")
    ("K" "9" "2") ("K" "T" "2") ("K" "J" "2") ("K" "Q" "2") ("K" "4" "3") ("K" "5" "3") ("K" "6" "3") ("K" "7" "3") ("K" "8" "3")
    ("K" "9" "3") ("K" "T" "3") ("K" "J" "3") ("K" "Q" "3") ("K" "5" "4") ("K" "6" "4") ("K" "7" "4") ("K" "8" "4") ("K" "9" "4")
    ("K" "T" "4") ("K" "J" "4") ("K" "Q" "4") ("K" "6" "5") ("K" "7" "5") ("K" "8" "5") ("K" "9" "5") ("K" "T" "5") ("K" "J" "5")
    ("K" "Q" "5") ("K" "7" "6") ("K" "8" "6") ("K" "9" "6") ("K" "T" "6") ("K" "J" "6") ("K" "Q" "6") ("K" "8" "7") ("K" "9" "7")
    ("K" "T" "7") ("K" "J" "7") ("K" "Q" "7") ("K" "9" "8") ("K" "T" "8") ("K" "J" "8") ("K" "Q" "8") ("K" "T" "9") ("K" "J" "9")
    ("K" "Q" "9") ("K" "J" "T") ("K" "Q" "T") ("K" "Q" "J") ("A" "3" "2") ("A" "4" "2") ("A" "5" "2") ("A" "6" "2") ("A" "7" "2")
    ("A" "8" "2") ("A" "9" "2") ("A" "T" "2") ("A" "J" "2") ("A" "Q" "2") ("A" "K" "2") ("A" "4" "3") ("A" "5" "3") ("A" "6" "3")
    ("A" "7" "3") ("A" "8" "3") ("A" "9" "3") ("A" "T" "3") ("A" "J" "3") ("A" "Q" "3") ("A" "K" "3") ("A" "5" "4") ("A" "6" "4")
    ("A" "7" "4") ("A" "8" "4") ("A" "9" "4") ("A" "T" "4") ("A" "J" "4") ("A" "Q" "4") ("A" "K" "4") ("A" "6" "5") ("A" "7" "5")
    ("A" "8" "5") ("A" "9" "5") ("A" "T" "5") ("A" "J" "5") ("A" "Q" "5") ("A" "K" "5") ("A" "7" "6") ("A" "8" "6") ("A" "9" "6")
    ("A" "T" "6") ("A" "J" "6") ("A" "Q" "6") ("A" "K" "6") ("A" "8" "7") ("A" "9" "7") ("A" "T" "7") ("A" "J" "7") ("A" "Q" "7")
    ("A" "K" "7") ("A" "9" "8") ("A" "T" "8") ("A" "J" "8") ("A" "Q" "8") ("A" "K" "8") ("A" "T" "9") ("A" "J" "9") ("A" "Q" "9")
    ("A" "K" "9") ("A" "J" "T") ("A" "Q" "T") ("A" "K" "T") ("A" "Q" "J") ("A" "K" "J") ("A" "K" "Q")))

(defun major-rank-value (major-rank)
  (cond
    ((string= major-rank "2") 1)
    ((string= major-rank "3") 50)
    ((string= major-rank "4") 100)
    ((string= major-rank "5") 150)
    ((string= major-rank "6") 200)
    ((string= major-rank "7") 250)
    ((string= major-rank "8") 300)
    ((string= major-rank "9") 350)
    ((string= major-rank "T") 400)
    ((string= major-rank "J") 450)
    ((string= major-rank "Q") 500)
    ((string= major-rank "K") 550)
    ((string= major-rank "A") 600)))

(defun eval-hand (cards kind)
  (let ((packed (pack-by-rank cards)))
    (cond
      ((eq kind 'straight-flush) (rank-straight cards))
      ((eq kind 'quads) (eval-quads packed))
      ((eq kind 'house)
       (let ((trips (if (= (length (car packed)) 3)
			(car packed)
			(car (cdr packed))))
	     (pair (if (= (length (car packed)) 2)
		       (car packed)
		       (car (cdr packed)))))
	 (reduce #'+
		 (list
		  (major-rank-value (subseq (first trips) 0 1))
		  (position (subseq (first pair) 0 1)
			    '("2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A")
			    :test #'string=)))))
      ((eq kind 'flush) (hand-value cards))
      ((eq kind 'straight) (rank-straight cards))
      ((eq kind 'trips)
       (eval-trips packed))
      ((eq kind 'two-pair)
       (eval-two-pair packed))
      ((eq kind 'pair) (eval-pair packed))
      ((eq kind 'high-card) (hand-value cards)))))

(defun major-pair-factor (n)
  (cond
    ((= n 1) 50)
    ((= n 2) 150)
    ((= n 3) 250)
    ((= n 4) 350)
    ((= n 5) 450)
    ((= n 6) 550)
    ((= n 7) 650)
    ((= n 8) 750)
    ((= n 9) 850)
    ((= n 10) 950)
    ((= n 11) 1050)
    ((= n 12) 1150)
    ((= n 13) 1250)))

(defun get-rank-or-suit (card &optional (rank t))
  "Return the rank (default) or suit of a card (if rank is nil)"
  (if rank
      (subseq card 0 1)
      (subseq card 1 2)))

(defun pack-by-rank (ranks)
  (cond
    ((null ranks) nil)
    ((not (member (get-rank (car ranks))
    		  (mapcar #'get-rank (cdr ranks))
		  :test #'string=))
     (cons (car ranks) (pack-by-rank (cdr ranks))))
    (t
     (cons
      (cons (car ranks) (remove-if
			 #'(lambda (c)
			     (not
			      (string=
			       (get-rank c)
			       (get-rank (car ranks)))))
			 (cdr ranks)))
      (pack-by-rank (remove-if
	       #'(lambda (x)
		   (string=
		    (get-rank x)
		    (get-rank (car ranks))))
	       (cdr ranks)))))))

(defun get-rank (card)
  (subseq card 0 1))

;; identifiers
(defun straight-p (cards)
  (let ((sorted (sort (mapcar #'get-rank-or-suit cards) #'string<)))
    (cond
      ((equal sorted '("2" "3" "4" "5" "A")) t)
      ((equal sorted '("2" "3" "4" "5" "6")) t)
      ((equal sorted '("3" "4" "5" "6" "7")) t)
      ((equal sorted '("4" "5" "6" "7" "8")) t)
      ((equal sorted '("5" "6" "7" "8" "9")) t)
      ((equal sorted '("6" "7" "8" "9" "T")) t)
      ((equal sorted '("7" "8" "9" "Q" "T")) t)
      ((equal sorted '("8" "9" "J" "Q" "T")) t)
      ((equal sorted '("9" "J" "K" "Q" "T")) t)
      ((equal sorted '("A" "J" "K" "Q" "T")) t))))

(defun flush-p (cards)
  "If there are 5 cards, all of the same suit, return t; else nil"
  (let* ((suits (mapcar #'(lambda (x) (subseq x 1 2)) cards))
	 (tar (first suits))
	 (res t))
    (if (= (length suits) 5)
	(progn
	  (dolist (suit suits)
	    (when (not (string= suit tar))
	      (setf res nil)))
	  res))))

(defun straight-flush-p (cards)
  (if (and (straight-p cards) (flush-p cards))
      t
      nil))

(defun quads-p (cards)
  (let* ((packed (pack-by-rank cards))
	(lists (remove-if-not #'listp packed)))
    (when (and (= 1 (length lists)) (= 4 (length (car lists))))
      t)))

(defun house-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= 2 (length lists))
	       (or (= 3 (length (car lists)))
		   (= 3 (length (car (last lists)))))
	       (or (= 2 (length (car lists)))
		   (= 2 (length (car (last lists))))))
      t)))

(defun trips-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= 1 (length lists))
	       (= 3 (length (car lists))))
      t)))

(defun two-pair-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= (length lists) 2)
	       (= (length (car lists)) 2)
	       (= (length (car (last lists))) 2))
      t)))

(defun pair-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= (length lists) 1)
	       (= (length (car lists)) 2))
      t)))

(defun id-5 (cards)
  (cond
    ((straight-flush-p cards)
     (list 'straight-flush cards (eval-hand cards 'straight-flush)))
    ((quads-p cards)
     (list 'quads cards (eval-hand cards 'quads)))
    ((house-p cards)
     (list 'house cards (eval-hand cards 'house)))
    ((flush-p cards)
     (list 'flush cards (eval-hand cards 'flush)))
    ((straight-p cards)
     (list 'straight cards (eval-hand cards 'straight)))
    ((trips-p cards)
     (list 'trips cards (eval-hand cards 'trips)))
    ((two-pair-p cards)
     (list 'two-pair cards (eval-hand cards 'two-pair)))
    ((pair-p cards)
     (list 'pair cards (eval-hand cards 'pair)))
    (t
     (list 'high-card cards (eval-hand cards 'high-card)))))

(defun get-best-5-card-hand (raw-cards)
  (let ((source (mapcar #'id-5 (combinations 5 raw-cards)))
	(made '(straight-flush quads house flush straight
		trips two-pair pair high-card))
	(best 20)
	(res nil))
    (dolist (h source)
      (when (< (position (car h) made) best)
	(setf best (position (car h) made))))
    (dolist (h source)
      (when (= (position (car h) made) best)
	(push h res)))
    (cond
      ((= (length res) 1) (car res))
      (t
       (car (sort res #'> :key #'(lambda (x) (third x))))))))

(defun rank-straight (cards)
  (let ((sorted (sort (mapcar #'get-rank-or-suit
			      (copy-list cards)) #'string<)))
    (cond
      ((equal sorted '("2" "3" "4" "5" "A")) 1)
      ((equal sorted '("2" "3" "4" "5" "6")) 2)
      ((equal sorted '("3" "4" "5" "6" "7")) 3)
      ((equal sorted '("4" "5" "6" "7" "8")) 4)
      ((equal sorted '("5" "6" "7" "8" "9")) 5)
      ((equal sorted '("6" "7" "8" "9" "T")) 6)
      ((equal sorted '("7" "8" "9" "Q" "T")) 7)
      ((equal sorted '("8" "9" "J" "Q" "T")) 8)
      ((equal sorted '("9" "J" "K" "Q" "T")) 9)
      ((equal sorted '("A" "J" "K" "Q" "T")) 10))))

(defun permutations (bag)
  "Return a list of all the permutations of the input.
(From Peter Norvig)"
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
      '(())
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1 :test #'eq))))
              bag)))

(defun combinations (m list)
  "Find all m-sized combinations from list"
  (let ((res nil))
    (labels ((comb1 (l c m)
	       (when (>= (length l) m)
		 (if (zerop m) (return-from comb1 (setq res (cons c res))))
		 (comb1 (cdr l) c m)
		 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    res))
