(in-package :lispbot.plugins)

(defclass texas-holdem-plugin (plugin)
  ()
  (:default-initargs :name "texas-holdem"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plugin Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*))))
    (cond
      (*game-started*
       (reply "A holdem game has already been started!" t))
      (t
       (setf *players* nil
             *game-started* t
             *bets* nil
             *prev-bets* nil
             *hand-number* 0
             *community-cards* nil
             *stage* "Pre-Flop")
       (push (make-player player) *players*)
       (reply (format nil "~a has offered a game of Texas Holdem!" player))
       (reply (format nil "Use '!join-holdem' to join the game."))))))

(defun is-player-p (someone)
  (member someone (mapcar #'pname *players*) :test #'string=))

(defcommand join-holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (let ((player (nick (sender *last-message*)))
        (already-joined (mapcar #'pname *players*)))
    (cond
      ((is-player-p player)
       (reply "You are already in the game!" t))
      (t
       (push (make-player player) *players*)
       (reply (format nil "~a has joined Texas Holdem!" player))
       (reply (format nil "There are ~a players in the game. Anyone else?"
                      (length *players*)))))))

(defcommand start-holdem ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (when (and (> (length *players*) 1)
             (is-player-p (nick (sender *last-message*))))
    (reply "Texas Holdem has begun.")
    (display-game-state)))

(defcommand rd ((plugin texas-holdem-plugin))
  "Redraw the display manually when it gets stuck."
  (declare (ignore plugin))
  (display-game-state))

(defun get-player (name)
  (dolist (p *players*)
    (when (string= name (pname p))
      (return p))))

(defmacro make-act (act &optional amt)
  `(when (string= (pname (get-acting *players*)) (nick (sender *last-message*)))
     (handle-player-action (get-player (nick (sender *last-message*))) ,act ,amt)
     (advance-game)
     (display-game-state)))

(defcommand fold ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (make-act 'fold))

(defcommand call ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (make-act 'call))

(defcommand check ((plugin texas-holdem-plugin))
  (declare (ignore plugin))
  (make-act 'check))

(defcommand bet ((plugin texas-holdem-plugin) amt)
  (declare (ignore plugin))
  (make-act 'bet amt))

(defcommand raise ((plugin texas-holdem-plugin) amt)
  (declare (ignore plugin))
  (make-act 'raise amt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun community-cards ()
  (cond
    ((string= *stage* "Pre-Flop") "X X X X X")
    ((string= *stage* "Flop") (format nil "~a ~a ~a X X"
				      (first *community-cards*)
				      (second *community-cards*)
				      (third *community-cards*)))
    ((string= *stage* "Turn") (format nil "~a ~a ~a ~a X"
				      (first *community-cards*)
				      (second *community-cards*)
				      (third *community-cards*)
				      (fourth *community-cards*)))
    ((string= *stage* "River") (format nil "~a ~a ~a ~a ~a"
				      (first *community-cards*)
				      (second *community-cards*)
				      (third *community-cards*)
				      (fourth *community-cards*)
				      (fifth *community-cards*)))))

(defun seating-format-values ()
  (let ((n (- (length *players*) 3))
	(res (list "[sb]" "[bb]")))
    (dotimes (i n)
      (setf res (append res (list (+ i 3)))))
    (append res (list "[d]"))))

(defun display-game-state ()
  (let ((seats (seating-format-values)))
    (reply (format nil "Stage: ~a   Pot: ~a  Hand: ~a"
                   *stage*
                   (compute-pot *prev-bets* *bets*)
                   *hand-number*))
    (sleep .5)
    (reply (format nil "Community Cards: ~a" (community-cards)))
    (sleep .5)
    (reply (format nil "~5a ~10a ~4a ~7a ~4a ~3a~%"
                   "Seat" "Name" "Nxt" "Chips" "Bet" "Fld"))
    (sleep .5)
    (dolist (p *players*)
      (sleep .5)
      (reply (format nil "~5a ~10a ~4a ~7a ~4a ~3a~%"
              (pop seats)
              (pname p)
              (if (acting p) "*" "")
              (chips p)
              ;; not sure how best to fix this yet.
              (if (null (get-bet p *bets*)) 0 (get-bet p *bets*))
              (if (folded p) (folded p) "--"))))))

(defun display-winners (winners)
  (reply (format nil "The winners of hand ~a are:" *hand-number*))
  (dolist (w winners)
    (sleep .5)
    (reply (format nil "~a: ~a  Holding: ~a"
                   (pname w)
                   (best-hand w)
                   (pockets w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *debug* nil)

(defparameter *game-started* nil)

(defparameter *hand-number* 0)

(defparameter *prev-bets* nil
  "After each betting round, *bets* are pushed here. This is a list of
  lists, each of which was formerly the value of *bets*.")

(defparameter *bets* nil
  "Collect all bets for the current betting round.

  This is a list of the form: (AMT1 (PLAYER1 . AMT2) (PLAYER2
  . AMT3)), where the AMT1 is the largest bet at any given time, and
  other AMTn's are the largest amt a given player has put in the pot
  in the present round (which can be less than AMT1). The amt a player
  can extract from a pot (assuming there is only one round of betting)
  is the largest amt he has put in multiplied by the number of players
  in the list (including himself). The largest amt a player can
  extract from the pot at the end of a hand is the sum of the amts he
  can extract for each betting round. NOTE: check whether AMT1 is
  actually used for anything, or is just vestigial.")

(defparameter *players* nil)

(defparameter *community-cards* nil)

(defparameter *stage* "Pre-Flop")

(defparameter *first-check* t
  "I hate this, but without it I cannot determine that a betting round
  in which all players have checked has ended. To do so, we have to
  set the flag of the first (but only the first) player that has
  checked. So, once the first player in the round has checked, this is
  set to nil.")

(defparameter *starting-chips* 1000)

(defparameter *small-blind* 5)

(defparameter *big-blind* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Poker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass player ()
  ((player-name :initarg :name :reader pname)
   (chips :initarg :chips :accessor chips)
   (pocket-cards :accessor pockets)
   (best-hand :initform nil :accessor hand)
   (acting-player :initform nil :accessor acting)
   (folded-player :initform nil :accessor folded)
   (player-payout :initform 0 :accessor payout)
   (first-bettor-or-last-raiser-flag :initform nil :accessor flag)))

(defun make-player (name)
  (make-instance 'player :name name :chips *starting-chips*))

(defun bet (player amt)
  (cond
    ((> amt (chips player))
     (let ((amt (chips player)))
       (setf (chips player) 0)
       (list amt (cons player amt))))
    (t
     (setf (chips player) (- (chips player) amt))
     (list amt (cons player amt)))))

(defun call (player amt)
  (cond
    ((< (chips player) amt)
     (let ((new-amt (chips player)))
       (setf (chips player) 0)
       (cons player new-amt)))
    (t
     (let ((p (player-in-bets? *bets* player)))
       (if p
	   (progn
	     (setf (chips player) (- (chips player) (- amt (cdr p)))))
	   (progn
	     (setf (chips player) (- (chips player) amt))))
       (cons player amt)))))

(defun raise (player amt)
  (cadr (bet player amt)))

(defun player-in-bets? (bet-list player)
  "If a player has already placed a bet or called, retrieve him from
*bets*"
  (labels ((get-p (bets)
	     (cond
	       ((null bets) nil)
	       ((not (consp (car bets)))
		(get-p (cdr bets)))
	       ((equal player (caar bets))
		(car bets))
	       (t (get-p (cdr bets))))))
    (get-p bet-list)))

(defun update-player-in-bets (player-bet bet-list)
  (append
   (list (car bet-list))
   (remove-if #'(lambda (y) (equal y (car player-bet)))
	      (cdr bet-list) :key #'(lambda (x) (car x)))
   (list player-bet)))

(defun handle-player-action (player action &optional amt)
  (cond
    ((eq action 'fold)
     (setf (folded player) t))
    ((eq action 'check)
     ;; we only need this for Flop and later anyway
     (when (and (string/= *stage* "Pre-Flop") *first-check*)
       (set-player-flag player)
       (setf *first-check* nil)))
    ((eq action 'call)
     ;; in order to give the big blind an option, have to move the flag
     (when (and (string= *stage* "Pre-Flop") (= (length (cdr *bets*)) 2))
       (set-player-flag player))
     (let ((p (player-in-bets? *bets* player)))
       (if p
	   (setf *bets* (update-player-in-bets
			   (call player (car *bets*)) *bets*))
	   (setf *bets*
		 (append *bets* (list (call player (car *bets*))))))))
    ((eq action 'bet)
     (if *bets*
	 (handle-player-action player 'raise amt)
	 (progn
	   (if (or (and (string= *stage* "Pre-Flop") (= amt 5))
		   (>= amt 10))
	       (progn
		 (setf *bets* (bet player amt))
		 (set-player-flag player))
	       nil))))
    ((eq action 'raise)
     (cond
       ((null *bets*)
	(handle-player-action player 'bet amt))
       ((< amt (* 2 (car *bets*)))
	nil) ; illegal raise
       (t
	(set-player-flag player)
	(setf *bets* (append *bets* (list (raise player amt))))
	(setf (car *bets*) amt))))))

(defun set-player-flag (player)
  "When a player bets or raises, set this flag. When we see it again,
the betting-round is over."
  (dolist (p *players*)
    (setf (flag p) nil))
  (setf (flag player) t))

(defun betting-round-over? (player)
  "When getting the next player to act, check his flag. If it is true,
  then he was the original bettor (or the last raiser) and no one has
  reraised; the round is over."
  (when (flag player) t))

(defun hand-over? ()
  (let ((acting (get-acting *players*)))
    (when (or (and (eq (flag acting) t) (string= *stage* "River"))
	      (< (length (remove-if #'(lambda (p) (= (chips p) 0))
				    (get-unfolded *players*)))
		 2)) t)))

(defun game-over? ()
  (when (< (length *players*) 2) t))

(defun compute-pot (prev-bets bets)
  (let ((res nil))
    (dolist (pb prev-bets)
      (setf res (append pb res)))
    (reduce #'+ (remove-if-not #'consp (append bets res))
	    :key #'(lambda (x) (cdr x)))))

(defun deal-cards ()
  (let ((cards
	 (subseq (hshuffle) 0 (+ (* 2 (length *players*)) 5))))
    (dolist (p *players*)
      (setf (pockets p)
	    (list
	     (pop cards)
	     (pop cards))))
    (setf *community-cards* cards)))

(defun rotate-players (players)
  (append (list (car (last players))) (butlast *players*)))

(defun set-blinds (players)
  "Always call this after rotating."
  (handle-player-action (car players) 'bet *small-blind*)
  (handle-player-action (cadr players) 'raise *big-blind*))

(defun get-acting (players)
  (let ((acting (remove-if-not #'(lambda (p) (acting p)) players)))
    (if (> (length acting) 1)
	(error "More than one acting player")
	(car acting))))

(defun get-unfolded (players &optional ignore-acting)
  (if ignore-acting
      (remove-if
       #'(lambda (x) (or
		      (and (folded x) (not (acting x)))
		      (and (= (chips x) 0) (not (acting x)))))
		 players)
      (remove-if
       #'(lambda (x) (folded x)) players)))

(defun advance-acting (players)
  (let ((active (get-unfolded players t)))
    (cond
      ((acting (car (last active)))
       (setf (acting (car (last active))) nil)
       (setf (acting (car active)) t))
      (t
       (labels ((move-acting (source)
		  (cond
		    ((null source) nil)
		    ((acting (car source))
		     (setf (acting (car source)) nil)
		     (setf (acting (cadr source)) t))
		    (t (move-acting (cdr source))))))
	 (move-acting active))))))

(defun reset-hand ()
  (remove-busted-players)
  (if (game-over?)
      (reply (format nil "The game is over. ~a has won!" (car *players*)))
      (progn
        (mapcar #'(lambda (x)
                    (setf (acting x) nil)
                    (setf (folded x) nil)) *players*)
        (setf *players* (rotate-players *players*))
        (setf (acting (third *players*)) t)
        (setf *stage* "Pre-Flop")
        (setf *first-check* t)
        (setf *hand-number* (1+ *hand-number*))
        (setf *bets* nil)
        (setf *prev-bets* nil)
        (set-blinds *players*)
        (deal-cards))))

(defun advance-stage ()
  (cond
    ((string= *stage* "Pre-Flop") (setf *stage* "Flop"))
    ((string= *stage* "Flop") (setf *stage* "Turn"))
    ((string= *stage* "Turn") (setf *stage* "River"))
    ((string= *stage* "River") (setf *stage* "Pre-flop"))))

;;; call eval-hands and then get-winners, maybe combine these two.
(defun eval-hands (players)
  (dolist (p players)
    (setf (hand p) (best-poker-hand (append (pockets p) *community-cards*)))))

(defun get-winners (players)
  "Return a list of the players with the highest value hands"
  (let ((best 0))
    (dolist (p players)
      (when (> (car (hand p)) best)
	(setf best (car (hand p)))))
    (remove-if #'(lambda (x) (< x best)) players
	       :key #'(lambda (p) (car (hand p))))))

(defun sort-players-by-hand-val (players)
  (sort (copy-list players) #'> :key (lambda (p) (car (hand p)))))

(defun get-player-payout (player bets)
  "bets is a single betlist of all bets"
  (let ((payout 0))
    (dolist (b (cdr bets))
      (let ((check (if (equal (car b) player) b nil)))
	(when check
	  (setf payout
		(+ payout (* (cdr check) (- (length bets) 1))))
	  (return payout))))
    payout))

(defun remove-players-from-all-betlists (players)
  (dolist (player players)
    (setf *bets*
	  (remove-if
	   #'(lambda (x) (if (and (consp x) (equal (car x) player)) t nil))
	   *bets*))
    (setf *prev-bets*
	  (remove-if
	   #'(lambda (x) (if (and (consp x) (equal (car x) player)) t nil))
	   *prev-bets*))))

(defun get-bet (player bet-list)
  (dolist (b bet-list)
    (when (consp b)
      (if (equal (car b) player)
	  (return (cdr b))))))

(defun pay-winners (list-of-winners betlist)
  (dolist (w list-of-winners)
    (let ((amt 0))
      (dolist (b betlist)
	(setf amt (+ amt (get-player-payout w b))))
      (setf (payout w) (float (/ amt (length list-of-winners)))))))

(defun adjust (winners betlist)
  (dolist (b betlist)
    (dolist (w winners)
      (let ((bet (get-bet w b)))
	(if bet
	    (dolist (c (cdr b))
	      (if (< (- (cdr c) bet) 0)
		  (setf (cdr c) 0)
		  (setf (cdr c) (- (cdr c) bet)))))))))

;; TODO: test this more throughly
(defun calculate-player-payouts (winners players bets prev-bets)
  (pay-winners winners (cons bets prev-bets))
  (remove-players-from-all-betlists winners)
  (adjust winners (cons bets prev-bets))
  (when (and (> (compute-pot prev-bets bets) 0) (not (null players)))
    (let ((new-players
	   (remove-if #'(lambda (p) (member p winners :test #'equal)) players)))
      (calculate-player-payouts (get-winners new-players)
				new-players
				bets
				prev-bets))))

(defun credit-payouts (players)
  "Move chips into players chip stack from payout."
  (dolist (p players)
    (setf (chips p) (+ (chips p) (payout p)))
    (setf (payout p) 0)))

(defun clean-up-and-advance-stage ()
  (setf *prev-bets* (cons *bets* *prev-bets*))
  (setf *bets* nil)
  (advance-stage)
  (setf *first-check* t)
  (mapcar #'(lambda (p) (setf (flag p) nil)) *players*)
  (when (string/= *stage* "Pre-Flop")
    (mapcar #'(lambda (x) (setf (acting x) nil)) *players*)
    (dolist (p *players*)
      (when (not (folded p))
	(setf (acting p) t)
	(return)))))

(defun remove-busted-players ()
  (setf *players* (remove-if #'(lambda (p) (= (chips p) 0)) *players*)))

(defun advance-game ()
  (advance-acting *players*)
  (when (betting-round-over? *players*)
    (cond
      ((hand-over?)
       (eval-hands *players*)
       (let ((winners (get-winners *players*)))
	 (calculate-player-payouts winners *players* *bets* *prev-bets*)
	 (credit-payouts *players*)
	 (display-winners winners)
	 (reset-hand)))
      (t (clean-up-and-advance-stage)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-deck ()
  (let ((res '()))
    (dolist (rank '("A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"))
      (dolist (suit '("h" "s" "c" "d"))
	(push (list rank suit) res)))
    (mapcar #'(lambda (x) (concatenate 'string (first x) (second x))) res)))

(defun shuff (deck)
  "nicked from pastebin, author unknown"
  (do ((k (random (length deck)) (random (length deck)))
       (result nil))
      ((null (rest deck)) (cons (first deck) result))
    (setq result (cons (nth k deck) result)
          deck (reverse (cdr (reverse
                              (subst (car (reverse deck))
                                     (nth k deck)
                                     deck)))))))

(defun hshuffle ()
  (shuff (make-deck)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hand Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun best-poker-hand (raw-cards)
  "Accepts 1 or more cards (which should be two character strings,
e.g., \"As\") and identifies the best poker hand among them. Returns a
list of the value of the hand, the hand name, and the cards making the
hand."
  (cond
    ((null raw-cards) nil)
    ((> (length raw-cards) 5)
     (let ((source (combinations 5 raw-cards))
	   (res nil))
      (dolist (hand source)
	(push (append (eval-hand hand) (list hand)) res))
      (car (sort res #'> :key #'(lambda (x) (car x))))))
    (t (append (eval-hand raw-cards) (list raw-cards)))))

(defun eval-hand (hand)
  (cond
    ((straight-flush-p hand) (list (straight-flush-rank hand) 'straight-flush))
    ((quads-p hand) (list (quads-rank hand) 'quads))
    ((house-p hand) (list (house-rank hand) 'house))
    ((flush-p hand) (list (flush-rank hand) 'flush))
    ((straight-p hand) (list (straight-rank hand) 'straight))
    ((trips-p hand) (list (trips-rank hand) 'trips))
    ((two-pair-p hand)
     (list (two-pair-rank hand) 'two-pair))
    ((pair-p hand)
     (list (pair-rank hand) 'pair))
    (t (list (high-card-rank hand) 'high-card))))

;;;; hand tests

(defun straight-p (cards)
  (let ((sorted (sort (mapcar #'card-rank-values cards) #'<)))
    (cond
      ((equal sorted '(2 3 4 5 14)) t)
      ((equal sorted '(2 3 4 5 6)) t)
      ((equal sorted '(3 4 5 6 7)) t)
      ((equal sorted '(4 5 6 7 8)) t)
      ((equal sorted '(5 6 7 8 9)) t)
      ((equal sorted '(6 7 8 9 10)) t)
      ((equal sorted '(7 8 9 10 11)) t)
      ((equal sorted '(8 9 10 11 12)) t)
      ((equal sorted '(9 10 11 12 13)) t)
      ((equal sorted '(10 11 12 13 14)) t))))

(defun flush-p (cards)
  "If there are 5 cards, all of the same suit, return t; else nil"
  (let* ((suits (mapcar #'(lambda (x) (subseq x 1 2)) cards))
	 (tar (first suits))
	 (res t))
    (when (= (length suits) 5)
      (dolist (suit suits)
	(when (not (string= suit tar))
	  (setf res nil)))
      res)))

(defun straight-flush-p (cards)
  (when (and (straight-p cards) (flush-p cards)) t))

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

;;;; rankers

;; these functions assign an integer to each hand based on its type
;; and strength, including any kickers. since the stronger of any
;; hands always has a higher value, comparison of hands is very easy.

(defun high-card-rank (hand)
  "Return the high-card-rank of a given hand."
  (car (card-rank-values hand)))

(defun get-kickers-value (pair kickers kicker-type)
  (let ((val (position
		  (sort (mapcar #'card-rank-values kickers) #'>)
		  (remove-kickers (card-rank-values (car pair)) kicker-type)
		  :test #'equal)))
    (if val val 0)))

(defun pair-rank (hand)
  (let* ((packed (pack-by-rank hand))
	 (pair (car (remove-if-not #'listp packed)))
	 (kickers (remove-if #'listp packed)))
    (if kickers
	(progn
	  (+ (get-pair-value (card-rank-values (car pair)))
	     (1+ (get-kickers-value pair kickers *pair-kickers*))))
	(progn
	  (get-pair-value (card-rank-values (car pair)))))))

(defun two-pair-rank (cards)
  (let ((packed (sort-packed (pack-by-rank cards))))
    (if (= (length packed) 3)
	(progn
	  (+ (get-two-pair-values
	      (card-rank-values (car (nth 0 packed)))
	      (card-rank-values (car (nth 1 packed))))
	     (card-rank-values (car (last packed)))))
	(progn
	  (get-two-pair-values
	   (card-rank-values (car (nth 0 packed)))
	   (card-rank-values (car (nth 1 packed))))))))

(defun trips-rank (cards)
  (let* ((packed (pack-by-rank cards))
	 (trips (car
		 (remove-if
		  #'(lambda (x) (not (and (listp x) (= (length x) 3))))
		  packed)))
	 (kickers (remove-if #'listp packed)))
    (if (= (length kickers) 2)
	(progn
	  (+ (get-trips-value (card-rank-values (car trips)))
	     (1+ (position
		  (sort (mapcar #'card-rank-values kickers) #'>)
		  (remove-kickers (card-rank-values (car trips)) *trips-kickers*)
		  :test #'equal))))
	(progn
	  (get-trips-value (card-rank-values (car trips)))))))

(defun straight-rank (cards)
  (let ((sorted (sort (mapcar #'card-rank-values
			      cards) #'<)))
    (cond
      ((equal sorted '(2 3 4 5 14)) 4496)
      ((equal sorted '(2 3 4 5 6)) 4497)
      ((equal sorted '(3 4 5 6 7)) 4498)
      ((equal sorted '(4 5 6 7 8)) 4499)
      ((equal sorted '(5 6 7 8 9)) 4500)
      ((equal sorted '(6 7 8 9 10)) 4501)
      ((equal sorted '(7 8 9 10 11)) 4502)
      ((equal sorted '(8 9 10 11 12)) 4503)
      ((equal sorted '(9 10 11 12 13)) 4504)
      ((equal sorted '(10 11 12 13 14)) 4505))))

(defun flush-rank (flush)
  (+ 4506 (car (sort (mapcar #'card-rank-values flush) #'>))))

(defun house-rank (hand)
  (let ((ahand (sort-packed (pack-by-rank hand))))
    (labels ((get-rank (trip-rank pair-rank)
	       (cond
		 ((and (= trip-rank 2) (= pair-rank 3)) 4521)
		 ((> pair-rank 2) (1+ (get-rank trip-rank (1- pair-rank))))
		 ((= pair-rank 2) (1+ (get-rank (1- trip-rank) 14))))))
      (get-rank
       (card-rank-values (caar ahand))
       (card-rank-values (car (cadr ahand)))))))

(defun quads-rank (hand)
  (let ((packed (sort-packed (pack-by-rank hand))))
    (labels ((get-rank (quad-rank kicker-rank)
	       (cond
		 ((and (= quad-rank 2) (= kicker-rank 3)) 4688)
		 ((> kicker-rank 2) (1+ (get-rank quad-rank (1- kicker-rank))))
		 ((= kicker-rank 2) (1+ (get-rank (1- quad-rank) 14))))))
      (if (cadr packed)
	  (get-rank (card-rank-values (caar packed)) (card-rank-values (cadr packed)))
	  (get-rank (card-rank-values (caar packed)) 3)))))

(defun straight-flush-rank (str-fl)
  (+ 192 (straight-rank str-fl)))

;;;; helpers

(defun sort-packed (packed)
  (let ((lists
	 (sort (remove-if-not #'listp packed)
	       #'>
	       :key #'(lambda (x) (card-rank-values (car x)))))
	(cards (remove-if #'listp packed)))
    (append lists cards)))

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

(defun card-rank-values (item)
  "Convert a card or a list of cards to their rank values."
  (let ((vals '(("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5) ("6" . 6)
		("7" . 7) ("8" . 8) ("9" . 9) ("T" . 10) ("J" . 11)
		("Q" . 12) ("K" . 13) ("A" . 14))))
    (cond
      ((stringp item)
       (cdr (assoc (subseq item 0 1) vals :test #'string=)))
      ((and (listp item) (> (length item) 0))
       (sort (mapcar #'card-rank-values item) #'>))
      (t (error "Cannot apply rank-value to ~a" item)))))

(defun get-pair-value (pair-rank)
"There are 220 possible sets of kickers that can be combined with any
given pair."
  (if (> pair-rank 2)
      (+ 221 (get-pair-value (1- pair-rank)))
      2))

(defun get-two-pair-values (pair1-rank pair2-rank)
  (cond
    ((and (= pair1-rank 2) (= pair2-rank 3)) 2875)
    ((> pair2-rank 2) (1+ (get-two-pair-values pair1-rank (1- pair2-rank))))
    (t (1+ (get-two-pair-values (1- pair1-rank) 14)))))

(defun get-trips-value (trips-rank)
  (if (> trips-rank 2)
      (+ 67 (get-trips-value (1- trips-rank)))
      3625))

(defun remove-kickers (rank from)
  (remove-if #'(lambda (x) (member rank x)) from))

(defun combinations (m list)
  "Find all m-sized combinations from list. Nicked this from
somewhere (pastebin? stackoverflow?), but I can't recall where...:/"
  (let ((res nil))
    (labels ((comb1 (l c m)
	       (when (>= (length l) m)
		 (if (zerop m) (return-from comb1 (setq res (cons c res))))
		 (comb1 (cdr l) c m)
		 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    res))

(defparameter *trips-kickers*
  (sort (combinations 2 '(2 3 4 5 6 7 8 9 10 11 12 13 14))
	#'< :key #'(lambda (x) (car x))))

(defparameter *pair-kickers*
  (sort (combinations 3 '(2 3 4 5 6 7 8 9 10 11 12 13 14))
	#'< :key #'(lambda (x) (car x))))
