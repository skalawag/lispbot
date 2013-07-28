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
