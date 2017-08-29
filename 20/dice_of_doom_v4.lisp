(load "dice_of_doom_v3.lisp")

(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63) (63 63 255) (63 255 63)
			     (255 63 255)))
(defparameter *max-dice* 5)
(defparameter *ai-level* 2)

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
	   (lazy-mapcan
	    (lambda (dst)
	      (if (and (not (eq (player dst) cur-player))
		       (> (dice src) 1))
		  (make-lazy (list
			      (list
			       (list src dst)
			       (game-tree (board-attack board cur-player
							src dst (dice src))
					  cur-player
					  (+ spare-dice (dice dst))
					  nil)
			       (game-tree (board-attack-fail board cur-player
							     src dst (dice src))
					  cur-player
					  (+ spare-dice (dice dst))
					  nil))))
		(lazy-nil)))
	    (make-lazy (neighbors src)))
	 (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
		  collect n)))))

(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
		 for hex across board
		 collect (if (eq pos src)
			     (list player 1)
			   hex))))

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
		 sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a." dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
	     (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
					(dice (cadr path))))
	  (cadr move)
	(caddr move)))))

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
	((eq pos 'pass) (setf *cur-game-tree*
			      (cadr (lazy-car (caddr *cur-game-tree*))))
	 (princ "Your reinforcments have been placed.")
	 (tag a (href (make-game-link nil))
	      (princ "continue")))
	 ((not *from-tile*) (setf *from-tile* pos)
	  (princ "Now choose a destination:"))
	 ((eq pos *from-tile*) (setf *from-tile* nil)
	  (princ "Move cancelled."))
	 (t (setf *cur-game-tree*
		  (pick-chance-branch
		   (cadr *cur-game-tree*)
		   (lazy-find-if (lambda (move)
				   (equal (car move)
					  (list *from-tile* pos)))
				 (caddr *cur-game-tree*))))
	    (setf *from-tile* nil)
	    (princ "You may now ")
	    (tag a (href (make-game-link 'pass))
		 (princ "pass"))
	    (princ " or make another move:"))))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defparameter *dice-profitability* #(#(0.84 0.97 1.0 1.0)
				     #(0.44 0.78 0.94 0.99)
				     #(0.15 0.45 0.74 0.91)
				     #(0.04 0.19 0.46 0.72)
				     #(0.01 0.06 0.22 0.46)))

(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
	       (cadr (aref board pos))))
      (take-all (lazy-mapcar
		 (lambda (move)
		   (let ((path (car move)))
		     (if path
			 (let* ((src (car path))
				(dst (cadr path))
				(profitability (aref (aref *dice-profitability*
							   (1- (dice dst)))
						     (- (dice src) 2))))
			   (+ (* profitability (rate-position (cadr move) player))
			      (* (- 1 profitability) (rate-position (caddr move)
								    player))))
		       (rate-position (cadr move) player))))
		 (caddr tree))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil)
	  (lazy-mapcar (lambda (move)
			 (cons (car move)
			       (mapcar (lambda (x)
					 (limit-tree-depth x (1- depth)))
				       (cdr move))))
		       (caddr tree)))))

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
	     (if (and (eq (car (aref board pos)) player)
		      (not (member pos visited)))
		 (check-neighbors (neighbors pos) (cons pos visited))
	       visited))
	   (check-neighbors (lst visited)
	     (if lst
		 (check-neighbors (cdr lst) (check-pos (car lst) visited))
	       visited)))
    (check-pos pos '())))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
	     (if (< pos *board-hexnum*)
		 (if (and (eq (car (aref board pos)) player)
			  (not (member pos visited)))
		     (let* ((cluster (get-connected board player pos))
			    (size (length cluster)))
		       (if (> size best)
			   (f (1+ pos) (append cluster visited) size)
			 (f (1+ pos) (append cluster visited) best)))
		   (f (1+ pos) visited best))
	       best)))
    (f 0 '() 0)))
