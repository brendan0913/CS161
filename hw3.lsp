;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload()
  (load "hw3.lsp")
  (load "a-star.lsp")
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;
; containsKeeperOrBox (row), where row is a row (list) of current state
; Returns NIL if there is no Keeper or Box in the row.
; Returns T if the Keeper or a Box is in the row.
; 	Goes through row, checking if each element is the Keeper or 
; 	a Box.
;
(defun containsKeeperOrBox (row)
  (cond 
	((not row) NIL)
	((or (isKeeper (car row)) (isBox (car row))) T)
	(T (containsKeeperOrBox (cdr row)))
  )
)

;
; goal-test (s), where s is a state of the game
; Retruns T if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
; 	Checks if the first row contains the Keeper or a Box, 
; 	and moves on to the following rows in s.
;
(defun goal-test (s)
  (cond 
	((not s) T)
	(T (and (not (containsKeeperOrBox (car s))) (goal-test (cdr s))))
  )
)


; ENTRY POINT OF next-states FUNCTION
; Since getKeeperPosition returns squares in (c r) format, 
; squares are formatted as (c r)

;
; out-of-bounds (s c r), where s is a state and c, r are column/row number
; Returns T if square (c,r) is out-of-bounds, NIL otherwise.
;
(defun out-of-bounds (s c r)
  (cond 
	((or (< r 0) (< c 0)) T)
	((or (> r (- (length s) 1)) (> c (- (length (car s)) 1))) T)
	(T NIL)
  )
)

;
; getToSquare (s c r), where s is a state and c, r are column/row number
; Returns the integer content of state s at square (c,r). 
;
(defun getToSquare (s c r)
  (cond 
	((and (= r 0) (= c 0)) (car (car s)))
	((> r 0) (getToSquare (cdr s) c (- r 1))) 				; go to next row
	((> c 0) (getToSquare (list (cdr (car s))) (- c 1) r))  ; go to next column in same row
  )
)

; 
; get-square (s c r), where s is a state and c, r are column/row number
; Returns the value of a wall (1) if the square is out of bounds; else,
; returns the integer content of state s at square (c,r). 
;
(defun get-square (s c r)
  (cond 
	((out-of-bounds s c r) wall)
	(T (getToSquare s c r))
  )
)

;
; set-row (row c v), where row is a row of the state (list), c is col number,
; 	v is new square content
; Returns new row with changed value v by either appending the rest of the row to the new content.
;
(defun set-row (row c v)
  (cond 
	((not row) NIL)
	((= c 0) (cons v (cdr row)))
	(T (cons (car row) (set-row (cdr row) (- c 1) v)))
  )
)

; 
; set-square (s c r v), where s is a state, c and r are column/row number, 
; 	and v is square content (integer)
; Sets square (c,r) to value v and returns new state s' using set-row, appending the 
; 	rest of the state to the new row
;
(defun set-square (s c r v)
  (cond 
	((out-of-bounds s c r) s)
	((not s) NIL)
	((= r 0) (cons (set-row (car s) c v) (cdr s)))
	(T (cons (car s) (set-square (cdr s) c (- r 1) v)))
  )
)

; 
; try-move (s dc dr), where s is state, dc/dr is change in column/row number
; Tries to move keeper in direction given by dc dr
; UP is (0, -1): go up one row
; DOWN is (0, 1): go down one row
; LEFT is (-1, 0): go left one column
; RIGHT is (1, 0): go right one column
; Returns NIL if:
; 	keeper moves into wall/OoB, keeper pushes box into wall/OoB, 
; 	keeper pushes box into another box
; Prints new state s' (move keeper and box if necessary) if the desired spot is empty or has a box, 
;   and the other side of the box is empty (blank or star).
; Checks if all squares (old keeper square, new keeper square, new box square) are either keeperstar or boxstar
; 	to replace with star after keeper/box move.
; 
(defun try-move (s dc dr)
  (let* (
	(old_pos (getKeeperPosition s 0))
	(old_c (car old_pos))
	(old_r (cadr old_pos))
	(old_square (get-square s old_c old_r))
	(new_c (+ old_c dc))
	(new_r (+ old_r dr))
	(new_square (get-square s new_c new_r))
	(new_box_c (+ new_c dc))
	(new_box_r (+ new_r dr))
	(new_box_square (get-square s new_box_c new_box_r)))

	(cond 
	  ; old square can either have Keeper or KeeperStar
	  ; new square for keeper can either be blank, star, box, or boxStar; else NIL
	  ; new box square for box can either be blank or star; else NIL
	  ; else, NIL
	  ((isKeeper old_square)
	  	(cond 
		  	((isBlank new_square) (set-square (set-square s old_c old_r blank) new_c new_r keeper))
	  		((isStar new_square) (set-square (set-square s old_c old_r blank) new_c new_r keeperstar))
			; check if box
			((isBox new_square)
				(cond 
					((isBlank new_box_square) (set-square (set-square (set-square s old_c old_r blank) new_c new_r keeper) new_box_c new_box_r box))
					((isStar new_box_square) (set-square (set-square (set-square s old_c old_r blank) new_c new_r keeper) new_box_c new_box_r boxstar))
					(T NIL)
				)
			)
			; check if boxstar
			((isBoxStar new_square)
				(cond 
					((isBlank new_box_square) (set-square (set-square (set-square s old_c old_r blank) new_c new_r keeperstar) new_box_c new_box_r box))
					((isStar new_box_square) (set-square (set-square (set-square s old_c old_r blank) new_c new_r keeperstar) new_box_c new_box_r boxstar))
					(T NIL)
				)
			)
			(T NIL)
		)
	  )
	  ((isKeeperStar old_square)
	  	(cond 
		  	((isBlank new_square) (set-square (set-square s old_c old_r star) new_c new_r keeper))
			((isStar new_square) (set-square (set-square s old_c old_r star) new_c new_r keeperstar))
			; check if box
			((isBox new_square)
				(cond 
					((isBlank new_box_square) (set-square (set-square (set-square s old_c old_r star) new_c new_r keeper) new_box_c new_box_r box))
					((isStar new_box_square) (set-square (set-square (set-square s old_c old_r star) new_c new_r keeper) new_box_c new_box_r boxstar))
					(T NIL)
				)
			)
			; check if boxstar
			((isBoxStar new_square)
				(cond 
					((isBlank new_box_square) (set-square (set-square (set-square s old_c old_r star) new_c new_r keeperstar) new_box_c new_box_r box))
					((isStar new_box_square) (set-square (set-square (set-square s old_c old_r star) new_c new_r keeperstar) new_box_c new_box_r boxstar))
					(T NIL)
				)
			)
			(T NIL)
		)
	  )
	  (T NIL)  
    )
  )
)

;
; next-states (s), where s is a state of the game
; Returns list of successor states of s by creating list of try-move in 4 directions (UP, DOWN, LEFT, RIGHT):
; 	UP is (0, -1): go up one row
; 	DOWN is (0, 1): go down one row
; 	LEFT is (-1, 0): go left one column
; 	RIGHT is (1, 0): go right one column
; try-move either returns successor state s' (if successful move) or NIL, so next-states uses cleanUpList to remove NIL (if any).
;
(defun next-states (s)
  (cleanUpList
	; 				   UP 			   RIGHT			DOWN			   LEFT
  	(list (try-move s 0 -1) (try-move s 1 0) (try-move s 0 1) (try-move s -1 0))
  )
)

;
; h0 (s), where s is a state of the game
; Returns the trivial admissible heuristic (0).
;
(defun h0 (s) 0)

;  
; h1 (s), where s is a state of the game
; h1 is admissible (the number of misplaced boxes is at least 0, and h1 does not overestimate the cost).
; Returns the number of misplaced boxes in s by counting misplaced boxes in each row and getting the sum.
;
(defun h1 (s)
  (cond 
	((not s) 0)
	(T (+ (count box (car s)) (h1 (cdr s))))
  )
)

;
; distance (s d), where s is source position (c r) 
; 	and d is destination position (c r)
; Returns the distance between s and d, which is the sum of 
; 	the absolute differences of their coordinates.
;
(defun distance (s d)
	(+ (abs (- (first s) (first d))) 
	   (abs (- (second s) (second d)))
	)
)

; 
; minDistance (source destinations cur_min), where source is source (c r), 
; 	destinations in list of destinations (c r), and cur_min is the distance 
; 	between the source and current closest destination in destinations
; Returns the distance from source (c r) to the closest destination (c r) in destinations. 
;
(defun minDistance (source destinations cur_min)
  (if (not destinations) cur_min
	(let
	  ((d (distance source (car destinations))))
		(cond
		  ((> d cur_min) (minDistance source (cdr destinations) cur_min))
		  (T (minDistance source (cdr destinations) d))
		)
	)
  )
)

; 
; minDistanceSum (sources destinations), where sources is a list of sources (c r) 
; 	and destinations is a list of destinations (c r)
; Returns the sum of the minimum distances from every item in sources to a destination
; 	in destinations.
;
(defun minDistanceSum (sources destinations)
  (cond 
	((not destinations) 0)
	((not sources) 0)
	(T (+ (minDistance (car sources) destinations 1000) (minDistanceSum (cdr sources) destinations)))
  )
)

;
; keeperDistanceToBoxes (keeperPos boxesPos), where keeperPos is position of keeper (c r), 
; 	and boxesPos is list of positions (c r) of boxes
; Returns the sum of the distance of the keeper to every box.
;
(defun keeperDistanceToBoxes (keeperPos boxesPos)
  (if (not boxesPos) 0
	(+ (distance keeperPos (car boxesPos)) (keeperDistanceToBoxes keeperPos (cdr boxesPos)))
  )
)

; 
; getBoxesHelper (row r c), where row is row (list) in s, c/r are number of column/row
; Returns position of each box in given row.
;
(defun getBoxesHelper (row c r)
  (cond 
	((not row) NIL)
	((isBox (car row)) (cons (list c r) (getBoxesHelper (cdr row) (+ c 1) r)))
	(T (getBoxesHelper (cdr row) (+ c 1) r))
  )
)

; 
; getBoxPositions (s r), where s is a state and r is a row number
; Returns positions (c r) of every misplaced box in s.
;
(defun getBoxPositions (s r)
  (if (not s) NIL 
	(append (getBoxesHelper (car s) 0 r) (getBoxPositions (cdr s) (+ r 1)))
  )
)

; 
; getStarsHelper (row r c), where row is row (list) in s, c/r are number of column/row
; Returns position of each star in given row.
;
(defun getStarsHelper (row c r)
  (cond 
	((not row) NIL)
	((isStar (car row)) (cons (list c r) (getStarsHelper (cdr row) (+ c 1) r)))
	(T (getStarsHelper (cdr row) (+ c 1) r))
  )
)

; 
; getStarPositions (s r), where s is a state and r is a row number
; Returns positions (c r) of every star without a box or keeper in s.
;
(defun getStarPositions (s r)
  (if (not s) NIL 
	(append (getStarsHelper (car s) 0 r) (getStarPositions (cdr s) (+ r 1)))
  )
)

;
; h2 (s), where s is a state of the game
; Computes an admissible heuristic by summing the distances between all
; 	entities (keeper and misplaced boxes) and closest goals and the 
;   distances between the keeper and all misplaced boxes.
;
(defun h2 (s)
  (let
    (
	  (keeperPos (getKeeperPosition s 0))
	  (boxesPos (getBoxPositions s 0))
	  (starsPos (getStarPositions s 0))
	)
	(+ (minDistanceSum (cons keeperPos boxesPos) starsPos) (keeperDistanceToBoxes keeperPos boxesPos))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
;(6, 0.0395)
;(6, 0.0258)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
;(15, 0.1793)
;(15, 0.0975)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
;(13, 0.1918)
;(13, 0.0845)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
;(17, 0.3351)
;(17, 0.1123)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
;(12, 0.3576)
;(12, 0.1044)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
;(13, 0.3821)
;(13, 0.0851)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
;(47, 1.763)
;(47, 0.9455)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
;(22, 0.8582)
;(22, 0.3134)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
;(34, 0.8761)
;(34, 0.7937)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
;(59, 8.063)
;(59, 13.53)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(51)
;(51, 21.21)
;(51, 15.98)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(41, 76.14)
;(41, 62.67)
;(41, 28.68)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;h2: (78, 1217s)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;h0: stack overflow
;h1: stack overflow
;h2: (28, 2.187)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
; Usage: (printstates (a* start-state #’goal-test #’next-states #’heuristic) 0.2)
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
