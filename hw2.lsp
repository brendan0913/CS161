; BFS takes a single argument that is the list representation of the tree, and returns
; a single, top-level list of the terminal nodes in the order they would be visited by
; a left-to-right breadth-first search.

; (BFS '((A (B)) C (D))) --> (C A D B)
; (BFS '((w x) (y z))) --> (W X Y Z)
; (BFS '((A B) C (D E))) --> (C A B D E)
; (BFS 1) --> (1)
; (BFS '((a (b)) c d ((((e)))) f (g))) --> (C D F A G B E)
; (BFS '(A (B C) (D) (E (F G)))) --> (A B C D E F G)
(defun BFS (tree)
  (cond 
    ((not tree) NIL)
    ((atom tree) (list tree))
    ((atom (car tree)) (cons (car tree) (BFS (cdr tree))))
    (T (BFS (append (cdr TREE) (car TREE))))
  )
)

; DFS takes a single argument that is the list representation of the tree, and returns
; a single, top-level list of the terminal nodes in the order they would be visited by
; a left-to-right depth-first search.

; (DFS '((A (B)) C (D))) --> (A B C D)
; (DFS '((w x) (y z))) --> (W X Y Z)
; (DFS ' ((A B) C (D E))) --> (A B C D E)
; (DFS 1) --> (1)
; (DFS '((a (b)) c d ((((e)))) f (g))) --> (A B C D E F G)
; (DFS '(A (B C) (D) (E (F G)))) --> (A B C D E F G)
(defun DFS (tree)
  (cond 
    ((not tree) NIL)
    ((atom tree) (list tree))
    (T (append (DFS (car tree)) (DFS (cdr tree))))
  )
)

; 3. DFID_single (helper function for DFID) and DFID
; DFID_single takes two arguments, tree and depth, where depth is the current specified 
; depth, and returns a single, top-level list of the terminal nodes in the order they 
; would be visited by a RIGHT-TO-LEFT depth-first search.
(defun DFID_single (tree cur_depth)
  (cond 
    ((not tree) NIL)
    ((atom tree) (list tree))
    ((= cur_depth 0) NIL)
    (T (append (DFID_single (cdr tree) cur_depth) (DFID_single (car tree) (- cur_depth 1))))
  )
)


; DFID takes two arguments, the list representation of the tree, and an integer representing
; the maximum depth of the tree, and returns a single top-level list of the terminal nodes 
; in the order that they would be visited by a right-to-left depth-first iterative-deepening 
; search. 
; Note that those nodes that are visited in multiple iterations will appear multiple times in
; the output list. 

; (DFID '((A (B)) C (D)) 3) --> (C D C A D C B A)
; (DFID '(A B) 2) --> (B A B A)
; (DFID '((W X) (Y Z)) 1) --> NIL
; (DFID '((W X) (Y Z)) 2) --> (Z Y X W)
; (DFID '((w x) (y z)) 3) --> (Z Y X W Z Y X W)
; (DFID '(A (B C) (D) (E (F G))) 1) --> (A)
; (DFID '(A (B C) (D) (E (F G))) 2) --> (A E D C B A)
; (DFID '(A (B C) (D) (E (F G))) 3) --> (A E D C B A G F E D C B A)
; (DFID '((A (B)) C (D)) 0) --> NIL
; (DFID '((A (B)) C (D)) 1) --> (C)
; (DFID '((A (B)) C (D)) 2) --> (C D C A)
; (DFID '((A (B)) C (D)) 4) --> (C D C A D C B A D C B A)
; (DFID '((A (B)) C (D)) 5) --> (C D C A D C B A D C B A D C B A)
; (DFID '1 1) --> (1 1)
; (DFID '1 0) --> (1)
(defun DFID (tree max_depth)
  (cond 
    ((not tree) NIL)
    ((atom tree)
      (cond 
        ((= max_depth 0) (list tree))
        (T (append (DFID tree (- max_depth 1)) (DFID_single tree max_depth)))
      )
    )
    ((= max_depth 0) NIL)
    ((= max_depth 1) (DFID_single tree 1))
    (T (append (DFID tree (- max_depth 1)) (DFID_single tree max_depth)))
  )
)

; hw2_skeleton.lsp
; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.

; (next-state '(3 3 t) 1 0) -> NIL
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))
(defun next-state (s m c)
  (let
    (
      (current (list (- (car s) m) (- (cadr s) c) (caddr s)))
      (next (list (+ m (- 3 (car s))) (+ c (- 3 (cadr s))) (not (caddr s))))
    ) 
    (cond ; first 4 should not happen (succ-fn will append valid next-states)
      ((not s) NIL)                                                    ; empty state
      ((or (< m 0) (< c 0)) NIL)                                       ; negative x or negative o
      ((>= 0 (+ m c)) NIL)                                             ; boat is empty or has negative travelers
      ((< 2 (+ m c)) NIL)                                              ; boat has more than 2 travelers
      ((> m (car s)) NIL)                                              ; boat has more x than possible
      ((> c (cadr s)) NIL)                                             ; boat has more o than possible
      ((and (> (car current) 0) (> (cadr current) (car current))) NIL) ; more o than x on current side
      ((and (> (car next) 0) (> (cadr next) (car next))) NIL)          ; more o than x on next side
      (T (list next))
    )
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
(defun succ-fn (s)
  (append 
    (next-state s 2 0)  ; all legal operators (boat not empty, boat has max 2 travelers)
    (next-state s 0 2)
    (next-state s 1 1)
    (next-state s 1 0)
    (next-state s 0 1)
  )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.

; (on-path '(3 3 nil) '((1 1 t) (3 3 nil))) --> t
; (on-path '(3 3 nil) '((3 3 t) (0 2 nil) (3 2 t))) --> NIL
(defun on-path (s states)
  (cond 
    ((not states) NIL)
    ((equal s (car states)) T)
    (T (on-path s (cdr states)))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)

; (mult-dfs (succ-fn '(3 3 t)) '((3 3 t))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) 
;           (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))
; (mult-dfs (succ-fn '(0 2 nil)) '((0 2 nil) (3 3 t))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) 
;           (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))
; (mult-dfs (succ-fn '(2 2 nil)) '((2 2 nil) (3 1 t))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T)
;                                                          (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T))
; (mult-dfs (succ-fn '(1 1 nil)) '((1 1 nil) (3 3 t) (0 2 nil))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) 
;       (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T) (0 2 NIL))
(defun mult-dfs (states path)
  (cond 
    ((not states) NIL)
    ((mc-dfs (car states) path) (mc-dfs (car states) path))
    (T (mult-dfs (cdr states) path))
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)

; (mc-dfs '(3 3 t) NIL) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) (2 2 NIL)
;                                              (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))
; (mc-dfs '(0 2 nil) '((3 3 t))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) 
;                                     (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (0 2 NIL) (3 3 T))
; (mc-dfs '(3 3 t) '((3 3 t))) --> NIL
; (mc-dfs '(3 3 nil) '((3 3 nil))) --> ((3 3 NIL) (3 3 NIL))
; (mc-dfs '(3 3 t) '((0 2 nil))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) 
;                           (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T) (0 2 NIL))
; (mc-dfs '(1 1 nil) '((3 3 t) (0 2 nil))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) 
;                                     (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T) (0 2 NIL))
; (mc-dfs '(1 1 nil) '((3 3 t) (0 2 nil) (3 3 t))) --> ((3 3 NIL) (1 1 T) (3 2 NIL) (0 3 T) (3 1 NIL) (2 2 T) 
;                                     (2 2 NIL) (3 1 T) (0 3 NIL) (3 2 T) (1 1 NIL) (3 3 T) (0 2 NIL) (3 3 t))
(defun mc-dfs (s path)
  (cond
    ((final-state s) (append (list s) path))
    ((not (on-path s path)) (mult-dfs (succ-fn s) (append (list s) path)))
    (T NIL) ; do not revisit repeated states already on the search path
  )
)

; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

