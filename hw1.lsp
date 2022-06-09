; HW1: Brendan Rossmango, 505370692
; Overall, my solutions use (cond) to go through all cases of the problem recursively, with base cases first

; 1. TREE-CONTAINS
;   N - number
;   TREE - ordered tree (L m R)
;   Returns T if N is in TREE, else NIL
; Base case - empty TREE, TREE is a number, or N is equal to m in the ordered tree
; Recursive case - if N is less than m in the ordered tree, check if N is in the left side of the tree L
;                  else (N is greater than m), check if N is in the right side of the tree R
(defun TREE-CONTAINS (N TREE)
    (cond
        ((not TREE) NIL)
        ((numberp TREE) (= N TREE))
        ((= N (cadr TREE)) T)
        ((< N (cadr TREE)) (TREE-CONTAINS N (car TREE)))
        ((> N (cadr TREE)) (TREE-CONTAINS N (caddr TREE)))
    )
)

; 2. TREE-MAX
;   TREE - ordered tree (L m R)
;   Returns maximum number in TREE
; Base case - empty TREE or TREE is a number
; Recursive case - since an ordered tree always has 3 elements (L m R) - the rightmost side of the tree R is the max
(defun TREE-MAX (TREE)
    (cond 
        ((not TREE) NIL)
        ((numberp TREE) TREE)
        (T (TREE-MAX (caddr TREE)))
    )
)

; 3. TREE-ORDER
;   TREE - ordered tree (L m R)
;   Returns post-ordered list of numbers appearing in TREE
; Base case - empty TREE or TREE is a number (if so, just return a list of the number)
; Recursive case - append post order traversal of L, then R, then m
(defun TREE-ORDER (TREE)
    (cond
        ((not TREE) NIL)
        ((numberp TREE) (list TREE))
        (T (append (TREE-ORDER (car TREE))
            (append (TREE-ORDER (caddr TREE)) (list (cadr TREE)))
           )
        )
    )
)

; 4. SUB-LIST
;   L - list
;   START - nonnegative integer
;   LEN - nonnegative integer
;   Returns sub-list L' of L starting at position START with length LEN
; Base case - empty list or LEN is 0
; Recursive case - if START index is 0, then construct list with first of L and rest of L with LEN - 1
;                  else (if START is greater than 0), then recursively make sublist on rest of L, moving the start index
(defun SUB-LIST (L START LEN)
    (cond
        ((not L) NIL)
        ((= LEN 0) NIL)
        ( (= START 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))) )
        ( (> START 0) (SUB-LIST (cdr L) (- START 1) LEN) )
    )
)

; 5. SPLIT-LIST
;   L - list
;   Returns list of two lists L1 and L2, where L is the result of appending L1 and L2, 
;       and the length of L1 minus the length of L2 is 0 or 1
; No recursion: if list is even length, then make 2 sublists from 0 to midpoint and midpoint to end
;               if list is odd length, then make 2 sublists from 0 to midpoint + 1 and midpoint to end
;                   (the first list is longer if the original list is odd length)
(defun SPLIT-LIST (L)
    (if (evenp (length L))
			(let ( (midpoint (/ (length L) 2)) )
				(list (SUB-LIST L 0 midpoint) (SUB-LIST L midpoint midpoint))
			)
        (let ( (midpoint (/ (+ (length L) 1) 2) ) )
            (list (SUB-LIST L 0 midpoint) (SUB-LIST L midpoint (- midpoint 1)))
        )
    )
)

; 6. BTREE-HEIGHT
;   TREE - binary tree (a tree in which each node has 0 or 2 children)
;       A leaf node (node with 0 children) is represented by atom N
;       An internal node (node with 2 children) is represented by a list (L R), 
;           where L is the left child and R is the right child
;   Returns the height of TREE, which is the length of the longest path between root node and farthest leaf node
; Base case - TREE is an atom
; Recursive case - if height of left subtree is greater than height of right subtree, then increase the height of the left tree by 1
;                  else, increase the height of the right tree by 1
(defun BTREE-HEIGHT (TREE)
    (cond 
        ((atom TREE) 0)
        (T
            (cond
                ((> (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))) (+ (BTREE-HEIGHT (car TREE)) 1))
                (T (+ (BTREE-HEIGHT (cadr TREE)) 1))
            )
        )
    )
)

; 7. LIST2BTREE
;   LEAVES - nonempty list of atoms
;   Returns binary tree such that the leaves are the elements of LEAVES,
;           and for any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the
;           number of leaves in its right branch is 0 or 1.
; Base case - empty list of leaves, or length of leaves is 1
; Recursive case - create list from left of split list and right of split list recursively
(defun LIST2BTREE (LEAVES)
    (cond 
        ((not LEAVES) NIL)
        ((= (length LEAVES) 1) (car LEAVES))
        (T 
            (list 
                (LIST2BTREE (car (SPLIT-LIST LEAVES))) (LIST2BTREE (cadr (SPLIT-LIST LEAVES)))
            )
        )
    )
)

; 8. BTREE2LIST
;   TREE - binary tree, where each node has at most 2 children
;   Returns a list of atoms of TREE
; Base case - empty tree or TREE is atom
; Recursive case - append left of tree and right of tree recursively
(defun BTREE2LIST (TREE)
    (cond 
        ((not TREE) NIL)
        ((atom TREE) (list TREE))
        ; Otherwise, length of BTREE in this form is always 2
        (T (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
    )
)

; 9. IS-SAME
;   E1 and E2 are LISP expressions where all atoms are numbers
;   Returns T if E1 and E2 are identical; NIL otherwise
; Base case - check if both are NIL or atoms; if both atoms, check if equal
;             check if one is an atom and the other is a list
; Recursive case - check if the first and rest of E1 and E2 are the same
(defun IS-SAME (E1 E2)
    (cond 
        ((not E1) (not E2)) ; if E1 is NIL, then T if E2 is NIL; NIL otherwise
        ((and (atom E1) (atom E2)) (= E1 E2))
        ((or (and (atom E1) (listp E2)) (and (listp E1) (atom E2))) NIL)
        (T (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
    )
)

; 10. FLATTEN-APPEND
;   E1 and E2 are LISP expressions where all atoms are numbers
;       E1 cannot be an atom (so to mark that we have already appended to E1, we can safely pass 'x as E1)
;   Returns a list of all the atoms of E2 appended to E1, in left-to-right, depth-first order of occurrence of atoms in E2
; Append the resulting flattened list of E2 to E1 (to only append E1 once, we mark E1 as 'x after, to mark that we have already 
;                                                                                               appended to E1)
; Base case - empty E2 or E2 is an atom (just put it in a list and append it)
; Recursive case - if first element of E2 is atom, add it to a list and flatten the rest
;                  else, append the first flattened list of E2 and the rest of E2
(defun FLATTEN-APPEND (E1 E2)
    (cond 
        ((not (equal E1 'x)) (append E1 (FLATTEN-APPEND 'x E2))) ; will happen only once (afterwards, E1 will be'x)
        ((not E2) NIL)  ; from here downwards, E1 is 'x, so we never append E1 again; we just flatten E2
        ((atom E2) (list E2))
        ((atom (first E2)) (cons (first E2) (FLATTEN-APPEND 'x (rest E2))))
        (T (append (FLATTEN-APPEND 'x (first E2)) (FLATTEN-APPEND 'x (rest E2))))
    )
)
