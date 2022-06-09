;;;;;;;;;;;;;;;;
;; Homework 4 ;;
;;;;;;;;;;;;;;;;

;
; eval-literal (literal assignment)
; Checks if a variable in the current assignment is equal to the literal
; Returns T if a variable in assignment is equal to the literal, NIL if the variable's value is the opposite
;
(defun eval-literal (literal assignment)
  (let ((var (first assignment)))
    (cond
      ((not var) T) ; T if no assignment, var can be assigned to anything
      ((= (+ literal var) 0) NIL)
      ((= literal var) T)
      (T (eval-literal literal (rest assignment)))
    )
  )
)

; 
; eval-clause (clause assignment)
; Checks a clause to see if it is satisfied by the current assignment of variables.
;
(defun eval-clause (clause assignment) 
  (cond 
    ((not clause) NIL)
    ((eval-literal (first clause) assignment) T)
    (T (eval-clause (rest clause) assignment))
  )
)

;
; eval-CSP (assignment clauses)
; Checks the clauses of the CSP to see if they are satisfied by the current assignment of variables.
;
(defun eval-CSP (assignment clauses)
  (cond 
    ((not clauses) T)
    ((not (eval-clause (first clauses) assignment)) NIL)
    (T (eval-CSP assignment (rest clauses)))
  )
)

;
; backtracking (assignment n clauses)
; If the current assignment is valid for the clauses with all variables assigned, returns assignment.
; Else, assigns - or + to the next unassigned variable and checks the new assignment.
;
(defun backtracking (assignment n clauses variables)
  (let ((next-var (first variables)))
    (cond 
      ((not (eval-CSP assignment clauses)) NIL)
      ((= (length assignment) n) assignment)
      (T (or (backtracking (append assignment (list (- next-var))) n clauses (rest variables)) 
            (backtracking (append assignment (list next-var)) n clauses (rest variables))
      ))
    )
  )
)

;
; get-variables (dict), where dict is the counts dictionary
; Returns list of variables without their counts.
;
(defun get-variables (dict)
  (cond
    ((not dict) NIL)
    (T (append (list (first (first dict))) (get-variables (rest dict))))
  )
)

; 
; sort-list (lst), where lst is a list of list (counts dict)
; Quicksort algorithm: sorts greater, then the first element (pivot), then the lesser
;
(defun sort-list (lst)
  (cond
    ((not lst) NIL)
    (T (let ((greater (sort-greater (first lst) (rest lst))) (lesser (sort-lesser (first lst) (rest lst))))
        (append (sort-list greater) (list (car lst)) (sort-list lesser)))
    )
  )
)

; Helper function for sort-list: sorts greater than the pivot
(defun sort-greater (head tail)
  (cond
    ((not head) NIL)
    ((not tail) NIL)
    ((> (second head) (second (first tail))) (sort-greater head (rest tail)))
    (T (append (list (first tail)) (sort-greater head (rest tail))))
  )
)

; Helper function for sort-list: sorts less than the pivot
(defun sort-lesser (head tail)
  (cond
    ((not head) NIL)
    ((not tail) NIL)
    ((<= (second head) (second (first tail))) (sort-lesser head (rest tail)))
    (T (append (list (first tail)) (sort-lesser head (rest tail))))
  )
)

;
; increase-var-count (literal counts-dict n)
; Updates dictionary of counts by increasing the count of given variable by 1.
;
(defun increase-var-count (literal counts-dict n)
  (let ((var (abs literal))
        (var-n-count (first counts-dict)))
    (cond 
      ((not counts-dict) NIL)
      ((= var n) (append (list (list (first var-n-count) (+ (second var-n-count) 1))) (rest counts-dict)))
      (T (append (list var-n-count) (increase-var-count (+ var 1) (rest counts-dict) n)))
    )
  )
)

;
; count-vars-in-clause (clause counts-dict n)
; Returns updated dictionary of counts after counting occurrences of all literals in a clause.
;
(defun count-vars-in-clause (clause counts-dict n)
  (cond
    ((not clause) counts-dict)
    (T (count-vars-in-clause (rest clause) (increase-var-count (first clause) counts-dict n) n))
  )
)

;
; count-occurences (delta counts-dict n)
; Returns updated dictionary of counts after counting occurrences of all literals in all clauses.
;
(defun count-occurences (delta counts-dict n)
  (cond
    ((not delta) counts-dict)
    (T (count-occurences (rest delta) (count-vars-in-clause (first delta) counts-dict n) n))
  )
)

; 
; init-counts-dict (n), where n is the number of variables
; Initializes dictionary with variable (1-n) as key and count as 0.
;
(defun init-counts-dict (n)
  (cond
    ((< n 1) NIL)
    (T (append (list (list n 0)) (init-counts-dict (- n 1))))
  )
)

;
; sat? (n delta), where n is the number of variables and delta is the clauses in CNF form
; Does backtracking search to solve SAT problem with given parameters and variable ordering.
;   count-occurences gets the occurences of all variables in all clauses in delta, 
;   sort-list sorts the list by the occurences in decreasing order, 
;   get-variables gets the variable ordering without their respective occurences
;
(defun sat? (n delta) 
  (backtracking NIL n delta (get-variables (sort-list (count-occurences delta (init-counts-dict n) n))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (first cnf) (rest cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
