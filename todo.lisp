;;; substitue numbers (non-terms) in algebra with corresponding terms

(defun substitute-generators-by-variables (algebra generators)
  "Substitues in ALGEBRA all numbers being in GENERATORS with variable symbols."
  (iterate-over-array algebra i j
    (when (member (aref algebra i j) generators)
      (setf (aref algebra i j) (nth (aref algebra i j) *variables*)))))

(defun find-next-number (algebra)
  "Finds next position in ALGEBRA containing a number."
  (iterate-over-array algebra i j
    (when (and (>= i 1) (>= j 1)
               (not (termp (aref algebra i j)))
               (termp (aref algebra i 0))
               (termp (aref algebra 0 j)))
      (return-from find-next-number (list i j))))
  nil)

(defun substitute-number-by-term (algebra number term)
  "Substitues all occurences of NUMBER in ALGEBRA with TERM."
  (iterate-over-array algebra i j
    (when (equal (aref algebra i j) number)
      (setf (aref algebra i j) term))))

(defun label-numbers-in-algebra (algebra generators)
  "Substitutes all numbers in the free algebra ALGEBRA into terms in terms of GENERATORS.
GENERATORS should contain all numbers of ALGEBRA generating it."
  (substitute-generators-by-variables algebra generators)
  (do ((next-number (find-next-number algebra) (find-next-number algebra)))
      ((not next-number) algebra)
    (substitute-number-by-term algebra 
      (aref algebra (first next-number) (second next-number))
      (list
        (get-operation-symbol algebra)
        (aref algebra (first next-number) 0)
        (aref algebra 0 (second next-number))))))

; free-algebra

(defun extract-all-equations (labled-algebra)
  "Returns all equations given by LABLED-ALGEBRA"
  (let ((found-equations ()))
    (iterate-over-array labled-algebra i j
      (when (and (>= i 1) (>= j 1))
        (push (list (list (get-operation-symbol labled-algebra)
                          (aref labled-algebra i 0)
                          (aref labled-algebra 0 j))
                    (aref labled-algebra i j))
              found-equations)))
    found-equations))

;;; eliminating dependent equations

; equations

(defun toggle-equation (eqn)
  "If (EQUAL (EQN '(X Y))), then (EQUAL (TOGGLE-EQUATION EQN) '(Y X))."
  (declare (inline))
  (list (second eqn) (first eqn)))

(defun equation-match-p (eqn1 eqn2)
  "Returns T if EQN1 can be matched on EQN2 or on (TOGGLE-EQUATION EQN2)"
  (let ((left-match (match (first eqn1) (first eqn2))))
    (when left-match
      (let ((right-match (match (second eqn1) (second eqn2))))
        (when (and right-match
                   (subsetp left-match right-match :test #'equal)
                   (subsetp right-match left-match :test #'equal))
          t)))))

(defun implies-equation-p (eqn1 eqn2)
  "Returns non-NIL if EQN1 implies EQN2 (i.e. EQN1 matches EQN2)"
  (or (equation-match-p eqn1 eqn2)
      (equation-match-p eqn1 (toggle-equation eqn2))))

(defun weakly-dependent-p (equation set-of-equations recursion-depth)
  "Returns non-NIL if EQUATION can be deduced from SET-OF-EQUATIONS in not more than
RECURSION-DEPTH steps"
  (cond 
    ((<= recursion-depth 0)
       (some #'(lambda (x) (implies-equation-p x equation)) set-of-equations))
    (t (dolist (eqn (all-possible-transformation equation set-of-equations))
         (when (weakly-dependent-p eqn set-of-equations (1- recursion-depth))
           (return-from weakly-dependent-p t))))))

(defun all-possible-transformation (equation set-of-equations)
  "Returns all possible transformations of EQUATION under use of
equations in SET-OF-EQUATIONS (one step only)."
  (let ((appliable-equations (find-all-appliable-equations equation set-of-equations))
        (all-transformation ()))
    (loop for transformation in appliable-equations
          do (setf all-transformation
                   (nconc all-transformation 
                          (apply-transformation-to-all-matching-subterms transformation
                                                                         equation)))
          finally (return all-transformation))))
             
(defun find-all-appliable-equations (equation set-of-equations)
  "Returns a pair of terms (TERM1 TERM2) s.t. TERM1 is subterm of (FIRST EQUATION)
and there is a equation eqn in SET-OF-EQUATIONS that implies (TERM1 TERM2)."
  (let ((appliable-functions ()))
    (flet ((apply-equation (eqn equation)
             (let ((matches (matches-subterm (first eqn) (first equation))))
               (dolist (match matches)
                 (push (list (first match) ; this is the subterm in equation
                             ; which shall be substitute by the right side of
                             ; eqn (with actual matching applied on it)
                             (apply-matching (second match) (second eqn)))
                       appliable-functions)))))
      (loop for eqn in set-of-equations
            do
              (apply-equation eqn equation)
              (apply-equation (toggle-equation eqn) equation)
            finally (return appliable-functions)))))

(defun apply-transformation-to-all-matching-subterms (pair equation)
  "Returns a list of equations s.t. every occurence of (FIRST PAIR) in
(FIRST EQUATION) is substituted by (SECOND PAIR)."
  (mapcar #'(lambda (x) (list x (second equation)))
          (remove-duplicates (apply-to-all-subterms pair (first equation)) :test #'equal)))

(defun apply-to-all-subterms (pair term)
  "Returns the list of terms yield from substituting a subterm of TERM
EQUAL to (FIRST PAIR) by (SECOND PAIR)."
  (cond
    ((equal (first pair) term)
      (list (second pair)))
    ((variablep term)
      nil) ; no subterms left
    (t (let ((result ()))
         (loop for i from 1 to (length term)
               do
                 (let ((transformed-term (apply-to-all-subterms pair (nth i term))))
                   (dolist (myterm transformed-term)
                     (push (append (subseq term 0 i)
                                   (list myterm)
                                   (subseq term (1+ i)))
                           result)))
               finally (return result))))))

(defun rotate-list (list)
  "Returns left-rotate of LIST."
  (setf list (append (rest list) (list (first list)))))

(defun remove-all-weakly-dependent-equations (equations recursion-depth)
  "Removes all equations in EQUATIONS which are WEAKLY-DEPENDENT-P in RECURSION-DEPTH
steps."
  (let ((my-equations equations))
    (loop with substitutions = 0
          do
            (cond
              ((null my-equations) nil)
              ((weakly-dependent-p (first my-equations) (rest my-equations) 
                                   recursion-depth)
                 (setf my-equations (rest my-equations))
                 (setf substitutions 0))
              (t (setf my-equations (rotate-list my-equations))
                 (incf substitutions)))
            (when (> substitutions (length my-equations)) 
              (return my-equations)))))

(defun show-all-equations (algebra generating-elements n)
  "Shows all equations represented by the free algebra ALGEBRA 
generated by GENERATING-ELEMENTS being weakly-independent
of level n."
  (mapc #'pprint-term-pair
        (remove-all-weakly-dependent-equations 
          (extract-all-equations 
            (label-numbers-in-algebra algebra generating-elements)) 
          n))
  (values))

;;; test cases

(set-variable-set '(v0 v1 v2 v3 v4 v5))
(set-signature '((+ 2)))

; define interpretation of an operation in *signature*
; with a values table
(defparameter *free-algebra*
  #2A((+ 0 1 2 3 4)
      (0 0 2 3 3 2)
      (1 2 1 4 2 4)
      (2 3 4 2 3 4)
      (3 3 2 3 3 2)
      (4 2 4 4 2 4)))

(defun test-case-1 ()
  (show-all-equations *free-algebra* 
                      (calculate-generating-elements (list *free-algebra*))
                      2))
