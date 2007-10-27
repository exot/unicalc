(in-package :equations)

; evalutaing terms
;
; belegung
;
; auswertung von termen
;
; equation-holds-in-algebra-p
;
; models-p (synonym for equation-holds-in-algebra-p)

; equations

(defun toggle-equation (eqn)
  "If (EQUAL (EQN '(X Y))), then (EQUAL (TOGGLE-EQUATION EQN) '(Y X))."
  (declare (inline))
  (list (second eqn) (first eqn)))

(defun equation-match-p (term-algebra eqn1 eqn2)
  "Returns T if EQN1 can be matched on EQN2 or on (TOGGLE-EQUATION EQN2) in TERM-ALGEBRA"
  (let ((left-match (match term-algebra (first eqn1) (first eqn2))))
    (when left-match
      (let ((right-match (match term-algebra (second eqn1) (second eqn2))))
        (when (and right-match
                   (subsetp left-match right-match :test #'equal)
                   (subsetp right-match left-match :test #'equal))
          t)))))

(defun implies-equation-p (term-algebra eqn1 eqn2)
  "Returns non-NIL if EQN1 implies EQN2 in TERM-ALGEBRA (i.e. EQN1 matches EQN2)"
  (or (equation-match-p term-algebra eqn1 eqn2)
      (equation-match-p term-algebra eqn1 (toggle-equation eqn2))))

(defun weakly-dependent-p (term-algebra equation set-of-equations recursion-depth)
  "Returns non-NIL if EQUATION can be deduced from SET-OF-EQUATIONS in not more than
RECURSION-DEPTH steps in TERM-ALGEBRA."
  (cond
    ((some #'(lambda (x) (implies-equation-p term-algebra x equation)) set-of-equations)
     t)
    ((<= recursion-depth 0)
     nil)
    (t (dolist (eqn (all-possible-transformation term-algebra equation set-of-equations))
         (when (weakly-dependent-p term-algebra eqn set-of-equations (1- recursion-depth))
           (return-from weakly-dependent-p t))))))

(defun all-possible-transformation (term-algebra equation set-of-equations)
  "Returns all possible transformations of EQUATION in TERM-ALGEBRA under use of
equations in SET-OF-EQUATIONS (one step only)."
  (let ((appliable-equations (find-all-appliable-equations term-algebra
							   equation
							   set-of-equations))
        (all-transformation ()))
    (loop for transformation in appliable-equations
          do (setf all-transformation
                   (nconc all-transformation
                          (apply-transformation-to-all-matching-subterms term-algebra
									 transformation
                                                                         equation)))
          finally (return all-transformation))))

(defun find-all-appliable-equations (term-algebra equation set-of-equations)
  "Returns a pair of terms (TERM1 TERM2) s.t. TERM1 is subterm of (FIRST EQUATION)
and there is a equation eqn in SET-OF-EQUATIONS that implies (TERM1 TERM2) in TERM-ALGEBRA."
  (let ((appliable-functions ()))
    (flet ((apply-equation (eqn equation)
             (let ((matches (matches-subterm term-algebra (first eqn) (first equation))))
               (dolist (match matches)
                 (push (list (first match) ; this is the subterm in equation
                             ; which shall be substitute by the right side of
                             ; eqn (with actual matching applied on it)
                             (apply-matching term-algebra (second match) (second eqn)))
                       appliable-functions)))))
      (loop for eqn in set-of-equations
            do
              (apply-equation eqn equation)
              (apply-equation (toggle-equation eqn) equation)
            finally (return appliable-functions)))))

(defun apply-transformation-to-all-matching-subterms (term-algebra pair equation)
  "Returns a list of equations s.t. every occurence of (FIRST PAIR) in
(FIRST EQUATION) is substituted by (SECOND PAIR)."
  (mapcar #'(lambda (x) (list x (second equation)))
          (remove-duplicates (apply-to-all-subterms term-algebra pair (first equation))
                             :test #'equal)))

(defun apply-to-all-subterms (term-algebra pair term)
  "Returns the list of terms yield from substituting a subterm of TERM
EQUAL to (FIRST PAIR) by (SECOND PAIR)."
  (cond
    ((equal (first pair) term)
      (list (second pair)))
    ((variablep term-algebra term)
      nil) ; no subterms left
    (t (let ((result ()))
         (loop for i from 1 to (length term)
               do
                 (let ((transformed-term (apply-to-all-subterms
					  term-algebra pair (nth i term))))
                   (dolist (myterm transformed-term)
                     (push (append (subseq term 0 i)
                                   (list myterm)
                                   (subseq term (1+ i)))
                           result)))
               finally (return result))))))

(defun rotate-list (list)
  "Returns left-rotate of LIST."
  (setf list (append (rest list) (list (first list)))))

(defun remove-all-weakly-dependent-equations (term-algebra equations recursion-depth)
  "Removes all equations in EQUATIONS which are WEAKLY-DEPENDENT-P in RECURSION-DEPTH
steps."
  (let ((my-equations equations))
    (loop with substitutions = 0
          do
            (cond
              ((null my-equations) nil)
              ((weakly-dependent-p term-algebra (first my-equations) (rest my-equations)
                                   recursion-depth)
                 (setf my-equations (rest my-equations))
                 (setf substitutions 0))
              (t (setf my-equations (rotate-list my-equations))
                 (incf substitutions)))
            (when (> substitutions (length my-equations))
              (return my-equations)))))