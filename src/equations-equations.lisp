(in-package :equations)

;;; evalutaing terms

; belegungen sind funktionen oder graphen von funktionen

(define-simple-condition equations-error)

(defgeneric ensure-association (func-or-graph)
  (:documentation "Ensures FUNC-OR-GRAPH to be a function"))

(defmethod ensure-association ((func-or-graph algebraic-function))
  func-or-graph)

(defmethod ensure-association ((func-or-graph list))
 (make-function (mapcar #'first func-or-graph)
                  (mapcar #'second func-or-graph)
                  func-or-graph))

(defmethod ensure-association ((func-or-graph standard-set))
  (ensure-association (set-to-list func-or-graph)))

(defun evaluate-term-in-algebra (algebra term association)
  "Evaluates TERM in ALGEBRA with given ASSOCIATION."
  (declare (type algebra algebra)
           (type term term)
           (type t association))
  (let* ((evaluator (ensure-association association))
         (term-algebra (make-term-algebra (source evaluator)
                                          (signature-of algebra))))
    (cond
      ((not (termp term-algebra term))
       (error 'equations-error :text
              (format nil "~a is not a term with variables ~a and signature ~a"
                      term (source evaluator) (signature-of algebra))))
      (t (%evaluate-term-in-algebra term-algebra algebra term evaluator)))))

(defun %evaluate-term-in-algebra (term-algebra algebra term association)
  (declare (type term-algebra term-algebra)
           (type algebra algebra)
           (type term term)
           (type algebraic-function association))
    (cond
      ((variablep term-algebra term)
       (apply-function-to-element association term))
      ((set-member-s term (base-set-of algebra))
       term)
      (t
       (apply-operation-in-algebra (first term)
                                   (mapcar #'(lambda (x)
                                               (%evaluate-term-in-algebra
                                                term-algebra
                                                algebra
                                                x
                                                association))
                                           (rest term))
                                   algebra))))

; equation-holds-in-algebra-p

(defun equation-holds-in-algebra-p (algebra variables equation)
  "Returns non-NIL if EQUATION holds in ALGEBRA."
  (declare (type algebra algebra)
           (type standard-set variables)
           (type list equation))
  (forall (assignment (all-assignments variables (base-set-of algebra)))
    (set-equal (evaluate-term-in-algebra algebra (first equation) assignment)
               (evaluate-term-in-algebra algebra (second equation) assignment))))

; models-p (synonym for equation-holds-in-algebra-p)
(declaim (inline models-p))
(defun models-p (algebra variables equation)
  "Synonym for EQUATION-HOLDS-IN-ALGEBRA-P."
  (declare (type algebra algebra)
           (type standard-set variables)
           (type list equation))
  (equation-holds-in-algebra-p algebra variables equation))

; equations

(defun toggle-equation (eqn)
  "If (EQUAL (EQN '(X Y))), then (EQUAL (TOGGLE-EQUATION EQN) '(Y X))."
  (declare (type list eqn))
  (declare (inline))
  (list (second eqn) (first eqn)))

(defun equation-match-p (term-algebra eqn1 eqn2)
  "Returns T if EQN1 can be matched on EQN2 or on (TOGGLE-EQUATION EQN2) in
 TERM-ALGEBRA"
  (declare (type term-algebra term-algebra)
           (type list eqn1 eqn2))
  (let ((left-match (match term-algebra (first eqn1) (first eqn2))))
    (when left-match
      (let ((right-match (match term-algebra (second eqn1) (second eqn2))))
        (when (and right-match
                   ;; #'equal because we are working with symbols
                   (subsetp left-match right-match :test #'equal)
                   (subsetp right-match left-match :test #'equal))
          t)))))

(defun implies-equation-p (term-algebra eqn1 eqn2)
  "Returns non-NIL if EQN1 implies EQN2 in TERM-ALGEBRA (i.e. EQN1 matches
 EQN2)"
  (declare (type term-algebra term-algebra)
           (type list eqn1 eqn2))
  (or (equation-match-p term-algebra eqn1 eqn2)
      (equation-match-p term-algebra eqn1 (toggle-equation eqn2))))

(defun weakly-dependent-p (term-algebra equation set-of-equations
                           recursion-depth)
  "Returns non-NIL if EQUATION can be deduced from SET-OF-EQUATIONS in not
  more than RECURSION-DEPTH steps in TERM-ALGEBRA."
  (declare (type term-algebra term-algebra)
           (type list equation)
           (type (or list standard-set) set-of-equations)
           (type integer recursion-depth))
  (cond
    ((exists (x set-of-equations)
       (implies-equation-p term-algebra x equation))
     t)
    ((<= recursion-depth 0)
     nil)
    (t (let ((all-eqns (all-possible-transformation term-algebra equation
						    (set-to-list set-of-equations))))
	 (loop for eqn in all-eqns
	       do (when (weakly-dependent-p term-algebra eqn set-of-equations
					    (1- recursion-depth))
		    (return-from weakly-dependent-p t)))))))

(defun all-possible-transformation (term-algebra equation set-of-equations)
  "Returns all possible transformations of EQUATION in TERM-ALGEBRA under use
  of equations in SET-OF-EQUATIONS (one step only)."
  (declare (type term-algebra term-algebra)
           (type list equation)
           (type list set-of-equations))
  (let ((appliable-equations (find-all-appliable-equations term-algebra
							   equation
							   set-of-equations))
        (all-transformation ()))
    (loop for transformation in appliable-equations
          do (setf all-transformation
                   (nconc all-transformation
                          (apply-transformation-to-all-matching-subterms
                            term-algebra
			    transformation
                            equation)))
          finally (return (remove-duplicates all-transformation
					     :test #'equal)))))

;(declaim (ftype (function (term-algebra list standard-set) t) ...))
(defun find-all-appliable-equations (term-algebra equation set-of-equations)
  "Returns a pair of terms (TERM1 TERM2) s.t. TERM1 is subterm of
  (FIRST EQUATION) and there is a equation eqn in SET-OF-EQUATIONS that implies
  (TERM1 TERM2) in TERM-ALGEBRA."
  (declare (type term-algebra term-algebra)
           (type list equation)
           (type list set-of-equations))
  (let ((appliable-functions ()))
    (flet ((apply-equation (eqn equation)
             (let ((matches (matches-subterm term-algebra
                                             (first eqn) (first equation))))
               (dolist (match matches)
                 (push (list (first match) ; this is the subterm in equation
                             ; which shall be substitute by the right side of
                             ; eqn (with actual matching applied on it)
                             (apply-matching term-algebra (second match)
                                                          (second eqn)))
                       appliable-functions)))))
      (loop for eqn in set-of-equations
            do
              (apply-equation eqn equation)
              (apply-equation (toggle-equation eqn) equation)
            finally (return appliable-functions)))))

(defun apply-transformation-to-all-matching-subterms (term-algebra pair
                                                      equation)
  "Returns a list of equations s.t. every occurence of (FIRST PAIR) in
(FIRST EQUATION) is substituted by (SECOND PAIR)."
  (declare (type term-algebra term-algebra)
           (type list pair equation))
  (mapcar #'(lambda (x) (list x (second equation)))
          (remove-duplicates (apply-to-all-subterms term-algebra pair
                                                    (first equation))
                             :test #'equal)))

(defun apply-to-all-subterms (term-algebra pair term)
  "Returns the list of terms yield from substituting a subterm of TERM
EQUAL to (FIRST PAIR) by (SECOND PAIR)."
  (declare (type term-algebra term-algebra)
           (type list pair)
           (type term term))
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
  (declare (type list list))
  (declare (inline))
  (setf list (append (rest list) (list (first list)))))

(defun remove-all-weakly-dependent-equations (term-algebra equations
                                              recursion-depth)
  "Removes all equations in EQUATIONS which are WEAKLY-DEPENDENT-P in
  RECURSION-DEPTH steps."
  (declare (type term-algebra term-algebra)
           (type standard-set equations)
           (type integer recursion-depth))
  (let ((my-equations (copy-list (set-to-list equations))))
    (loop with substitutions = 0
          do
            (cond
              ((null my-equations) nil)
              ((weakly-dependent-p term-algebra
                                   (first my-equations)
                                   (rest my-equations)
                                   recursion-depth)
                 (setf my-equations (rest my-equations))
                 (setf substitutions 0))
              (t (setf my-equations (rotate-list my-equations))
                 (incf substitutions)))
            (when (> substitutions (length my-equations))
              (return my-equations)))))
