(in-package :terms)

(defclass term-algebra ()
  ((variables :accessor variables-of :initarg :variables)
   (signature :accessor signature-of :initarg :signature)))

(defun make-term-algebra (variables signature)
  (make-instance 'term-algebra
                 :variables variables
                 :signature signature))

(defun make-term-algebra-from-scratch (variables function-symbols rank-alphabet)
  (make-term-algebra variables (make-signature function-symbols rank-alphabet)))

(defun variablep (term-algebra x)
  "Returns non-NIL if X designates a variable in TERM-ALGEBRA."
  (member x (variables-of term-algebra)))

(defun function-symbol-p (term-algebra f)
  "Returns non-NIL if F designates a function symbol in TERM-ALGEBRA."
  (member f (function-symbols-of (signature-of term-algebra))))

(defmethod arity-of-function-symbol ((source term-algebra) function-symbol)
  (and (function-symbol-p source function-symbol)
       (arity-of-function-symbol (signature-of source) function-symbol)))

;;; terms

(defun termp (term-algebra x)
  "Tests, whether X \in T_{*signature*}(*variables*)"
   (or (variablep term-algebra x)
       (and (listp x)
            (function-symbol-p term-algebra (first x))
            (equal (length (rest x)) (arity-of-function-symbol term-algebra (first x)))
            (every #'(lambda (x) (termp term-algebra x)) (rest x)))))

(defun composed-term-p (term-algebra x)
  (listp x))

(defun operation-symbol-of (term-algebra x)
  (when (composed-term-p term-algebra x)
    (first x)))

(defun first-match (term-algebra term1 term2)
  "Returns first candidate for matching between TERM1 and TERM2 in TERM-ALGEBRA"
  (cond
    ((variablep term-algebra term1) (list (list term1 term2)))
    (t (cond
         ((or (variablep term-algebra term2)
              (not (equal (first term1) 
                          (first term2)))) 
           nil)
         (t (reduce #'(lambda (x y)
                        (if (or (not x) (not y)) ; if first-match returns nil
                            nil                  ; then no matching is possible
                            (nunion x y :test #'equal)))
                    (mapcar #'(lambda (t1 t2) 
                                (first-match term-algebra t1 t2)) 
                            (rest term1) (rest term2))))))))

(defun valid-match-p (match)
  "Returns T if match describes a function"
  (equal (length match)
         ; check whether variables have been matched twice with different values
         (length (remove-duplicates (mapcar #'first match) :test #'equal))))

(defun match (term-algebra term1 term2)
  "Checks whether TERM1 can be matched on TERM2 in TERM-ALGEBRA returning a matching from TERM1 to TERM2 or NIL
if not."
  (if (not (and (termp term-algebra term1) 
                (termp term-algebra term2)))
    nil
    (let ((try (first-match term-algebra term1 term2)))
      (if (valid-match-p try)
        try
        nil))))

(let ((matching-subterms ()))
  (defun find-matching-subterm (term-algebra term1 term2) 
    "returns subterm of TERM2 if TERM1 in TERM-ALGEBRA can be matched on it, NIL otherwise."
    (when (match term-algebra term1 term2) 
       (push (list term2 (match term-algebra term1 term2)) matching-subterms))
    (cond
       ((variablep term-algebra term2) nil) ; no subterms left
       (t (dolist (subterm (rest term2)) ; check whether one subterms may match
             (find-matching-subterm term-algebra term1 subterm)))))
  
  (defun matches-subterm (term-algebra term1 term2)
    "Returns subterm of TERM2 and matching, if TERM1 can be matched
on this subterm. Otherwise NIL is returned."
    (when (and (termp term-algebra term1)
               (termp term-algebra term2))
      (setf matching-subterms ()) ;;; SETF
      (find-matching-subterm term-algebra term1 term2)
      (remove-duplicates matching-subterms :test #'equal))))

(defun apply-matching (term-algebra matching term)
  "Applies MATCHING to TERM in TERM-ALGEBRA"
  (cond
     ((variablep term-algebra term) (or (second (assoc term matching)) term))
     (t (cons (first term) (mapcar #'(lambda (term)
                                        (apply-matching term-algebra matching term)) (rest term))))))

;;; cosmetics - printing terms in human readable forms

(defun pprint-term (term-algebra term1)
  "Prints TERM1 in TERM-ALGEBRA in infix notation"
  (when (termp term-algebra term1)
    (cond
      ((variablep term-algebra term1) (format t "~a " term1))
      ;; for our unary symbols ... grrr ...
      ((= (arity-of-function-symbol term-algebra (operation-symbol-of term-algebra term1)) 1)
       (format t "( ")
       (format t "~a " (operation-symbol-of term-algebra term1))
       (mapc #'(lambda (term)
                 (pprint-term term-algebra term))
             (rest term1))
       (format t ") "))
      (t (format t "( ")
         (pprint-term term-algebra (second term1))
         (format t "~a " (first term1))
         (mapc #'(lambda (term)
                   (pprint-term term-algebra term))
               (rest (rest term1)))
         (format t ") ")))))

(defun pprint-term-list (term-algebra list-of-terms)
  "Prints LIST-OF-TERMS in TERM-ALGEBRA in infix notation with '=' between them."
  (when (and (listp list-of-terms)
             (every #'(lambda (term) (termp term-algebra term)) list-of-terms))
    (labels ((recursive-pprint (list)
               (case (length list)
                 (1 (pprint-term term-algebra (first list))
                    (values))
                 (otherwise 
                   (pprint-term term-algebra (first list))
                   (format t "= ")
                   (recursive-pprint (rest list))))))
      (format t "~&")
      (recursive-pprint list-of-terms)
      (format t "~%"))))

;; ;;; calculate generating elements

;; ; TODO: deal with more than one table (just make the substitute* and 
;; ;   find-next-number functions run over all algebras given)

;; (defun calculate-generating-elements (algebra)
;;   "Returns list of generating elements of ALGEBRA of minimal length."
;;   (loop for number-of-elements from 1 
;;         do 
;;           (let ((elements (n-elements-generate-algebra algebra 
;;                                                        number-of-elements)))
;;             (when elements
;;               (return elements)))))

;; (defun n-elemental-subsets (set n)
;;   "Returns set of all N elemental subsets of SET."
;;   (cond
;;     ((= n 0) (list ()))
;;     ((null set) nil)
;;     (t (let ((subsets ()))
;;          (loop for element in set
;;                do (let ((shorter-subsets (n-elemental-subsets 
;;                                            (remove element set) (1- n))))
;;                     (mapc #'(lambda (x) (push (cons element x) subsets))
;;                           shorter-subsets)))
;;          subsets))))

;; (defun n-elements-generate-algebra (algebra number-of-elements)
;;   "Returns list of NUMBER-OF-ELEMENTS if NUMBER-OF-ELEMENTS generate
;; the value-tables in the list ALGEBRA"
;;   (let ((subsets (n-elemental-subsets (elements-of-algebra algebra) number-of-elements)))
;;     (loop for subset in subsets
;;           when (elements-generate-algebra subset algebra)
;;           do (return subset))))

;; (defun elements-of-algebra (algebras)
;;   "Returns list of elements in ALGEBRAS"
;;   (let ((algebra (first algebras)))
;;     (loop for i from 1 to (1- (array-dimension algebra 1))
;;           collect (aref algebra i 0))))

;; (defun elements-generate-algebra (elements algebras)
;;   "Returns non-NIL if ELEMENTS generate ALGEBRAS given 
;; by a list of value-tabels."
;;   (let ((reachable-elements elements))
;;     (loop 
;;       (let ((new-element (get-next-reachable-element algebras reachable-elements)))
;;         (if new-element
;;             (push new-element reachable-elements)
;;             (return))))
;;     (if (equal (length reachable-elements)
;;                (length (elements-of-algebra algebras)))
;;         t
;;         nil)))

;; (defun get-next-reachable-element (algebras reachable-elements)
;;   "Returns new element in ALGEBRAS which is reachable by elements
;; from REACHABLE-ELEMENTS"
;;   (loop for algebra in algebras
;;         do (iterate-over-array algebra i j
;;              ; TODO: make this abstract, invent iterate-over-value-table
;;              (when (and (>= i 1) (>= j 1) 
;;                         (not (member (aref algebra i j) reachable-elements))
;;                         (member (aref algebra i 0) reachable-elements)
;;                         (member (aref algebra 0 j) reachable-elements))
;;                (return-from get-next-reachable-element (aref algebra i j))))))
  
;; ;;; substitue numbers (non-terms) in algebra with corresponding terms

;; (defun substitute-generators-by-variables (algebra generators)
;;   "Substitues in ALGEBRA all numbers being in GENERATORS with variable symbols."
;;   (iterate-over-array algebra i j
;;     (when (member (aref algebra i j) generators)
;;       (setf (aref algebra i j) (nth (aref algebra i j) *variables*)))))

;; (defun find-next-number (algebra)
;;   "Finds next position in ALGEBRA containing a number."
;;   (iterate-over-array algebra i j
;;     (when (and (>= i 1) (>= j 1)
;;                (not (termp (aref algebra i j)))
;;                (termp (aref algebra i 0))
;;                (termp (aref algebra 0 j)))
;;       (return-from find-next-number (list i j))))
;;   nil)

;; (defun substitute-number-by-term (algebra number term)
;;   "Substitues all occurences of NUMBER in ALGEBRA with TERM."
;;   (iterate-over-array algebra i j
;;     (when (equal (aref algebra i j) number)
;;       (setf (aref algebra i j) term))))

;; (defun label-numbers-in-algebra (algebra generators)
;;   "Substitutes all numbers in the free algebra ALGEBRA into terms in terms of GENERATORS.
;; GENERATORS should contain all numbers of ALGEBRA generating it."
;;   (substitute-generators-by-variables algebra generators)
;;   (do ((next-number (find-next-number algebra) (find-next-number algebra)))
;;       ((not next-number) algebra)
;;     (substitute-number-by-term algebra 
;;       (aref algebra (first next-number) (second next-number))
;;       (list
;;         (get-operation-symbol algebra)
;;         (aref algebra (first next-number) 0)
;;         (aref algebra 0 (second next-number))))))

;; (defun extract-all-equations (labled-algebra)
;;   "Returns all equations given by LABELED-ALGEBRA"
;;   (let ((found-equations ()))
;;     (iterate-over-array labled-algebra i j
;;       (when (and (>= i 1) (>= j 1))
;;         (push (list (list (get-operation-symbol labled-algebra)
;;                           (aref labled-algebra i 0)
;;                           (aref labled-algebra 0 j))
;;                     (aref labled-algebra i j))
;;               found-equations)))
;;     found-equations))

;; ; only for testing purpose

;; (defun print-all-equations-nonreduced (algebra generating-elements)
;;   "Prints all equations represented by the free algebra ALGEBRA."
;;   (mapc #'pprint-term-pair 
;;         (extract-all-equations (label-numbers-in-algebra algebra 
;;                                                          generating-elements)))
;;   (values))

;; ;;; eliminating dependent equations

;; (defun toggle-equation (eqn)
;;   "If (EQUAL (EQN '(X Y))), then (EQUAL (TOGGLE-EQUATION EQN) '(Y X))."
;;   (declare (inline))
;;   (list (second eqn) (first eqn)))

;; (defun equation-match-p (eqn1 eqn2)
;;   "Returns T if EQN1 can be matched on EQN2 or on (TOGGLE-EQUATION EQN2)"
;;   (let ((left-match (match (first eqn1) (first eqn2))))
;;     (when left-match
;;       (let ((right-match (match (second eqn1) (second eqn2))))
;;         (when (and right-match
;;                    (subsetp left-match right-match :test #'equal)
;;                    (subsetp right-match left-match :test #'equal))
;;           t)))))

;; (defun implies-equation-p (eqn1 eqn2)
;;   "Returns non-NIL if EQN1 implies EQN2 (i.e. EQN1 matches EQN2)"
;;   (or (equation-match-p eqn1 eqn2)
;;       (equation-match-p eqn1 (toggle-equation eqn2))))

;; (defun weakly-dependent-p (equation set-of-equations recursion-depth)
;;   "Returns non-NIL if EQUATION can be deduced from SET-OF-EQUATIONS in not more than
;; RECURSION-DEPTH steps"
;;   (cond 
;;     ((<= recursion-depth 0)
;;        (some #'(lambda (x) (implies-equation-p x equation)) set-of-equations))
;;     (t (dolist (eqn (all-possible-transformation equation set-of-equations))
;;          (when (weakly-dependent-p eqn set-of-equations (1- recursion-depth))
;;            (return-from weakly-dependent-p t))))))

;; (defun all-possible-transformation (equation set-of-equations)
;;   "Returns all possible transformations of EQUATION under use of
;; equations in SET-OF-EQUATIONS (one step only)."
;;   (let ((appliable-equations (find-all-appliable-equations equation set-of-equations))
;;         (all-transformation ()))
;;     (loop for transformation in appliable-equations
;;           do (setf all-transformation
;;                    (nconc all-transformation 
;;                           (apply-transformation-to-all-matching-subterms transformation
;;                                                                          equation)))
;;           finally (return all-transformation))))
             
;; (defun find-all-appliable-equations (equation set-of-equations)
;;   "Returns a pair of terms (TERM1 TERM2) s.t. TERM1 is subterm of (FIRST EQUATION)
;; and there is a equation eqn in SET-OF-EQUATIONS that implies (TERM1 TERM2)."
;;   (let ((appliable-functions ()))
;;     (flet ((apply-equation (eqn equation)
;;              (let ((matches (matches-subterm (first eqn) (first equation))))
;;                (dolist (match matches)
;;                  (push (list (first match) ; this is the subterm in equation
;;                              ; which shall be substitute by the right side of
;;                              ; eqn (with actual matching applied on it)
;;                              (apply-matching (second match) (second eqn)))
;;                        appliable-functions)))))
;;       (loop for eqn in set-of-equations
;;             do
;;               (apply-equation eqn equation)
;;               (apply-equation (toggle-equation eqn) equation)
;;             finally (return appliable-functions)))))

;; (defun apply-transformation-to-all-matching-subterms (pair equation)
;;   "Returns a list of equations s.t. every occurence of (FIRST PAIR) in
;; (FIRST EQUATION) is substituted by (SECOND PAIR)."
;;   (mapcar #'(lambda (x) (list x (second equation)))
;;           (remove-duplicates (apply-to-all-subterms pair (first equation)) :test #'equal)))
;; (defun apply-to-all-subterms (pair term)
;;   "Returns the list of terms yield from substituting a subterm of TERM
;; EQUAL to (FIRST PAIR) by (SECOND PAIR)."
;;   (cond
;;     ((equal (first pair) term)
;;       (list (second pair)))
;;     ((variablep term)
;;       nil) ; no subterms left
;;     (t (let ((result ()))
;;          (loop for i from 1 to (length term)
;;                do
;;                  (let ((transformed-term (apply-to-all-subterms pair (nth i term))))
;;                    (dolist (myterm transformed-term)
;;                      (push (append (subseq term 0 i)
;;                                    (list myterm)
;;                                    (subseq term (1+ i)))
;;                            result)))
;;                finally (return result))))))

;; (defun rotate-list (list)
;;   "Returns left-rotate of LIST."
;;   (setf list (append (rest list) (list (first list)))))

;; (defun remove-all-weakly-dependent-equations (equations recursion-depth)
;;   "Removes all equations in EQUATIONS which are WEAKLY-DEPENDENT-P in RECURSION-DEPTH
;; steps."
;;   (let ((my-equations equations))
;;     (loop with substitutions = 0
;;           do
;;             (cond
;;               ((null my-equations) nil)
;;               ((weakly-dependent-p (first my-equations) (rest my-equations) 
;;                                    recursion-depth)
;;                  (setf my-equations (rest my-equations))
;;                  (setf substitutions 0))
;;               (t (setf my-equations (rotate-list my-equations))
;;                  (incf substitutions)))
;;             (when (> substitutions (length my-equations)) 
;;               (return my-equations)))))

;; (defun show-all-equations (algebra generating-elements n)
;;   "Shows all equations represented by the free algebra ALGEBRA 
;; generated by GENERATING-ELEMENTS being weakly-independent
;; of level n."
;;   (mapc #'pprint-term-pair
;;         (remove-all-weakly-dependent-equations 
;;           (extract-all-equations 
;;             (label-numbers-in-algebra algebra generating-elements)) 
;;           n))
;;   (values))

;; ;;; test cases

;; (set-variable-set '(v0 v1 v2 v3 v4 v5))
;; (set-signature '((+ 2)))

;; ; define interpretation of an operation in *signature*
;; ; with a values table
;; (defparameter *free-algebra*
;;   #2A((+ 0 1 2 3 4)
;;       (0 0 2 3 3 2)
;;       (1 2 1 4 2 4)
;;       (2 3 4 2 3 4)
;;       (3 3 2 3 3 2)
;;       (4 2 4 4 2 4)))

;; (defun test-case-1 ()
;;   (show-all-equations *free-algebra* 
;;                       (calculate-generating-elements (list *free-algebra*))
;;                       2))
