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
  (and (termp term-algebra x) (listp x)))

; (defun free-variables-of-term (...) ... )

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
                                        (apply-matching term-algebra matching term))
                                   (rest term))))))

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
