(in-package :functions-and-relations)

;; relations

(defclass relation ()
  ((source :accessor source-of-relation :initarg :source)
   (target :accessor target-of-relation :initarg :target)
   (graph  :accessor graph-of-relation  :initarg :graph )))

(defun relation-p (rel A B)
  "Returns non-NIL if REL is a relation on AxB."
  (and (subsetp (source-of-relation rel) A)
       (subsetp (target-of-relation rel) B)
       (valid-graph-p (graph-of-relation rel) A B)))

(defun valid-graph-p (graph A B)
  "Returns non-NIL if GRAPH is subset of AxB."
  (and (subsetp (mapcar #'first graph) A)
       (subsetp (mapcar #'second graph) B)))

(defun make-relation (source target graph)
  (cond
    ((valid-graph-p graph source target)
     (make-instance 'relation
                    :source source
                    :target target
                    :graph  graph))
    (t (error 'malformed-relation-definition 
              :text (format nil "~A is not a Graph on ~A times ~A"
                            graph source target)))))

(define-simple-condition malformed-relation-definition)

(defgeneric source (func-or-rel)
  (:documentation "Returns SOURCE of FUNC-OR-REL"))

(defgeneric target (func-or-rel)
  (:documentation "Returns TARGET of FUNC-OR-REL"))

(defgeneric graph (func-or-rel)
  (:documentation "Returns GRAPH of FUNC-OR-REL"))

(defmethod source ((func-or-rel relation))
  (source-of-relation func-or-rel))

(defmethod target ((func-or-rel relation))
  (target-of-relation func-or-rel))

(defmethod graph (func-or-rel)
  (graph-of-relation func-or-rel))

;; functions

(defclass algebraic-function (relation) ())

(defgeneric make-function (source target graph-or-function)
  (:documentation "Returns ALGEBRAIC-FUNCTION object describing GRAPH-OR-FUNCTION."))

(defmethod make-function (source target (graph list))
  (when (function-graph-p graph source target)
    (make-instance 'algebraic-function
                   :source source
                   :target target
                   :graph graph)))

(defun function-graph-p (graph A B)
  "Returns non-NIL if GRAPH describes a function from A to B."
  (and (valid-graph-p graph A B)
       (let ((graph-arguments (mapcar #'first (remove-duplicates graph :test #'equal))))
         (= (length graph-arguments)
            (length (remove-duplicates graph-arguments :test #'equal))))))

(defmethod make-function (source target (function function))
  (make-function source target (function-to-graph source function)))

(defun function-to-graph (source function)
  "Converts FUNCTION on SOURCE to a function graph."
  (mapcar #'(lambda (x) (list x (funcall function x))) source))

(defmethod make-function (source target (relation relation))
  (make-function source target (graph-of-relation relation)))

;; doing something with functions

(define-simple-condition wrong-argument)

(defun apply-function-to-element (function element)
  "Applies FUNCTION to ELEMENT."
  (or (second (assoc element (graph function)))
      (error 'wrong-argument 
             :text (format nil "Cannot apply ~A to ~A" function element))))

(defun apply-function-to-tuple (function tuple)
  "Applies FUNCTION to elements of SET, not neccessarily returning a set."
  (mapcar #'(lambda (element) 
              (apply-function-to-element function element))
          tuple))

(defun apply-function-to-set (function set)
  "Applies FUNCTION to elements of SET."
  (make-set (apply-function-to-tuple function set)))

(defun range (function)
  (remove-duplicates (mapcar #'second (graph function)) :test #'equal))

(defun surjective-p (function)
  (set-equal (target function)
             (range function)))

(defun injective-p (function)
  (equal (length (source function))
         (length (range function))))

(defun bijective-p (function)
  (and (injective-p function)
       (surjective-p function)))

(defun homomorphism-p (function algebra1 algebra2)
  "Returns non-NIL if FUNCTION is a homomorphism between ALGEBRA1 and ALGEBRA2."
  (when (and (algebras-of-same-signature-p algebra1 algebra2)
             (set-equal (source function) (base-set-of algebra1) :test #'equal)
             (subsetp (target function) (base-set-of algebra2) :test #'equal))
    (let ((signature (signature-of algebra1)))
      (every #'(lambda (operation) 
                 (compatible-with-operation-p function operation algebra1 algebra2))
             (function-symbols-of signature)))))

(defun compatible-with-operation-p (function operation algebra1 algebra2)
  "Returns non-NIL if FUNCTION is compatible with OPERATION on ALGEBRA1 and on ALGEBRA2."
  (let ((arity (arity-of-function-symbol (signature-of algebra1) operation)))
    (labels ((check-all-arguments (argument)
               (cond 
                 ((null argument) t)
                 ((not (equal (apply-function-to-element function (apply-operation-in-algebra operation argument algebra1))
                              (apply-operation-in-algebra operation (apply-function-to-tuple function argument) algebra2)))
                  nil)
                 (t (check-all-arguments (next-argument (base-set-of algebra1) argument))))))
      (check-all-arguments (symbols arity (first (base-set-of algebra1)))))))

(defun isomorphism-p (function algebra1 algebra2)
  "Returns non-NIL if FUNCTION is a isomorphism between ALGEBRA1 and ALGEBRA2."
  (and (bijective-p function)
       (homomorphism-p function algebra1 algebra2)))

(defun quasi-homomorphism-p (function algebra)
  "Returns non-NIL if FUNCTION is a quasi-homomorphism on ALGEBRA.

That is: f is named quasi-homomorph on algebra A iff for all operations op on 
  A, (a_1,...,a_2), (b_1,...,b_n) in A^n holds

   (f(a_1),...,f(a_n)) = (f(b_1),...,f(b_n)) => f(op(a_1,...,a_n)) = f(op(b_1,...,b_n))

"
  (let ((symbols (function-symbols-of (signature-of algebra))))
    (check-for-all-operations function symbols algebra)))

(defun check-for-all-operations (function symbols algebra)
  (cond
    ((null symbols) t)
    (t (let* ((arity (arity-of-function-symbol (signature-of algebra) (first symbols)))
              (start-pos (symbols arity (first (base-set-of algebra)))))
         (cond
           ((not (check-for-all-first-arguments start-pos function (first symbols) algebra))
            nil)
           (t (check-for-all-operations function (rest symbols) algebra)))))))

(defun check-for-all-first-arguments (first-argument function operation algebra)
  "Recursion over first argument."
  (cond
    ((null first-argument) t)
    ((not (check-for-all-second-arguments first-argument (symbols (length first-argument) (first (base-set-of algebra)))
                                          function operation algebra))
     nil)
    (t (check-for-all-first-arguments (next-argument (base-set-of algebra) first-argument) 
                                      function operation algebra))))

(defun check-for-all-second-arguments (first-argument second-argument function operation algebra)
  "Recursion over second argument."
  (cond
    ((null second-argument) t)
    ((and (equal (apply-function-to-tuple function first-argument) 
                 (apply-function-to-tuple function second-argument))
          (not (equal (apply-function-to-element function (apply-operation-in-algebra operation first-argument algebra))
                      (apply-function-to-element function (apply-operation-in-algebra operation second-argument algebra)))))
     nil)
    (t (check-for-all-second-arguments first-argument (next-argument (base-set-of algebra) second-argument)
                                       function operation algebra))))

(defun apply-quasihomomorphism-to-algebra (function algebra)
  "Applies the quasihomomorphism FUNCTION to ALGEBRA yielding the image algebra."
  (cond
     ((not (quasi-homomorphism-p function algebra))
      (error 'no-quasihomomorphism
             :text (format nil "~A is not a quasihomomorphis on ~A" function algebra)))
     (t (let ((new-base-set (apply-function-to-set function (base-set-of algebra)))
              (signature (signature-of algebra))
              (new-interpretations (apply-function-to-interpretations 
                                    function
                                    (interpretations-on algebra))))
          (make-algebra new-base-set signature new-interpretations)))))

(define-simple-condition no-quasihomomorphism)

(defun apply-function-to-interpretations (function interpretations)
  (mapcar #'(lambda (table) (apply-function-to-table function table))
          interpretations))

(defun apply-function-to-table (function table)  ;;; uses internal structure of table
  (let ((new-table ()))
    (iterate-over-value-table table element
      (push (list (apply-function-to-tuple function (all-operands element))
                  (apply-function-to-element function (value-of-element element)))
            new-table))
    (let ((new-table-set (make-set new-table)))
      (push (function-symbol-of table) new-table-set)
      new-table-set)))

(defun kernel (function)
  "Returns kernel of FUNCTION."
  (let ((base-set (source function)))
    (labels ((kernel-element (pair pairs)
               (cond
                 ((null pair) pairs)
                 (t (if (equal (apply-function-to-element function (first pair))
                               (apply-function-to-element function (second pair)))
                      (kernel-element (next-argument base-set pair) (cons pair pairs))
                      (kernel-element (next-argument base-set pair) pairs))))))
      (kernel-element (symbols 2 (first base-set)) ()))))

(define-simple-condition function-error)

(defun inverse-image (function set)
  "Returns the inverse image of SET under FUNCTION."
  (cond
    ((not (subsetp set (target function) :test #'equal))
     (error 'function-error
            :text (format nil "~A is not a subset of ~A" set function)))
    (t (mapcan #'(lambda (element) (inverse-image-of-element function element))
               set))))

(defun inverse-image-of-element (function element)
  (labels ((origin-of-element (argument all-origins)
             (let ((next-element (first (rest (member argument (source function))))))
               (cond
                 ((null argument) all-origins)
                 ((equal element
                         (apply-function-to-element function argument))
                  (origin-of-element next-element
                                     (make-set (cons argument all-origins))))
                 (t 
                  (origin-of-element next-element all-origins))))))
    (origin-of-element (first (source function)) ())))