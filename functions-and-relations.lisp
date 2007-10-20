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

(defun homomorphic-p (function algebra1 algebra2)
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