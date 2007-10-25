(in-package :fundamental-functions)

;; relations

(defclass relation ()
  ((source :accessor source-of-relation :initarg :source)
   (target :accessor target-of-relation :initarg :target)
   (graph  :accessor graph-of-relation  :initarg :graph )))

(defmethod print-object ((obj relation) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "{~&~{ ~S~^,~&~I~}}" (graph obj))))

(defun relation-p (rel A B)
  "Returns non-NIL if REL is a relation on AxB."
  (and (subsetp (source-of-relation rel) A)
       (subsetp (target-of-relation rel) B)
       (valid-graph-p (graph-of-relation rel) A B)))

(defun valid-graph-p (graph A B)
  "Returns non-NIL if GRAPH is subset of AxB."
  (and (subsetp (mapcar #'first graph) A :test #'set-equal)
       (subsetp (mapcar #'second graph) B :test #'set-equal)))

(defun make-relation (source target graph)
  "Creates RELATION out of SOURCE, TARGET and GRAPH."
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

(defgeneric algebraic-function-p (func)
  (:documentation "Tests whether FUNC is a ALGEBRAIC-FUNCTION or not."))

(defmethod algebraic-function-p ((func algebraic-function))
  (declare (ignore func))
  t)

(defmethod algebraic-function-p ((func t))
  (declare (ignore func))
  nil)

(defgeneric make-function (source target graph-or-function)
  (:documentation "Returns ALGEBRAIC-FUNCTION object describing GRAPH-OR-FUNCTION."))

(define-simple-condition malformed-function-definition)

(defmethod make-function (source target (graph list))
  (cond
    ((function-graph-p graph source target)
     (make-instance 'algebraic-function
                    :source source
                    :target target
                    :graph graph))
    (t (error 'malformed-function-definition :text "Given graph is not a function graph."))))

(defun function-graph-p (graph A B)
  "Returns non-NIL if GRAPH describes a function from A to B."
  (and (valid-graph-p graph A B)
       (let ((graph-arguments (mapcar #'first (remove-duplicates graph :test #'equal))))
         (and (= (length graph-arguments)
                 (length (remove-duplicates graph-arguments :test #'equal)))
              (set-equal A graph-arguments)))))

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
  (or (second (assoc element (graph function) :test #'set-equal))
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

;;; iterating over function graphs (needed)

(defmacro iterate-over-function-graph (function element &body body)
  "Iterates with ELEMENT over all elements in (GRAPH FUNCTION)"
  (let ((graph (gensym "GRAPH"))
        (pair (gensym "PAIR")))
    `(let ((,graph (graph ,function)))
      (loop for ,pair in ,graph
            do (let ((,element ,pair))
                 ,@body)))))

(defun value-of-element (element)
  "Returns value of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (second element))

(defun all-operands (element)
  "Returns all operands of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (first element))

(defun nth-operand (element n)
  "Returns nth operand of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (nth n (all-operands element)))

;;; tables (pairs of function symbol and implementing function; use in universal-algebra)

(defun function-symbol-of (table)
  "Returns the function symbol of TABLE being an entry in the set
  of interpretations of a given algebra."
  (first table))

(defun implementing-function-of (table)
  "Returns the implementing function of the function symbol of table
  in a given interpretation of a given algebra."
  (second table))

(defun get-arity-of-table (table)
  "DONT USE."
  (length (first (first (rest table)))))

(defun arity-of-function (func)
  "Returns 'dimension' of source of FUNC."
  (length (first (source func))))
