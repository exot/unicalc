(in-package :fundamental-functions)

;; relations

(defclass relation ()
  ((source :accessor source-of-relation :initarg :source)
   (target :accessor target-of-relation :initarg :target)
   (graph  :accessor graph-of-relation  :initarg :graph )))

(defmethod print-object ((obj relation) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "{~&~{~2:T~S~^,~&~I~}}" (graph obj))))

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

(defclass algebraic-function (relation)
  ((equal-pred :accessor equal-pred :initarg :equal-pred :initform #'equal)))

(defgeneric algebraic-function-p (func)
  (:documentation "Tests whether FUNC is a ALGEBRAIC-FUNCTION or not."))

(defmethod algebraic-function-p ((func algebraic-function))
  (declare (ignore func))
  t)

(defmethod algebraic-function-p ((func t))
  (declare (ignore func))
  nil)

(defgeneric make-function (source target graph-or-function &key equal-pred)
  (:documentation "Returns ALGEBRAIC-FUNCTION object describing GRAPH-OR-FUNCTION."))

(define-simple-condition malformed-function-definition)

(defmethod make-function (source target (graph list) &key (equal-pred #'equal))
  (cond
    ((function-graph-p graph source target)
     (make-instance 'algebraic-function
                    :source nil
                    :target target
                    :graph graph
                    :equal-pred equal-pred))
    (t (error 'malformed-function-definition
              :text (format nil "Given graph ~a is not a function graph." graph)))))

(defmethod source ((func-or-rel algebraic-function))
  (mapcar #'first (graph func-or-rel)))

(defun function-graph-p (graph A B)
  "Returns non-NIL if GRAPH describes a function from A to B."
  (and (valid-graph-p graph A B)
       (let ((graph-arguments (mapcar #'first (remove-duplicates graph :test #'equal))))
         (and (= (length graph-arguments)
                 (length (remove-duplicates graph-arguments :test #'equal)))
              (set-equal A graph-arguments)))))

(defmethod make-function (source target (function function) &key (equal-pred #'equal))
  (make-function source target (function-to-graph source function :equal-pred equal-pred)))

(defun function-to-graph (source function)
  "Converts FUNCTION on SOURCE to a function graph."
  (mapcar #'(lambda (x) (list x (funcall function x))) source))

(defmethod make-function (source target (relation relation) &key (equal-pred #'equal))
  (make-function source target (graph-of-relation relation) :equal-pred equal-pred))

;; doing something with functions

(define-simple-condition function-error)

(defun apply-function-to-element (function element)
  "Applies FUNCTION to ELEMENT."
  (or (second (assoc element (graph function) :test (equal-pred function)))
      (error 'function-error
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
  (remove-duplicates (mapcar #'second (graph function)) :test (equal-pred function)))

(defun surjective-p (function)
  (set-equal (target function)
             (range function)))

(defun injective-p (function)
  (= (length (source function))
     (length (range function))))

(defun bijective-p (function)
  (and (injective-p function)
       (surjective-p function)))

(defun kernel (function)
  "Returns kernel of FUNCTION."
  (let ((base-set (source function)))
    (labels ((kernel-element (pair pairs)
               (cond
                 ((null pair) pairs)
                 (t (if (set-equal (apply-function-to-element function (first pair)) ;;; !!!
                                   (apply-function-to-element function (second pair)))
                      (kernel-element (next-argument base-set pair) (cons pair pairs))
                      (kernel-element (next-argument base-set pair) pairs))))))
      (kernel-element (symbols 2 (first base-set)) ()))))

(defun inverse-image (function set)
  "Returns the inverse image of SET under FUNCTION."
  (cond
    ((not (subsetp set (target function) :test (equal-pred function)))
     (error 'function-error
            :text (format nil "~A is not a subset of ~A" set function)))
    (t (mapcan #'(lambda (element) (inverse-image-of-element function element))
               set))))
  ;; (make-set (loop for i in (source function)
  ;;                 when (member (apply-function-to-element function i) set)
  ;;                 collect i))

(defun inverse-image-of-element (function element)
  (labels ((origin-of-element (argument all-origins)
             (let ((next-element (first (rest (member argument (source function) :test (equal-pred function))))))
               (cond
                 ((null argument) all-origins)
                 ((set-equal element
                             (apply-function-to-element function argument))
                  (origin-of-element next-element
                                     (make-set (cons argument all-origins))))
                 (t
                  (origin-of-element next-element all-origins))))))
    (origin-of-element (first (source function)) ())))
  ;; (inverse-image function {element})
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

;;; more on functions

(defun restrict-function-on-source-and-traget (function new-source new-target)
  "Restricts FUNCTION being a function on NEW-SOURCE\times NEW-TARGET."
  (flet ((calc-new-func-graph ()
            (let ((new-graph ()))
              (iterate-over-function-graph function element
                (when (member (all-operands element) new-source :test (equal-pred function))
                  (push element new-graph)))
             new-graph)))
    (let ((new-graph (calc-new-func-graph)))
      (make-function new-source
                     new-target
                     new-graph))))

(defun restrict-function-on-target (function new-target)
  "Returns algebraic function begin FUNCTION with target restricted to NEW-TARGET"
  (make-function (source function)
                 new-target
                 (graph function)))

(defun restrict-function-on-source (function new-source)
  "Returns algebraic functions as FUNCTION restricted to NEW-SOURCE."
  (restrict-function-on-source-and-traget function new-source (target function)))
