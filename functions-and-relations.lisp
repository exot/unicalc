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
       (let* ((graph-arguments (mapcar #'first (remove-duplicates graph :test #'equal))))
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

(defun apply-to-element (function element)
  "Applies FUNCTION to ELEMENT."
  (second (assoc element (graph function))))