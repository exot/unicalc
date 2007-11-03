(in-package :fundamental-functions)

;; functions

(define-simple-condition function-error)
(define-simple-condition malformed-function-definition)

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
  (make-function source target (function-to-graph source function) :equal-pred equal-pred))

(defun function-to-graph (source function)
  "Converts FUNCTION on SOURCE to a function graph."
  (mapcar #'(lambda (x) (list x (funcall function x))) source))

(defmethod make-function (source target (relation relation) &key (equal-pred #'equal))
  (make-function source target (graph-of-relation relation) :equal-pred equal-pred))
