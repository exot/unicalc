(in-package :fundamental-functions)

;; functions

(define-simple-condition function-error)
(define-simple-condition malformed-function-definition)

(defclass algebraic-function (relation) ())

(defgeneric algebraic-function-p (func)
  (:documentation "Tests whether FUNC is a ALGEBRAIC-FUNCTION or not."))

(defmethod algebraic-function-p ((func algebraic-function))
  (declare (ignore func))
  t)

(defmethod algebraic-function-p ((func t))
  (declare (ignore func))
  nil)

(defgeneric make-function (source target graph-or-function &key equal-pred)
  (:documentation "Returns ALGEBRAIC-FUNCTION object describing
GRAPH-OR-FUNCTION."))

(defmethod make-function (source target (graph list) &key (equal-pred #'equal))
  (declare (type standard-set source target))
  (cond
    ((function-graph-p graph source target :equal-pred equal-pred)
     (make-instance 'algebraic-function
                    :source nil
                    :target target
                    :graph graph
                    :equal-pred equal-pred))
    (t (error 'malformed-function-definition
              :text (format nil "Given graph ~a is not a function graph with ~@
                                 source ~A and target ~A."
                            graph source target)))))

(defmethod source ((func-or-rel algebraic-function))
  (mapcar #'first (graph func-or-rel)))

(defun function-graph-p (graph A B &key (equal-pred #'equal))
  "Returns non-NIL if GRAPH describes a function from A to B."
  (declare (type standard-set graph A B))
  (and (valid-graph-p graph A B)
       (let ((graph-arguments (mapcar #'first
                                      (remove-duplicates graph
                                                         :test equal-pred))))
         (and (= (length graph-arguments)
                 (length (remove-duplicates graph-arguments :test equal-pred)))
              (set-equal A graph-arguments)))))

(defmethod make-function (source target (function function)
                          &key (equal-pred #'equal))
  (declare (type standard-set source target))
  (make-function source target (function-to-graph source function)
                 :equal-pred equal-pred))

(defun function-to-graph (source function)
  "Converts FUNCTION on SOURCE to a function graph."
  (declare (type standard-set source)
	   (type function function))
  (mapcar #'(lambda (x) (list x (funcall function x))) source))

(defmethod make-function (source target (relation relation)
			  &key (equal-pred #'equal))
  (declare (type standard-set source target))
  (make-function source target (graph-of-relation relation)
		 :equal-pred equal-pred))

(defun value-of-function (function argument)
  (declare (type algebraic-function function)
	   (type t argument))
  (second (assoc argument (graph function) :test (equal-pred function))))

;;; iterating over function graphs (needed)

(defmacro iterate-over-function-graph (function element &body body)
  "Iterates with ELEMENT over all elements in (GRAPH FUNCTION)"
  `(iterate-over-relation-graph ,function ,element ,@body))