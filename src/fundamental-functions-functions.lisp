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

(defgeneric make-function (source target graph-or-function &key untested)
  (:documentation "Returns ALGEBRAIC-FUNCTION object describing
GRAPH-OR-FUNCTION."))

(defmethod make-function ((source list) target graph-or-function &key (untested nil))
  (make-function (make-set source) target graph-or-function :untested untested))

(defmethod make-function (source (target list) graph-or-function &key (untested nil))
  (make-function source (make-set target) graph-or-function :untested untested))

(defmethod make-function (source target (graph standard-set) &key (untested nil))
  (declare (type standard-set source target))
  (cond
    ((or untested
	 (function-graph-p graph source target))
     (make-instance 'algebraic-function
                    :source nil
                    :target target
                    :graph graph))
    (t (error 'malformed-function-definition
              :text (format nil "Given graph ~a is not a function graph with ~@
                                 source ~A and target ~A."
                            graph source target)))))

(defmethod make-function (source target (graph list) &key (untested nil))
  (make-function source target (make-set graph) :untested untested))

(defmethod source ((func-or-rel algebraic-function))
  (mapset #'first (graph func-or-rel)))

(defun function-graph-p (graph A B)
  "Returns non-NIL if GRAPH describes a function from A to B."
  (declare (type standard-set graph A B))
  (and (valid-graph-p graph A B)
       (let ((graph-arguments (mapset #'first graph)))
	 (and (= (card-s graph)
		 (card-s A))
	      (set-equal A graph-arguments)))))

(defmethod make-function (source target (function function) &key (untested nil))
  (declare (type standard-set source target))
  (make-function source target (function-to-graph source function)
		 :untested untested))

(defun function-to-graph (source function)
  "Converts FUNCTION on SOURCE to a function graph."
  (declare (type standard-set source)
	   (type function function))
  (mapset #'(lambda (x) (list x (funcall function x))) source))

(defmethod make-function (source target (relation relation) &key (untested nil))
  (declare (type standard-set source target))
  (make-function source target (graph-of-relation relation)
		 :untested untested))

(defun value-of-function (function argument)
  (declare (type algebraic-function function)
	   (type t argument))
  (let ((val (second (assoc-s argument (graph function)))))
    (cond
      ((null val) (error 'function-error :text
			(format nil "Argument ~A not valid for function ~A."
				argument function)))
      (t val))))

;;; iterating over function graphs (needed)

(defmacro iterate-over-function-graph (function element &body body)
  "Iterates with ELEMENT over all elements in (GRAPH FUNCTION)"
  `(iterate-over-relation-graph ,function ,element ,@body))