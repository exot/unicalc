(in-package :fundamental-functions)

(deftype table ()
  "TABLE is defined as a pair holding the operation symbol and the value table
of the operation in the specific algebra."
  'list)

(declaim (inline function-symbol-of
		 implementing-function-of
		 arity-of-function))

(defun function-symbol-of (table)
  "Returns the function symbol of TABLE being an entry in the set
  of interpretations of a given algebra."
  (declare (type table table))
  (first table))

(defun implementing-function-of (table)
  "Returns the implementing function of the function symbol of table
  in a given interpretation of a given algebra."
  (declare (type table table))
  (second table))

(defun arity-of-function (func)
  "Returns 'dimension' of source of FUNC."
  (declare (type algebraic-function func))
  (let ((first (first-s (source func))))
    (cond
      ((tuple-p first) (length first))
      (t 1))))

;;;; interpretations on algebras implemented as value tables

(defmacro define-operation (name arguments &body body)
  "Defines function which can be used to generate interpretations
instead of value tables."
  `(progn
     (setf (get ',name :is-interpretation-function) t)
     (setf (get ',name :arity) (length ',arguments))

     (defun ,name ,arguments
       ,@body)))

(defun interpretation-function-p (func)
  (and (symbolp func)
       (get func :is-interpretation-function)))

(defun get-arity-of-interpretation-function (func)
  (when (interpretation-function-p func)
    (get func :arity)))

(defun interpretation-function-to-graph (base-set interpretation)
  (declare (type standard-set base-set))
  (function-to-graph (tuples base-set
			     (get-arity-of-interpretation-function
			       interpretation))
                     #'(lambda (x) (apply interpretation x))))

(define-simple-condition malformed-table)

(defun normalize-table-representation (base-set tables)
  "Normalizes TABLEs (aka pairs of function symbols and implementing
algebraic functions)."
  (declare (type standard-set base-set))
  (mapset #'(lambda (table)
	      (let ((function-symbol (first table)))
		(list function-symbol
		      (normalize-interpretation (rest table) base-set))))
	  tables))

(defun normalize-interpretation (interpretation base-set)
  (declare (type standard-set base-set))
  (cond
    ((not (listp (first interpretation)))
     (let ((func (first interpretation)))
       (cond
	 ((interpretation-function-p func)
	  (normalize-from-interpretation-function func base-set))
	 ((algebraic-function-p func)
	  func)
	 (t (error 'malformed-table :text
		   (format nil "Non operation function ~A used as ~@
                               operation description"
			   func))))))
    (t (normalize-from-value-table interpretation base-set))))

(defun normalize-from-interpretation-function (ifunc base-set)
  (declare (type standard-set base-set)
	   (type (satisfies interpretation-function-p) ifunc))
  (let ((arity (get-arity-of-interpretation-function ifunc))
	(graph (interpretation-function-to-graph base-set ifunc)))
    (make-function (tuples base-set arity)
		   base-set
		   graph)))

(defun normalize-from-value-table (table base-set)
  (declare (type standard-set base-set)
	   (type list table))
  (make-function (tuples base-set (length (first (first table))))
		 base-set
		 table))
