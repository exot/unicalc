(in-package :fundamental-functions)

(define-simple-condition cayley-table-error)

(defclass cayley-table ()
  ((source :type standard-set :accessor source :initarg :source)
   (target :type standard-set :accessor target :initarg :target)
   (table  :type array        :accessor table  :initarg :table)
   (converter :type function  :accessor converter :initarg :converter)))

(defmacro iterate-over-cayley-table (element table &body body)
  (with-gensyms (dimens arg)
  `(let ((dimens (array-dimensions ,table)))
    (loop for arg = (numbers (length dimens) 0) then (next-arg dimens arg)
          while arg
          do (let ((,element (list arg (apply #'aref ,table arg))))
	       ,@body)))))

(defun next-function (table)
  (declare (type cayley-table table))
  (let* ((dimens (array-dimensions (table table)))
	 (arg    (numbers (length dimens) 0)))
    #'(lambda ()
	(cond
	  ((null arg) arg)
	  (t (let ((result arg))
	       (setf arg (next-arg dimens arg))
	       (apply #'aref (table table) result)))))))

(defun next-arg (mask arg)
  (cond
    ((null arg) arg)
    (t (let ((first (first arg)))
	 (cond
	   ((< first (1- (first mask)))
	    (cons (1+ first) (rest arg)))
	   (t (let ((next (next-arg (rest mask) (rest arg))))
		(when next
		  (cons 0 next)))))))))

(defun make-lazy-table (table)
  (declare (type cayley-table table))
  (define-lazy-set (next-function table)))

(defmacro forall-in-cayley-table ((element table) &body body)
  (with-gensyms (lazy-table)
    `(let ((lazy-table (make-lazy-table ,table)))
      (forall (,element lazy-table) ,@body))))

(defmacro exists-in-cayley-table ((element table) &body body)
  (with-gensyms (lazy-table)
    `(let ((lazy-table (make-lazy-table ,table)))
      (exists (,element lazy-table) ,@body))))

(defun symbolized-coordinates (source arguments)
  (declare (type standard-set source)
	   (type list arguments))
  (mapcar #'(lambda (x) (elt-at-position-s x source))
	  arguments))

(defmethod print-object ((obj cayley-table) stream)
  (print-unreadable-object (obj stream :type t)
    (print-table-entries stream obj)))

(defun print-table-entries (stream table)
  (declare (type stream stream)
	   (type cayley-table table))
  (iterate-over-cayley-table elt (table table)
    (format stream "~&~2:T~A"
	    (list (symbolized-coordinates (source table)
					  (all-operands elt))
		  (value-of-element elt)))))

(defgeneric make-cayley-table (source target table)
  (declare (type standard-set source target)
	   (type (or standard-set array) table))
  (:documentation "Returns Cayley table from SOURCE, TARGET and TABLE."))

(defmethod make-cayley-table (source target (table array))
  (let ((dimens (array-dimensions table))
	(number-of-elements (card-s source)))
    (cond
      ((not (every #'(lambda (dimension) (= dimension number-of-elements))
		   dimens))
       (error 'cayley-table-error :text
	      (format nil "Given table ~A doesn't have the right dimensions"
		      table)))
      (t (let ((cayley-table (make-instance 'cayley-table
					    :source source
					    :target target
					    :table table)))
	   (cond
	     ((not (forall-in-cayley-table (elt cayley-table)
		     (set-member-s elt target)))
	      (error 'cayley-table-error :text
		     (format nil "Given table ~A contains elements not in target ~A"
			     table target)))
	     (t cayley-table)))))))

(defun arity-of-table (table)
  (declare (type cayley-table table))
  (let ((dimens (array-dimensions (table table))))
    (cond
      ((equal dimens '(0)) 0)
      (t (length dimens)))))

(defun value-from-argument (table &rest arguments)
  (declare (type cayley-table table))
  "Returns value stored in cayley-table TABLE at position given by ARGUMENTS."
  (cond
    ((not (forall (x arguments) (set-member-s x (source table))))
     (error 'cayley-table-error :text
	    (format nil "Given arguments ~A are not in source ~A."
		    arguments (source table))))
    ((not (= (arity-of-table table) (length arguments)))
     (error 'cayley-table-error :text
	    (format nil "Wrong number ~A of arguments given."
		    arguments)))
    (t (apply #'aref (table table) (numerical-coordinates (source table) arguments)))))

(defun numerical-coordinates (source arguments)
  (declare (type standard-set source)
	   (type list arguments))
  (print arguments)
  (print source)
  (print (mapcar #'(lambda (arg) (position-s arg source))
	  arguments)))

(defun function-graph-to-table (source target graph)
  (declare (type standard-set source target graph))
  (let ((first-pair (first-s graph)))
  (cond
    ((not first-pair) #())
    ((not (or (forall ((x y) graph)
		(not (tuple-p x)))
	      (forall ((x y) (rest-s graph))
		(= (length (first first-pair))
		   (length x)))))
     (error 'cayley-table-error :text
	    (format nil "Given graph ~A is not a valid function graph."
		    graph)))
    (t (let* ((dimens (if (tuple-p (first first-pair))
			  (numbers (length (first first-pair))
				   (card-s source))
			  (list (card-s source))))
	      (table (make-array dimens :initial-element nil)))
	 (loop-over-set (x y) graph
	   (let ((argument (numerical-coordinates source (list x)))
		 (value y))
	     (cond
	       ((not (or (set-member-s x source)
			 (set-member-s y target)))
		(error 'cayley-table-error :text
		       (format nil "Pair (~A,~A) is not in ~A x ~A."
			       x y source target)))
	       ((not (null (apply #'aref table argument)))
		(error 'cayley-table-error :text
		       (format nil "Argument ~A is given twice."
			       x)))
	       (t (setf (apply #'aref table argument)
			value)))))
	 (make-cayley-table source target table))))))

(defmethod make-cayley-table (source target (table standard-set))
  (function-graph-to-table source target table))

(defmethod make-cayley-table (source target (table cayley-table))
  (make-instance 'cayley-table :source source
		               :target target
			       :contents (table table)))