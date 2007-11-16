(in-package :fundamental-functions)

(define-simple-condition cayley-table-error)

(defclass cayley-table ()
  ((equal-pred :accessor equal-pred :initarg :equal-pred)
   (source     :accessor source     :initarg :source)
   (target     :accessor target     :initarg :target)
   (table      :accessor table      :initarg :table)))

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
  (define-lazy-set (next-function table)))

(defmacro forall-in-cayley-table ((element table) &body body)
  (with-gensyms (lazy-table)
    `(let ((lazy-table (make-lazy-table ,table)))
      (forall (,element lazy-table) ,@body))))

(defmacro exists-in-cayley-table ((element table) &body body)
  (with-gensyms (lazy-table)
    `(let ((lazy-table (make-lazy-table ,table)))
      (exists (,element lazy-table) ,@body))))

(defmethod print-object ((obj cayley-table) stream)
  (print-unreadable-object (obj stream :type t)
    (iterate-over-cayley-table elt (table obj)
      (format stream "~&~2:T~A" elt))))

(defun make-cayley-table (source target table &key (equal-pred #'equal))
  (declare (type list source target)
	   (type array table))
  (let ((dimens (array-dimensions table))
	(number-of-elements (length source)))
    (cond
      ((not (every #'(lambda (dimension) (= dimension number-of-elements))
		   dimens))
       (error 'cayley-table-error :text
	      (format nil "Given table ~A doesn't have the right dimensions"
		      table)))
      ((or (contains-duplicates source equal-pred)
	   (contains-duplicates target equal-pred))
       (error 'cayley-table-error :text
	      (format nil "Given source ~A or target ~A contain duplicates."
		      source target)))
      (t (let ((cayley-table (make-instance 'cayley-table
					    :equal-pred equal-pred
					    :source source
					    :target target
					    :table table)))
	   (cond
	     ((not (forall-in-cayley-table (elt cayley-table)
		     (member elt target :test equal-pred)))
	      (error 'cayley-table-error :text
		     (format nil "Given table ~A contains elements not in target ~A"
			     table target)))
	     (t cayley-table)))))))

(defun contains-duplicates (list equal-pred)
  (cond
    ((null list) nil)
    (t (cond
	 ((member (first list) (rest list) :test equal-pred) t)
	 (t (contains-duplicates (rest list) equal-pred))))))

(defun arity-of-table (table)
  (declare (type cayley-table table))
  (length (array-dimensions (table table))))

(defun value-from-argument (table &rest arguments)
  (declare (type cayley-table table))
  "Returns value stored in cayley-table TABLE at position given by ARGUMENTS."
  (cond
    ((not (subsetp arguments (source table) :test (equal-pred table)))
     (error 'cayley-table-error :text
	    (format nil "Given arguments ~A are not in source ~A."
		    arguments (source table))))
    ((not (= (arity-of-table table) (length arguments)))
     (error 'cayley-table-error :text
	    (format nil "Wrong number ~A of arguments given."
		    arguments)))
    (t (let ((positions (mapcar #'(lambda (arg) (position arg (source table)))
				arguments)))
	 (apply #'aref (table table) positions)))))

;;; HERE