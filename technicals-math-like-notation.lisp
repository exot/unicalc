(in-package :technicals)

;;; lazy sets

(defclass lazy-set ()
  ((next  :accessor next  :initarg :next)
   (reset :accessor reset :initarg :reset :initform #'(lambda () nil))))

(defun define-lazy-set (next-function &optional (end-symbol nil))
  "Returns a LAZY-SET with next-function being NEXT-FUNCTION"
  (declare (type function next-function))
  (flet ((next-element ()
	   (let ((next (funcall next-function)))
	     (cond
	       ((eq end-symbol next) (values nil nil))
	       (t (values next t))))))
    (make-instance 'lazy-set :next #'next-element)))

(defmethod ensure-standard-set ((set lazy-set) &key (equal #'equal))
  (flet ((next-element ()
	   (funcall (next set))))
    (let ((temp-set (loop for element = (next-element) then (next-element)
			  while element
			  collect element)))
      (make-set temp-set :test equal))))

(defgeneric ensure-lazy-set (set)
  (:documentation "Ensures SET to be a lazy set"))

(defmethod ensure-lazy-set ((set lazy-set))
  set)

(defmethod ensure-lazy-set ((set list))
  (let ((myset set))
    (flet ((next-function ()
             (cond
               ((null myset)
                (setf myset set)
                nil) ; start again
               (t (let ((result (first myset)))
                    (setf myset (rest myset))
                    result)))))
           ;(reset-function ()
           ;  (setf myset set))) ; not needed by now
      (make-instance 'lazy-set
                     :next #'next-function))))

;;; (defun next-tuple-from-lazy-set (lazy-set tuple)
;;;   "Returns next TUPLE with base-set being LAZY-SET."
;;;   (cond
;;;     ((null tuple) nil)
;;;     (t (let ((next (funcall (next lazy-set))))
;;;          (cond
;;;            ((null next)
;;;             (let ((next-next (next-tuple-from-lazy-set lazy-set (rest tuple))))
;;;               (when next-next
;;;                 (cons (funcall (next lazy-set)) next-next))))
;;;            (t (cons next (rest tuple))))))))
;;;
;;; (defmacro tuples-lazy (set power)
;;;   (let ((func-name (gensym "FUNC-NAME"))
;;;         (already-known (gensym "ALREADY-KNOWN")))
;;;     `(let ((,already-known ()))
;;;        (flet ((,func-name ()
;;;                 (cond
;;;                   ((null ,already-known)
;;;                    (setf ,already-known (symbols ,power (first ,set))))
;;;                   (t
;;;                    (setf ,already-known (next-argument ,set ,already-known))))
;;;                    ,already-known))
;;;          (define-lazy-set #',func-name)))))

;; lazy up to here

(defparameter *end-of-set* (gensym "END-OF-SET")
  "Unique symbol to signal the end of a set")

(defun next-function (set)
  (etypecase set
    (lazy-set (next set))
    (list     (next (define-lazy-set
			#'(lambda ()
			    (cond
			      ((emptyp set) *end-of-set*)
			      (t (let ((result (first set)))
				   (setf set (rest set))
				   result))))
			*end-of-set*)))))

(defmacro check-for-all (default-result test test-result-if-success (variable set) &body body)
  "Returns non-NIL if BODY holds for all VARIABLE in SET."
  (let ((myset (gensym "MYSET"))
        (next-function (gensym "NEXT-FUNCTION"))
        (helper (gensym "HELPER"))
	(element (gensym "ELEMENT"))
	(valid (gensym "VALID")))
    `(let* ((,myset ,set)
            (,next-function (technicals::next-function ,myset)))
      (labels ((,helper (argument end-of-set)
                 (cond
                   ((not end-of-set) ,default-result)
                   ((funcall ,test (destructuring-bind (,variable)
				       (list argument)
				     ,(if (listp variable)
					  `(declare (ignorable ,@variable))
					  `(declare (ignorable ,variable)))
                                     ,@body))
                    ,test-result-if-success)
                   (t (multiple-value-bind (,element ,valid)
			  (funcall ,next-function)
			  (,helper ,element ,valid))))))
	(multiple-value-bind (element valid) (funcall ,next-function)
	  (,helper element valid))))))

(defmacro forall ((variable set) &body body)
  `(check-for-all t #'not nil (,variable ,set) ,@body))

(defmacro exists ((variable set) &body body)
  `(check-for-all nil #'(lambda (x) x) t (,variable ,set) ,@body))

(defmacro => (a b)
  `(if ,a
     ,b
     t))
