(in-package :technicals)

;;; lazy sets

(defclass lazy-set ()
  ((next  :accessor next  :initarg :next)
   (reset :accessor reset :initarg :reset :initform #'(lambda () nil))))

(defmacro define-lazy-set (next-function)
  `(make-instance 'lazy-set
    :next ,next-function))

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
                    result))))
           (reset-function ()
             (setf myset set))) ; not needed by now
      (make-instance 'lazy-set
                     :next #'next-function))))

(defun next-tuple-from-lazy-set (lazy-set tuple)
  "Returns next TUPLE with base-set being LAZY-SET."
  (cond
    ((null tuple) nil)
    (t (let ((next (funcall (next lazy-set))))
         (cond
           ((null next)
            (let ((next-next (next-tuple-from-lazy-set lazy-set (rest tuple))))
              (when next-next
                (cons (funcall (next lazy-set)) next-next))))
           (t (cons next (rest tuple))))))))

(defmacro tuples-lazy (set power)
  (let ((func-name (gensym "FUNC-NAME"))
        (already-known (gensym "ALREADY-KNOWN")))
    `(let ((,already-known ()))
       (flet ((,func-name ()
                (cond
                  ((null ,already-known)
                   (setf ,already-known (symbols ,power (first ,set))))
                  (t
                   (setf ,already-known (next-argument ,set ,already-known))))
                   ,already-known))
         (define-lazy-set #',func-name)))))

;; lazy up to here

(defun next-argument (base-set argument)
  "Returns next ARGUMENT in BASE-SET of length (LENGTH ARGUMENT)"
  (cond
    ((null argument) nil)
    (t (let ((rest (rest (member (first argument) base-set)))); all elements after current
	 (cond
	   ((null rest) ; increment next position
	    (let ((next (next-argument base-set (rest argument))))
	      (when next
		(cons (first base-set) next)))) ; and start with first element again
	   (t (cons (first rest) (rest argument))))))))

(defun n-elemental-subsets (set n)
  "Returns set of all N elemental subsets of SET."
  (cond
    ((= n 0) (list ()))
    ((null set) nil)
    (t (let ((subsets ()))
         (loop for element in set
               do (let ((shorter-subsets (n-elemental-subsets
                                           (remove element set) (1- n))))
                    (mapc #'(lambda (x) (push (cons element x) subsets))
                          shorter-subsets)))
         (make-set subsets :test #'set-equal)))))

(defmacro tuples (set power)
  (let ((tuple (gensym "TUPLE-"))
        (myset (gensym "MYSET-"))
        (mypower (gensym "MYPOWER-")))
    `(let ((,myset ,set)
           (,mypower ,power))
       (loop for ,tuple = (technicals::symbols ,mypower (first ,myset))
             then (technicals::next-argument ,myset ,tuple)
             until (null ,tuple)
             collect ,tuple))))

(defmacro subsets (set)
  (let ((all-subsets (gensym "ALL-SUBSETS-")))
    `(labels ((,all-subsets (set)
               (cond
                 ((null set) (list nil))
                 (t (let ((element (first set))
                          (subsets ()))
                      (let ((shorter-subsets (,all-subsets (rest set))))
                        (mapc #'(lambda (x)
                                  (push (cons element x) subsets)
                                  (push x subsets))
                              shorter-subsets))
                      subsets)))))
      (make-set (,all-subsets ,set) :test #'set-equal))))

(defmacro next-function (set)
  `(typecase ,set
     (lazy-set (next ,set))
     (list     #'(lambda ()
		   (let ((result (first ,set)))
		     (setf ,set (rest ,set))
		     result)))))

(defmacro check-for-all (default-result test test-result-if-success (variable set) &body body)
  "Returns non-NIL if BODY holds for all VARIABLE in SET."
  (let ((myset (gensym "MYSET"))
        (next-function (gensym "NEXT-FUNCTION"))
        (helper (gensym "HELPER")))
    `(let* ((,myset ,set)
            (,next-function (technicals::next-function ,myset)))
      (labels ((,helper (argument)
                 (cond
                   ((null argument) ,default-result)
                   ((funcall ,test (destructuring-bind (,variable) (list argument)
                                     ,@body))
                    ,test-result-if-success)
                   (t (,helper (funcall ,next-function))))))
        (,helper (funcall ,next-function))))))

(defmacro forall ((variable set) &body body)
  `(check-for-all t #'not nil (,variable ,set) ,@body))

(defmacro exists ((variable set) &body body)
  `(check-for-all nil #'(lambda (x) x) t (,variable ,set) ,@body))

(defun read-set (stream char)
  (declare (ignore char))
  `(make-set ',(read-delimited-list #\} stream t) :test #'equal))

(set-macro-character #\{ #'read-set)
(set-macro-character #\} (get-macro-character #\)))

(defmacro => (a b)
  `(if ,a
     ,b
     t))

(defmacro forall-in-table ((variable table) &body body)
  `(forall (,variable (rest ,table)) ,@body))

(defmacro exists-in-table ((variable table) &body body)
  `(exists (,variable (rest ,table)) ,@body))
