(in-package :technicals)

;;; lazy sets

(defclass lazy-set ()
  ((next  :accessor next  :initarg :next)))

(defmacro define-lazy-set (next-function)
  `(make-instance 'lazy-set :next ,next-function))

(defmacro tuples (set power)
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

(defmacro check-for-all (default-result test test-result-if-success (variable set) &body body)
  "Returns non-NIL if BODY holds for all VARIABLE in SET."
  (let ((myset (gensym "MYSET"))
        (next-function (gensym "NEXT-FUNCTION"))
        (helper (gensym "HELPER")))
    `(let* ((,myset ,set)
            (,next-function (typecase ,myset
                              (lazy-set (next ,myset))
                              (list     #'(lambda ()
                                            (let ((result (first ,myset)))
                                              (setf ,myset (rest ,myset))
                                              result))))))
      (labels ((,helper (argument)
                 (cond
                   ((null argument) ,default-result)
                   ((funcall ,test (let ((,variable argument))
                                     ,@body))
                    ,test-result-if-success)
                   (t (,helper (funcall ,next-function))))))
        (,helper (funcall ,next-function))))))

(defmacro forall ((variable set) &body body)
  `(check-for-all t #'not nil (,variable ,set) ,@body))

(defmacro exists ((variable set) &body body)
  `(check-for-all nil #'(lambda (x) x) t (,variable ,set) ,@body))
