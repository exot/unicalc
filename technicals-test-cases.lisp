(in-package :technicals)

;;;; simple framework for writing test cases for unicalc

(define-simple-condition test-case-error)

(define-simple-condition test-failed)

(defmacro define-test-case (name arguments result &body body)
  (let ((real-result (gensym "REAL-RESULT")))
    `(defun ,name ,arguments
      (let ((,real-result (handler-case
			      ,@body
			    (simple-condition (v) (print v) nil))))
	(cond
	  ((not (equal ,real-result ,result))
	   (print ,real-result)
	   (print ,result)
	   (raise-error (format nil "~&Test case failed: ~A~%" ',name)))
	  (t (format t "~&Test case succeded: ~A~%" ',name)))))))

(defmacro define-test-case-without-errors (name arguments &body body)
  `(define-test-case ,name ,arguments t
      (progn
	,@body
	t)))

(defmacro define-test-case-with-errors (name arguments &body body)
  `(define-test-case ,name ,arguments nil
      (progn
	,@body
	t)))

(let ((error-handling nil))
  (defun set-error-handling (value)
    (cond
      ((equal value nil) (setf error-handling nil))
      ((equal value 'error) (setf error-handling 'error))
      (t (error 'test-case-error :text
		(format nil "~&~A is not a valid error handling specifier.~%"
			value))))
    t)

  (defun raise-error (text)
    (cond
      ((equal error-handling 'error)
       (error 'technicals::test-failed :text text))
      ((not error-handling) (format t text)))))

(defun run-test (name)
  (funcall name))

(defun run-tests (tests)
  (mapc #'run-test tests))