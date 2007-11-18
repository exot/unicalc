(in-package :technicals)

(defun true-value-p (x)
  (not (not x)))

(defun numbers (n number)
  "Returns list of N symbols of NUMBER."
  (declare (type integer n))
  (cond
    ((>= 0 n) ())
    (t (cons number (numbers (1- n) number)))))

(defun symbols (n number)
  "Returns list of N symbols of NUMBER."
  (declare (type integer n))
  (numbers n number))

(defun pair (x y)
  (declare (type t x y))
  (list x y))

(defun toggle-pair (pair)
  (declare (type list pair))
  (list (second pair) (first pair)))

(defun all-zero-except-n (list n)
  "Returns LIST with zeros except in position n"
  (declare (type list list) (type integer n))
  (cond
    ((or (null list) (>= n (length list)))
     (numbers (length list) 0))
    (t (let ((zeros (numbers (length list) 0)))
	 (setf (nth n zeros) (nth n list))
	 zeros))))

(defun positive-number-p (x)
  "Tests whether X is NUMBERP and PLUSP."
  (and (numberp x)
       (plusp x)))

(defun non-negative-number-p (x)
  "Tests whether X is NUMBERP and (NOT (MINUSP))."
  (and (numberp x)
       (not (minusp x))))

(defmacro with-gensyms (symbols &body body)
  "Expands to BODY with all SYMBOLS replaced with gensyms.
Based on macro from 'Tutorial on Good Lisp Programming Style'
by Norvig and Pitman. "
  `(progn
    ,@(sublis (mapcar #'(lambda (sym)
                          (cons sym (gensym (string sym))))
                      symbols)
              body)))

(defun mapunion (fun list &key (test #'equal))
  (declare (type function fun)
           (type list list))
  "Maps fun to list and unions the result."
  (reduce #'(lambda (x y)
              (union x y :test test))
          (mapcar fun list)))

(defmacro define-simple-condition (name)
  `(progn
    (define-condition ,name ()
      ((text :initarg :text :reader text :initform "")))

    (defmethod print-object ((obj ,name) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (technicals::text obj))))))

(defun number-list (n)
  "Returns list (0 .. (1- n))"
  (declare (type integer n))
  (labels ((count-down (n list)
	     (cond
	       ((zerop (1+ n)) list)
	       (t (count-down (1- n) (cons n list))))))
    (when (>= n 0)
      (count-down (1- n) ()))))

(defun operation-symbol (symbol number)
  "Returns symbol concatenated from SYMBOL and NUMBER."
  (declare (type (or symbol string) symbol)
	   (type integer number))
  (intern (concatenate 'string
		       (princ-to-string symbol)
		       (princ-to-string number))))

(defun symbol-list (n &optional (name "V"))
  "Returns list of N symbols named V0 ... VN unless otherwise specified"
  (declare (type integer n) (type string name))
  (labels ((recursive-symbol-list (n list)
	     (cond
	       ((< n 0) list)
	       (t (recursive-symbol-list
		    (1- n)
		    (cons (operation-symbol name n) list))))))
    (recursive-symbol-list (1- n) ())))

(defun split-by-predicate (list pred)
  (labels ((helper (original-list list1 list2)
	     (cond
	       ((null original-list) (values list1 list2))
	       ((funcall pred (first original-list))
		(helper (rest original-list)
			(cons (first original-list) list1)
			list2))
	       (t (helper (rest original-list)
			  list1
			  (cons (first original-list) list2))))))
    (helper list () ())))

;;; from en.wikipedia.org
(defun nth-permutation (num list)
  (declare (type integer num)
	   (type list list))
  (let ((sequence (coerce (copy-list list) 'vector)))
    (loop for factorial = 1 then (* factorial j)
	  for j from 1 below (length list)
	  do (rotatef (aref sequence (- j (mod (truncate (/ num factorial)) (1+ j))))
		      (aref sequence j))
	  finally (return (coerce sequence 'list)))))

(defun factorial (n)
  (labels ((fac-hlper (n result)
	     (cond
	       ((>= 0 n) result)
	       (t (fac-hlper (1- n) (* result n))))))
    (fac-hlper n 1)))
