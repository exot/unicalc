(in-package :technicals)

(defun numbers (n number)
  "Returns list of N symbols of NUMBER."
  (cond 
    ((>= 0 n) ())
    (t (cons number (numbers (1- n) number)))))

(defun symbols (n number)
  "Returns list of N symbols of NUMBER."
  (numbers n number))

(defun pair (x y)
  (list x y))

(defun all-zero-except-n (list n)
  "Returns LIST with zeros except in position n"
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

(defmacro define-simple-condition (name)
  `(progn
    (define-condition ,name ()
      ((text :initarg :text :reader text :initform "")))

    (defmethod print-object ((obj ,name) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (technicals::text obj))))))

(defun number-list (n)
  "Returns list (0 .. (1- n))"
  (labels ((count-down (n list)
	     (cond
	       ((zerop (1+ n)) list)
	       (t (count-down (1- n) (cons n list))))))
    (when (>= n 0)
      (count-down (1- n) ()))))

(defun operation-symbol (symbol number)
  "Returns symbol concatenated from SYMBOL and NUMBER."
  (intern (concatenate 'string
		       (princ-to-string symbol)
		       (princ-to-string number))))

(defun symbol-list (n &optional (name "V"))
  "Returns list of N symbols named V0 ... VN unless otherwise specified"
  (labels ((recursive-symbol-list (n list)
	     (cond
	       ((< n 0) list)
	       (t (recursive-symbol-list
		    (1- n)
		    (cons (operation-symbol name n) list))))))
    (recursive-symbol-list (1- n) ())))