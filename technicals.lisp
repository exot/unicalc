(in-package :technicals)

(defun numbers (n number)
  (cond 
    ((>= 0 n) ())
    (t (cons number (numbers (1- n) number)))))

(defun symbols (n number)
  (numbers n number))

(defun all-zero-except-n (list n)
  "Returns LIST with zeros except in position n"
  (cond
    ((or (null list) (>= n (length list)))
     (numbers (length list) 0))
    (t (let ((zeros (numbers (length list) 0)))
	 (setf (nth n zeros) (nth n list))
	 zeros))))

(defun value-of-element (element)
  (second element))

(defun nth-operand (element n)
  (nth n (all-operands element)))

(defun all-operands (element)
  (first element))

(defun element-at-position (table position)
  (second (assoc position (rest table))))

(defmacro iterate-over-value-table (table element &body body)
  (let ((pair (gensym "PAIR")))
    `(block nil
        (loop for ,pair in (rest ,table)
              do (let ((,element ,pair))
	  	   ,@body)))))

(defun positive-number-p (x)
  (and (numberp x)
       (plusp x)))

(defun non-negative-number-p (x)
  (and (numberp x)
       (not (minusp x))))

(defmacro define-simple-condition (name)
  `(define-condition ,name ()
     ((text :initarg text :reader text))))

(defun set-equal (set1 set2 &key (test #'equal))
  "Returns T if (AND (SUBSETP SET1 SET2) (SUBSETP SET2 SET1))"
  (and (subsetp set1 set2 :test test)
       (subsetp set2 set1 :test test)))

(defun make-set (set &key (test #'equal))
  (remove-duplicates set :test test))