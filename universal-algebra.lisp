(in-package :universal-algebra)

(defclass signature ()
  ((function-symbols :initarg :function-symbols :accessor function-symbols-of)
   (arities          :initarg :arities          :accessor arities-of)))

(defun create-signature-from-arity-function (function-symbols arity-function)
  "Returns SIGNATURE with FUNCTION-SYMBOLS and corresponding arities given by ARITY-FUNCTION"
  (make-instance 'signature 
                 :function-symbols function-symbols
                 :arities (make-rank-alphabet-from-arity-function function-symbols arity-function)))

(defun make-rank-alphabet-from-arity-function (symbols func)
  "Converts arity-function FUNC on SYMBOLS to a rank alphabet"
  (mapcar #'(lambda (symbol) (list symbol (funcall func symbol))) symbols))

(defmacro define-arity-function (name &body body)
  "Defines arity function NAME for use with :UNIVERSAL-ALGEBRA. BODY should be an ALIST."
  (let ((symbol (gensym "SYMBOL")))
    `(defun ,name (,symbol)
       (ecase ,symbol
         ,@body))))

(defun create-signature-from-rank-alphabet (function-symbols rank-alphabet)
  "Returns SIGNATURE object with FUNCTION-SYMBOLS and RANK-ALPHABET"
  (when (rank-alphabet-valid-p function-symbols rank-alphabet)
    (make-instance 'signature 
                   :function-symbols function-symbols
                   :arities rank-alphabet)))

(defun positive-number-p (x)
  (and (numberp x)
       (plusp x)))

(defun rank-alphabet-valid-p (symbols alphabet)
  "Return non-NIL if ALPHABET is valid alphabet for SYMBOLS"
  (if (and (= (length symbols)
              (length alphabet))
           (set-equal symbols (mapcar #'first alphabet))
           (every #'(lambda (x) (positive-number-p (second x))) alphabet))
      t
      (error 'malformed-rank-alphabet :text "Invalid rank-alphabet for given symbol set.")))

(defmacro define-simple-condition (name)
  `(define-condition ,name ()
     ((text :initarg text :reader text))))

(define-simple-condition malformed-rank-alphabet)

(defun set-equal (set1 set2)
  "Returns T if (AND (SUBSETP SET1 SET2) (SUBSETP SET2 SET1))"
  (and (subsetp set1 set2)
       (subsetp set2 set1)))

(defun make-signature (function-symbols arities)
  (etypecase arities
    (function (create-signature-from-arity-function function-symbols arities))
    (list     (create-signature-from-rank-alphabet  function-symbols arities))))

(defun get-arity-of-function-symbol (func alphabet)
  "Return arity of FUNC in ALPHABET, NIL if not there"
  (second (find-if #'(lambda (x) (equal (first x) func)) alphabet)))

;;; algebras

(defclass algebra ()
  ((base-set        :accessor base-set-of        :initarg :base-set)
   (signature       :accessor signature-of       :initarg :signature)
   (interpretations :accessor interpretations-on :initarg :interpretations)))

(defun make-algebra (base-set signature interpretations)
  "Returns ALGEBRA object. INTERPRETATIONS shall be an alist of (SYMBOL INTERPRETATION)
where INTERPRETATION is a value table of the given interpretation."
  (make-instance 'algebra 
                 :base-set  (make-set base-set)
                 :signature signature
                 :interpretations (make-interpretation base-set signature interpretations)))

(defun make-algebra-from-scratch (base-set function-symbols arities interpretations)
  "Returns ALGEBRA object given by BASE-SET, FUNCTION-SYMBOLS and ARITIES of FUNCTION-SYMBOLS (given as rank-alphabet or as arity-function)"
  (let ((signature (make-signature function-symbols arities)))
    (make-instance 'algebra
                   :base-set  (make-set base-set)
                   :signature signature 
                   :interpretations (make-interpretation base-set signature interpretations))))

(defun make-set (set &key (test #'equal))
  (remove-duplicates set :test test))

(defun make-interpretation (base-set signature interpretations)
  "Returns set of functions that represent INTERPRETATIONS in <BASE-SET,SIGNATURE>"
  (cond
    ((valid-interpretations-in-algebra base-set signature interpretations) interpretations)
    (t (error 'malformed-interpretation :text "Invalid interpretation given"))))

(define-simple-condition malformed-interpretation)

; make interpretation out of function, base-set and arity

(defun generate-all-tuples (base-set n)
  "Returns list of all N-tuples of elements in BASE-SET"
  nil) ; HERE

(defun valid-interpretations-in-algebra (base-set signature interpretations)
  "Returns non-NIL if INTERPRETATIONS is valid in <BASE-SET,SIGNATURE>"
  (let ((rank-alphabet (arities-of signature)))
    (check-interpretations base-set rank-alphabet interpretations)))
    
(defun check-interpretations (base-set rank-alphabet interpretations)
  (cond 
    ((and (null interpretations)
          (null rank-alphabet)) t)
    ((or  (null interpretations)
          (null rank-alphabet)) nil)
    (t (let* ((table (first interpretations))
              (function-symbol (function-symbol-of table))
              (arity (get-arity-of-function-symbol function-symbol rank-alphabet)))
         (cond
           ((not arity) nil)
           ((and (arity-correct-p arity table)
                 (defines-function-on-set-p base-set table))
            (check-interpretations base-set 
                                   (remove-if #'(lambda (x) 
						  (equal (first x) 
							 function-symbol)) 
					      rank-alphabet)
                                   (rest interpretations)))
           (t nil))))))

(defun arity-correct-p (arity table)
  (= arity (get-arity-of-table table)))

(defun defines-function-on-set-p (base-set table)
  (and (defined-on-all-possible-inputs base-set table)
       (values-are-in-base-set base-set table)))

(defun defined-on-all-possible-inputs (base-set table)
  (let ((arguments (mapcar #'first (rest table)))
	(arity (get-arity-of-table table)))
    (labels ((recursive-check (n)
	       (cond
		 ((zerop n) t)
		 (t (let ((used-arguments (mapcar #'(lambda (x) (nth n x)) arguments)))
		      (cond
			((not (set-equal used-arguments base-set)) nil)
			(t (recursive-check (1- n)))))))))
      (recursive-check (1- arity)))))

(defun values-are-in-base-set (base-set table)
  (iterate-over-value-table table element
    (when (not (member (value-of-element element) base-set))
      (return nil)))
  t)

;;; iterating over value tables

(defun function-symbol-of (table)
  (first table))

(defun get-arity-of-table (table)
  (length (first (first (rest table)))))

(defun numbers (n number)
  (cond 
    ((>= number n) ())
    (t (cons number (numbers (1- n) number)))))

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
    `(loop for ,pair in (rest ,table)
           do (let ((,element ,pair))
		,@body))))
