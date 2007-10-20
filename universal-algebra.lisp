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

(defun rank-alphabet-valid-p (symbols alphabet)
  "Return non-NIL if ALPHABET is valid alphabet for SYMBOLS"
  (if (and (= (length symbols)
              (length alphabet))
           (set-equal symbols (mapcar #'first alphabet))
           (every #'(lambda (x) (positive-number-p (second x))) alphabet))
      t
      (error 'malformed-rank-alphabet 
	     :text "Invalid rank-alphabet for given symbol set.")))

(define-simple-condition malformed-rank-alphabet)

(defun make-signature (function-symbols arities)
  (etypecase arities
    (function (create-signature-from-arity-function function-symbols arities))
    (list     (create-signature-from-rank-alphabet  function-symbols arities))))

(defun get-arity-of-function-symbol (func alphabet)
  "Return arity of FUNC in ALPHABET, NIL if not there"
  (second (find-if #'(lambda (x) (equal (first x) func)) alphabet)))

(defgeneric arity-of-function-symbol (term-algebra-or-signature function-symbol)
  (:documentation "Returns arity of FUNCTION-SYMBOL from TERM-ALGEBRA-OR-SIGNATURE"))

(defmethod arity-of-function-symbol ((source signature) function-symbol)
  (get-arity-of-function-symbol function-symbol (arities-of source)))

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
                 :interpretations (make-interpretation base-set 
						       signature 
						       interpretations)))

(defun make-algebra-from-scratch (base-set function-symbols arities interpretations)
  "Returns ALGEBRA object given by BASE-SET, FUNCTION-SYMBOLS and ARITIES of FUNCTION-SYMBOLS (given as rank-alphabet or as arity-function)"
  (let ((signature (make-signature function-symbols arities)))
    (make-instance 'algebra
                   :base-set  (make-set base-set)
                   :signature signature 
                   :interpretations (make-interpretation base-set 
							 signature 
							 interpretations))))

(defun make-interpretation (base-set signature interpretations)
  "Returns set of functions that represent INTERPRETATIONS in <BASE-SET,SIGNATURE>
INTERPRETATIONS should have the form (... (SYMBOL TABLE) ...) or (... (SYMBOL
FUNCTION)...) whereas TABLE should be a value table describing SYMBOL and
FUNCTION should be an operation defined with DEFINE-OPERATION."
  (let ((normalized-interpretations (normalize-interpretations base-set interpretations)))
    (cond
      ((valid-interpretations-in-algebra base-set signature normalized-interpretations) 
       normalized-interpretations)
      (t (error 'malformed-interpretation :text "Invalid interpretation given")))))

(define-simple-condition malformed-interpretation)

(defun normalize-interpretations (base-set interpretations)
  "Normalizes INTERPRETATIONS to only consist of value tables."
  (mapcar #'(lambda (table)
	      (let ((function-symbol (function-symbol-of table))
		    (interpretation (second table)))
		(cond
		  ((interpretation-function-p interpretation)
		   (cons
		    function-symbol
		    (interpretation-function-to-value-table base-set interpretation)))
		  (t table))))
	  interpretations))

(defun interpretation-function-to-value-table (base-set ifunc)
  "Converts IFUNC to value-table."
  (let ((value-table ()))
    (labels ((collect-all-values (argument)
	       (cond
		 ((null argument) value-table)
		 (t (push (list argument (apply (symbol-function ifunc) argument)) 
			  value-table)
		    (collect-all-values (next-argument base-set argument))))))
      (collect-all-values (numbers (get-arity-of-interpretation-function ifunc) 
				   (first base-set))))))

(defmacro define-operation (name arguments &body body)
  "Defines function which can be used to generate interpretations 
instead of value tables."
  `(progn
     (setf (get ',name :is-interpretation-function) t)
     (setf (get ',name :arity) (length ',arguments))
    
     (defun ,name ,arguments
       ,@body)))

(defun interpretation-function-p (func)
  (and (symbolp func)
       (get func :is-interpretation-function)))

(defun get-arity-of-interpretation-function (func)
  (when (interpretation-function-p func)
    (get func :arity)))

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
    (and (every #'(lambda (x) (= (length x) arity)) arguments)
	 (every #'(lambda (x) (every #'(lambda (y) (member y base-set)) x)) arguments)
	 (= (expt (length base-set) arity)
	    (length (remove-duplicates arguments :test #'equal))))))

(defun values-are-in-base-set (base-set table)
  (iterate-over-value-table table element
    (when (not (member (value-of-element element) base-set))
      (return-from values-are-in-base-set nil)))
  t)
