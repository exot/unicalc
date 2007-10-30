(in-package :universal-algebra)

(defclass signature ()
  ((function-symbols :initarg :function-symbols :accessor function-symbols-of)
   (arities          :initarg :arities          :accessor arities-of)))

(defmethod print-object ((obj signature) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~&~2:Tsymbols: {~{~a~^,~}}" (function-symbols-of obj))
    (format stream "~&~2:Tarities: {~{~a~^,~}}" (arities-of obj))))

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
           (every #'(lambda (x) (non-negative-number-p (second x))) alphabet))
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
  ((base-set        :accessor base-set-of           :initarg :base-set)
   (signature       :accessor signature-of          :initarg :signature)
   (interpretations :accessor interpretations-on    :initarg :interpretations)
   (equal-pred      :accessor equal-pred-of-algebra :initarg :equal-pred :initform #'equal)))

(defmethod print-object ((obj algebra) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~&~2:Tbase set: {~{~a~^,~}}" (base-set-of obj))
    (format stream "~&~2:Tsignature: ~a" (signature-of obj))
    (format stream "~&~2:Tinterpretations: ~a" (interpretations-on obj))))

(defun make-algebra (base-set signature interpretations &key (equal-pred #'equal))
  "Returns ALGEBRA object. INTERPRETATIONS shall be an alist of (SYMBOL INTERPRETATION)
where INTERPRETATION is a value table of the given interpretation."
  (make-instance 'algebra 
                 :base-set  (make-set base-set)
                 :signature signature
                 :interpretations (make-interpretation base-set 
						       signature 
						       interpretations)
                 :equal-pred equal-pred))

(defun make-algebra-from-scratch (base-set function-symbols arities interpretations &key (equal-pred #'equal))
  "Returns ALGEBRA object given by BASE-SET, FUNCTION-SYMBOLS and ARITIES of FUNCTION-SYMBOLS (given as rank-alphabet or as arity-function)"
  (let ((signature (make-signature function-symbols arities)))
    (make-instance 'algebra
                   :base-set  (make-set base-set)
                   :signature signature
                   :interpretations (make-interpretation base-set
							 signature
							 interpretations
                                                         :equal-pred equal-pred)
                   :equal-pred equal-pred)))

(defun make-interpretation (base-set signature interpretations &key (equal-pred #'equal))
  "Returns set of functions that represent INTERPRETATIONS in <BASE-SET,SIGNATURE>
INTERPRETATIONS should have the form (... (SYMBOL TABLE) ...) or (... (SYMBOL
FUNCTION)...) whereas TABLE should be a value table describing SYMBOL and
FUNCTION should be an operation defined with DEFINE-OPERATION."
  (let ((normalized-interpretations (normalize-interpretations
				     base-set
				     interpretations
                                     :equal-pred equal-pred)))
    (cond
      ((valid-interpretations-in-algebra base-set signature normalized-interpretations :equal-pred equal-pred)
       normalized-interpretations)
      (t (error 'malformed-interpretation :text "Invalid interpretation given")))))

(define-simple-condition malformed-interpretation)

(defun normalize-interpretations (base-set interpretations &key (equal-pred #'equal))
  "Normalizes INTERPRETATIONS to only consist of tables (aka pairs of function
symbols and implementing algebraic functions)."
  (mapcar #'(lambda (table)
	      (let ((function-symbol (function-symbol-of table)))
		(cond
		  ((interpretation-function-p (second table))
		   (list
		    function-symbol
		    (make-function (tuples base-set (get-arity-of-interpretation-function (second table)))
                                   base-set
                                   (interpretation-function-to-graph base-set (second table))
                                   :equal-pred equal-pred)))
                  ((algebraic-function-p (second table))
                   (list function-symbol (second table)))
		  (t
                   (list function-symbol
                         (make-function (tuples base-set (get-arity-of-table table))
                                        base-set
                                        (rest table)
                                        :equal-pred equal-pred))))))
	  interpretations))

(defun interpretation-function-to-graph (base-set interpretation)
  (function-to-graph (tuples base-set (get-arity-of-interpretation-function interpretation))
                     #'(lambda (x) (apply interpretation x))))

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

(defun valid-interpretations-in-algebra (base-set signature interpretations &key (equal-pred #'equal))
  "Returns non-NIL if INTERPRETATIONS is valid in <BASE-SET,SIGNATURE>"
  (let ((rank-alphabet (arities-of signature)))
    (check-interpretations base-set rank-alphabet interpretations :equal-pred equal-pred)))
    
(defun check-interpretations (base-set rank-alphabet interpretations &key (equal-pred #'equal))
  (cond 
    ((and (null interpretations)
          (null rank-alphabet)) t)
    ((or  (null interpretations)
          (null rank-alphabet)) nil)
    (t (let* ((interpretation (first interpretations))
	      (function-symbol (function-symbol-of interpretation))
	      (arity (get-arity-of-function-symbol function-symbol rank-alphabet)))
         (cond
           ((not arity) nil)
           ((and (arity-correct-p arity interpretation)
                 (defines-function-on-set-p base-set interpretation :equal-pred equal-pred))
            (check-interpretations base-set 
                                   (remove-if #'(lambda (x) 
						  (equal (first x) 
							 function-symbol)) 
					      rank-alphabet)
                                   (rest interpretations)))
           (t nil))))))

(defun arity-correct-p (arity interpre)
  (= arity (arity-of-function (implementing-function-of interpre))))

(defun defines-function-on-set-p (base-set interpre &key (equal-pred #'equal))
  (and (defined-on-all-possible-inputs base-set interpre)
       (values-are-in-base-set base-set interpre)))

(defun defined-on-all-possible-inputs (base-set interpre &key (equal-pred #'equal))
  (let* ((ifunc (implementing-function-of interpre))
         (arguments (source ifunc))
         (arity (arity-of-function (implementing-function-of interpre))))
    (and (forall (x arguments) (= (length x) arity))
         (forall (x arguments) (subsetp x base-set :test equal-pred))
	 (= (expt (length base-set) arity)
	    (length (remove-duplicates arguments :test equal-pred))))))

(defun values-are-in-base-set (base-set interpre &key (equal-pred #'equal))
  (let ((ifunc (implementing-function-of interpre)))
    (subsetp (target ifunc) base-set
             :test #'(lambda (x y) (set-equal x y :test equal-pred)))))

;;

(defun algebras-of-same-signature-p (algebra1 algebra2)
  "Returns non-NIL if ALGEBRA1 and ALGEBRA2 are over the same signature."
  (let ((signature1 (signature-of algebra1))
        (signature2 (signature-of algebra2)))
    (and (set-equal (function-symbols-of signature1)
                    (function-symbols-of signature2))
         (set-equal (arities-of signature1)
                    (arities-of signature2)))))

(defun implementing-function-of-operation-symbol (operation-symbol algebra)
  (implementing-function-of (assoc operation-symbol (interpretations-on algebra))))

(defun apply-operation-in-algebra (operation-symbol arguments algebra)
  "Applies OPERATION-SYMBOL in ALGEBRA on ARGUMENTS and returns result."
  (let ((arity (get-arity-of-function-symbol
		operation-symbol
		(arities-of (signature-of algebra)))))
    (cond
      ((and arity
            (= arity (length arguments)))
       (let ((afunc (implementing-function-of-operation-symbol operation-symbol algebra)))
         (apply-function-to-element afunc arguments))) ;;; equal predicate!
      (t (error 'operation-not-appliable
                :text "Operation cannot be applied to argument in algebra.")))))