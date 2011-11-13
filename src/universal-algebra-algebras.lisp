(in-package :universal-algebra)

;;; algebras

(defclass algebra ()
  ((base-set        :type standard-set :accessor base-set-of
		    :initarg :base-set)
   (signature       :type signature    :accessor signature-of
		    :initarg :signature)
   (interpretations :type standard-set :accessor interpretations-on
		    :initarg :interpretations)))

(defmethod print-object ((obj algebra) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~&~2:Tbase set: {~{~a~^,~}}" (set-to-list (base-set-of obj)))
    (format stream "~&~2:Tsignature: ~a" (signature-of obj))
    (format stream "~&~2:Tinterpretations: ~a" (set-to-list (interpretations-on obj)))))

(defgeneric algebra-p (algebra)
  (:documentation "Tests whether ALGEBRA is an algebra."))

(defmethod algebra-p ((anything t))
  nil)

(defmethod algebra-p ((algebra algebra))
  t)

(defun make-algebra (base-set signature interpretations)
  (declare (type standard-set base-set)
           (type signature signature)
           (type (or standard-set list) interpretations))
  "Returns ALGEBRA object. INTERPRETATIONS shall be an alist of
(SYMBOL INTERPRETATION) where INTERPRETATION is a value table of the given interpretation."
  (make-instance 'algebra
                 :base-set  base-set
                 :signature signature
                 :interpretations (make-interpretation
                                    base-set
				    signature
				    (ensure-standard-set interpretations))))

(defun make-algebra-from-scratch (base-set function-symbols arities
                                  interpretations)
  (declare (type standard-set base-set  interpretations)
	   (type (or standard-set list) arities function-symbols))
  "Returns ALGEBRA object given by BASE-SET, FUNCTION-SYMBOLS and ARITIES of
FUNCTION-SYMBOLS (given as rank-alphabet or as arity-function)"
  (let ((signature (make-signature (ensure-standard-set function-symbols)
				   arities)))
    (make-algebra base-set signature interpretations)))

(defun make-interpretation (base-set signature interpretations)
  (declare (type standard-set base-set interpretations)
           (type signature signature))
  "Returns set of functions that represent INTERPRETATIONS in
<BASE-SET,SIGNATURE> INTERPRETATIONS should have the form
(... (SYMBOL TABLE) ...) or (... (SYMBOL FUNCTION)...) whereas TABLE should be
a value table describing SYMBOL and FUNCTION should be an operation defined
with DEFINE-OPERATION."
  (let ((normalized-interpretations (normalize-table-representation
				     base-set
				     interpretations)))
    (cond
      ((valid-interpretations-in-algebra signature normalized-interpretations)
       normalized-interpretations)
      (t (error 'malformed-interpretation :text
		"Invalid interpretation given")))))

(define-simple-condition malformed-interpretation)

(defun valid-interpretations-in-algebra (signature interpretations)
  (declare (type standard-set interpretations)
           (type signature signature))
  (let ((rank-alphabet (arities-of signature)))
    (check-interpretations rank-alphabet interpretations)))

(defun check-interpretations (rank-alphabet interpretations)
  (declare (type standard-set interpretations)
	   (type list rank-alphabet))
  (and (= (length rank-alphabet) (card-s interpretations))
       (forall (interpretation interpretations)
	 (let* ((function-symbol (function-symbol-of interpretation))
		(arity (get-arity-of-function-symbol function-symbol
						     rank-alphabet)))
	   (arity-correct-p arity interpretation)))))

(defun arity-correct-p (arity interpre)
  (declare (type integer arity)
           (type table interpre))
  (= arity (arity-of-function (implementing-function-of interpre))))
