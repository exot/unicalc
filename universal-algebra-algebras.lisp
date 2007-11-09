(in-package :universal-algebra)

;;; algebras

(defclass algebra ()
  ((base-set        :accessor base-set-of           :initarg :base-set)
   (signature       :accessor signature-of          :initarg :signature)
   (interpretations :accessor interpretations-on    :initarg :interpretations)
   (equal-pred      :accessor equal-pred-of-algebra :initarg :equal-pred
		    :initform #'equal)))

(defmethod print-object ((obj algebra) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~&~2:Tbase set: {~{~a~^,~}}" (base-set-of obj))
    (format stream "~&~2:Tsignature: ~a" (signature-of obj))
    (format stream "~&~2:Tinterpretations: ~a" (interpretations-on obj))))

(defun make-algebra (base-set signature interpretations
                     &key (equal-pred #'equal))
  (declare (type standard-set base-set)
           (type signature signature)
           (type standard-set interpretations))
  "Returns ALGEBRA object. INTERPRETATIONS shall be an alist of
(SYMBOL INTERPRETATION) where INTERPRETATION is a value table of the given
interpretation."
  (make-instance 'algebra
                 :base-set  (make-set base-set)
                 :signature signature
                 :interpretations (make-interpretation base-set
						       signature
						       interpretations)
                 :equal-pred equal-pred))

(defun make-algebra-from-scratch (base-set function-symbols arities
                                  interpretations &key (equal-pred #'equal))
  (declare (type standard-set base-set function-symbols
                 arities interpretations))
  "Returns ALGEBRA object given by BASE-SET, FUNCTION-SYMBOLS and ARITIES of
FUNCTION-SYMBOLS (given as rank-alphabet or as arity-function)"
  (let ((signature (make-signature function-symbols arities)))
    (make-instance 'algebra
                   :base-set  (make-set base-set)
                   :signature signature
                   :interpretations (make-interpretation base-set
							 signature
							 interpretations
                                                         :equal-pred equal-pred)
                   :equal-pred equal-pred)))

(defun make-interpretation (base-set signature interpretations
                            &key (equal-pred #'equal))
  (declare (type standard-set base-set interpretations)
           (type signature signature))
  "Returns set of functions that represent INTERPRETATIONS in
<BASE-SET,SIGNATURE> INTERPRETATIONS should have the form
(... (SYMBOL TABLE) ...) or (... (SYMBOL FUNCTION)...) whereas TABLE should be
a value table describing SYMBOL and FUNCTION should be an operation defined
with DEFINE-OPERATION."
  (let ((normalized-interpretations (normalize-table-representation
				     base-set
				     interpretations
                                     :equal-pred equal-pred)))
    (cond
      ((valid-interpretations-in-algebra base-set signature
                                         normalized-interpretations
					 :equal-pred equal-pred)
       normalized-interpretations)
      (t (error 'malformed-interpretation :text
		"Invalid interpretation given")))))

(define-simple-condition malformed-interpretation)

(defun valid-interpretations-in-algebra (base-set signature interpretations
                                         &key (equal-pred #'equal))
  (declare (type standard-set base-set interpretations)
           (type signature signature))
  "Returns non-NIL if INTERPRETATIONS is valid in <BASE-SET,SIGNATURE>"
  (let ((rank-alphabet (arities-of signature)))
    (check-interpretations base-set rank-alphabet interpretations
			   :equal-pred equal-pred)))

(defun check-interpretations (base-set rank-alphabet interpretations
			      &key (equal-pred #'equal))
  (declare (type standard-set base-set rank-alphabet interpretations))
  (and (= (length rank-alphabet) (length interpretations))
       (every #'(lambda (interpretation)
                  (let* ((function-symbol (function-symbol-of interpretation))
                         (arity (get-arity-of-function-symbol function-symbol
                                                              rank-alphabet)))
                    (and (arity-correct-p arity interpretation)
                         (defines-function-on-set-p base-set interpretation
                           :equal-pred equal-pred))))
              interpretations)))

(defun arity-correct-p (arity interpre)
  (declare (type integer arity)
           (type table interpre))
  (= arity (arity-of-function (implementing-function-of interpre))))

(declaim (inline defines-function-on-set-p))
(defun defines-function-on-set-p (base-set interpre &key (equal-pred #'equal))
  (declare (type standard-set base-set)
           (type table interpre)
           (ignore base-set interpre equal-pred))
;; we don't need to check anything here because it has been checked already
;; by make-function in normalize-functions
  t)
