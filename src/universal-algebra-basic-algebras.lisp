(in-package :universal-algebra)

(defun algebras-of-same-signature-p (algebra1 algebra2)
  (declare (type algebra algebra1 algebra2))
  "Returns non-NIL if ALGEBRA1 and ALGEBRA2 are over the same signature."
  (let ((signature1 (signature-of algebra1))
        (signature2 (signature-of algebra2)))
    (and (set-equal (function-symbols-of signature1)
                    (function-symbols-of signature2))
         (set-equal (arities-of signature1)
                    (arities-of signature2)))))

(defun implementing-function-of-operation-symbol (operation-symbol algebra)
  (declare (type algebra algebra)
	   (type symbol operation-symbol))
  (implementing-function-of
    (assoc-s operation-symbol (interpretations-on algebra))))

(defun apply-operation-in-algebra (operation-symbol arguments algebra)
  (declare (type symbol operation-symbol)
	   (type list arguments)
	   (type algebra algebra))
  "Applies OPERATION-SYMBOL in ALGEBRA on ARGUMENTS and returns result."
  (let ((arity (get-arity-of-function-symbol
		operation-symbol
		(arities-of (signature-of algebra)))))
    (cond
      ((and arity
            (= arity (length arguments)))
       (let ((afunc (implementing-function-of-operation-symbol
		      operation-symbol algebra)))
         (apply-function-to-element afunc arguments)))
      (t (error 'universal-algebra-error :text
		(format nil
			"Operation ~A cannot be applied to argument ~A in given algebra."
			operation-symbol arguments))))))

(define-simple-condition universal-algebra-error)