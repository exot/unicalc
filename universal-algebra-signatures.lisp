(in-package :universal-algebra)

(defclass signature ()
  ((function-symbols :initarg :function-symbols :accessor function-symbols-of)
   (arities          :initarg :arities          :accessor arities-of)))

(defmethod print-object ((obj signature) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~&~2:Tsymbols: {~{~a~^,~}}" (function-symbols-of obj))
    (format stream "~&~2:Tarities: {~{~a~^,~}}" (arities-of obj))))

(defun create-signature-from-arity-function (function-symbols arity-function)
  "Returns SIGNATURE with FUNCTION-SYMBOLS and corresponding
arities given by ARITY-FUNCTION"
  (declare (type list function-symbols)
	   (type function arity-function))
  (make-instance 'signature
                 :function-symbols function-symbols
                 :arities (make-rank-alphabet-from-arity-function
			    function-symbols arity-function)))

(defun make-rank-alphabet-from-arity-function (symbols func)
  "Converts arity-function FUNC on SYMBOLS to a rank alphabet"
  (declare (type list symbols)
	   (type function func))
  (mapcar #'(lambda (symbol) (list symbol (funcall func symbol))) symbols))

(defmacro define-arity-function (name &body body)
  "Defines arity function NAME for use with :UNIVERSAL-ALGEBRA.
BODY should be an ALIST."
  (let ((symbol (gensym "SYMBOL")))
    `(defun ,name (,symbol)
       (ecase ,symbol
         ,@body))))

(defun create-signature-from-rank-alphabet (function-symbols rank-alphabet)
  "Returns SIGNATURE object with FUNCTION-SYMBOLS and RANK-ALPHABET"
  (declare (type list function-symbols rank-alphabet))
  (when (rank-alphabet-valid-p function-symbols rank-alphabet)
    (make-instance 'signature
                   :function-symbols function-symbols
                   :arities rank-alphabet)))

(defun rank-alphabet-valid-p (symbols alphabet)
  "Return non-NIL if ALPHABET is valid alphabet for SYMBOLS"
  (declare (type list symbols alphabet))
  (if (and (= (length symbols)
              (length alphabet))
           (set-equal symbols (mapcar #'first alphabet))
           (every #'(lambda (x) (non-negative-number-p (second x))) alphabet))
      t
      (error 'malformed-rank-alphabet
	     :text "Invalid rank-alphabet for given symbol set.")))

(define-simple-condition malformed-rank-alphabet)

(defun make-signature (function-symbols arities)
  (declare (type list function-symbols))
  (etypecase arities
    (function (create-signature-from-arity-function function-symbols arities))
    (list     (create-signature-from-rank-alphabet  function-symbols arities))))

(defun get-arity-of-function-symbol (func alphabet)
  "Return arity of FUNC in ALPHABET, NIL if not there"
  (declare (type symbol func)
	   (type list alphabet))
  (second (find-if #'(lambda (x) (equal (first x) func)) alphabet)))

(defgeneric arity-of-function-symbol (term-algebra-or-signature
				      function-symbol)
  (:documentation "Returns arity of FUNCTION-SYMBOL from TERM-ALGEBRA-OR-SIGNATURE"))

(defmethod arity-of-function-symbol ((source signature) function-symbol)
  (declare (type symbol function-symbol))
  (get-arity-of-function-symbol function-symbol (arities-of source)))
