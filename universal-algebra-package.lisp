(defpackage :universal-algebra
 (:use
   :cl
   :fundamental-functions
   :technicals)
 (:export
  :signature
  :function-symbols-of
  :arities-of
  :create-signature-from-arity-function
  :make-rank-alphabet-from-arity-function
  :define-arity-function
  :create-signature-from-rank-alphabet
  :positive-number-p
  :define-simple-condition
  :malformed-rank-alphabet
  :set-equal
  :make-signature
  :arity-of-function-symbol
  :algebra
  :equal-pred-of-algebra
  :base-set-of
  :signature-of
  :interpretations-on
  :make-algebra
  :make-algebra-from-scratch
  :make-set
  :make-interpretation
  :malformed-interpretation
  :define-operation
  :iterate-over-value-table
  :apply-operation-in-algebra
  :algebras-of-same-signature-p))
