(defpackage :universal-algebra
 (:use
   :cl
   :fundamental-functions
   :technicals)
 (:export
;; signatures
  :signature
  :function-symbols-of
  :arities-of
  :malformed-rank-alphabet
  :make-signature
  :arity-of-function-symbol
;; algebras
  :algebra
  :algebra-p
  :base-set-of
  :signature-of
  :interpretations-on
  :make-algebra
  :make-algebra-from-scratch
  :malformed-interpretation
;; basic-algebras
  :apply-operation-in-algebra
  :algebras-of-same-signature-p))
