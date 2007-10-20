(defpackage :terms
 (:use :cl :universal-algebra)
 (:export
  :term-algebra
  :variables-of
  :signature-of
  :make-term-algebra
  :make-term-algebra-from-scratch
  :variablep
  :function-symbol-p
  :arity-of-function-symbol
  :termp
  :composed-term-p
  :operation-symbol-of
  :match
  :matches-subterm
  :apply-matching
  :pprint-term
  :pprint-term-list))