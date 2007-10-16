(defpackage :universal-algebra
 (:use :cl)
 (:export
    :signature
    :create-signature-from-arity-function
    :make-rank-alphabet-from-arity-function
    :define-arity-function
    :create-signature-from-rank-alphabet
    :positive-number-p
    :define-simple-condition
    :malformed-rank-alphabet
    :set-equal
    :make-signature
    :get-arity-of-function-symbol
    :algebra
    :make-algebra
    :make-algebra-from-scratch
    :make-set
    :make-interpretation
    :malformed-interpretation
    :function-symbol-of
    :get-arity-of-table
    :numbers
    :all-zero-except-n
    :value-of-element
    :element-at-position
    :iterate-over-value-table))
