(defpackage #:unicalc
  (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :terms
   :subalgebras
   :equations
   :functions-and-relations
   :uacalc-interface)
  (:export
;; technicals
   :true-value-p
   :pair
   :positive-number-p
   :non-negative-number-p
   :standard-set
   :make-set
   :card
   :emptyp
   :set-equal
   :tuple-equal
   :tuples
   :subsets
   :n-elemental-subsets
   :define-lazy-set
   :forall
   :exists
   :=>
;; fundamental-functions
   :relation
   :relation-p
   :source
   :target
   :graph
   :function-error
   :malformed-function-definition
   :algebraic-function
   :equal-pred
   :algebraic-function-p
   :make-function
   :iterate-over-function-graph
   :value-of-element
   :all-operands
   :nth-operand
   :apply-function-to-element
   :apply-function-to-tuple
   :apply-function-to-set
   :range
   :surjective-p
   :injective-p
   :bijective-p
   :kernel
   :inverse-image
   :inverse-image-of-element
   :restrict-function-on-source-and-target
   :restrict-function-on-target
   :restrict-function-on-source
   :all-functions
   :all-functions-with-predicate
   :all-bijective-functions
   :forall-in-table
   :exists-in-table
   :define-operation
;; universal-algebra
   :signature
   :function-symbols-of
   :arities-of
   :malformed-rank-alphabet
   :make-signature
   :arity-of-function-symbol
   :algebra
   :equal-pred-of-algebra
   :base-set-of
   :signature-of
   :interpretations-on
   :make-algebra
   :make-algebra-from-scratch
   :malformed-interpretation
   :apply-operation-in-algebra
   :algebras-of-same-signature
;; terms
   :term-algebra
   :variables-of
   :make-term-algebra
   :make-term-algebra-from-scratch
   :variablep
   :function-symbol-p
   :arity-of-function-symbol
   :term
   :termp
   :composed-term-p
   :operation-symbol-of
   :pprint-term
   :pprint-term-list
;; subalgebras
   :subalgebra-error
   :calculate-generating-elements
   :elements-generate-algebra-p
   :subalgebra-generated-by-elements
   :homomorphism-from-assignment ;; experimental
;; equations
   :evalueta-term-in-algebra
   :equation-holds-in-algebra-p
   :models-p
   :weakly-dependent-p
   :remove-all-weakly-dependent-equations
;; functions-and-relations
   :homomorphism-p
   :isomorphism-p
   :quasi-homomorphism-p
   :apply-quasihomomorphism-to-algebra
   :no-quasihomomorphism
   :all-isomorphisms
   :isomorphic-p
;; uacalc-interface
   :uacalc-interface-error
   :uacalc-io-error
   :uacalc-write-algebra-to-file
   :uacalc-read-algebra-from-file
   :calculate-free-algebra
   :symbolize-free-algebra
   :extract-all-equations
   :pprint-all-equations))
