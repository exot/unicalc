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
   :partitions
   :print-partitions
   :define-lazy-set
   :forall
   :exists
   :=>
;; fundamental-functions
   :relation-error
   :relation
   :relation-p
   :source
   :target
   :graph
   :iterate-over-relation-graph
   :value-of-element
   :all-operands
   :nth-operand
   :in-relation-p
   :reflexiv-p
   :symmetric-p
   :anti-symmetric-p
   :transitive-p
   :equivalence-relation-p
   :equivalence-relation-from-partition
   :all-in-relation-to-element
   :partition-from-equivalence-relation
   :order-relation-p
   :inverse-relation
   :relation-product
   :function-error
   :malformed-function-definition
   :algebraic-function
   :equal-pred
   :algebraic-function-p
   :make-function
   :iterate-over-function-graph
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
   :inverse-function
   :restrict-function-on-source-and-target
   :restrict-function-on-target
   :restrict-function-on-source
   :all-functions
   :all-functions-with-predicate
   :all-bijective-functions
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
   :all-homomorphisms
   :all-isomorphisms
   :isomorphic-p
   :congruence-relation-p
   :all-congruences-symbolically
;; uacalc-interface
   :uacalc-interface-error
   :uacalc-io-error
   :uacalc-read-algebra-from-file
   :uacalc-write-algebra-to-file
   :uacalc-write-vector-list-to-file
   :uacalc-read-vector-list-from-file
   :uacalc-write-congruences-to-file
   :uacalc-read-congruences-from-file
   :calculate-direct-power-numerically
   :calculate-direct-product-numerically
   :calculate-free-algebra
   :symbolize-free-algebra
   :extract-all-equations
   :pprint-all-equations
   :compute-congruence-lattice-numerically
   :all-congruences
   :all-principal-congruences
   :all-meet-irreducible-congruences))