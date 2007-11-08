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
;;
))
