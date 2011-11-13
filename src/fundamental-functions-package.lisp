(defpackage :fundamental-functions
 (:use :cl :technicals)
 (:export
;; relations
  :relations-error
  :relation
  :*relation-print-max-size*
  :*relation-print-max-print*
  :*relation-print-all*
  :print-relation
  :relation-p
  :valid-graph-p
  :make-relation
  :source
  :target
  :graph
  :iterate-over-relation-graph
  :value-of-element
  :all-operands
  :nth-operand
;; basic-relations
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
;; functions
  :function-error
  :malformed-function-definition
  :algebraic-function
  :equal-pred
  :algebraic-function-p
  :make-function
  :function-graph-p
  :function-to-graph
  :iterate-over-function-graph
;; basic functions
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
  :all-assignments
  :all-functions
  :all-functions-with-predicate
  :all-bijective-functions
;; tables
  :table
  :function-symbol-of
  :implementing-function-of
  :get-arity-of-table
  :arity-of-function
  :define-operation
  :normalize-table-representation))
