(defpackage :fundamental-functions
 (:use :cl :technicals)
 (:export
;; relations
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
  :value-of-element
  :all-operands
  :nth-operand
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
  :forall-in-table
  :exists-in-table
  :define-operation
  :normalize-table-representation))
