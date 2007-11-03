(defpackage :fundamental-functions
 (:use :cl :technicals)
 (:export
;; relations
  :relation
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
  :algebraic-function-p
  :make-function
  :function-graph-p
  :function-to-graph
;; basic functions
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
;; tables
  :function-symbol-of
  :implementing-function-of
  :get-arity-of-table
  :arity-of-function
  :forall-in-table
  :exists-in-table))
