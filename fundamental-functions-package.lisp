(defpackage :fundamental-functions
 (:use :cl :technicals)
 (:export
   :relation
   :relation-p
   :valid-graph-p
   :make-relation
   :malformed-relation-definition
   :source
   :target
   :graph
   :algebraic-function
   :algebraic-function-p
   :make-function
   :function-graph-p
   :function-to-graph
   :apply-function-to-element
   :apply-function-to-tuple
   :apply-function-to-set
   :range
   :surjective-p
   :injective-p
   :bijective-p
   :function-symbol-of
   :implementing-function-of
   :get-arity-of-table))
