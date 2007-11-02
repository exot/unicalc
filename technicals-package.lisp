(defpackage :technicals
  (:use :cl)
  (:export
;; technicals.lisp
   :numbers
   :symbols
   :pair
   :all-zero-except-n
   :positive-number-p
   :non-negative-number-p
   :define-simple-condition
   :number-list
   :operation-symbol
   :symbol-list
;; sets.lisp
   :algebraic-set
   :make-set
   :next-argument
   :next-assignment
   :card
   :emptyp
   :set-equal
   :tuple-equal
   :tuples
   :subsets
   :n-elemental-subsets
;; math-like-notation.lisp
   :lazy-set
   :next
   :define-lazy-set
   :forall
   :exists
   :=>
   :forall-in-table
   :exists-in-table))
