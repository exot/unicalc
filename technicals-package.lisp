(defpackage :technicals
  (:use :cl)
  (:export
;; technicals.lisp
   :true-value-p
   :numbers
   :symbols
   :pair
   :toggle-pair
   :all-zero-except-n
   :positive-number-p
   :non-negative-number-p
   :with-gensyms
   :mapunion
   :define-simple-condition
   :number-list
   :operation-symbol
   :symbol-list
   :split-by-predicate
;; sets.lisp
   :standard-set
   :make-set
   :next-argument
   :card
   :emptyp
   :set-equal
   :set-equal-p
   :tuple-equal
   :tuple-equal-p
   :tuples
   :subsets
   :n-elemental-subsets
   :partitions
   :print-partitions
;; math-like-notation.lisp
   :lazy-set
   :next
   :define-lazy-set
   :forall
   :exists
   :=>
;; test-cases
   :test-case-error
   :test-failed
   :define-test-case
   :define-test-case-without-errors
   :define-test-case-with-errors
   :set-error-handling
   :run-test
   :run-tests))
