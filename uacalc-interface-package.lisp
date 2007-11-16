(defpackage :UACalc-interface
 (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :functions-and-relations
   :subalgebras
   :terms
   :equations)
 (:export
;; uacalc-interface-basic-io
   :uacalc-interface-error
   :uacalc-io-error
;; uacalc-interface-io
   :uacalc-read-algebra-from-file
   :uacalc-write-algebra-to-file
   :uacalc-write-vector-list-to-file
   :uacalc-read-vector-list-from-file
   :uacalc-write-congruences-to-file
   :uacalc-read-congruences-from-file
;; uacalc-interface-uab-standard
;; uacalc-interface-direct-product
   :calculate-direct-power-numerically
   :calculate-direct-product-numerically
;; uacalc-interface-free-algebra
   :calculate-free-algebra
   :symbolize-free-algebra
   :extract-all-equations
   :pprint-all-equations
;; uacalc-interface-congruence-lattic
   :compute-congruence-lattice-numerically
   :all-congruences
   :all-principal-congruences
   :all-meet-irreducible-congruences))
