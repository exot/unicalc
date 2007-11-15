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
;; uacalc-interface-io
   :uacalc-interface-error
   :uacalc-io-error
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
