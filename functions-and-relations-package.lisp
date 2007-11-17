(defpackage :functions-and-relations
  (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra)
  (:export
   :homomorphism-p
   :isomorphism-p
   :quasi-homomorphism-p
   :apply-quasihomomorphism-to-algebra
   :no-quasihomomorphism
   :all-homomorphisms
   :all-isomorphisms
   :isomorphic-p
   :congruence-relation-p
   :all-congruences-symbolically))
