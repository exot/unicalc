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
   :all-isomorphisms
   :isomorphic-p))
