(defpackage :subalgebras
  (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :terms
   :functions-and-relations)
  (:export 
   :subalgebra-error
   :calculate-generating-elements
   :elements-generate-algebra-p
   :subalgebra-generated-by-elements
   :homomorphism-from-assignment))
