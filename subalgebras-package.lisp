(defpackage :subalgebras
  (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra)
  (:export 
   :subalgebra-error
   :calculate-generating-elements
   :elements-generate-algebra-p
   :subalgebra-generated-by-elements))
