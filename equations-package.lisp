(defpackage :equations
  (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :terms)
  (:export
   :evaluate-term-in-algebra
   :equation-holds-in-algebra-p
   :models-p
   :weakly-dependent-p
   :remove-all-weakly-dependent-equations))
