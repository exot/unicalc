(defpackage :equations
  (:use
   :cl
   :fundamental-functions
   :universal-algebra
   :terms)
  (:export
   :evaluate-term-in-algebra
   :weakly-dependent-p
   :remove-all-weakly-dependent-equations))
