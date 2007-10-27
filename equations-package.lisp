(defpackage :equations
  (:use
   :cl
   :fundamental-functions
   :universal-algebra
   :terms)
  (:export
   :weakly-dependent-p
   :remove-all-weakly-dependent-equations))
