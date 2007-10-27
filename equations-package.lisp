(defpackage :equations
  (:use
   :cl
   :technicals
   :terms)
  (:export
   :weakly-dependent-p
   :remove-all-weakly-dependent-equations))