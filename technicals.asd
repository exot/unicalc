(defpackage :technicals-system (:use :cl :asdf))
(in-package :technicals-system)

(defsystem technicals
  :components ((:file "technicals-package")
               (:file "technicals-technicals")
	       (:file "technicals-sets")
               (:file "technicals-math-like-notation")
               (:file "technicals-test-cases"))
  :serial t)
