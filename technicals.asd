(defpackage :technicals-system (:use :cl :asdf))
(in-package :technicals-system)

(defsystem technicals
  :components ((:file "technicals-package")
               (:file "technicals")
	       (:file "sets")
               (:file "math-like-notation")
               (:file "test-cases"))
  :serial t)
