(defpackage :uacalc-interface-system (:use :cl :asdf))
(in-package :uacalc-interface-system)

(defsystem uacalc-interface
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra"
               "subalgebras"
	       "terms"
	       "equations"
               "functions-and-relations")
  :components ((:file "uacalc-interface-package")
	       (:file "uacalc-interface-io")
               (:file "uacalc-interface-uab-standard")
	       (:file "uacalc-interface-direct-product")
	       (:file "uacalc-interface-free-algebra")
	       (:file "uacalc-interface"))
  :serial t)
