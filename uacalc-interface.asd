(defpackage :UACalc-interface-system (:use :cl :asdf))
(in-package :UACalc-interface-system)

(defsystem UACalc-interface
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra"
               "subalgebras"
               "functions-and-relations")
  :components ((:file "UACalc-interface-package")
	       (:file "UACalc-technicals")
	       (:file "UACalc-interface"))
  :serial t)
