(defpackage :UACalc-interface-system (:use :cl :asdf))
(in-package :UACalc-interface-system)

(defsystem UACalc-interface
  :depends-on ("technicals"
               "universal-algebra"
               "subalgebras"
               "functions-and-relations")
  :components ((:file "UACalc-interface-package")))
