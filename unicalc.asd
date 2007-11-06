(defpackage :unicalc-system (:use :cl :asdf))
(in-package :unicalc-system)

(defsystem unicalc
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra"
               "terms"
               "subalgebras"
               "functions-and-relations"
	       "equations"
               "uacalc-interface")
  :components ((:file "unicalc-package")))
