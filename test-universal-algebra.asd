(defpackage :test-universal-algebra-system (:use :cl :asdf))
(in-package :test-universal-algebra-system)

(defsystem test-universal-algebra
  :depends-on ("technicals" 
               "fundamental-functions"
               "universal-algebra"
               "terms"
               "functions-and-relations"
               "subalgebras"
               "equations"
               "uacalc-interface")
  :components ((:file "test-universal-algebra-package")
               (:file "test-universal-algebra" 
                :depends-on ("test-universal-algebra-package"))))
