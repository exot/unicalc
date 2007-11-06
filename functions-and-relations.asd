(defpackage :functions-and-relations-system (:use :cl :asdf))
(in-package :functions-and-relations-system)

(defsystem functions-and-relations
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra")
  :components ((:file "functions-and-relations-package")
               (:file "functions-and-relations-homomorphisms"))
  :serial t)
