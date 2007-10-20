(defpackage :functions-and-relations-system (:use :cl :asdf))
(in-package :functions-and-relations-system)

(defsystem functions-and-relations
  :depends-on ("technicals" "universal-algebra")
  :components ((:file "functions-and-relations-package")
               (:file "functions-and-relations" 
                :depends-on ("functions-and-relations-package"))))
