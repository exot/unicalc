(defpackage :functions-system (:use :cl :asdf))
(in-package :functions-system)

(defsystem functions
  :depends-on ("technicals" "universal-algebra")
  :components ((:file "functions-package")
               (:file "functions" :depends-on ("functions-package"))))
