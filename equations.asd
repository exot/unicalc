(defpackage :equations-system (:use :cl :asdf))
(in-package :equations-system)

(defsystem equations
  :depends-on ("technicals"
               "terms")
  :components ((:file "equations-package")
               (:file "equations-equations"))
  :serial t)
