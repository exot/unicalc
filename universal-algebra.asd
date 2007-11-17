(defpackage :universal-algebra-system (:use :cl :asdf))
(in-package :universal-algebra-system)

(defsystem universal-algebra
  :depends-on ("technicals"
               "fundamental-functions")
  :components ((:file "universal-algebra-package")
	       (:file "universal-algebra-signatures")
	       (:file "universal-algebra-algebras")
	       (:file "universal-algebra-basic-algebras"))
  :serial t)

