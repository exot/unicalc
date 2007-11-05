(defpackage :universal-algebra-system (:use :cl :asdf))
(in-package :universal-algebra-system)

(defsystem universal-algebra
  :depends-on ("technicals"
               "fundamental-functions")
  :components ((:file "universal-algebra-package")
	       (:file "signatures")
	       (:file "algebras")
	       (:file "basic-algebras"))
  :serial t)
