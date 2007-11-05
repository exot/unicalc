(defpackage :fundamental-functions-system (:use :cl :asdf))
(in-package :fundamental-functions-system)

(defsystem fundamental-functions
  :depends-on ("technicals")
  :components ((:file "fundamental-functions-package")
               (:file "relations")
	       (:file "functions")
	       (:file "basic-functions")
	       (:file "tables"))
  :serial t)
