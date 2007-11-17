(defpackage :fundamental-functions-system (:use :cl :asdf))
(in-package :fundamental-functions-system)

(defsystem fundamental-functions
  :depends-on ("technicals")
  :components ((:file "fundamental-functions-package")
               (:file "fundamental-functions-relations")
	       (:file "fundamental-functions-basic-relations")
;	       (:file "fundamental-functions-cayley-tables")
	       (:file "fundamental-functions-functions")
	       (:file "fundamental-functions-basic-functions")
;	       (:file "fundamental-functions-tables")
	       )
  :serial t)
