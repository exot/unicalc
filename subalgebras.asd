(defpackage :subalgebras-system (:use :cl :asdf))
(in-package :subalgebras-system)

(defsystem subalgebras
  :depends-on ("technicals"
               "fundamental-functions"
               "terms"
               "universal-algebra"
	       "functions-and-relations")
  :components ((:file "subalgebras-package")
               (:file "subalgebras" :depends-on ("subalgebras-package"))))
