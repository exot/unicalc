(defpackage :subalgebras-system (:use :cl :asdf))
(in-package :subalgebras-system)

(defsystem subalgebras
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra")
  :components ((:file "subalgebras-package")
               (:file "subalgebras" :depends-on ("subalgebras-package"))))
