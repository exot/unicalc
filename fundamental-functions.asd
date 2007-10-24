(defpackage :fundamental-functions-system (:use :cl :asdf))
(in-package :fundamental-functions-system)

(defsystem fundamental-functions
  :components ((:file "fundamental-functions-package")
               (:file "fundamental-functions"
                :depends-on ("fundamental-functions-package"))))
