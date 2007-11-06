(defpackage :terms-system (:use :cl :asdf))
(in-package :terms-system)

(defsystem terms
  :depends-on ("technicals"
               "fundamental-functions"
               "universal-algebra")
  :components ((:file "terms-package")
               (:file "terms"))
  :serial t)
