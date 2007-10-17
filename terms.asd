(defpackage :terms-system (:use :cl :asdf))
(in-package :terms-system)

(defsystem terms
  :depends-on ("universal-algebra")
  :components ((:file "terms-package")
               (:file "terms" :depends-on ("terms-package"))))
