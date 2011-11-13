(defpackage :test-unicalc-system (:use :cl :asdf))
(in-package :test-unicalc-system)

(defsystem test-unicalc
  :depends-on (unicalc)
  :components ((:module tests
                :pathname "src/"
                :components ((:file "test-unicalc-package")
                             (:file "test-unicalc"))
                :serial t)))
