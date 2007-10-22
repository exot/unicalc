(defpackage :technicals-system (:use :cl :asdf))
(in-package :technicals-system)

(defsystem technicals
  :components ((:file "technicals-package")
               (:file "technicals" :depends-on ("technicals-package"))
               (:file "math-like-notation" :depends-on ("technicals"))))
