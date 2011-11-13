(defpackage :unicalc-system
  (:use :cl :asdf))
(in-package :unicalc-system)

(defsystem unicalc
  :name "unicalc"
  :author "Daniel Borchmann"
  :version "0.0.0"
  :license "see LICENSE"
  :description "A Universal Algebra Calculator"
  :components ((:file "unicalc"
                :depends-on (technicals
                             fundamental-functions
                             universal-algebra
                             terms
                             subalgebras
                             functions-and-relations
                             equations
                             uacalc-interface))
               (:module technicals
                :pathname "src/"
                :components ((:file "technicals-package")
                             (:file "technicals-technicals")
                             (:file "technicals-sets")
                             (:file "technicals-math-like-notation")
                             (:file "technicals-test-cases"))
                :serial t)
               (:module fundamental-functions
                :depends-on (technicals)
                :pathname "src/"
                :components ((:file "fundamental-functions-package")
                             (:file "fundamental-functions-relations")
                             (:file "fundamental-functions-basic-relations")
                             ;(:file "fundamental-functions-cayley-tables")
                             (:file "fundamental-functions-functions")
                             (:file "fundamental-functions-basic-functions")
                             (:file "fundamental-functions-tables"))
                :serial t)
               (:module universal-algebra
                :depends-on (technicals
                             fundamental-functions)
                :pathname "src/"
                :components ((:file "universal-algebra-package")
                             (:file "universal-algebra-signatures")
                             (:file "universal-algebra-algebras")
                             (:file "universal-algebra-basic-algebras"))
                :serial t)
               (:module terms
                :depends-on (technicals
                             fundamental-functions
                             universal-algebra)
                :pathname "src/"
                :components ((:file "terms-package")
                             (:file "terms-terms"))
                :serial t)
               (:module  subalgebras
                :depends-on (technicals
                             fundamental-functions
                             terms
                             universal-algebra
                             functions-and-relations)
                :pathname "src/"
                :components ((:file "subalgebras-package")
                             (:file "subalgebras-subalgebras"))
                :serial t)
               (:module functions-and-relations
                :depends-on (technicals
                             fundamental-functions
                             universal-algebra)
                :pathname "src/"
                :components ((:file "functions-and-relations-package")
                             (:file "functions-and-relations-homomorphisms")
                             (:file "functions-and-relations-congruences"))
                :serial t)
               (:module equations
                :depends-on (technicals
                             terms)
                :pathname "src/"
                :components ((:file "equations-package")
                             (:file "equations-equations"))
                :serial t)
               (:module uacalc-interface
                :depends-on (technicals
                             fundamental-functions
                             universal-algebra
                             subalgebras
                             terms
                             equations
                             functions-and-relations)
                :pathname "src/"
                :components ((:file "uacalc-interface-package")
                             (:file "uacalc-interface-basic-io")
                             (:file "uacalc-interface-io")
                             (:file "uacalc-interface-uab-standard")
                             (:file "uacalc-interface-direct-product")
                             (:file "uacalc-interface-free-algebra")
                             (:file "uacalc-interface-congruence-lattice"))
                :serial t)))

