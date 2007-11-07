(defpackage :UACalc-interface
 (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :functions-and-relations
   :subalgebras)
 (:export
;; uacalc-interface-io
   :uacalc-interface-error
   :uacalc-write-algebra-to-file
   :uacalc-read-algebra-from-file
;; uacalc-interface-uab-standard
;; uacalc-interface-free-algebra
   :calculate-free-algebra
   :symbolize-free-algebra
   :extract-all-equations
   :pprint-all-equations))
