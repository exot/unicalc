(defpackage :UACalc-interface
 (:use
   :cl
   :technicals
   :fundamental-functions
   :universal-algebra
   :functions-and-relations
   :subalgebras)
 (:export
   :uacalc-interface-error
   :uacalc-write-algebra-to-file
   :uacalc-read-algebra-from-file))
