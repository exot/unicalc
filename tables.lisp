(in-package :fundamental-functions)

(defun function-symbol-of (table)
  "Returns the function symbol of TABLE being an entry in the set
  of interpretations of a given algebra."
  (first table))

(defun implementing-function-of (table)
  "Returns the implementing function of the function symbol of table
  in a given interpretation of a given algebra."
  (second table))

(defun get-arity-of-table (table)
  "DONT USE."
  (length (first (first (rest table)))))

(defun arity-of-function (func)
  "Returns 'dimension' of source of FUNC."
  (length (first (source func))))

(defmacro forall-in-table ((variable table) &body body)
  `(forall (,variable (rest ,table)) ,@body))

(defmacro exists-in-table ((variable table) &body body)
  `(exists (,variable (rest ,table)) ,@body))
