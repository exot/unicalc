(in-package :functions-and-relations)

(defun relation-is-compatible-with-operation (relation operation algebra)
  (declare (type relation relation)
           (type symbol operation)
           (type algebra algebra))
  "Returns non-NIL if RELATION is compatible with OPERATION on ALGEBRA."
  (let ((arity (arity-of-function-symbol (signature-of algebra) operation)))
    (and (not (null arity))
         (error "Not implemented"))))
;;; HERE ;;;
