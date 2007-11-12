(in-package :functions-and-relations)

(defun relation-is-compatible-with-operation-p (relation operation algebra)
  (declare (type relation relation)
           (type symbol operation)
           (type algebra algebra))
  "Returns non-NIL if RELATION is compatible with OPERATION on ALGEBRA."
  (let ((arity (arity-of-function-symbol (signature-of algebra) operation)))
    (and (not (null arity))
         (forall (tuples (n-elemental-subsets (graph relation) arity))
           (let ((first-arguments (mapcar #'first tuples))
                 (second-arguments (mapcar #'second tuples)))
             (in-relation-p relation
                            (apply-operation-in-algebra operation first-arguments algebra)
                            (apply-operation-in-algebra operation second-arguments
                                                        algebra)))))))

(defun relation-is-compatible-with-all-operations-on-algebra-p (relation algebra)
  (declare (type relation relation)
           (type algebra algebra))
  "Returns non-NIL if RELATION is compatible with all operations on ALGEBRA."
  (every #'(lambda (op) (relation-is-compatible-with-operation-p relation op algebra))
         (function-symbols-of (signature-of algebra))))

(defun congruence-relation-p (relation algebra)
  (declare (type relation relation)
           (type algebra algebra))
  "Returns non-NIL if RELATION is a congruence relation on ALGEBRA."
  (and (equivalence-relation-p relation)
       (relation-is-compatible-with-all-operations-on-algebra-p relation algebra)))

(defun all-congruences (algebra)
  (declare (type algebra algebra))
  "Returns set of all congruence relations on algebra."
  (let ((all-congruences ()))
    (loop for partition in (partitions (base-set-of algebra))
	  do (let ((er (equivalence-relation-from-partition partition)))
	       (when (congruence-relation-p er algebra)
		 (push er all-congruences))))
    all-congruences))
