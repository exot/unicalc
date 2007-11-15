(in-package :uacalc-interface)

(defun uab-calculate-congruence-lattice (input-project output-project)
  (declare (type uacalc-project input-project output-project))
  "Calculates the congruence lattice of INPUT-PROJECT."
  (with-uab-command-to-file (stream (command-file input-project))
    (uab-commands-to-compute-congruence-lattice stream
                                                (file-name input-project)
                                                (file-name output-project)
                                                (cong-file input-project)
                                                (princ-file input-project)
                                                (meet-irr-file input-project)))
  (run-uab (pure-file-name input-project)))

(defun compute-congruence-lattice-numerically (algebra)
  (declare (type algebra algebra))
  "Returns lattice being isomorphic to the congruence lattice of ALGEBRA."
  (with-algebras
      ((input-project (numerize-algebra algebra)))
      ((algebra-file-name output-project))
    (uab-calculate-congruence-lattice input-project output-project)))

(defmacro compute-part-of-congruences (algebra what)
  (with-gensyms (new-alg input-project output-project)
    `(let ((input-project (algebra-to-project ,algebra))
	   (output-project (make-uacalc-project (generate-unique-pathname))))
       (uab-calculate-congruence-lattice input-project output-project)
       (values (read-from-uacalc-project ',what input-project)))))

(defmacro congruence-computation (algebra what)
  (with-gensyms (numerized-algebra renaming-function all-congs-num)
  `(multiple-value-bind (numerized-algebra renaming-function)
       (numerize-algebra ,algebra)
     (let ((all-congs-num (compute-part-of-congruences numerized-algebra ,what)))
       (mapcar #'equivalence-relation-from-partition
	(apply-function-to-numbers (inverse-function renaming-function)
				   all-congs-num))))))

(defun apply-function-to-numbers (func list)
  (declare (type algebraic-function func)
	   (type (or integer list) list))
  (cond
    ((null list) list)
    ((atom list) (apply-function-to-element func list))
    (t (mapcar #'(lambda (x) (apply-function-to-numbers func x))
	       list))))

(defun all-congruences (algebra)
  (declare (type algebra algebra))
  "Return all congruences of ALGEBRA."
  (congruence-computation algebra cong-file))

(defun all-principal-congruences (algebra)
  (declare (type algebra algebra))
  "Return all principal congruences of ALGEBRA."
  (congruence-computation algebra princ-file))

(defun all-meet-irreducible-congruences (algebra)
  (declare (type algebra algebra))
  "Return all meet irreducible congruences of ALGEBRA."
  (congruence-computation algebra meet-irr-file))