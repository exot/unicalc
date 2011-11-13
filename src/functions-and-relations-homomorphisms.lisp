(in-package :functions-and-relations)

(defun op-homomorphic-p (function operation algebra1 algebra2)
  (declare (type algebra algebra1 algebra2)
           (type algebraic-function function)
           (type symbol operation))
  (forall (x (tuples (base-set-of algebra1)
                     (arity-of-function-symbol (signature-of algebra1)
                                               operation)))
             (equal (apply-function-to-element
                     function
                     (apply-operation-in-algebra operation x algebra1))
                    (apply-operation-in-algebra
                     operation
                     (apply-function-to-tuple function x)
                     algebra2))))

(defun op-homomorphism-p (function operation algebra1 algebra2)
  (declare (type algebra algebra1 algebra2)
           (type algebraic-function function)
           (type symbol operation))
  "Returns non-NIL if FUNCTION is a OPERATION-homomorphism between ALGEBRA1 and ALGEBRA2."
   (and (algebras-of-same-signature-p algebra1 algebra2)
        (set-equal (source function) (base-set-of algebra1))
        (subsetp-s (target function) (base-set-of algebra2))
        (op-homomorphic-p function operation algebra1 algebra2)))

(defun homomorphism-p (function algebra1 algebra2)
  (declare (type algebraic-function function)
           (type algebra algebra1 algebra2))
  "Returns non-NIL if FUNCTION is a homomorphism between ALGEBRA1 and
  ALGEBRA2."
  (and (algebras-of-same-signature-p algebra1 algebra2)
       (set-equal (source function) (base-set-of algebra1))
       (subsetp-s (target function) (base-set-of algebra2))
       (let ((signature (signature-of algebra1)))
	       (forall (op (function-symbols-of signature))
                 (op-homomorphic-p function op algebra1 algebra2)))))

(defun isomorphism-p (function algebra1 algebra2)
  (declare (type algebraic-function function)
           (type algebra algebra1 algebra2))
  "Returns non-NIL if FUNCTION is a isomorphism between ALGEBRA1 and ALGEBRA2."
  (and (bijective-p function)
       (homomorphism-p function algebra1 algebra2)))

(defun quasi-homomorphism-p (function algebra)
  (declare (type algebraic-function function)
           (type algebra algebra))
  "Returns non-NIL if FUNCTION is a quasi-homomorphism on ALGEBRA.

That is: f is named quasi-homomorph on algebra A iff for all operations op on
  A, (a_1,...,a_2), (b_1,...,b_n) in A^n holds

   (f(a_1),...,f(a_n)) = (f(b_1),...,f(b_n)) =>
     f(op(a_1,...,a_n)) = f(op(b_1,...,b_n))"
  (and (set-equal (source function) (base-set-of algebra))
       (let ((signature (signature-of algebra))
	     (base-set  (base-set-of algebra)))
	 (forall (op (function-symbols-of signature))
	   (let ((arity (arity-of-function-symbol signature op)))
	     (forall (x (tuples base-set arity))
	       (forall (y (tuples base-set arity))
		 (=> (set-equal (apply-function-to-tuple function x)
				(apply-function-to-tuple function y))
		     (set-equal (apply-function-to-element
				 function
				 (apply-operation-in-algebra op x algebra))
				(apply-function-to-element
				 function
				 (apply-operation-in-algebra op y algebra)))))))))))

(defun apply-quasihomomorphism-to-algebra (function algebra)
  (declare (type algebraic-function function)
           (type algebra algebra))
  "Applies the quasihomomorphism FUNCTION to ALGEBRA yielding the image
  algebra."
  (cond
    ((not (quasi-homomorphism-p function algebra))
      (error 'no-quasihomomorphism
             :text (format nil "~A is not a quasihomomorphis on ~A"
                           function algebra)))
     (t (let ((new-base-set (apply-function-to-set function
                                                   (base-set-of algebra)))
              (signature (signature-of algebra))
              (new-interpretations (apply-function-to-interpretations
                                    function
                                    (interpretations-on algebra))))
          (make-algebra new-base-set signature new-interpretations)))))

(define-simple-condition no-quasihomomorphism)

(defun apply-function-to-interpretations (function interpretations)
  (declare (type algebraic-function function)
           (type standard-set interpretations))
  "Applies FUNCTION to INTERPRETATIONS consisting of pairs of function symbols
   and implementing functions."
  (mapset #'(lambda (table) (apply-function-to-table function table))
          interpretations))

(defun apply-function-to-table (function table)
  "Applies FUNCTION to TABLE being a pair of function symbols and implementing
   functions."
  (declare (type algebraic-function function)
	   (type table table))
  (let ((new-graph ()))
    (iterate-over-function-graph (implementing-function-of table) element
      (push (list (apply-function-to-tuple function (all-operands element))
                  (apply-function-to-element function
                                             (value-of-element element)))
            new-graph))
    (let ((new-source (tuples (apply-function-to-set function
                                                     (source function))
                              (arity-of-function
                                (implementing-function-of table))))
          (new-target (apply-function-to-set function (source function))))
      (list (function-symbol-of table)
            (make-function new-source new-target
			   (make-set new-graph))))))

(defun all-homomorphisms (algebra1 algebra2)
  (declare (type algebra algebra1 algebra2))
  "Returns lazy-set of all homomorphisms between ALGEBRA1 and ALGEBRA2."
  (all-functions-with-predicate (base-set-of algebra1)
                                (base-set-of algebra2)
                                #'(lambda (x)
                                    (homomorphism-p x algebra1 algebra2))))

(defun all-isomorphisms (algebra1 algebra2)
  (declare (type algebra algebra1 algebra2))
  "Returns lazy set of all isomorphisms between ALGEBRA1 and ALGEBRA2."
  (let ((all-bijective-functions (all-bijective-functions (base-set-of algebra1)
							  (base-set-of algebra2))))
    (labels ((next-function ()
	       (let ((next (funcall (next all-bijective-functions))))
		 (cond
		   ((not next) nil)
		   ((not (homomorphism-p next algebra1 algebra2))
		    (next-function))
		   (t next)))))
      (define-lazy-set #'next-function))))

(defun isomorphic-p (algebra1 algebra2)
  (declare (type algebra algebra1 algebra2))
  "Returns isomorphmis between ALGEBRA1 and ALGEBRA2 if existent,
   NIL otherwise."
  (when (and (= (card-s (base-set-of algebra1))
		(card-s (base-set-of algebra2)))
	     (algebras-of-same-signature-p algebra1 algebra2))
    (let ((isos (all-isomorphisms algebra1 algebra2)))
      (funcall (next isos)))))