(in-package :functions-and-relations)

(defun homomorphism-p (function algebra1 algebra2)
  "Returns non-NIL if FUNCTION is a homomorphism between ALGEBRA1 and ALGEBRA2."
  (and (algebras-of-same-signature-p algebra1 algebra2)
       (set-equal (source function) (base-set-of algebra1))
       (subsetp (target function) (base-set-of algebra2) :test #'equal)
       (let ((signature (signature-of algebra1)))
	       (forall (op (function-symbols-of signature))
		 (forall (x (tuples (base-set-of algebra1)
				    (arity-of-function-symbol signature op)))
		   (equal (apply-function-to-element
			   function
			   (apply-operation-in-algebra op x algebra1))
			  (apply-operation-in-algebra
			   op
			   (apply-function-to-tuple function x)
			   algebra2)))))))

(defun isomorphism-p (function algebra1 algebra2)
  "Returns non-NIL if FUNCTION is a isomorphism between ALGEBRA1 and ALGEBRA2."
  (and (bijective-p function)
       (homomorphism-p function algebra1 algebra2)))

(defun quasi-homomorphism-p (function algebra)
  "Returns non-NIL if FUNCTION is a quasi-homomorphism on ALGEBRA.

That is: f is named quasi-homomorph on algebra A iff for all operations op on 
  A, (a_1,...,a_2), (b_1,...,b_n) in A^n holds

   (f(a_1),...,f(a_n)) = (f(b_1),...,f(b_n)) => f(op(a_1,...,a_n)) = f(op(b_1,...,b_n))"
  (and (set-equal (source function) (base-set-of algebra))
       (let ((signature (signature-of algebra))
	     (base-set  (base-set-of algebra)))
	 (forall (op (function-symbols-of signature))
	   (let ((arity (arity-of-function-symbol signature op)))
	     (forall (x (tuples base-set arity))
	       (forall (y (tuples base-set arity))
		 (=> (equal (apply-function-to-tuple function x)
			    (apply-function-to-tuple function y))
		     (equal (apply-function-to-element
			     function
			     (apply-operation-in-algebra op x algebra))
			    (apply-function-to-element
			     function
			     (apply-operation-in-algebra op y algebra)))))))))))

(defun apply-quasihomomorphism-to-algebra (function algebra)
  "Applies the quasihomomorphism FUNCTION to ALGEBRA yielding the image algebra."
  (cond
     ((not (quasi-homomorphism-p function algebra))
      (error 'no-quasihomomorphism
             :text (format nil "~A is not a quasihomomorphis on ~A" function algebra)))
     (t (let ((new-base-set (apply-function-to-set function (base-set-of algebra)))
              (signature (signature-of algebra))
              (new-interpretations (apply-function-to-interpretations 
                                    function
                                    (interpretations-on algebra))))
          (make-algebra new-base-set signature new-interpretations)))))

(define-simple-condition no-quasihomomorphism)

(defun apply-function-to-interpretations (function interpretations)
  (mapcar #'(lambda (table) (apply-function-to-table function table))
          interpretations))

(defun apply-function-to-table (function table)  ;;; uses internal structure of table
  (let ((new-table ()))
    (iterate-over-value-table table element
      (push (list (apply-function-to-tuple function (all-operands element))
                  (apply-function-to-element function (value-of-element element)))
            new-table))
    (let ((new-table-set (make-set new-table)))
      (push (function-symbol-of table) new-table-set)
      new-table-set)))

(defun kernel (function)
  "Returns kernel of FUNCTION."
  (let ((base-set (source function)))
    (labels ((kernel-element (pair pairs)
               (cond
                 ((null pair) pairs)
                 (t (if (equal (apply-function-to-element function (first pair))
                               (apply-function-to-element function (second pair)))
                      (kernel-element (next-argument base-set pair) (cons pair pairs))
                      (kernel-element (next-argument base-set pair) pairs))))))
      (kernel-element (symbols 2 (first base-set)) ()))))

(define-simple-condition function-error)

(defun inverse-image (function set)
  "Returns the inverse image of SET under FUNCTION."
  (cond
    ((not (subsetp set (target function) :test #'equal))
     (error 'function-error
            :text (format nil "~A is not a subset of ~A" set function)))
    (t (mapcan #'(lambda (element) (inverse-image-of-element function element))
               set))))
  ;; (make-set (loop for i in (source function)
  ;;                 when (member (apply-function-to-element function i) set)
  ;;                 collect i))

(defun inverse-image-of-element (function element)
  (labels ((origin-of-element (argument all-origins)
             (let ((next-element (first (rest (member argument (source function))))))
               (cond
                 ((null argument) all-origins)
                 ((equal element
                         (apply-function-to-element function argument))
                  (origin-of-element next-element
                                     (make-set (cons argument all-origins))))
                 (t 
                  (origin-of-element next-element all-origins))))))
    (origin-of-element (first (source function)) ())))
  ;; (inverse-image function {element})
