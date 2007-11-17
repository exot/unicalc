(in-package :test-universal-algebra)

(set-error-handling 'error)

(defparameter *debugging-outputs* ())

(defmacro define-output-test (&body body)
  (let ((name (gensym "NAME")))
    `(push (define-test-case-without-errors ,name ()
	     (format t "~& --- Printing ~A ---~%" ',body)
	     ,@body)
      *debugging-outputs*)))

(defun print-all-debugging-outputs ()
  (run-tests (reverse *debugging-outputs*))
  (values))

;; sets

(define-test-case set-test-1 () t
  (set-equal {1} {1}))

(define-test-case set-test-2 () nil
  (set-equal {1} {{1}}))

(define-test-case set-test-3 () t
  (set-equal {{3} {2 1} 4} {{1 2} 4 {3}}))

(define-test-case set-test-4 () 512
  (card-s (subsets (tuples {1 2 3} 2))))

(define-test-case set-test-5 () 64
  (card-s (tuples (subsets {1 2 3}) 2)))

(define-test-case set-test-6 () 10
  (card-s (n-elemental-subsets {0 1 2 3 4} 3)))

(define-test-case set-test-7 () 5
  (card-s (partitions {0 1 2})))

(define-test-case set-test-8 () 15
  (card-s (partitions {0 1 2 3})))

(define-test-case set-test-9 () 52
  (card-s (partitions {0 1 2 3 4})))

(run-tests '(set-test-1 set-test-2 set-test-3
	     set-test-4 set-test-5 set-test-6
	     set-test-7 set-test-8 set-test-9))

;; math-like-notation

(define-test-case math-test-1 () t
  (forall (x (subsets {1 2 3}))
    (<= 0 (card-s x))))

(define-test-case math-test-2 () nil
  (exists (x (tuples {0 1 2} 2))
    (< 5 (length x))))

(define-test-case math-test-3 () t
  (forall (x '(a b 3 4 c))
    (atom x)))

(run-tests '(math-test-1 math-test-2 math-test-3))

;; relations

(defparameter *A* {a b c d e})
(defparameter *B* {1 2 3 4 5 6 7})

(defparameter *R* nil)

(define-test-case-without-errors rel-test-1 ()
  (setq *R* (make-relation *A* *B* {(a 1) (b 2) (c 3) (d 4) (e 7)})))

(defparameter *C* {1 2})
(defparameter *D* {a b})

(define-test-case-with-errors rel-test-2 ()
  (make-relation *A* *B* {(a 1) (b 2) (c 3 d)}))

(define-test-case-with-errors rel-test-3 ()
  (make-relation *A* *B* {(b 2) (1 a)}))

(run-tests '(rel-test-1 rel-test-2 rel-test-3))

(define-test-case rel-test-4 () nil
  (reflexiv-p *R*))

(define-test-case rel-test-5 () t
  (anti-symmetric-p *R*))

(define-test-case rel-test-6 () nil
  (transitive-p *R*))

(define-test-case rel-test-7 () t
  (equivalence-relation-p (equivalence-relation-from-partition {{1 2} {3} {4 5}})))

(define-test-case rel-test-8 () t
  (set-equal (graph *R*)
	     (graph (inverse-relation (inverse-relation *R*)))))

(define-test-case rel-test-9 () t
  (emptyp-s (graph (relation-product *R* *R*))))

(define-test-case rel-test-a () t
  (set-equal (source (relation-product *R* (inverse-relation *R*)))
	     {a b c d e}))

(run-tests '(rel-test-4 rel-test-5 rel-test-6
	     rel-test-7 rel-test-8 rel-test-9
	     rel-test-a))

;; functions

(defparameter *myfunc* (make-function *A* *B* *R*))

(defparameter *identity* (make-function *C* *D* '((1 a) (2 b))))

(defparameter *constantly-b* (make-function *C* {b} '((1 b) (2 b))))

(define-test-case func-test-1 () t
  (set-equal (target *constantly-b*) {b}))

(define-test-case func-test-2 () t
  (set-equal (source *identity*) *C*))

(define-test-case func-test-3 () nil
  (defparameter *bad-func* (make-function {a b} {c} {(a d)})))

(define-test-case func-test-4 () nil
  (defparameter *bad-func* (make-function {a b} {1} {(2)})))

(define-test-case func-test-5 () 'a
  (apply-function-to-element *identity* 1))

(define-test-case func-test-6 () 4
  (apply-function-to-element *myfunc* 'd))

(define-test-case func-test-7 () '(b b b)
  (apply-function-to-tuple *constantly-b* '(1 2 2)))

(define-test-case func-test-8 () nil
  (apply-function-to-set *myfunc* {2 2 4 2 2 4}))

(define-test-case func-test-9 () t
  (set-equal {2 4} (apply-function-to-set *myfunc* {b b d b b d})))

(define-test-case func-test-a () t
  (set-equal {b} (range *constantly-b*)))

(define-test-case func-test-b () t
  (surjective-p *constantly-b*))

(define-test-case func-test-c () t
  (surjective-p *identity*))

(define-test-case func-test-d () t
  (injective-p *myfunc*))

(define-test-case func-test-e () t
  (bijective-p *identity*))

(define-test-case func-test-f () nil
  (bijective-p *myfunc*))

(run-tests '(func-test-1 func-test-2 func-test-3
	     func-test-4 func-test-5 func-test-6
	     func-test-7 func-test-8 func-test-9
	     func-test-a func-test-b func-test-c
	     func-test-d func-test-e func-test-f))

(define-test-case func-test-g () 4
  (card-s (graph (kernel *constantly-b*))))

(define-test-case func-test-h () 2
  (card-s (graph (kernel *identity*))))

(define-test-case func-test-i () t
  (set-equal {1 2} (inverse-image *constantly-b* {b})))

(define-test-case func-test-j () t
  (set-equal {} (inverse-image *constantly-b* {})))

(define-test-case func-test-k () t
  (set-equal (source *myfunc*) (inverse-image *myfunc* (range *myfunc*))))

(define-test-case func-test-l () t
  (set-equal (inverse-image *constantly-b* {b})
	     (inverse-image-of-element *constantly-b* 'b)))

(define-test-case func-test-m () t
  (set-equal {}
	     (inverse-image (restrict-function-on-source-and-target
			      *constantly-b*
			      (source *constantly-b*)
			      {b c})
			    {c})))

(define-test-case func-test-n () nil
  (inverse-function *constantly-b*))

(define-test-case func-test-o () t
  (set-equal (range *identity*)
	     (source (inverse-function *identity*))))

(run-tests '(func-test-g func-test-h func-test-i
	     func-test-j func-test-k func-test-l
	     func-test-m func-test-n func-test-o))

(define-test-case func-test-p () t
  (= 4 (card-s (ensure-standard-set (all-functions {1 2} {3 4})))))

(define-test-case func-test-q () t
  (= 2 (card-s (ensure-standard-set (all-bijective-functions {1 2} {3 4})))))

(run-tests '(func-test-p func-test-q))

(define-output-test (forall (x (all-functions {1 2 3} {a b})) (print x)))

;; signatures

(defparameter *signature-3* nil)
(define-test-case-without-errors sig-test-1 ()
  (setf *signature-3* (make-signature {+ -} {(+ 2) (- 1)})))

(define-test-case sig-test-2 () nil
  (defparameter *bad-signature* (make-signature {+ -} {(+ 2)})))

(define-test-case sig-test-3 () nil
  (defparameter *bad-signature* (make-signature {+ -} {(+ 2) (+ 3) (- 1)})))

(define-test-case sig-test-4 () nil
  (defparameter *bad-signature* (make-signature {+ -} {(+ 2) (- 1) (p 2)})))

(run-tests '(sig-test-1 sig-test-2 sig-test-3 sig-test-4))

(defparameter *interpretation*  {(+
				  ((1 1) 2)
				  ((1 2) 2)
				  ((2 1) 1)
				  ((2 2) 2))
	                         (-
				  ((1) 2)
				  ((2) 1))})

(defparameter *algebra-C* nil)

(define-test-case-without-errors alg-test-1 ()
  (setq *algebra-C* (make-algebra *C* *signature-3* *interpretation*)))

(defparameter *algebra-D* nil)

(define-test-case-without-errors alg-test-2 ()
  (setq *algebra-D* (make-algebra *D* *signature-3*
				  {(+
				    ((a a) b)
				    ((a b) b)
				    ((b a) a)
				    ((b b) b))
				   (-
				    ((a) b)
				    ((b) a))})))

(run-tests '(alg-test-1 alg-test-2))

(defparameter *base-set* {0 1 2})

(defparameter *signature* (make-signature {+ ^ p} {(+ 1) (^ 3) (p 2)}))

(define-operation p-impl (a b)
  (rem (+ a b) 3))

(defparameter *algebra* (make-algebra *base-set* *signature*
				      {(+ ((0) 0)
					  ((1) 1)
					  ((2) 2))
				       (^ ((0 0 0) 1)
					  ((0 0 1) 2)
					  ((0 0 2) 0)
					  ((0 1 0) 1)
					  ((0 1 1) 1)
					  ((0 1 2) 0)
					  ((0 2 0) 2)
					  ((0 2 1) 2)
					  ((0 2 2) 0)
					  ((1 0 0) 0)
					  ((1 0 1) 2)
					  ((1 0 2) 0)
					  ((1 1 0) 1)
					  ((1 1 1) 2)
					  ((1 1 2) 0)
					  ((1 2 0) 0)
					  ((1 2 1) 0)
					  ((1 2 2) 1)
					  ((2 0 0) 1)
					  ((2 0 1) 1)
					  ((2 0 2) 2)
					  ((2 1 0) 0)
					  ((2 1 1) 1)
					  ((2 1 2) 1)
					  ((2 2 0) 1)
					  ((2 2 1) 0)
					  ((2 2 2) 1))
				       (p p-impl)}))

(define-output-test (print (interpretations-on *algebra*)))
(define-output-test (print (signature-of *algebra*)))
(define-output-test (print (base-set-of *algebra*)))

(define-operation NAND (x y)
  (if (and (plusp x) (plusp y))
        0
        1))

(defparameter *boolean-algebra*
  (make-algebra-from-scratch {0 1} {NAND} {(NAND 2)}
    {(NAND NAND)}))

(define-output-test (print (interpretations-on *boolean-algebra*)))

(define-test-case-with-errors alg-test-3 ()
  (make-algebra {0 1} *signature* *interpretation*))

(define-test-case-with-errors alg-test-4 ()
  (make-algebra *C* *signature-3*
		{(+
		  ((a a) b)
		  ((a b) b)
		  ((b a) a)
		  ((b b) b))
		(-
		 ((a) b)
		 ((b) a))}))

(run-tests '(alg-test-3 alg-test-4))

(define-test-case alg-test-5 () t
  (algebras-of-same-signature-p *algebra-c* *algebra-d*))

(define-test-case alg-test-6 () nil
  (algebras-of-same-signature-p *algebra* *algebra-c*))

(run-tests '(alg-test-5 alg-test-6))

(define-test-case alg-test-7 () 0
  (apply-operation-in-algebra 'p '(1 2) *algebra*))

(define-test-case alg-test-8 () 2
  (apply-operation-in-algebra '+ '(1 2) *algebra-c*))

(define-test-case-with-errors alg-test-9 ()
  (apply-operation-in-algebra '+ '(1 2 3) *algebra-c*))

(define-test-case-with-errors alg-test-a ()
  (apply-operation-in-algebra '* '(1 2) *algebra-c*))

(run-tests '(alg-test-8 alg-test-9 alg-test-a))

;;; terms

(defparameter *global-term-algebra*
  (make-term-algebra '(v0 v1 v2 v3 v4 v5 v6)
                      *signature*))

(define-test-case var-test-1 () t
  (true-value-p (variablep *global-term-algebra* 'v0)))

(define-test-case var-test-2 () nil
  (true-value-p (variablep *global-term-algebra* 'v9)))

(define-test-case var-test-3 () t
  (true-value-p (termp *global-term-algebra* '(+ (p v0 (^ v0 v1 v2))))))

(run-tests '(var-test-1 var-test-2 var-test-3))

(defparameter *signature-2* (make-signature {+ -} {(+ 2) (- 2)}))

(defparameter *algebra-2* (make-algebra {0 1 2} *signature-2*
                                        {(+
					  ((0 0) 0)
					  ((1 0) 1)
					  ((0 1) 1)
					  ((0 2) 2)
					  ((1 1) 2)
					  ((2 0) 2)
					  ((1 2) 0)
					  ((2 1) 0)
					  ((2 2) 1))
					 (-
					  ((0 0) 0)
					  ((1 0) 0)
					  ((0 1) 0)
					  ((0 2) 0)
					  ((2 0) 0)
					  ((1 1) 0)
					  ((1 2) 0)
					  ((2 1) 0)
					  ((2 2) 0))}))

(defparameter *signature-empty* (make-signature {*} {(* 0)}))

(define-operation *-impl () 0) ; this is only a test, you may want to write this directly into the definition

(defparameter *algebra-3* (make-algebra {0 1 2} *signature-empty* '((* *-impl))))

;;; functions and relations

(define-test-case hom-test-1 () t
  (homomorphism-p *identity* *algebra-C* *algebra-D*))

(define-test-case hom-test-2 () nil
  (homomorphism-p *constantly-b* *algebra-C* *algebra-D*))

(define-test-case quasi-hom-test () t
  (quasi-homomorphism-p *constantly-b* *algebra-C*))

(defparameter *image-under-c* (apply-quasihomomorphism-to-algebra *constantly-b*
								  *algebra-c*))

(define-test-case hom-test-3 () t
  (homomorphism-p *constantly-b* *algebra-C* *image-under-c*))

(defparameter *non-quasi-homomorph* (make-function *base-set* *base-set*
                                                   '((0 1) (1 1) (2 2))))

(define-test-case hom-test-4 () nil
  (quasi-homomorphism-p *non-quasi-homomorph* *algebra*))

(run-tests '(hom-test-1 hom-test-2 quasi-hom-test hom-test-4 hom-test-4))

(define-test-case iso-test-1 () t
  (true-value-p (isomorphic-p *algebra-c* *algebra-d*)))

(define-test-case iso-test-2 () nil
  (isomorphic-p *algebra* *algebra-2*))

(run-tests '(iso-test-1 iso-test-2))

(define-test-case cong-test-1 () t
  (let ((congruences (all-congruences-symbolically *algebra-3*)))
    (and (= (length congruences) 5)
	 (every #'(lambda (rel) (congruence-relation-p rel *algebra-3*))
		congruences))))

(run-tests '(cong-test-1))

;;; subalgebras

(define-test-case sub-test-1 () t
  (set-equal (calculate-generating-elements *algebra*)
	     {2}))

(define-test-case sub-test-2 () t
  (set-equal (calculate-generating-elements *algebra-d*)
	     {b}))

(define-test-case sub-test-3 () t
  (set-equal (calculate-generating-elements *algebra-3*)
	     {1 2}))

(define-test-case sub-test-4 () t
  (set-equal (base-set-of (subalgebra-generated-by-elements *algebra-3* {}))
	     {0}))

(define-test-case sub-test-5 () t
  (set-equal (apply-function-to-element
	      (homomorphism-from-assignment *algebra* {1}
					    {(1 1)})
	      0)
	     '(^ 1 1 (^ 1 1 1))))

(run-tests '(sub-test-1 sub-test-2 sub-test-3
	     sub-test-4 sub-test-5))

;;; equations

(define-test-case eqn-test-1 () t
  (equal 2 (evaluate-term-in-algebra *algebra-c* '(+ x y) '((x 1) (y 2)))))

(define-test-case eqn-test-2 () t
  (equal 1 (evaluate-term-in-algebra *algebra-c* '(+ x y) '((x 2) (y 1)))))

(define-test-case eqn-test-3 () nil
  (equation-holds-in-algebra-p *algebra* {x} '((^ x x x) x)))

(define-test-case eqn-test-4 () t
  (equation-holds-in-algebra-p *algebra* {x y} '((p x y) (p y x))))

(run-tests '(eqn-test-1 eqn-test-2 eqn-test-3 eqn-test-4))

;;;

(defparameter *ganters-algebra*
  (make-algebra {0 1 2 3 4}
		(make-signature {+} {(+ 2)})
		{(+
		  ((0 0) 0)
		  ((0 1) 0)
		  ((0 2) 0)
		  ((0 3) 0)
		  ((0 4) 2)
		  ((1 0) 0)
		  ((1 1) 1)
		  ((1 2) 1)
		  ((1 3) 2)
		  ((1 4) 4)
		  ((2 0) 0)
		  ((2 1) 1)
		  ((2 2) 2)
		  ((2 3) 3)
		  ((2 4) 4)
		  ((3 0) 0)
		  ((3 1) 2)
		  ((3 2) 3)
		  ((3 3) 3)
		  ((3 4) 4)
		  ((4 0) 2)
		  ((4 1) 4)
		  ((4 2) 4)
		  ((4 3) 4)
		  ((4 4) 4))}))

(defparameter *free-algebra*
  (make-algebra {0 1 2 3 4}
		(make-signature {+} {(+ 2)})
		{(+
		  ((0 0) 0)
		  ((0 1) 2)
		  ((0 2) 3)
		  ((0 3) 3)
		  ((0 4) 2)
		  ((1 0) 2)
		  ((1 1) 1)
		  ((1 2) 4)
		  ((1 3) 2)
		  ((1 4) 4)
		  ((2 0) 3)
		  ((2 1) 4)
		  ((2 2) 2)
		  ((2 3) 3)
		  ((2 4) 4)
		  ((3 0) 3)
		  ((3 1) 2)
		  ((3 2) 3)
		  ((3 3) 3)
		  ((3 4) 2)
		  ((4 0) 2)
		  ((4 1) 4)
		  ((4 2) 4)
		  ((4 3) 2)
		  ((4 4) 4))}))

;; (defparameter *symbolized-free-algebra*
;;   (symbolize-free-algebra *free-algebra*))

;; (define-output-test (print *free-algebra*))
;; (define-output-test (print *symbolized-free-algebra*))
;; (define-output-test (print (extract-all-equations *symbolized-free-algebra*)))

;; (defun test-case-1 ()
;;   (pprint-all-equations (symbolize-free-algebra *free-algebra*)))

;;;; miscellaneous test cases

;; tests from mike

(define-operation f-impl-1 (x y)
  (aref #2A((0 1 3 1) (0 0 3 0) (1 1 2 0) (2 2 0 2)) x y))

(define-operation g-impl-1 (x y)
  (aref #2A((0 1 3 1) (0 0 3 0) (1 1 3 0) (2 2 0 3)) x y))

(defparameter *signature-4* (make-signature {f} {(f 2)}))

(defparameter *4-f* (make-algebra {0 1 2 3} *signature-4* {(f f-impl-1)}))

(defparameter *4-g* (make-algebra {0 1 2 3} *signature-4* {(f g-impl-1)}))

(define-test-case misc-test-1 () t
  (= (length (all-congruences-symbolically *4-f*))
     (length (all-congruences-symbolically *4-g*))))

(run-tests '(misc-test-1))