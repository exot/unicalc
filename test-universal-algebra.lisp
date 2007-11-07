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

;; functions and relations

(defparameter *A* (make-set '(a b c d e)))
(defparameter *B* (make-set '(1 2 3 4 5 6 7)))

(defparameter *R* (make-relation *A* *B* '((a 1) (b 2) (c 3) (d 4) (e 7))))

(defparameter *myfunc* (make-function *A* *B* *R*))

(defparameter *C* (make-set '(1 2)))
(defparameter *D* (make-set '(a b)))

(defparameter *identity* (make-function *C* *D* '((1 a) (2 b))))

(defparameter *constantly-b* (make-function *C* '(b) '((1 b) (2 b))))

(defparameter *signature-3* (make-signature '(+ -) '((+ 2) (- 1))))

(defparameter *interpretation*  '((+
				   ((1 1) 2)
				   ((1 2) 2)
				   ((2 1) 1)
				   ((2 2) 2))
				  (-
				   ((1) 2)
				   ((2) 1))))

(defparameter *algebra-C* (make-algebra *C* *signature-3* *interpretation*))

(defparameter *algebra-D* (make-algebra *D* *signature-3*
					'((+
					   ((a a) b)
					   ((a b) b)
					   ((b a) a)
					   ((b b) b))
					  (-
					   ((a) b)
					   ((b) a)))))

(defparameter *base-set* (make-set '(0 1 2)))

(defparameter *signature* (make-signature '(+ ^ p) '((+ 1) (^ 3) (p 2))))

(define-operation p-impl (a b)
  (rem (+ a b) 3))

(defparameter *algebra* (make-algebra *base-set* *signature*
				      '((+ ((0) 0) 
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
					(p p-impl))))

(define-output-test (print (interpretations-on *algebra*)))
(define-output-test (print (signature-of *algebra*)))
(define-output-test (print (base-set-of *algebra*)))

(define-operation NAND (x y)
  (if (and (plusp x) (plusp y))
        0
        1))

(defparameter *boolean-algebra* 
  (make-algebra-from-scratch '(0 1) '(NAND) '((NAND 2))
    '((NAND NAND))))

(define-output-test (print (interpretations-on *boolean-algebra*)))

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

;; subalgebras

(defparameter *signature-2* (make-signature '(+ -) '((+ 2) (- 2))))

(defparameter *algebra-2* (make-algebra '(0 1 2) *signature-2*
                                        '((+
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
                                           ((2 2) 0)))))

(defparameter *signature-empty* (make-signature '(*) '((* 0))))

(define-operation *-impl () 0) ; this is only a test, you may want to write this directly into the definition

(defparameter *algebra-3* (make-algebra '(0 1 2) *signature-empty* '((* *-impl))))

(define-test-case calc-test () t
  (set-equal (calculate-generating-elements *algebra-3*)
	     {1 2}))

(define-test-case hom-test-1 () t
  (homomorphism-p *identity* *algebra-C* *algebra-D*))

(define-test-case hom-test-2 () nil
  (homomorphism-p *constantly-b* *algebra-C* *algebra-D*))

(define-test-case quasi-hom-test () t
  (quasi-homomorphism-p *constantly-b* *algebra-C*))

(defparameter *image-under-c* (apply-quasihomomorphism-to-algebra *constantly-b* *algebra-c*))

(define-test-case hom-test-3 () t
  (homomorphism-p *constantly-b* *algebra-C* *image-under-c*))

(defparameter *non-quasi-homomorph* (make-function *base-set* *base-set*
                                                   '((0 1) (1 1) (2 2))))

(define-test-case hom-test-4 () nil
  (quasi-homomorphism-p *non-quasi-homomorph* *algebra*))

(run-tests '(hom-test-1 hom-test-2 quasi-hom-test hom-test-4 hom-test-4))

(define-output-test (print (subalgebra-generated-by-elements *algebra-3* {})))

;;; equations

(define-test-case eqn-test-1 () t
  (equal 2 (evaluate-term-in-algebra *algebra-c* '(+ x y) '((x 1) (y 2)))))

(define-test-case eqn-test-2 () t
  (equal 1 (evaluate-term-in-algebra *algebra-c* '(+ x y) '((x 2) (y 1)))))

(run-tests '(eqn-test-1 eqn-test-2))

;;; iterating over functions

(define-output-test (forall (x (all-functions {1 2 3} {a b})) (print x)))

;;; isomorphics

(define-test-case iso-test-1 () t
  (when (isomorphic-p *algebra-c* *algebra-d*)
    t))

(define-test-case iso-test-2 () nil
  (isomorphic-p *algebra* *algebra-2*))

(run-tests '(iso-test-1 iso-test-2))

;;; assigment extension

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

(defparameter *symbolized-free-algebra*
  (symbolize-free-algebra *free-algebra*))

(define-output-test (print *free-algebra*))
(define-output-test (print *symbolized-free-algebra*))
(define-output-test (print (extract-all-equations *symbolized-free-algebra*)))

(defun test-case-1 ()
  (pprint-all-equations (symbolize-free-algebra *free-algebra*)))