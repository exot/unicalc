(in-package :test-universal-algebra)

;; functions and relations

(defparameter *A* (make-set '(a b c d e)))
(defparameter *B* (make-set '(1 2 3 4 5 6 7)))

(defparameter *R* (make-relation *A* *B* '((a 1) (b 2) (c 3) (d 4) (e 7))))

(defparameter *myfunc* (make-function *A* *B* *R*))

(defparameter *C* (make-set '(1 2)))
(defparameter *D* (make-set '(a b)))

(defparameter *identity* (make-function *C* *D* '((1 a) (2 b)))) ; is homomorphic between *algebra-C* and *algebra-D*

(defparameter *constantly-b* (make-function *C* '(b) '((1 b) (2 b)))) ; is not homomorphic between *algebra-C* and *algebra-D*

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

(print (interpretations-on *algebra*))
(print (signature-of *algebra*))
(print (base-set-of *algebra*))

(define-operation NAND (x y)
  (if (and (plusp x) (plusp y))
        0
        1))

(defparameter *boolean-algebra* 
  (make-algebra-from-scratch '(0 1) '(NAND) '((NAND 2))
    '((NAND NAND))))

(print (interpretations-on *boolean-algebra*))

;;; terms

(defparameter *global-term-algebra* 
  (make-term-algebra '(v0 v1 v2 v3 v4 v5 v6)
                     *signature*))

(print (variablep *global-term-algebra* 'v0))
(print (variablep *global-term-algebra* 'v9))

(print (termp *global-term-algebra* '(+ (p v0 (^ v0 v1 v2)))))

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

(print (calculate-generating-elements *algebra-3*)) ; should be '(1 2)

(print (homomorphism-p *identity* *algebra-C* *algebra-D*)) ;; T
(print (homomorphism-p *constantly-b* *algebra-C* *algebra-D*)) ;; NIL
(print (quasi-homomorphism-p *constantly-b* *algebra-C*)) ;; T

(defparameter *image-under-c* (apply-quasihomomorphism-to-algebra *constantly-b* *algebra-c*))

(print (homomorphism-p *constantly-b* *algebra-C* *image-under-c*)) ;; T

(defparameter *non-quasi-homomorph* (make-function *base-set* *base-set*
                                                   '((0 1) (1 1) (2 2))))

(print (quasi-homomorphism-p *non-quasi-homomorph* *algebra*)) ;; NIL

(print (subalgebra-generated-by-elements *algebra-3* {})) ;;; calc. subalgebra generated by {}
