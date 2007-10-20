(defpackage :test-universal-algebra
  (:use 
   :cl
   :universal-algebra
   :terms
   :subalgebras))

(in-package :test-universal-algebra)

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

(pprint (interpretations-on *algebra*))
(pprint (signature-of *algebra*))
(pprint (base-set-of *algebra*))

(define-operation NAND (x y)
  (if (and (plusp x) (plusp y))
        0
        1))

(defparameter *boolean-algebra* 
  (make-algebra-from-scratch '(0 1) '(NAND) '((NAND 2))
    '((NAND NAND))))

(pprint (interpretations-on *boolean-algebra*))

;;; terms

(defparameter *global-term-algebra* 
  (make-term-algebra '(v0 v1 v2 v3 v4 v5 v6)
                     *signature*))

(pprint (variablep *global-term-algebra* 'v0))
(pprint (variablep *global-term-algebra* 'v9))

(pprint (termp *global-term-algebra* '(+ (p v0 (^ v0 v1 v2)))))

;;; subalgebras

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

(defparameter *algebra-3* (make-algebra '(0 1 2) *signature-empty* '((* (() 0)))))

(print (calculate-generating-elements *algebra-3*)) ; should be '(1 2)