(in-package :fundamental-functions)

;;; stuff for relations

(declaim (inline equal-in-relation))
(defun equal-in-relation (relation x y)
  (declare (type relation relation)
	   (type t x y))
  (set-equal x y :test (equal-pred relation)))

(declaim (inline in-relation-p))
(defun in-relation-p (relation a b)
  (declare (type relation relation)
	   (type t a b))
  (true-value-p (member (pair a b) (graph relation)
			:test (tuple-equal-p (equal-pred relation)))))

(defun relation-on-one-set-p (relation)
  (set-equal (source relation)
	     (target relation)
	     :test (equal-pred relation)))

(defun reflexiv-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (forall (a (source relation)) (in-relation-p relation a a))))

(defun symmetric-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (forall ((x y) (graph relation))
	 (in-relation-p relation y x))))

(defun anti-symmetric-p (relation)
  (declare (type relation relation))
  (forall ((x y) (graph relation))
    (=> (in-relation-p relation y x)
	(equal-in-relation relation x y))))

(defun transitive-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (forall ((x y) (graph relation))
	 (forall ((a b) (graph relation))
	   (=> (equal-in-relation relation y a)
	       (in-relation-p relation x b))))))

(defun equivalence-relation-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (reflexiv-p relation)
       (symmetric-p relation)
       (transitive-p relation)))

(defun order-relation-p (relation)
  (and (relation-on-one-set-p relation)
       (reflexiv-p relation)
       (anti-symmetric-p relation)
       (transitive-p relation)))

(defun inverse-relation (relation)
  (declare (type relation relation))
  (let ((new-graph ()))
    (iterate-over-relation-graph relation (x y)
      (push (pair y x) new-graph))
    (make-relation (target relation) (source relation) new-graph
		   :equal-pred (equal-pred relation))))

(defun relation-product (relation1 relation2)
  (declare (type relation relation1 relation2))
  (let ((new-graph ()))
    (iterate-over-relation-graph relation1 (x y)
      (iterate-over-relation-graph relation2 (a b)
	(when (equal-in-relation relation1 y a)
	    (push (pair x b) new-graph))))
    (make-relation (source relation1) (target relation2) new-graph
		   :equal-pred (equal-pred relation1))))