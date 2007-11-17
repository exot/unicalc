(in-package :fundamental-functions)

;;; stuff for relations

(declaim (inline in-relation-p))
(defun in-relation-p (relation a b)
  (declare (type relation relation)
	   (type t a b))
  (true-value-p (set-member-s (pair a b) (graph relation))))

(defun relation-on-one-set-p (relation)
  (declare (type relation relation)
	   (inline relation-on-one-set-p))
  (set-equal (source relation)
	     (target relation)))

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
	(set-equal x y :test (equal-pred (source relation))))))

(defun transitive-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (forall ((x y) (graph relation))
	 (forall ((a b) (graph relation))
	   (=> (set-equal y a :test (equal-pred (source relation)))
	       (in-relation-p relation x b))))))

(defun equivalence-relation-p (relation)
  (declare (type relation relation))
  (and (relation-on-one-set-p relation)
       (reflexiv-p relation)
       (symmetric-p relation)
       (transitive-p relation)))

(defun equivalent-elements-from-set (set)
  (declare (type standard-set set))
  "Returns full equivalence relation on set."
  (tuples set 2))

(defun equivalence-relation-from-partition (partition)
  (declare (type standard-set partition))
  (let ((base-set (mapunion-s #'(lambda (x) x) partition))
	(new-graph {}))
    (loop-over-set part partition
	(setf new-graph
	      (set-union-s new-graph
			   (equivalent-elements-from-set part))))
    (make-relation base-set base-set new-graph)))

(defun all-in-relation-to-element (relation element)
  (declare (type relation relation)
	   (type t element))
  "Returns all element ELT with ELEMENT RELATION ELT."
  (let ((equiv-elts ()))
    (loop-over-set elt (source relation)
      (when (in-relation-p relation element elt)
	(push elt equiv-elts)))
    (make-set equiv-elts :test (equal-pred (source relation)))))

(defun partition-from-equivalence-relation (relation)
  (declare (type relation relation))
  "Returns partition described by the equivalence relation RELATION."
  (cond
    ((not (equivalence-relation-p relation))
     (error 'relations-error :text
	    (format nil "Given relation ~A is not a equivalence relation."
		    relation)))
    (t (let ((new-part ()))
	 (loop-over-set element (source relation)
	   (when (not (exists (parts new-part) (set-member-s element parts)))
	     (push (all-in-relation-to-element relation element)
		   new-part)))
	 (make-set new-part :test (equal-pred (source relation)))))))

(defun order-relation-p (relation)
  (and (relation-on-one-set-p relation)
       (reflexiv-p relation)
       (anti-symmetric-p relation)
       (transitive-p relation)))

(defun flip-graph (graph)
  (declare (type standard-set graph))
  (mapset #'(lambda (pair) (toggle-pair pair)) graph))

(defun inverse-relation (relation)
  (declare (type relation relation))
  (make-relation (target relation) (source relation) (flip-graph (graph relation))))

(defun relation-product (relation1 relation2)
  (declare (type relation relation1 relation2))
  (let ((new-graph ()))
    (iterate-over-relation-graph relation1 (x y)
      (iterate-over-relation-graph relation2 (a b)
	(when (set-equal y a :test (equal-pred (source relation1)))
	    (push (pair x b) new-graph))))
    (make-relation (source relation1) (target relation2)
		   (make-set new-graph :test
			     (tuple-equal-p (equal-pred (source relation1)))))))
