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

(defun equivalent-elements-from-set (set)
  (declare (type standard-set set))
  "Returns full equivalence relation on set."
  (let ((pairs (n-elemental-subsets set 2))
	(self  (mapcar #'(lambda (x) (pair x x)) set)))
    (append pairs (mapcar #'toggle-pair pairs) self)))

(defun equivalence-relation-from-partition (partition)
  (declare (type standard-set partition))
  (let ((base-set (reduce #'union partition))
	(new-graph ()))
    (loop for part in partition
	  do (setf new-graph
		   (union new-graph
			  (equivalent-elements-from-set part))))
    (make-relation base-set base-set new-graph)))

(defun all-in-relation-to-element (relation element)
  (declare (type relation relation)
	   (type t element))
  "Returns all element ELT with ELEMENT RELATION ELT."
  (loop for elt in (source relation)
	when (in-relation-p relation element elt)
	collect elt))

(defun partition-from-equivalence-relation (relation)
  (declare (type relation relation))
  "Returns partition described by the equivalence relation RELATION."
  (cond
    ((not (equivalence-relation-p relation))
     (error 'relations-error :text
	    (format nil "Given relation ~A is not a equivalence relation."
		    relation)))
    (t (let ((new-part ()))
	 (loop for element in (source relation)
	       do (when (not (some #'(lambda (set) (member element set)) new-part))
		    (push (all-in-relation-to-element relation element)
			  new-part)))
	 new-part))))

(defun order-relation-p (relation)
  (and (relation-on-one-set-p relation)
       (reflexiv-p relation)
       (anti-symmetric-p relation)
       (transitive-p relation)))

(defun flip-graph (graph)
  (mapcar #'(lambda (pair) (toggle-pair pair)) graph))

(defun inverse-relation (relation)
  (declare (type relation relation))
  (make-relation (target relation) (source relation) (flip-graph (graph relation))
		 :equal-pred (equal-pred relation)))

(defun relation-product (relation1 relation2)
  (declare (type relation relation1 relation2))
  (let ((new-graph ()))
    (iterate-over-relation-graph relation1 (x y)
      (iterate-over-relation-graph relation2 (a b)
	(when (equal-in-relation relation1 y a)
	    (push (pair x b) new-graph))))
    (make-relation (source relation1) (target relation2) new-graph
		   :equal-pred (equal-pred relation1))))