(in-package :technicals)

(deftype standard-set () "Type to hold sets" `list)

(defun make-set (set &key (test #'set-equal))
  "Makes set out of SET, i.e. removes all duplicates from set which are SET-EQUAL."
  (declare (type (or null list) set))
  (remove-duplicates set :test test))

(let ((started nil))
  (defun read-set (stream char)
    (declare (ignore char))
    (cond
      ((not started)
       (unwind-protect
	 (progn
	   (setf started t)
	   `',(make-set (read-delimited-list #\} stream t)))
	 (setf started nil)))
      (t (read-delimited-list #\} stream t)))))

(set-macro-character #\{ #'read-set)
(set-macro-character #\} (get-macro-character #\)))

(defgeneric ensure-standard-set (set &key equal)
  (:documentation "Ensures SET to be a standard set, i.e. a tuple
with no two elements being EQUAL"))

(defmethod ensure-standard-set ((set list) &key (equal #'equal))
  (make-set set :test equal))

(defun standard-set-p (set)
  "Predicate to test on STANDARD-SETs"
  (declare (inline standard-set-p))
  (listp set))

;;; next functions

(defun next-argument (base-set argument)
  "Returns next ARGUMENT in BASE-SET of length (LENGTH ARGUMENT)"
  (declare (type standard-set base-set)
	   (type (or list null) argument))
  (cond
    ((null argument) nil)
    (t (let ((rest (rest (member (first argument) base-set)))); all elements after current
	 (cond
	   ((null rest) ; increment next position
	    (let ((next (next-argument base-set (rest argument))))
	      (when next
		(cons (first base-set) next)))) ; and start with first element again
	   (t (cons (first rest) (rest argument))))))))

;;; common operations

(defun card (set)
  "Returns cardinality of set."
  (declare (type standard-set set))
  (length set))

(defun emptyp (set)
  "Returns T if set is empty"
  (declare (type standard-set set))
  (zerop (card set)))

;;; equal predicates

(defun set-equal (set1 set2 &key (test #'equal))
  "Returns T if SET1 and SET2 are equal in sense of sets. Does recursive
  check if needed."
  (cond
    ((and (listp set1)
          (listp set2))
     (and (subsetp set1 set2 :test #'(lambda (x y) (set-equal x y :test test)))
          (subsetp set2 set1 :test #'(lambda (x y) (set-equal x y :test test)))))
    ((and (not (listp set1))
          (not (listp set2)))
     (funcall test set1 set2))
    (t nil)))

(defun set-equal-p (&optional (equal-pred #'equal))
  #'(lambda (x y)
      (set-equal x y :test equal-pred)))

(defun tuple-equal (equal-pred x y)
  "Returns T if X and Y are lists with EQUAL-PRED elements at the same positions"
  (and (listp x)
       (listp y)
       (= (length x)
          (length y))
       (every equal-pred x y)))

(defun tuple-equal-p (&optional (equal-pred #'equal))
  #'(lambda (x y)
      (tuple-equal equal-pred x y)))

(defun tuples (given-set power)
  (declare (type standard-set given-set)
	   (type integer power))
  (let ((set (ensure-standard-set given-set)))
    (cond
      ((or (zerop power)
	   (emptyp set))
       (list nil))
      (t (let ((myset set)
	       (mypower power))
	   (loop for tuple = (technicals::symbols mypower (first myset))
		 then (technicals::next-argument myset tuple)
		 until (null tuple)
		 collect tuple))))))

(defun subsets (given-set &key (equal-pred #'equal))
  (declare (type standard-set given-set))
  (let ((set (ensure-standard-set given-set)))
    (labels ((all-subsets (set)
	       (cond
		 ((null set) (list nil))
		 (t (let ((element (first set))
			  (subsets ()))
		      (let ((shorter-subsets (all-subsets (rest set))))
			(mapc #'(lambda (x)
				  (push (cons element x) subsets)
				  (push x subsets))
			      shorter-subsets))
		      subsets)))))
      (make-set (all-subsets set) :test
		#'(lambda (x y)
		    (set-equal x y :test equal-pred))))))

(defun n-elemental-subsets (set n &key (equal-pred #'equal))
  "Returns set of all N elemental subsets of SET."
  (declare (type standard-set set))
  (cond
    ((= n 0) (list ()))
    ((null set) nil)
    (t (let ((subsets ()))
         (loop for element in set
               do (let ((shorter-subsets (n-elemental-subsets
                                           (remove element set) (1- n))))
                    (mapc #'(lambda (x) (push (cons element x) subsets))
                          shorter-subsets)))
         (make-set subsets :test
		   #'(lambda (x y) (set-equal x y :test equal-pred)))))))

(defun walk-with-element-through-partition (part element &key (equal-pred #'equal))
  (declare (type standard-set part)
           (type t element))
  (let ((new-part (list (union (list (list element)) part))))
    (labels ((add-to-part (head-set el tail-set)
               (cond
                 ((emptyp el) (mapcar #'(lambda (x)
					  (remove-if #'null x))
				      new-part))
                 (t (push (append head-set
                                  (list (union el (list element)
					       :test equal-pred))
                                  tail-set)
                          new-part)
                    (add-to-part (union head-set (list el)
					:test equal-pred)
                                 (first tail-set)
                                 (rest tail-set))))))
      (add-to-part () (first part) (rest part)))))

(defun partitions (set &key (equal-pred #'equal))
  (declare (type standard-set set))
  "Returns all partitions of SET."
  (cond
    ((emptyp set) (list nil))
    (t (let ((minor-partitions (partitions (rest set)))
             (first-element (first set)))
         (reduce #'(lambda (x y)
		     (union x y :test equal-pred))
                 (mapcar #'(lambda (part)
			     (walk-with-element-through-partition part first-element))
                         minor-partitions))))))

(defun print-partition (part &optional (stream t))
  (format stream "~&~A" (first part))
  (loop for sets in (rest part)
	do (format stream "~A" sets))
  (format stream "~%")
  part)