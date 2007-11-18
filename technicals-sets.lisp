(in-package :technicals)

(defclass standard-set ()
  ((contents   :type list     :accessor contents
	       :initarg :contents :initform ())
   (equal-pred :type function :accessor equal-pred
	       :initarg :equal-pred :initform #'set-equal)))

(defgeneric set-to-list (set)
  (:documentation "Returns contents of SET as a list"))

(defmethod set-to-list ((set list))
  set)

(defmethod set-to-list ((set standard-set))
  (contents set))

(defun set-equal (set1 set2 &key (test #'equal))
  "Returns T if SET1 and SET2 are equal in sense of sets. Does no recursive
  check. Preserves ordering."
   (cond
     ((and (standard-set-p set1)
	   (standard-set-p set2))
      (and (subsetp-s set1 set2 :test (set-equal-p test))
	   (subsetp-s set2 set1 :test (set-equal-p test))))
     ((and (not (standard-set-p set1))
	   (not (standard-set-p set2)))
      (funcall test set1 set2))
     (t nil)))

(defgeneric make-set (set &key test)
  (:documentation "Makes set out of SET, i.e. removes all duplicates from set which are
equal by TEST."))

(defmethod make-set ((set list) &key (test #'set-equal))
  (make-instance 'standard-set
		 :contents (remove-duplicates set :test test)
		 :equal-pred test))

(defmethod make-set ((set standard-set) &key (test #'set-equal))
  (make-set (list set) :test test))

(defparameter *empty-set* (make-set ()))

(defmethod print-object ((obj standard-set) stream)
  (format stream "~<{~;~:I~{~S~^,~:_~}~;}~:>" (list (contents obj))))

(defun %listify (tree)
  (cond
    ((null tree) tree)
    ((atom tree) `(quote ,tree))
    (t (cond
	 ((or (eq (first tree) 'make-set)
	      (eq (first tree) 'list))
	  `(,(first tree) ,@(mapcar #'%listify (rest tree))))
	 (t `(list ,@(mapcar #'%listify tree)))))))

(let ((started nil))
  (defun read-set (stream char)
    (declare (ignore char))
    (cond
      (started
       `(make-set ,(read-delimited-list #\} stream t)))
      (t
       (unwind-protect
	    (progn
	      (setf started t)
	      `(make-set ,(%listify (read-delimited-list #\} stream t))))
	 (setf started nil))))))

(set-macro-character #\{ #'read-set)
(set-macro-character #\} (get-macro-character #\)))

(defgeneric ensure-standard-set (set &key equal)
   (:documentation "Ensures SET to be a standard set, i.e. a tuple
 with no two elements being EQUAL"))

(defmethod ensure-standard-set ((set list) &key (equal #'set-equal))
   (make-set set :test equal))

(defmethod ensure-standard-set ((set standard-set) &key (equal (equal-pred set)))
  (declare (ignore equal))
  set)

(defgeneric standard-set-p (set)
  (:documentation "Predicate to test on STANDARD-SETs"))

(defmethod standard-set-p ((set standard-set))
  (declare (ignore set))
  t)

(defmethod standard-set-p ((set t))
  (declare (ignore set))
  nil)

;;; common operations

(defun card-s (set)
  "Returns cardinality of set. Preserves ordering."
  (declare (type standard-set set))
  (length (contents set)))

(defun emptyp-s (set)
  "Returns T if set is empty. Preserves ordering."
  (declare (type standard-set set))
  (null (contents set)))

(defun first-s (set)
  (declare (type standard-set set))
  "Returns 'first' element of SET. Preserves ordering. Returns NIL if SET is empty."
  (first (contents set)))

(defun rest-s (set)
  (declare (type standard-set set))
  "Returns 'rest' of SET. Ensures ordering."
  (let ((rest (rest (contents set))))
    (cond
      ((null rest) *empty-set*)
      (t (make-set rest :test (equal-pred set))))))


(defun set-equal-p (&optional (equal-pred #'equal))
  #'(lambda (x y)
      (set-equal x y :test equal-pred)))

(defun set-member-s (elt set &key (test (equal-pred set)))
  (declare (type t elt)
	   (type standard-set set))
  "Returns nonempty set of all elementes 'after' ELT if ELT is in SET, NIL otherwise.
Preserves ordering."
  (let ((result (member elt (contents set) :test test)))
    (cond
      ((null result) nil)
      (t (make-set result :test test)))))

(defun subsetp-s (set1 set2 &key (test (equal-pred set2)))
  (declare (type standard-set set1 set2))
  "Returns T if SET1 is subset of SET2. Preserves ordering"
  (subsetp (contents set1) (contents set2) :test test))

(defun set-union-s (set1 set2 &key (test #'(lambda (x y)
					     (or (funcall (equal-pred set1) x y)
						 (funcall (equal-pred set2) x y)))))
  (declare (type standard-set set1 set2))
  "Returns union of set1 and set2. Resulting set is order from left to right, duplicates
removed from left to right."
  (make-set (append (contents set1)
		    (contents set2))
	    :test test))

(defun add-element-s (elt set1 &key (test (equal-pred set1)))
  (declare (type standard-set set1)
	   (type t elt))
  "Adds ELT to the 'front' of SET1 if (NOT (SET-MEMBER-S ELT SET1))."
  (cond
    ((set-member-s elt set1 :test test) set1)
    (t (make-set (cons elt (contents set1)) :test test))))

(defun set-difference-s (set1 set2 &key (test (equal-pred set1)))
  (declare (type standard-set set1 set2))
  "Returns difference of set1 and set2. Does not preserve ordering."
  (make-set (set-difference (contents set1)
			    (contents set2)
			    :test test)
	    :test test))

(defun remove-element-s (elt set1 &key (test (equal-pred set1)))
  (declare (type standard-set set1)
	   (type t elt))
  "Removes ELT from SET1 if present. Preserves ordering."
  (make-set (remove elt (contents set1) :test test) :test test))

(defun set-intersection-s (set1 set2 &key (test #'equal))
  (declare (type standard-set set1 set2))
  "Returns intersection of SET1 and SET2. Does not preserve ordering."
  (make-set (intersection (contents set1)
			  (contents set2)
			  :test test)
	    :test test))

(defun map-on-elements (pred set1 &rest more-sets)
  (declare (type function pred)
	   (type standard-set set1)
	   (type list more-sets))
  (apply #'mapc pred (mapcar #'contents (cons set1 more-sets)))
  (values))

(defun mapset (pred set1 &rest more-sets)
  (declare (type function pred)
	   (type standard-set set1)
	   (type list more-sets))
  (make-set (apply #'mapcar pred (mapcar #'contents (cons set1 more-sets)))
	    :test (equal-pred set1)))

(defun mapunion-s (fun set &key (test (equal-pred set)))
  (declare (type function fun)
           (type standard-set set))
  "Maps fun to list and unions the result."
  (reduce #'(lambda (x y)
	      (set-union-s x y :test test))
	  (mapcar fun (contents set))))

(defun position-s (elt set)
  (declare (type t elt)
	   (type standard-set set))
  "Returns 'position' of ELT in SET."
  (position elt (contents set) :test (equal-pred set)))

(defun elt-at-position-s (pos set)
  (declare (type integer pos)
	   (type standard-set set))
  "Returns element in SET at 'position' POS."
  (nth pos (contents set)))

(defun assoc-s (elt set)
  (declare (type t elt)
	   (type standard-set set))
  (assoc elt (contents set) :test (equal-pred set)))

(defun singelton-s (elt)
  (declare (type t elt))
  "Returns singelton set {ELT}."
  (make-set (list elt)))

(defun min-s (set)
  (declare (type standard-set set))
  (apply #'min (contents set)))

(defun max-s (set)
  (declare (type standard-set set))
  (apply #'max (contents set)))

(defmacro loop-over-set (element set &body body)
  (with-gensyms (elt)
    `(loop for elt in (contents ,set)
           do (destructuring-bind (,element) (list elt)
		,@body))))

;;; next functions

(defun next-argument (list argument)
  "Returns next ARGUMENT in LIST of length (LENGTH ARGUMENT)"
  (declare (type list list)
	   (type list argument))
  (cond
    ((null argument) nil)
    (t (let ((rest (rest (member (first argument) list))))
	 (cond
	   ((null rest) ; increment next position
	    (let ((next (next-argument list (rest argument))))
	      (when next
		(cons (first list) next)))) ; and start with first element again
	   (t (cons (first rest) (rest argument))))))))

;;; equal predicates

(defun tuple-p (tuple)
  (listp tuple))

(defun tuple-equal (equal-pred x y)
  "Returns T if X and Y are lists with EQUAL-PRED elements at the same positions"
  (and (tuple-p x)
       (tuple-p y)
       (= (length x)
          (length y))
       (every equal-pred x y)))

(defun tuple-equal-p (&optional (equal-pred #'set-equal))
  #'(lambda (x y)
      (tuple-equal equal-pred x y)))

(defun tuples (given-set power)
  (declare (type integer power))
  "Return all tuples over GIVEN-SET of length POWER."
  (make-set (%tuples (contents (ensure-standard-set given-set)) power)))

(defun %tuples (given-set-list power)
  (declare (type list given-set-list)
	   (type integer power))
  (cond
    ((or (zerop power)
	 (null given-set-list))
     (list nil)) ; empty tuple
    (t (let ((mylist given-set-list)
	     (mypower power))
	 (loop for tuple = (technicals::symbols mypower (first mylist))
	       then (technicals::next-argument mylist tuple)
	       until (null tuple)
	       collect tuple)))))

(defun subsets (given-set &key (equal-pred #'set-equal))
  "Returns all subsets of GIVEN-SET."
  (let ((set (ensure-standard-set given-set :equal equal-pred)))
    (labels ((all-subsets (set)
	       (cond
		 ((emptyp-s set) (singelton-s *empty-set*))
		 (t (let ((element (first-s set))
			  (subsets ()))
		      (let ((shorter-subsets (all-subsets (rest-s set))))
			(loop-over-set x shorter-subsets
			  (push (add-element-s element x) subsets)
			  (push x subsets))
			(make-set subsets :test (set-equal-p equal-pred))))))))
      (all-subsets set))))

(defun n-elemental-subsets (set n &key (equal-pred #'equal))
  "Returns set of all N elemental subsets of SET."
  (declare (type standard-set set))
  (cond
    ((<= n 0) (singelton-s *empty-set*))
    ((emptyp-s set) *empty-set*)
    (t (let ((subsets ()))
         (loop-over-set element set
	   (let ((shorter-subsets (n-elemental-subsets
				   (remove-element-s element set) (1- n))))
	     (loop-over-set x shorter-subsets
	       (push (add-element-s element x) subsets))))
         (make-set subsets :test (set-equal-p equal-pred))))))

(defun extend-partition (part element)
  (declare (type standard-set part)
           (type t element))
  (cond
    ((set-equal part (make-set (make-set '())))
     (list (singelton-s (singelton-s element))))
    (t (let ((new-partitions ()))
	 (loop-over-set elt part
	   (push (add-element-s (add-element-s element elt)
				(remove-element-s elt part))
		 new-partitions))
	 (push (add-element-s (singelton-s element) part)
	       new-partitions)))))

(defun partitions (set &key (equal-pred #'equal))
  (declare (type standard-set set))
  "Returns all partitions of SET."
  (cond
    ((emptyp-s set) (singelton-s (singelton-s *empty-set*)))
    (t (let ((minor-partitions (partitions (rest-s set)))
             (first-element (first-s set)))
         (make-set
	  (mapcan #'(lambda (part)
		      (extend-partition part first-element))
		  (contents minor-partitions))
	  :test equal-pred)))))

(defun print-partition (part &optional (stream t))
  (format stream "~&|")
  (loop-over-set set part
    (format stream "~A" (first-s set))
    (loop-over-set elts (rest-s set)
      (format stream "-~A" elts))
    (format stream "|"))
  (format stream "~%"))