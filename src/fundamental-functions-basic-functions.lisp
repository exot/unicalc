(in-package :fundamental-functions)

;; doing something with functions

(defun apply-function-to-element (function element)
  "Applies FUNCTION to ELEMENT."
  (declare (type algebraic-function function))
  (or (value-of-function function element)
      (error 'function-error
             :text (format nil "Cannot apply ~A to ~A" function element))))

(defun apply-function-to-tuple (function tuple)
  "Applies FUNCTION to elements of SET, not neccessarily returning a set."
  (declare (type algebraic-function function)
	   (type list tuple))
  (mapcar #'(lambda (element)
              (apply-function-to-element function element))
          tuple))

(defun apply-function-to-set (function set)
  "Applies FUNCTION to elements of SET."
  (declare (type algebraic-function function)
	   (type standard-set set))
  (mapset #'(lambda (x) (apply-function-to-element function x)) set))

(defun range (function)
  "Returns the range of FUNCTION."
  (declare (type algebraic-function function))
  (mapset #'second (graph function)))

(defun surjective-p (function)
  "Tests whether FUNCTION is surjective or not."
  (declare (type algebraic-function function))
  (set-equal (target function)
             (range function)))

(defun injective-p (function)
  "Tests whether FUNCTION is injective or not."
  (declare (type algebraic-function function))
  (= (card-s (source function))
     (card-s (range function))))

(defun bijective-p (function)
  "Tests whether FUNCTION is bijective or not."
  (declare (type algebraic-function function))
  (and (injective-p function)
       (surjective-p function)))

(defun kernel (function)
  "Returns kernel of FUNCTION."
  (declare (type algebraic-function function))
  (let ((base-set (set-to-list (source function))))
    (labels ((kernel-element (pair pairs)
               (cond
                 ((null pair) pairs)
                 (t (if (set-equal (apply-function-to-element function
                                                              (first pair))
                                   (apply-function-to-element function
                                                              (second pair)))
                      (kernel-element (next-argument base-set pair)
                                      (cons pair pairs))
                      (kernel-element (next-argument base-set pair) pairs))))))
      (make-relation (make-set base-set)
		     (make-set base-set)
                     (make-set (kernel-element (symbols 2 (first base-set)) ()))))))

(defun inverse-image (function set)
  "Returns the inverse image of SET under FUNCTION."
  (declare (type algebraic-function function)
	   (type standard-set set))
  (cond
    ((not (subsetp-s set (target function)))
     (error 'function-error
            :text (format nil "~A is not a subset of ~A" set function)))
    (t (let ((new-set ()))
	 (loop-over-set element (source function)
	   (when (set-member-s (apply-function-to-element function element) set)
	     (push element new-set)))
	 (make-set new-set)))))

(defun inverse-image-of-element (function element)
  (declare (type algebraic-function function))
  (inverse-image function (singelton-s element)))

(defun restrict-function-on-source-and-target (function new-source new-target)
  "Restricts FUNCTION being a function on NEW-SOURCE\\times NEW-TARGET."
  (declare (type algebraic-function function)
	   (type standard-set new-source new-target))
  (flet ((calc-new-func-graph ()
            (let ((new-graph ()))
              (iterate-over-function-graph function element
                (when (set-member-s (all-operands element) new-source)
                  (push element new-graph)))
             new-graph)))
    (let ((new-graph (calc-new-func-graph)))
      (make-function new-source
                     new-target
                     new-graph))))

(defun restrict-function-on-target (function new-target)
  "Returns algebraic function begin FUNCTION with target restricted to
NEW-TARGET"
  (declare (type algebraic-function function)
	   (type standard-set new-target))
  (make-function (source function)
                 new-target
                 (graph function)))

(defun restrict-function-on-source (function new-source)
  "Returns algebraic functions as FUNCTION restricted to NEW-SOURCE."
  (declare (type algebraic-function function)
	   (type standard-set new-source))
  (restrict-function-on-source-and-target function new-source
					  (target function)))

(defun inverse-function (func)
  (declare (type algebraic-function func))
  (cond
    ((not (bijective-p func))
     (error 'function-error :text
	    (format nil "Given function ~A is not bijective and cannot be inverted."
		    func)))
    (t (make-function (target func)
		      (source func)
		      (flip-graph (graph func))))))

;;; for iteration over all assignments

(defun next-assignment (value-list assignment)
  "Returns next assignment in VALUE-SET after ASSIGNMENT being a tuple of
pairs, or NIL if ASSIGNMENT is last."
  (declare (type list value-list)
	   (type list assignment))
  (let ((argument (mapcar #'second assignment))
        (elements (mapcar #'first assignment)))
    (let ((next-argument (next-argument value-list argument)))
      (when next-argument
        (mapcar #'(lambda (x y) (list x y)) elements next-argument)))))

(defun all-assignments (variables value-set)
  "Returns lazy set consiting of all assignments of VARIABLES to value from
VALUE-SET."
  (declare (type standard-set variables value-set))
  (let* ((first-assign (symbols (card-s variables) (first-s value-set)))
	 (variable-list (let ((list ()))
			  (map-on-elements #'(lambda (x) (push x list)) variables)
			  list))
         (assign (mapcar #'pair variable-list first-assign))
	 (value-list (set-to-list value-set)))
    (flet ((new-assignment ()
             (let ((current-assign assign))
               (setf assign (next-assignment value-list assign))
               current-assign)))
      (define-lazy-set #'new-assignment))))

(defun all-functions (source target)
  "Returns lazy set of all functions between SOURCE and TARGET."
  (declare (type standard-set source target))
  (let ((assignments (all-assignments source target)))
    (flet ((next-function ()
	     (let ((next-assign (funcall (technicals::next assignments))))
	       (when next-assign
		 (make-function source target next-assign)))))
      (define-lazy-set #'next-function))))

(defun all-functions-with-predicate (source target predicate)
  "Returns lazy set of all function between SEOURCE and TARGET fulfilling
PREDICATE."
  (declare (type standard-set source target)
	   (type function predicate))
  (let ((functions (all-functions source target)))
    (labels ((next-function ()
	       (let ((next-fun (funcall (next functions))))
		 (cond
		   ((not next-fun) nil)
		   ((funcall predicate next-fun)
		    next-fun)
		   (t (next-function))))))
      (define-lazy-set #'next-function))))

(defun all-bijective-assignments (source target)
  (declare (type standard-set source target))
  (cond
    ((not (= (card-s source)
	     (card-s target)))
     (define-lazy-set (constantly nil)))
    (t (let ((counter 0)
	     (all (factorial (card-s source)))
	     (source-elt-list (set-to-list source))
	     (target-elt-list (set-to-list target)))
	 (flet ((next-permutation ()
		  (cond
		    ((>= counter all) nil)
		    (t (incf counter)
		       (let ((nth-permutation (nth-permutation (1- counter)
							       source-elt-list)))
			 (mapcar #'pair nth-permutation target-elt-list))))))
	   (define-lazy-set #'next-permutation))))))

(defun all-bijective-functions (source target)
  (declare (type standard-set source target))
  "Returns LAZY-SET of all bijective functions between SOURCE and TARGET."
  (let ((all-assigns (all-bijective-assignments source target)))
    (flet ((next-function ()
	     (let ((next (funcall (next all-assigns))))
	       (cond
		 ((not next) nil)
		 (t (make-function source target (make-set next)
				   :untested t))))))
      (define-lazy-set #'next-function))))
