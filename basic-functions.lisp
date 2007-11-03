(in-package :fundamental-functions)

;;; iterating over function graphs (needed)

(defmacro iterate-over-function-graph (function element &body body)
  "Iterates with ELEMENT over all elements in (GRAPH FUNCTION)"
  (let ((graph (gensym "GRAPH"))
        (pair (gensym "PAIR")))
    `(let ((,graph (graph ,function)))
      (loop for ,pair in ,graph
            do (let ((,element ,pair))
                 ,@body)))))

(defun value-of-element (element)
  "Returns value of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (second element))

(defun all-operands (element)
  "Returns all operands of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (first element))

(defun nth-operand (element n)
  "Returns nth operand of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (nth n (all-operands element)))

;; doing something with functions

(defun apply-function-to-element (function element)
  "Applies FUNCTION to ELEMENT."
  (or (second (assoc element (graph function) :test (equal-pred function)))
      (error 'function-error
             :text (format nil "Cannot apply ~A to ~A" function element))))

(defun apply-function-to-tuple (function tuple)
  "Applies FUNCTION to elements of SET, not neccessarily returning a set."
  (mapcar #'(lambda (element)
              (apply-function-to-element function element))
          tuple))

(defun apply-function-to-set (function set)
  "Applies FUNCTION to elements of SET."
  (make-set (apply-function-to-tuple function set)))

(defun range (function)
  (remove-duplicates (mapcar #'second (graph function)) :test (equal-pred function)))

(defun surjective-p (function)
  (set-equal (target function)
             (range function)
	     :test (equal-pred function)))

(defun injective-p (function)
  (= (length (source function))
     (length (range function))))

(defun bijective-p (function)
  (and (injective-p function)
       (surjective-p function)))

(defun kernel (function)
  "Returns kernel of FUNCTION."
  (let ((base-set (source function)))
    (labels ((kernel-element (pair pairs)
               (cond
                 ((null pair) pairs)
                 (t (if (set-equal (apply-function-to-element function (first pair)) ;;; !!!
                                   (apply-function-to-element function (second pair)))
                      (kernel-element (next-argument base-set pair) (cons pair pairs))
                      (kernel-element (next-argument base-set pair) pairs))))))
      (make-relation base-set base-set (kernel-element (symbols 2 (first base-set)) ())))))

(defun inverse-image (function set &key (equal-pred #'equal))
  "Returns the inverse image of SET under FUNCTION."
  (cond
    ((not (subsetp set (target function) :test (equal-pred function)))
     (error 'function-error
            :text (format nil "~A is not a subset of ~A" set function)))
    (t (mapcan #'(lambda (element) (inverse-image-of-element function element
							     :equal-pred equal-pred))
               set))))
  ;; (make-set (loop for i in (source function)
  ;;                 when (member (apply-function-to-element function i) set)
  ;;                 collect i))

(defun inverse-image-of-element (function element &key (equal-pred #'equal))
  (labels ((origin-of-element (argument all-origins)
             (let ((next-element (first (rest (member argument (source function)
						      :test (equal-pred function))))))
               (cond
                 ((null argument) all-origins)
                 ((set-equal element
                             (apply-function-to-element function argument)
			     :test equal-pred)
                  (origin-of-element next-element
                                     (make-set (cons argument all-origins))))
                 (t
                  (origin-of-element next-element all-origins))))))
    (origin-of-element (first (source function)) ())))
  ;; (inverse-image function {element})

(defun restrict-function-on-source-and-traget (function new-source new-target)
  "Restricts FUNCTION being a function on NEW-SOURCE\times NEW-TARGET."
  (flet ((calc-new-func-graph ()
            (let ((new-graph ()))
              (iterate-over-function-graph function element
                (when (member (all-operands element) new-source :test (equal-pred function))
                  (push element new-graph)))
             new-graph)))
    (let ((new-graph (calc-new-func-graph)))
      (make-function new-source
                     new-target
                     new-graph))))

(defun restrict-function-on-target (function new-target)
  "Returns algebraic function begin FUNCTION with target restricted to NEW-TARGET"
  (make-function (source function)
                 new-target
                 (graph function)))

(defun restrict-function-on-source (function new-source)
  "Returns algebraic functions as FUNCTION restricted to NEW-SOURCE."
  (restrict-function-on-source-and-traget function new-source (target function)))

;;; for iteration over all assignments

(defun next-assignment (value-set assignment)
  "Returns next assignment in VALUE-SET after ASSIGNMENT being a tuple of pairs, or NIL if ASSIGNMENT is last."
  (let ((argument (mapcar #'second assignment))
        (elements (mapcar #'first assignment)))
    (let ((next-argument (next-argument value-set argument)))
      (when next-argument
        (mapcar #'(lambda (x y) (list x y)) elements next-argument)))))

(defun all-assignments (variables value-set)
  "Returns lazy set consiting of all assignments of VARIABLES to value from VALUE-SET."
  (let* ((first-assign (symbols (card variables) (first value-set)))
         (assign (mapcar #'pair variables first-assign)))
    (flet ((new-assignment ()
             (let ((current-assign assign))
               (setf assign (next-assignment value-set assign))
               current-assign)))
      (define-lazy-set #'new-assignment))))

(defun all-functions (source target)
  "Returns lazy set of all functions between SOURCE and TARGET."
  (let ((assignments (all-assignments source target)))
    (flet ((next-function ()
	     (let ((next-assign (funcall (technicals::next assignments))))
	       (when next-assign
		 (make-function source target next-assign)))))
      (define-lazy-set #'next-function))))

(defun all-functions-with-predicate (source target predicate)
  "Returns lazy set of all function between SEOURCE and TARGET fulfilling PREDICATE."
  (let ((functions (all-functions source target)))
    (labels ((next-function ()
	       (let ((next-fun (funcall (next functions))))
		 (cond
		   ((not next-fun) nil)
		   ((funcall predicate next-fun)
		    next-fun)
		   (t (next-function))))))
      (define-lazy-set #'next-function))))

(defun all-bijective-functions (source target)
  (all-functions-with-predicate source target #'bijective-p))
