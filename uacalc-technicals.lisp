(in-package :UACalc-interface)

(define-simple-condition uacalc-interface-error)

;;; uacalc-write-algebra-to-file

(defun create-renaming-function (from-set to-set &key (equal #'equal))
  (cond
    ((not (= (card from-set)
	     (card to-set)))
     (error 'UACalc-interface-error :text
	    (format nil "Cannot rename: ~A is not of same cardinality as ~A"
		    from-set to-set)))
    (t (make-function from-set
		      to-set
		      (mapcar #'pair from-set to-set)
		      :equal-pred equal))))

(defun numerize-algebra (algebra)
  (let ((numbers (number-list (card (base-set-of algebra)))))
    (apply-quasihomomorphism-to-algebra
      (create-renaming-function (base-set-of algebra) numbers)
      algebra)))

(defun write-number-to-file (number file)
  (format file "~&~D~%" number))

(defun write-numerized-algebra-to-file (algebra file)
  (write-base-set-to-file algebra file)
  (write-all-operations-to-file algebra file))

(defun write-base-set-to-file (algebra file)
  (write-number-to-file (card (base-set-of algebra)) file))

(defun write-all-operations-to-file (algebra file)
  (loop for operation in (function-symbols-of (signature-of algebra))
	do (write-operation-to-file algebra operation file)))

(defun write-operation-to-file (algebra operation file)
  (let* ((arity (arity-of-function-symbol (signature-of algebra) operation))
	 (all-arguments (all-uacalc-arguments arity (base-set-of algebra))))
    (write-number-to-file arity file)
    (loop for arg = (funcall (next all-arguments))
	  while arg do
	  (write-number-to-file (apply-operation-in-algebra operation arg
                                                            algebra)
                                file))))

(defun uacalc-write-algebra-to-file (algebra file-name)
  "Writes ALGEBRA in UACalc format to FILE-NAME"
  (let ((numerized-algebra (numerize-algebra algebra)))
    (with-open-file (file file-name :direction :output)
      (write-numerized-algebra-to-file numerized-algebra file))))

;;; uacalc-read-algebra-from-file

;; this is a copy of next-argument to ensure correctness
(defun next-uacalc-tuple (base-set tuple)
  (cond
    ((null tuple) nil)
    (t (let ((rest (rest (member (first tuple) base-set)))); all elements after
                                                           ; current
	 (cond
	   ((null rest) ; increment next position
	    (let ((next (next-argument base-set (rest tuple))))
	      (when next
		(cons (first base-set) next)))) ; and start with first element
                                                ; again
	   (t (cons (first rest) (rest tuple))))))))

(defun all-uacalc-arguments (n base-set)
  "Returns lazy set for all arguments of a N-ary function on BASE-SET in order
  used by UACalc."
  (let ((set base-set)
	(start (numbers n 0)))
    (flet ((next-element ()
	     (let ((current start))
	       (setf start (next-uacalc-tuple set start))
	       current)))
      (define-lazy-set #'next-element))))

(defun read-next-number-from-file (file)
  (read file nil))

(defun read-base-set-from-file (file)
  (let ((number (read-next-number-from-file file)))
    (cond
      ((null number) (error 'UACalc-interface-error :text
			    (format nil "~A is invalid: no base set given"
                                    file)))
      (t (number-list number)))))

(defun read-operation-from-file (file operation-name base-set)
  (let ((arity (read-arity-from-file file)))
    (when arity
      (let ((graph (read-graph-from-file file arity base-set)))
	   (when graph
	     (list (cons operation-name graph) arity))))))

(defun read-arity-from-file (file)
  (read-next-number-from-file file))

(defun read-graph-from-file (file arity base-set)
  (let ((arguments (all-uacalc-arguments arity base-set))
	(graph ()))
    (loop for argument = (funcall (next arguments))
	  while argument
	  do
	  (let ((value (read-next-number-from-file file)))
	    (cond
	      ((and (null value)
		    (not (null graph)))
	       (error 'UACalc-interface-error :text
		      (format nil "~A is invalid: malformed function definition"
                              file)))
	      ((null value) nil)
	      (t (push (list argument value) graph))))
	  finally (return graph))))

(defun read-all-operations-from-file (file base-set)
  (loop for i = 0 then (1+ i)
	for pair = (read-operation-from-file file (operation-symbol "F" i)
                                             base-set)
	while pair
	collect (list (first (first pair))
		      (make-function (tuples base-set (second pair))
				     base-set
				     (rest (first pair))))))

(defun calculate-signature-from-operations (operations)
  (let ((rank-alphabet
	  (loop for table in operations
		collect (list (function-symbol-of table)
			      (arity-of-function (implementing-function-of
                                                   table))))))
    (make-signature (mapcar #'first rank-alphabet) rank-alphabet)))

(defun uacalc-read-algebra-from-file (file-name)
  "Reads algebra from file-name begin a UACalc algebra file."
  (with-open-file (file file-name)
    (let* ((base-set (read-base-set-from-file file))
	   (operations (read-all-operations-from-file file base-set))
	   (signature (calculate-signature-from-operations operations)))
      (make-algebra base-set signature operations :equal-pred #'equal))))

;;;
