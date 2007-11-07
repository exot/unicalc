(in-package :UACalc-interface)

(define-simple-condition uacalc-interface-error)

(define-simple-condition uacalc-io-error)

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

(defgeneric uacalc-write-algebra-to-file (algebra file-name-or-project)
  (declare (type algebra algebra))
  (:documentation "Writes ALGEBRA in UACalc format to FILE-NAME-OR-PROJECT."))

(defmethod uacalc-write-algebra-to-file (algebra (file-name string))
  (let ((numerized-algebra (numerize-algebra algebra)))
    (with-open-file (file file-name :direction :output :if-exists :error)
      (write-numerized-algebra-to-file numerized-algebra file))))

;;; uacalc-read-algebra-from-file

;; this is a copy of next-argument to ensure correctness
(defun next-uacalc-tuple (base-set tuple)
  (cond
    ((null tuple) nil)
    (t (let ((rest (rest (member (first tuple) base-set)))); all elements
                                                           ; after current
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
		      (format nil
                              "~A is invalid: malformed function definition"
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

(defgeneric uacalc-read-algebra-from-file (file-name-or-project)
  (:documentation "Reads algebra from file-name begin a UACalc algebra file."))

(defmethod uacalc-read-algebra-from-file ((file-name string))
  (with-open-file (file file-name :direction :input :if-does-not-exist :error)
    (let* ((base-set (read-base-set-from-file file))
	   (operations (read-all-operations-from-file file base-set))
	   (signature (calculate-signature-from-operations operations)))
      (make-algebra base-set signature operations :equal-pred #'equal))))

;;;

(define-simple-condition uacalc-project-error)

(defclass uacalc-project ()
  ((pure-file-name :type string :accessor pure-file-name
                   :initarg :pure-file-name)))

(defun make-uacalc-project (pathname)
  (make-instance 'uacalc-project :pure-file-name pathname))

(defmacro define-uacalc-file-accessor (name extension)
  "Defines acessor function NAME for a UACALC-PROJECT with EXTENSION"
  `(progn
     (defun ,name (project)
;       ,@(concatenate 'string
;                      "PURE-FILE-NAME of PROJECT appended with "
;                      (string extension))
       (declare (type uacalc-project project))
       (the string (concatenate 'string
                                (pure-file-name project)
                                (string ,extension))))))

;;; .alg files

(define-uacalc-file-accessor file-name ".alg")

(defmethod uacalc-write-algebra-to-file (algebra (project uacalc-project))
  (uacalc-write-algebra-to-file algebra (file-name project)))

(defmethod uacalc-read-algebra-from-file ((project uacalc-project))
  (uacalc-read-algebra-from-file (file-name project)))

;;; .vlf files

(define-uacalc-file-accessor vector-list-file-name ".vlf")

(defun write-vector-to-file (file vector)
  (declare (type stream file)
	   (type vector vector))
  (format file "~&")
  (format file "~A" (elt vector 0))
  (loop for entry across (subseq vector 1)
	do (format file ",~A" entry))
  (format file "~%"))

(defgeneric uacalc-write-vector-list-to-file (file-name-or-project vector-list)
  (declare (type list vector-list)
	   (type (or string uacalc-project) file-name-or-project))
  (:documentation "Writes VECTOR-LIST as a list of equal sized vectors
 to FILE-NAME-OR-PROJECT according to UACALC format rules"))

(defmethod uacalc-write-vector-list-to-file ((project uacalc-project)
					     vector-list)
  (uacalc-write-vector-list-to-file (vector-list-file-name project)
				    vector-list))

(defmethod uacalc-write-vector-list-to-file ((file-name string) vector-list)
  (with-open-file (stream file-name :direction :output
			  :if-exists :supersede)
    (write-vector-to-file stream
			  (vector (length vector-list)
			          (length (first vector-list))))
    (format stream "~%")
    (loop for vector in vector-list
	  do (write-vector-to-file stream vector))))

;;;

(define-uacalc-file-accessor command-file ".par")

;;;

(define-uacalc-file-accessor universe-file ".uni")

;;;