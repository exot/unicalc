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
  (let* ((numbers (number-list (card (base-set-of algebra))))
	 (rename-function (create-renaming-function (base-set-of algebra) numbers)))
    (values
     (apply-quasihomomorphism-to-algebra rename-function algebra)
     rename-function)))

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
  (let ((num (read-preserving-whitespace file nil)))
    (when (and num (not (numberp num)))
      (error 'uacalc-io-error :text
             (format nil "Got ~A where number was expected."
                     num)))
    num))

(defun read-next-char-from-file (file &key (should-be "" sb-p))
  (let ((char (read-char file nil)))
    (when (and sb-p char (not (equalp should-be char)))
      (error 'uacalc-io-error :text
             (format nil "Read character ~A should be ~A"
                     char should-be)))
    char))

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
	     (list operation-name graph arity))))))

(defun read-arity-from-file (file)
  (read-next-number-from-file file))

(defun read-graph-from-file (file arity base-set)
  (let ((arguments (all-uacalc-arguments arity base-set))
	(graph ()))
    (loop for argument = (funcall (next arguments))
	  while argument do
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
	for (op-name graph arity)
	     = (read-operation-from-file file (operation-symbol "F" i) base-set)
	while op-name
	collect (list op-name
		      (make-function (tuples base-set arity)
				     base-set
				     graph))))

(defun calculate-signature-from-operations (operations)
  (let ((rank-alphabet
	  (loop for table in operations
		collect (list (function-symbol-of table)
			      (arity-of-function (implementing-function-of
                                                   table))))))
    (make-signature (mapcar #'first rank-alphabet) rank-alphabet)))

(defun write-vector-to-file (file vector &key (prefix "" prefix-p))
  (declare (type stream file)
	   (type vector vector))
  (format file "~&~:[~;~A~]" prefix-p prefix)
  (format file "~A" (elt vector 0))
  (loop for entry across (subseq vector 1)
	do (format file ",~A" entry))
  (format file "~%"))

;; read-vector-from-file

(defun read-line-from-file (file &key (prefix #\. prefix-p))
  (declare (type stream file)
	   (type character prefix))
  (when prefix-p
    (read-next-char-from-file file :should-be prefix))
  (loop for line = (read-line file nil)
	while (and line (zerop (length line)))
	finally (return line)))

(defun read-vector-from-file (file &key (prefix #\. prefix-p))
  (declare (type stream file)
	   (type character prefix))
  (let ((line (if prefix-p
		  (read-line-from-file file :prefix prefix)
		  (read-line-from-file file))))
    (cond
      ((null line) nil)
      (t
       (with-input-from-string (stream (concatenate
					'string "#("
					(substitute #\Space #\, line) ")"))
	 (read stream))))))

(defun read-all-vectors-from-file (file file-name &key (prefix #\. prefix-p)
				                       (number-idx 0)
				                       (length-idx 1))
  (declare (type stream file)
	   (type character prefix))
  "Reads vector list from FILE given by FILE-NAME, where each vector (not the first one)
is preceeded by PREFIX if given."
  (let ((entries (read-vector-from-file file))
	(all-vectors (loop for vector = (if prefix-p
					    (read-vector-from-file file
								   :prefix  prefix)
					    (read-vector-from-file file))
			   while vector
			   collect vector)))
    (cond
      ((not (equal (length entries) 2))
       (error 'uacalc-io-error :text
	      (format nil "Malformed first line ~A in file ~A."
		      entries file-name)))
      (t (let ((total-number (elt entries number-idx))
	       (total-length (elt entries length-idx)))
	   (cond
	     ((not (or (zerop total-number)
		       (equal (length all-vectors) total-number)))
	      (error 'uacalc-io-error :text
		     (format nil "Incorrect number of vectors given in ~A"
			     file-name)))
	     ((not (every #'(lambda (vec) (equal (length vec) total-length))
			  all-vectors))
	      (error 'uacalc-io-error :text
		     (format nil "There are vectors not of length ~A in ~A"
			     total-length file-name)))
	     (t all-vectors)))))))

(defun congruence-to-uacalc-congruence (congruence)
  (declare (type relation congruence))
  "Returns vector representing congruence in UACalc."
  (let ((enumerated-partition
	 (enumerate-partition (partition-from-equivalence-relation congruence)))
	(vector (make-array (card (source congruence)))))
    (loop for (set number) in enumerated-partition
	  do (loop for elt in set
		   do (setf (elt vector elt) number))
	  finally (return vector))))

(defun enumerate-partition (partition)
  (declare (type standard-set partition))
  (let ((minimas (mapcar #'(lambda (set) (apply #'min set))
			 partition)))
    (mapcar #'(lambda (set number)
		(let ((uppers (count-if #'(lambda (x) (< x number))
					minimas)))
		  (pair set uppers)))
	    partition minimas)))

(defun uacalc-congruence-to-congruence (vector)
  (declare (type vector vector))
  "Returns congruence represented by VECTOR in UACalc congruence format."
  (labels ((helper (pairs number result-list)
	     (cond
	       ((null pairs) result-list)
	       (t (multiple-value-bind (goods bads)
		      (split-by-predicate pairs
					  #'(lambda (x) (= (second x) number)))
		    (helper bads (1+ number)
			    (cons (mapcar #'first goods)
				  result-list)))))))
    (helper (map 'list #'pair (number-list (length vector)) vector)
	    0 ())))
