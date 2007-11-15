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
  (:documentation "Writes ALGEBRA in UACalc format to FILE-NAME-OR-PROJECT."))

(defmethod uacalc-write-algebra-to-file (algebra (file-name string))
  (declare (type algebra algebra))
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
  (let ((num (read-preserving-whitespace file nil)))
    (when (and num (not (numberp num)))
      (error 'uacalc-io-error :text
             (format nil "Got ~A where number was expected."
                     num)))
    num))

(defun read-next-char-from-file (file &key (should-be "" sb-p))
  (let ((char (read-char file nil)))
    (when (and sb-p (not (equalp should-be char)))
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

(defgeneric uacalc-read-algebra-from-file (file-name-or-project)
  (:documentation "Reads algebra from file-name begin a UACalc algebra file."))

(defmethod uacalc-read-algebra-from-file ((file-name string))
  (with-open-file (file file-name :direction :input :if-does-not-exist :error)
    (let* ((base-set (read-base-set-from-file file))
	   (operations (read-all-operations-from-file file base-set))
	   (signature (calculate-signature-from-operations operations)))
      (make-algebra base-set signature operations :equal-pred #'equal))))

;;; syntactic abstraction

(define-simple-condition uacalc-project-error)

(defclass uacalc-project ()
  ((pure-file-name :type string :accessor pure-file-name
                   :initarg :pure-file-name)))

(defun make-uacalc-project (pathname)
  (make-instance 'uacalc-project :pure-file-name pathname))

(defgeneric read-from-uacalc-project (file-accessor project)
  (declare (type uacalc-project project))
  (:documentation "Reads anything from PROJECT."))

(defgeneric write-to-uacalc-project (file-accessor what project)
  (declare (type t what)
           (type uacalc-project project))
  (:documentation "Writes WHAT to PROJECT."))

(defmacro define-uacalc-file-accessor (name extension &rest io-functions)
  "Defines acessor function NAME for a UACALC-PROJECT with EXTENSION"
  `(progn
     (defun ,name (project)
       (declare (type uacalc-project project))
       (the string (concatenate 'string
                                (pure-file-name project)
                                (string ,extension))))

       ,(let ((reader (assoc :reader io-functions)))
          `(defmethod read-from-uacalc-project ((file-accessor (eql ',name)) project)
             ,(if reader
                  `(,(second reader) project)
                  `(error 'uacalc-io-error :text
                          (format nil "~&Read not defined for ~A" ',name)))))

       ,(let ((writer (assoc :writer io-functions)))
          `(defmethod write-to-uacalc-project ((file-accessor (eql ',name)) what project)
             ,(if writer
                  `(,(second writer) what project)
                  `(error 'uacalc-io-error :text
                          (format nil "~&Write not defined for ~A" ',name)))))))

;;; .alg files

(define-uacalc-file-accessor file-name ".alg")

(defmethod uacalc-write-algebra-to-file (algebra (project uacalc-project))
  (declare (type algebra algebra))
  (uacalc-write-algebra-to-file algebra (file-name project)))

(defmethod uacalc-read-algebra-from-file ((project uacalc-project))
  (uacalc-read-algebra-from-file (file-name project)))

(define-uacalc-file-accessor algebra-file-name ".alg"
  (:reader uacalc-read-algebra-from-file)
  (:writer uacalc-write-algebra-to-file))

;;; .vlf files

(defun write-vector-to-file (file vector &key (prefix "" prefix-p))
  (declare (type stream file)
	   (type vector vector))
  (format file "~&~:[~;~A~]" prefix-p prefix)
  (format file "~A" (elt vector 0))
  (loop for entry across (subseq vector 1)
	do (format file ",~A" entry))
  (format file "~%"))

(defgeneric uacalc-write-vector-list-to-file (vector-list file-name-or-project)
  (declare (type list vector-list)
	   (type (or string uacalc-project) file-name-or-project))
  (:documentation "Writes VECTOR-LIST as a list of equal sized vectors
 to FILE-NAME-OR-PROJECT according to UACALC format rules"))

(defmethod uacalc-write-vector-list-to-file (vector-list (project uacalc-project))
  (uacalc-write-vector-list-to-file (vector-list-file-name project)
				    vector-list))

(defmethod uacalc-write-vector-list-to-file (vector-list (file-name string))
  (with-open-file (stream file-name :direction :output
			  :if-exists :supersede)
    (write-vector-to-file stream
			  (vector (length vector-list)
			          (length (first vector-list))))
    (format stream "~%")
    (loop for vector in vector-list
	  do (write-vector-to-file stream vector))))

;; read-vector-from-file

(defun char-to-int (char)
  (declare (type character char))
  (case char
    (#\0 0)    (#\1 1)
    (#\2 2)    (#\3 3)
    (#\4 4)    (#\5 5)
    (#\6 6)    (#\7 7)
    (#\8 8)    (#\9 9)
    (otherwise (error 'uacalc-io-error :text
                      (format nil "Number expected but '~C' found"
                              char)))))

(defun read-vector-entry-from-file (file)
  (declare (type stream file))
  (let ((num  (read-next-number-from-file file))
        (char (read-next-char-from-file file)))
    (cond
      ((not (or (equalp char #\Newline)
                (equalp char #\,)))
       (error 'uacalc-io-error :text
              (format nil "Expected #\\Newline of #\\, but got ~C"
                      char)))
      (t (list num char)))))

(defun read-vector-from-file (file &key (prefix "" prefix-p))
  (declare (type stream file))
  (when prefix-p
    (read-next-char-from-file file :should-be prefix))
  (loop for (num char) = (read-vector-entry-from-file file)
        collect num
        until (equalp char #\Newline)))

(define-uacalc-file-accessor vector-list-file-name ".vlf"
  (:writer uacalc-write-vector-list-to-file))

;;;

; command files for uab
(define-uacalc-file-accessor command-file ".par")

;;;

; universes generated by create-direct-product and generate-subproduct-power
(define-uacalc-file-accessor universe-file ".uni")

;;;

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

(defgeneric uacalc-write-congruences-to-file (congruences file-name-or-project)
  (declare (type (or string uacalc-project) file-name-or-project)
           (type standard-set congruences))
  (:documentation "Writer function to write CONGRUENCES to FILE-NAME-OR-PROJECT."))

(defmethod uacalc-write-congruences-to-file (congruences (project uacalc-project))
  (uacalc-write-congruences-to-file congruences (cong-file project)))

(defmethod uacalc-write-congruences-to-file (congruences (file-name string))
  (let ((uacalc-congs (mapcar #'congruence-to-uacalc-congruence congruences)))
    (with-open-file (stream file-name :direction :output
			              :if-exists :supersede)
      (write-vector-to-file stream (vector (card uacalc-congs) 0)) ;;; ???
      (loop for cong in uacalc-congs do
	    (write-vector-to-file stream cong :prefix ",")))))

(defgeneric uacalc-read-congruences-from-file (file-name-or-project)
  (declare (type (or string uacalc-project) file-name-or-project))
  (:documentation "Reads all congruences from FILE-NAME-OR-PROJECT."))

(defmethod uacalc-read-congruence-from-file ((project uacalc-project))
  (uacalc-read-congruence-from-file (cong-file project)))

(defmethod uacalc-read-congruence-from-file ((file-name string))
  (error "To be done."))


; all congruences
(define-uacalc-file-accessor cong-file ".con"
  (:writer uacalc-write-congruences-to-file))

;;;

;all principal congruences
(define-uacalc-file-accessor princ-file ".pri")

;;;

;all meet-irreducibe congruences

(define-uacalc-file-accessor meet-irr-file ".mir")

;;;