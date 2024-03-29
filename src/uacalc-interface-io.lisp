(in-package :UACalc-interface)

(defgeneric uacalc-write-algebra-to-file (algebra file-name-or-project)
  (:documentation "Writes ALGEBRA in UACalc format to FILE-NAME-OR-PROJECT."))

(defmethod uacalc-write-algebra-to-file (algebra (file-name string))
  (declare (type algebra algebra))
  (let ((numerized-algebra (numerize-algebra algebra)))
    (with-open-file (file file-name :direction :output :if-exists :error)
      (write-numerized-algebra-to-file numerized-algebra file))))

(defgeneric uacalc-read-algebra-from-file (file-name-or-project &key untested)
  (:documentation "Reads algebra from file-name begin a UACalc algebra file."))

(defmethod uacalc-read-algebra-from-file ((file-name string) &key (untested nil))
  (with-open-file (file file-name :direction :input :if-does-not-exist :error)
    (let* ((base-set (read-base-set-from-file file))
	   (operations (read-all-operations-from-file file base-set
						      :untested untested))
	   (signature (calculate-signature-from-operations operations)))
      (make-algebra base-set signature operations))))

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

(defmethod uacalc-read-algebra-from-file ((project uacalc-project) &key (untested nil))
  (uacalc-read-algebra-from-file (file-name project) :untested untested))

(define-uacalc-file-accessor algebra-file-name ".alg"
  (:reader (lambda (file-name-or-project)
	     (uacalc-read-algebra-from-file file-name-or-project :untested t))) ;!!!
  (:writer uacalc-write-algebra-to-file))

;;; .vlf files

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
    (cond
      ((not (every #'(lambda (x) (= (length (first vector-list))
				    (length x)))
		   vector-list))
       (error 'uacalc-io-error :text
	      (format nil "Vector list ~A contains vectors of different length"
		      vector-list)))
      (t
       (write-vector-to-file stream
			     (vector (length vector-list)
				     (length (first vector-list))))
       (format stream "~%")
       (loop for vector in vector-list
	     do (write-vector-to-file stream vector))))))

(defgeneric uacalc-read-vector-list-from-file (file-name-or-project)
  (declare (type (or string uacalc-project) file-name-or-project))
  (:documentation "Reads a vector list from FILE-NAME-OR-PROJECT."))

(defmethod uacalc-read-vector-list-from-file ((project uacalc-project))
  (uacalc-read-vector-list-from-file (vector-list-file-name project)))

(defmethod uacalc-read-vector-list-from-file ((file-name string))
  (with-open-file (stream file-name :direction :input
			            :if-does-not-exist :error)
    (read-all-vectors-from-file stream file-name)))

(define-uacalc-file-accessor vector-list-file-name ".vlf"
  (:writer uacalc-write-vector-list-to-file)
  (:reader uacalc-read-vector-list-from-file))

;;;

; command files for uab
(define-uacalc-file-accessor command-file ".par")

;;;

; universes generated by create-direct-product and generate-subproduct-power
(define-uacalc-file-accessor universe-file ".uni")

;;;

(defgeneric uacalc-write-congruences-to-file (congruences file-name-or-project)
  (declare (type (or string uacalc-project) file-name-or-project)
           (type standard-set congruences))
  (:documentation "Writer function to write CONGRUENCES to FILE-NAME-OR-PROJECT."))

(defmethod uacalc-write-congruences-to-file (congruences (project uacalc-project))
  (uacalc-write-congruences-to-file congruences (cong-file project)))

(defmethod uacalc-write-congruences-to-file (congruences (file-name string))
  (let ((uacalc-congs (mapset #'congruence-to-uacalc-congruence congruences)))
    (with-open-file (stream file-name :direction :output
			              :if-exists :supersede)
      (write-vector-to-file stream (vector (card-s uacalc-congs) 0)) ;;; ???
      (loop-over-set cong uacalc-congs
	(write-vector-to-file stream cong :prefix ",")))))

(defgeneric uacalc-read-congruences-from-file (file-name-or-project)
  (declare (type (or string uacalc-project) file-name-or-project))
  (:documentation "Reads all congruences from FILE-NAME-OR-PROJECT."))

(defmethod uacalc-read-congruences-from-file ((project uacalc-project))
  (uacalc-read-congruences-from-file (cong-file project)))

(defmethod uacalc-read-congruences-from-file ((file-name string))
  (with-open-file (stream file-name :direction :input
			            :if-does-not-exist :error)
    (let ((vectors (read-all-vectors-from-file stream file-name
					       :prefix #\,
					       :number-idx 0
					       :length-idx 1)))
      (make-set (mapcar #'uacalc-congruence-to-congruence vectors)))))

; all congruences
(define-uacalc-file-accessor cong-file ".con"
  (:writer uacalc-write-congruences-to-file)
  (:reader uacalc-read-congruences-from-file))

;;;

;all principal congruences
(define-uacalc-file-accessor princ-file ".pri"
  (:writer uacalc-write-congruences-to-file)
  (:reader uacalc-read-congruences-from-file))

;;;

;all meet-irreducibe congruences

(define-uacalc-file-accessor meet-irr-file ".mir"
  (:writer uacalc-write-congruences-to-file)
  (:reader uacalc-read-congruences-from-file))

;;;