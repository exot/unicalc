(in-package :uacalc-interface)

;;; standard commands for uab

(defmacro with-uab-command-to-file ((stream file-name) &body body)
  `(with-open-file (,stream ,file-name :direction :output
                                       :if-exists :supersede)
    (write-uab-begin-command ,stream)
    ,@body
    (write-uab-end-command ,stream)))

(defmacro uab-commands (stream &body body)
  `(flet ((cmd (command &rest args)
	   (apply #'write-uab-command ,stream command args)))
    ,@body))

(defun write-uab-command (stream command &optional (pathname nil path-given-p))
  (if path-given-p
      (format stream "~&\\~A{~A}~%" command (pathname pathname))
      (format stream "~&\\~A~%" command)))

(defun write-uab-begin-command (stream)
  (write-uab-command stream "begin_command"))

(defun write-uab-end-command (stream)
  (write-uab-command stream "end_command"))

(defun write-uab-next-command (stream)
  (write-uab-end-command stream)
  (format stream "~&%%~%")
  (write-uab-begin-command stream))

;;;

(defun uab-commands-to-generate-subalgebra (stream
                                            algebra-pathname
                                            vectorlist-pathname
                                            output-pathname)
  (uab-commands stream
    (cmd "generate_subalgebra")
    (cmd "algebra_file"    algebra-pathname)
    (cmd "vectorlist_file" vectorlist-pathname)
    (cmd "output_file"     output-pathname)))

(defun uab-commands-to-create-subproduct-algebra (stream
                                                  vectorlist-pathname
                                                  algebra-pathnames
                                                  output-pathname)
  (uab-commands stream
    (cmd "create_subproduct_algebra")
    (cmd "vectorlist_file" vectorlist-pathname)
    (loop for algebra-pathname in (set-to-list algebra-pathnames)
          do (cmd "algebra_file" algebra-pathname))
    (cmd "output_file" output-pathname)))

(defun uab-commands-to-create-direct-subproduct-algebra (stream
							vectorlist-pathname
							order
							power-algebra-pathname
							output-pathname)
  (let ((algebras-pathname (loop repeat order
				 collect power-algebra-pathname)))
    (uab-commands-to-create-subproduct-algebra stream vectorlist-pathname
					      algebras-pathname
					      output-pathname)))

(defun uab-commands-to-create-direct-power (stream
					    algebra-pathname
					    power
					    output-pathname
					    universe-pathname)
  (uab-commands stream
    (cmd "create_direct_product")
    (cmd "number" (princ-to-string power)) ;;; is ok
    (loop repeat power
          do (cmd "algebra_file" algebra-pathname))
    (cmd "output_file" output-pathname)
    (cmd "output_file" universe-pathname)))

(defun uab-commands-to-create-direct-product (stream
					      algebra-pathnames
					      output-pathname
					      universe-pathname)
  (uab-commands stream
    (cmd "create_direct_product")
    (cmd "number" (princ-to-string (length algebra-pathnames))) ;;; is ok
    (loop for algebra-pathname in (set-to-list algebra-pathnames)
          do (cmd "algebra_file" algebra-pathname))
    (cmd "output_file" output-pathname)
    (cmd "output_file" universe-pathname)))

(defun uab-commands-to-compute-congruence-lattice (stream
                                                   algebra-pathname
                                                   congruence-lattice-pathname
                                                   congruences-pathname
                                                   principal-congruences-pathname
                                                   meet-irreducible-congruences-pathname)
  (uab-commands stream
    (cmd "compute_congruence_lattice")
    (cmd "algebra_file" algebra-pathname)
    (cmd "output_file" congruence-lattice-pathname)
    (cmd "output_file" congruences-pathname)
    (cmd "output_file" principal-congruences-pathname)
    (cmd "output_file" meet-irreducible-congruences-pathname)))

; to be continued

;;; run uab

(define-simple-condition uacalc-interface-uab-error)

(defparameter *uab-output* t)

(defun show-uab-output (yes-or-no)
  (setf *uab-output* yes-or-no))

(defun run-uab (par-pathname)
  "Runs UAB with pathname PAR-PATHNAME as argument."
  #+sbcl
  (let ((process (if *uab-output*
		     (sb-ext:run-program "uab"
					 (list par-pathname)
					 :search t
					 :output *standard-output*)
		     (sb-ext:run-program "uab"
					 (list par-pathname)
					 :search t))))
    (cond
      ((not (= (sb-ext:process-exit-code process) 0))
       (error 'uacalc-interface-uab-error :text
              (format nil "~&UAB return ~A~%"
		      (sb-ext:process-exit-code process))))
      (t t)))
  #-sbcl
  (error "run-uab not implemented on this lisp version"))

;;;; uab commands

(defun file-exists (pathname)
  (declare (type string pathname)
           (inline file-exists)
           (optimize (speed 0))) ;; no warnings
  #-sbcl(error "Function FILE-EXISTS not implemented.")
  #+sbcl
  (handler-case
      (progn
        (sb-posix:stat pathname)
        t)
    (sb-posix:syscall-error () nil)))

(defun generate-unique-pathname ()
  (loop for file-name = (concatenate 'string "/tmp/test" (string (gentemp)))
        until (and (not (file-exists (concatenate 'string file-name ".alg")))
		   (not (file-exists (concatenate 'string file-name ".con"))))
        finally (return file-name)))

(defun make-new-project ()
  (make-uacalc-project (generate-unique-pathname)))

(defun algebra-to-project (algebra)
  (declare (type algebra algebra))
  (let ((project (make-new-project)))
    (uacalc-write-algebra-to-file algebra project)
    project))

(defun collect-input-algebras (input-algebras)
  (declare (type list input-algebras))
  (cond
    ((every #'listp input-algebras)
     (loop for algebra-pair in input-algebras
           collect `(,(first algebra-pair)
                     (algebra-to-project ,(second algebra-pair)))))
    (t
     `((,(first input-algebras)
        (mapcar #'algebra-to-project ,(second input-algebras)))))))

(defmacro with-algebras (input-algebras output-algebras &body body)
  (let ((sym-input-algebras (collect-input-algebras input-algebras))
        (sym-output-algebras (loop for (file-accessor algebra) in output-algebras
                                   collect `(,algebra
                                             (make-uacalc-project
                                              (generate-unique-pathname))))))
    `(let* (,@sym-input-algebras ,@sym-output-algebras)
      ,@body
      (values ,@(mapcar #'(lambda (alg)
                            `(read-from-uacalc-project ',(first alg) ,(second alg)))
                        output-algebras)))))
