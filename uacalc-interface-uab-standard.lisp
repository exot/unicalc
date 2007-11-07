(in-package :uacalc-interface)

;;; standard commands for uab

(defmacro with-uab-command-to-file ((stream file-name) &body body)
  `(with-open-file (,stream ,file-name :direction :output
                                       :if-exists :supersede)
    (write-uab-begin-command ,stream)
    ,@body
    (write-uab-end-command ,stream)))

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
  (write-uab-command stream "generate_subalgebra")
  (write-uab-command stream "algebra_file" algebra-pathname)
  (write-uab-command stream "vectorlist_file" vectorlist-pathname)
  (write-uab-command stream "output_file" output-pathname))

(defun uab-commands-to-create-subproduct-algebra (stream
                                                  vectorlist-pathname
                                                  algebra-pathnames
                                                  output-pathname)
  (write-uab-command stream "create_subproduct_algebra")
  (write-uab-command stream "vectorlist_file" vectorlist-pathname)
  (loop for algebra-pathname in algebra-pathnames
        do (write-uab-command stream "algebra_file" algebra-pathname))
  (write-uab-command stream "output_file" output-pathname))

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

; to be continued

;;; run uab

(define-simple-condition uacalc-interface-uab-error)

(defun run-uab (par-pathname)
  "Runs UAB with pathname PAR-PATHNAME as argument."
  #+sbcl
  (let ((process (sb-ext:run-program "uab"
                                     (list par-pathname)
				     :search t
				     :output *standard-output*)))
    (cond
      ((not (= (sb-ext:process-exit-code process) 0))
       (error 'uacalc-interface-uab-error :text
              (format nil "~&UAB return ~A~%"
		      (sb-ext:process-exit-code process))))
      (t t)))
  #-sbcl
  (error "run-uab not implemented on this lisp version"))

;;;; uab commands

(defun generate-unique-pathname () ;;; FIXME!!!
  (concatenate 'string "/tmp/test" (string (gentemp))))
