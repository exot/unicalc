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

; to be continued

;;; run uab

(define-simple-condition uacalc-interface-uab-error)

(defun find-uab ()
  #p"/home/st4/borch/local/bin/uab") ;;; FIXME!!!

(defun run-uab (par-pathname)
  "Runs UAB with pathname PAR-PATHNAME as argument."
  #+sbcl
  (let ((process (sb-ext:run-program (find-uab)
                                     (list (pathname par-pathname)))))
    (cond
      ((not (= (sb-ext:process-exit-code process)
               0))
       (error 'uacalc-interface-uab-error :text
              (format nil "~&UAB return ~A~%"
                      (sb-ext:process-exit-code process))))
      (t t)))
  #-sbcl
  (error "run-uab not implemented on this lisp version"))

;;;; uab commands

;;; free algebra

(defun uab-calculate-free-algebra (algebra-project size-of-algebra
                                   number-of-generators
                                   output-project)
  (let ((free-algebra-size (expt size-of-algebra
                                 number-of-generators)))
    (write-free-algebra-generators-to-file
               (vector-list-file-name output-project)
               size-of-algebra
               number-of-generators)
    (with-uab-command-to-file (stream (command-file algebra-project))
      (print stream)
      (uab-commands-to-generate-subalgebra
                 stream (file-name algebra-project)
                 (vector-list-file-name
                  output-project)
                 (universe-file output-project))
      (write-uab-next-command stream)
      (uab-commands-to-create-subproduct-algebra
                 stream
                 (universe-file output-project)
                 (loop repeat free-algebra-size
                       collect (file-name algebra-project))
                 (file-name output-project)))))

(defun write-free-algebra-generators-to-file (filename size-of-algebra
                                              number-of-generators)
  (let ((total-size (expt size-of-algebra number-of-generators)))
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-vector-to-file stream
                            (vector number-of-generators
                                    total-size))
      (format stream "~%")
      (loop for chunks = (/ total-size size-of-algebra)
            then (/ chunks size-of-algebra)
            for repeats = 1
            then (* repeats size-of-algebra)
            for i = 0
            then (1+ i)
            until (= i number-of-generators)
            do (write-vector-to-file stream
                                     (distributed-vector total-size
                                                         chunks repeats))))))

(defun distributed-vector (size chunks repeats)
  (let ((vector (make-array size :initial-element 0))
        (upper-limit (/ size chunks repeats)))
    (loop for i from 0 to (1- size)
          for element = -1 then element
          when (zerop (mod i chunks))
          do (setf element (mod (1+ element) upper-limit))
          do (setf (elt vector i) element)
          finally (return vector))))