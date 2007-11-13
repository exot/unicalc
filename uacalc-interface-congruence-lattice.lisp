(in-package :uacalc-interface)

(defun uab-calculate-congruence-lattice (input-project output-project)
  (declare (type uacalc-project input-project output-project))
  "Calculates the congruence lattice of INPUT-PROJECT."
  (with-uab-command-to-file (stream (command-file input-project))
    (uab-commands-to-compute-congruence-lattice stream
                                                (file-name input-project)
                                                (file-name output-project)
                                                (cong-file input-project)
                                                (princ-file input-project)
                                                (meet-irr-file input-project)))
  (run-uab (pure-file-name input-project)))