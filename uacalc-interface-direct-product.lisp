(in-package :uacalc-interface)

;;; direct products

(defun uab-calculate-direct-power (input-project power output-project)
  (declare (type uacalc-project input-project output-project)
	   (type integer power))
  "Calculates the direct product ALGEBRA^POWER in UACalc."
  (with-uab-command-to-file (stream (command-file input-project))
    (uab-commands-to-create-direct-power stream
					 (file-name input-project)
					 power
					 (file-name output-project)
					 (universe-file output-project)))
  (run-uab (pure-file-name input-project)))

(defun calculate-direct-power-numerically (algebra power)
  "Calculates the POWERth direct power of ALGEBRA numerically with UACalc."
  (with-algebras ((input-project algebra)) ((algebra-file-name output-project))
    (uab-calculate-direct-power input-project power output-project)))

(defun uab-calculate-direct-product (input-projects output-project)
  (declare (type list input-projects)
	   (type uacalc-project output-project))
  "Calculates the direct product of INPUT-PROJECTS in UACalc."
  (with-uab-command-to-file (stream (command-file (first input-projects)))
    (uab-commands-to-create-direct-product stream
					 (mapcar #'file-name input-projects)
					 (file-name output-project)
					 (universe-file output-project)))
  (run-uab (pure-file-name (first input-projects))))

(defun calculate-direct-product-numerically (algebras)
  (declare (type list algebras))
  "Calculates the direct product of the list of ALGEBRAS numerically with
UACalc."
  (with-algebras (input-projects algebras) ((algebra-file-name output-project))
    (uab-calculate-direct-product input-projects output-project)))