(in-package :UACalc-interface)

(define-simple-condition UACalc-interface-error)

(defun create-renaming-function (from-set to-set &key (equal #'equal))
  (cond
    ((not (= (card from-set)
	     (card to-set)))
     (error 'UACalc-interface-error :text
	    (format nil "~A is not of same cardinality as ~A"
		    from-set to-set)))
    (t (make-function from-set
		      to-set
		      (mapcar #'pair from-set to-set)
		      :equal-pred equal))))
