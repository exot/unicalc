(in-package :fundamental-functions)

;; relations

(define-simple-condition relations-error)

; for the time being we only consider binary relations
(defclass relation ()
  ((source :type standard-set :accessor source-of-relation :initarg :source)
   (target :type standard-set :accessor target-of-relation :initarg :target)
   (graph  :type standard-set :accessor graph-of-relation  :initarg :graph )))

(defmethod print-object ((obj relation) stream)
  (print-unreadable-object (obj stream :type t)
    (print-relation obj stream)))

(defparameter *relation-print-max-size* 10)
(defparameter *relation-print-max-print* 5)
(defparameter *relation-print-all* nil)

(defun print-relation (relation stream
		       &key (max-size *relation-print-max-size*)
		       (max-print *relation-print-max-print*)
		       (all *relation-print-all*))
  (declare (type relation relation)
	   (type stream stream)
	   (type integer max-size max-print))
  "Print all entries of RELATION if |RELATION| <= MAX-SIZE, otherwise
prints only 5 entries"
  (cond
    ((or all (<= (card-s (graph relation)) max-size))
     (format stream "{")
     (loop-over-set elt (graph relation)
       (format stream "~&~2:T~S" elt))
     (format stream "}"))
    (t
     (format stream "{")
     (loop for rest = (graph relation) then (rest-s rest)
           for elt = (first-s rest)
	   for i below max-print
	   do (format stream "~&~2:T~S" elt))
     (format stream "}"))))

(defun relation-p (rel A B)
  "Returns non-NIL if REL is a relation on AxB."
  (declare (type relation rel)
	   (type standard-set A B))
  (and (subsetp-s (source-of-relation rel) A)
       (subsetp-s (target-of-relation rel) B)
       (valid-graph-p (graph-of-relation rel) A B)))

(defun valid-graph-p (graph A B)
  "Returns non-NIL if GRAPH is subset of AxB."
  (declare (type standard-set graph A B))
  (forall ((x y) graph)
    (and (set-member-s x A)
	 (set-member-s y B))))

(defun make-relation (source target graph)
  "Creates RELATION out of SOURCE, TARGET and GRAPH."
  (declare (type standard-set source target graph))
  (cond
    ((valid-graph-p graph source target)
     (make-instance 'relation
                    :source source
                    :target target
                    :graph  graph))
    (t (error 'malformed-relation-definition
              :text (format nil "~A is not a Graph on ~A times ~A"
                            graph source target)))))

(define-simple-condition malformed-relation-definition)

(defgeneric source (func-or-rel)
  (:documentation "Returns SOURCE of FUNC-OR-REL"))

(defgeneric target (func-or-rel)
  (:documentation "Returns TARGET of FUNC-OR-REL"))

(defgeneric graph (func-or-rel)
  (:documentation "Returns GRAPH of FUNC-OR-REL"))

(defmethod source ((func-or-rel relation))
  (source-of-relation func-or-rel))

(defmethod target ((func-or-rel relation))
  (target-of-relation func-or-rel))

(defmethod graph (func-or-rel)
  (graph-of-relation func-or-rel))

;;; iterating over relation graphs

(defmacro iterate-over-relation-graph (relation element &body body)
  "Iterates with ELEMENT over all elements in (GRAPH RELATION)"
  (with-gensyms (relation-graph pair)
    `(let ((relation-graph (graph ,relation)))
      (loop-over-set pair relation-graph
	(destructuring-bind ,element pair
	  ,(if (listp element)
	       `(declare (ignorable ,@element))
	       `(declare (ignorable ,element)))
	  ,@body)))))

(defun value-of-element (element)
  "Returns value of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (declare (type list element))
  (second element))

(defun all-operands (element)
  "Returns all operands of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (declare (type list element))
  (first element))

(defun nth-operand (element n)
  "Returns nth operand of ELEMENT when used in ITERATE-OVER-FUNCTION-GRAPH."
  (declare (type list element)
	   (type integer n))
  (nth n (all-operands element)))
