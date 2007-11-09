(in-package :fundamental-functions)

;; relations

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
    ((or all (<= (card (graph relation)) max-size))
     (format stream "{~&~{~2:T~S~^,~&~I~}}" (graph relation)))
    (t
     (format stream "{~&~{~2:T~S~^,~&~I~}}~&~2:T..."
	     (subseq (graph relation)
		     0 max-print)))))

(defun relation-p (rel A B)
  "Returns non-NIL if REL is a relation on AxB."
  (declare (type relation rel)
	   (type standard-set A B))
  (and (subsetp (source-of-relation rel) A)
       (subsetp (target-of-relation rel) B)
       (valid-graph-p (graph-of-relation rel) A B)))

(defun valid-graph-p (graph A B)
  "Returns non-NIL if GRAPH is subset of AxB."
  (declare (type standard-set graph A B))
  (and (subsetp (mapcar #'first graph) A :test #'set-equal)
       (subsetp (mapcar #'second graph) B :test #'set-equal)))

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
