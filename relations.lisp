(in-package :fundamental-functions)

;; relations

(defclass relation ()
  ((source :accessor source-of-relation :initarg :source)
   (target :accessor target-of-relation :initarg :target)
   (graph  :accessor graph-of-relation  :initarg :graph )))

(defmethod print-object ((obj relation) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "{~&~{~2:T~S~^,~&~I~}}" (graph obj))))

(defun relation-p (rel A B)
  "Returns non-NIL if REL is a relation on AxB."
  (and (subsetp (source-of-relation rel) A)
       (subsetp (target-of-relation rel) B)
       (valid-graph-p (graph-of-relation rel) A B)))

(defun valid-graph-p (graph A B)
  "Returns non-NIL if GRAPH is subset of AxB."
  (and (subsetp (mapcar #'first graph) A :test #'set-equal)
       (subsetp (mapcar #'second graph) B :test #'set-equal)))

(defun make-relation (source target graph)
  "Creates RELATION out of SOURCE, TARGET and GRAPH."
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
