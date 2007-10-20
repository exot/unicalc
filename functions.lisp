(in-package :functions-and-relations)

(defclass function ()
  ((source :accessor source-of :initarg :source)
   (target :accessor target-of :initarg :target)
   (graph  :accessor graph-of  :initarg :graph)))