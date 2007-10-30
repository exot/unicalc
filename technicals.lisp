(in-package :technicals)

(defun numbers (n number)
  "Returns list of N symbols of NUMBER."
  (cond 
    ((>= 0 n) ())
    (t (cons number (numbers (1- n) number)))))

(defun symbols (n number)
  "Returns list of N symbols of NUMBER."
  (numbers n number))

(defun pair (x y)
  (list x y))

(defun all-zero-except-n (list n)
  "Returns LIST with zeros except in position n"
  (cond
    ((or (null list) (>= n (length list)))
     (numbers (length list) 0))
    (t (let ((zeros (numbers (length list) 0)))
	 (setf (nth n zeros) (nth n list))
	 zeros))))

(defun positive-number-p (x)
  "Tests whether X is NUMBERP and PLUSP."
  (and (numberp x)
       (plusp x)))

(defun non-negative-number-p (x)
  "Tests whether X is NUMBERP and (NOT (MINUSP))."
  (and (numberp x)
       (not (minusp x))))

(defmacro define-simple-condition (name)
  `(progn
    (define-condition ,name ()
      ((text :initarg :text :reader text :initform "")))

    (defmethod print-object ((obj ,name) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (technicals::text obj))))))

(defun set-equal (set1 set2 &key (test #'equal))
  "Returns T if SET1 and SET2 are equal in sense of sets. Does recursive
  check if needed."
  (cond
    ((and (listp set1)
          (listp set2))
     (and (subsetp set1 set2 :test #'set-equal)
          (subsetp set2 set1 :test #'set-equal)))
    ((and (not (listp set1))
          (not (listp set2)))
     (funcall test set1 set2))
    (t nil)))

(defun tuple-equal (equal-pred x y)
  "Returns T if X and Y are lists with EQUAL-PRED elements at the same positions"
  (and (listp x)
       (listp y)
       (= (length x)
          (length y))
       (every equal-pred x y)))

(defun make-set (set &key (test #'set-equal))
  "Makes set out of SET, i.e. removes all duplicates from set which are SET-EQUAL."
  (remove-duplicates set :test test))

(defun card (set)
  "Returns cardinality of set."
  (length set))

(defun emptyp (set)
  "Returns T if set is empty"
  (zerop (card set)))

;;; permutations

;; (defun generate-all-permutations (set)
;;   "Returns function that generates permutations one by one."
;;   (labels ((<-set (a b)
;;              (member b (member a set)))
;;            (find-a-i (list)
;;              (cond
;;                ((<= (length list) 1) nil)
;;                ((<-set (second list) (first list)) (rest list))
;;                (t (find-a-i (rest list)))))
;;            (find-a-j (list a-i)
;;              (cond
;;                ((null list) nil)
;;                ((<-set a-i (first list)) list)
;;                (t (find-a-j (rest list) a-i))))
;;            (split (list1 list2)
;;              (cond
;;                ((null list2) list1)
;;                ((member (first list1) list2)
;;                 nil)
;;                (t (append (list (first list1))
;;                           (split (rest list1) list2))))))
;;     (let ((next-permutation (reverse set)))
;;       #'(lambda ()
;;           (let ((result next-permutation))
;;             (setf next-permutation
;;                   (when result
;;                     (let ((a-i-list (find-a-i result)))
;;                       (when a-i-list
;;                         (let* ((a-j-list (find-a-j result (first a-i-list)))
;;                                (between (split a-j-list a-i-list))
;;                                (rest (split result a-j-list)))
;;                           (print a-i-list)
;;                           (print a-j-list)
;;                           (print between)
;;                           (print rest)
;;                           (setf next-permutation
;;                                 (append (reverse rest)
;;                                         (list (first a-i-list))
;;                                         (rest between)
;;                                         (list (first a-j-list))
;;                                         (rest a-i-list))))))))
;;             result)))))
