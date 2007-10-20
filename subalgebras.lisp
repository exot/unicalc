(in-package :subalgebras)

;;; calculate generating elements

(defun calculate-generating-elements (algebra)
  "Returns list of generating elements of ALGEBRA of minimal length."
  (labels ((check-incremental (n)
             (let ((elements (n-elements-generate-algebra algebra n)))
               (cond
                 (elements elements)
                 (t (check-incremental (1+ n)))))))
    (check-incremental 0)))

(defun n-elements-generate-algebra (algebra number-of-elements)
  "Return list of NUMBER-OF-ELEMENTS elements of ALGEBRA if NUMBER-OF-ELEMENTS elements
in ALGEBRA exists which generate the whole ALGEBRA."
  (labels ((check-first-subset-generates-algebra (subset)
             (cond
               ((null subset) nil)
               ((elements-generate-algebra-p (first subset) algebra)
                (first subset))
               (t (check-first-subset-generates-algebra (rest subset))))))
    (check-first-subset-generates-algebra (n-elemental-subsets (base-set-of algebra)
                                                               number-of-elements))))

(defun elements-generate-algebra-p (elements algebra)
  "Return non-NIL if ELEMENTS generate ALGEBRA."
  (let ((reachable-elements (all-reachable-elements elements algebra)))
    (cond
      ((set-equal reachable-elements (base-set-of algebra)) t)
      (t nil))))

(defun all-reachable-elements (elements algebra)
  "Returns all reachable elements from ELEMENTS in ALGEBRA."
  (let ((new-element (next-reachable-element algebra elements)))
    (cond
      ((not new-element) elements)
      (t (all-reachable-elements (cons new-element elements) algebra)))))

(defun next-reachable-element (algebra reachable-elements)
  "Returns new element in ALGEBRAS which is reachable by elements
from REACHABLE-ELEMENTS"
  (let ((value-tables (interpretations-on algebra)))
    (loop for table in value-tables
          do (iterate-over-value-table table element
               (when (and (subsetp (all-operands element) reachable-elements)
                          (not (member (value-of-element element) reachable-elements)))
                 (return-from next-reachable-element (value-of-element element)))))))

;;; here is still a lot todo
