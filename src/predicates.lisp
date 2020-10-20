(in-package :ttt)
;; predicates.lisp
;; predicates always match single trees
;; predicates do not modify bindings and therefore cannot be bound or sticky
;; predicates do not take arguements
;; they are boolean functions over trees
;; tree access is done via the tree class
;; when predicates are single TTT patterns,
;; some things are precomputed (such as min/max height and keys)
;; recursive or mutually recursive predicates are not supported
;; when build-pattern is called on a predicate, it should search
;; the predicate table and return the proper object,
;; which was previously created by defpred-ttt OR is an fbound
;; function with name ending in "?" which accepts a tree-seq
;; as its only argument

;;uses patterns.lisp
(defclass predicate-patt (pattern)
  ((patt :accessor patt)
   (ttt-pred :accessor ttt-pred :initform nil))
  (:documentation
   "A predicate may only match tree-sequences of length 1.
    Their only arguments should be a tree sequence.
    They do not modify bindings."))

(defmethod match ((pred predicate-patt) tree-seq bindings)
  "Predicates do not access or modify bindings."
  (declare (type list tree-seq))
  (if (= (length tree-seq) 1)
      (if
       (if (ttt-pred pred)
           (funcall (match-fn pred) (car tree-seq))
           (funcall (match-fn pred)
                    (to-expr (car tree-seq))))
       (add-binding (mk-binding (to-expr pred) tree-seq) bindings))))


(defparameter *predicate-table*
  (make-hash-table))
(defun get-pred-instance (pred-op)
  (let ((pred (gethash pred-op *predicate-table*)))
    (if pred (return-from get-pred-instance pred))
    (setf pred (make-instance 'predicate-patt))
    (setf (min-width pred) 1
          (max-width pred) 1)
    (setf (to-expr pred) pred-op)
    (setf (match-fn pred)
          (get-pred-fn pred-op))
    pred))


;; TODO: use this function in 'pred-sym-p' of operators.lisp. will need to move
;; function or reorder files since this file comes after operators.lisp
(defun get-pred-fn (sym)
  "Return the function for the base predicate symbol."
  (declare (type symbol sym))
  (let* ((str (string sym))
         (pkg (symbol-package sym))
         (qps (position #\? str)))
    (symbol-function
      (intern (subseq str 0 (if qps (1+ qps) nil)) pkg))))


(defun mk-pred-ttt (pred-op patt-expr)
  "Defines a predicate function within the TTT package. The predicate operator
  will be interned to TTT and lose the package it is in when called. TODO: make
  a version that is more general to other packages."
  (declare (type symbol pred-op))
  (let ((pred (make-instance 'predicate-patt))
        (pat (build-pattern patt-expr))
        (pred-op (intern (string pred-op) :ttt)))
    (setf (patt pred) pat)
    (setf (match-fn pred)
          (lambda (tree)
            (funcall (match-fn pat) (list tree) t)))
    (setf (to-expr pred) pred-op)
    (setf (min-width pred) 1
          (max-width pred) 1
          (min-height pred) (min-height pat)
          (max-height pred) (max-height pat)
          (keys pred) (keys pat))
    (setf (ttt-pred pred) t)
    (setf (get pred-op 'pred-keys) (keys pred))
    (add-op pred-op :predicate t)
    (setf (gethash pred-op *predicate-table*) pred)))


(defun store-pred (symbol function)
  (setf *built-patterns* (make-hash-table :test #'equal))
  (setf (symbol-function symbol) function)
  (setf (get symbol 'op) symbol))

