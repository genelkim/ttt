(in-package :ttt)
;; keys.lisp
;;
;; a key is a pair of the form (symbol . depth)
;; For examples of keys, see documentation of extract-keys, below.
;;
;; If a pattern has a required key that is not present in a tree,
;;   then the match will also fail.
;; A split into multiple sets  "must have at least one from each set" might be
;;   useful to further prune matching, but for now I am only using keys as one
;;   such set.

;; There are three potential uses of keys:
;;   1 speed up deep-match  (which in turn speeds up apply-rule)
;;   2 speed up apply-rules
;;   3 speed up operators involving choices: ! + * ? {} ^

;; use 1 is accomplished by mapping from the keyset of a pattern
;;  to all sub-trees of a tree which have a non-empty keyset intersection

;; use 2 is accomplished by mapping from trees to applicable patterns

;; use 3 is accomplished in the same way as use 2

;; note that maintaining rule application order (and subtree search order)
;; may require computation of complete conflict sets and sorting of
;; testable subtrees by dfs-order

(defclass index ()
  ((index :type (simple-array hash-table *) :accessor index)))
(defun make-index ()
  (let ((new-index (make-instance 'index)))
    (setf (index new-index) (make-array 3))
    (loop for n from 0 to 2 do
         (setf (aref (the (simple-array hash-table *) (index new-index))
                     n)
               (make-hash-table :test #'eq)))
    new-index))
(declaim (inline fastmap))
(defun fastmap (key idx)
  "used as an accessor by build-tree to reverse the list"
  (gethash (the tree-expr (car key))
           (aref (the (simple-array hash-table *) (index idx))
                 (the fixnum (cdr key)))))
(declaim (inline add-to-index))
(defun add-to-index (key value idx)
  "add key/value pair to index.  does not check for duplication of values.
   values may be stored in any order."
  (push value (gethash (the tree-expr (car key))
                       (aref (the (simple-array hash-table *) (index idx))
                             (the fixnum (cdr key))))))


(declaim (inline extract-keys))
(defun extract-keys (pattern &key (with-ops nil) (no-dups t) (maxdepth 2))
  "return a list of keys, where each key is a cons of the form
  (token . depth)
  => (extract-keys 'X)
  ((X . 0)
  => (extract-keys '(S (NP (NN He)) (VP (VBZ Ran)) (|.| |.|)))
  ((S . 1) (NP . 2)  (VP . 2) (|.| . 2))
  => (extract-keys '(<> X Y (S (NP (NN He)) (VP (VBZ Ran)) (|.|  |.|))))
  ((X . 0) (Y . 0) (S . 1) (NP . 2) (VP . 2) (|.| . 2))

  This was re-implemented for better factoring and to eliminate
  the depth 2 limit."
  (declare (type fixnum maxdepth))
  (labels
    ((rechelper (pat depth)
       (declare (type fixnum depth))
       (cond
         ((> depth maxdepth) nil)
         ((atom pat) nil)
         ((listp pat)
          (let (;; Recursive result.
                (recres (apply #'append 
                               (mapcar #'(lambda (child)
                                           (rechelper child (1+ depth)))
                                       pat)))
                ;; Current result.
                ;; Take atomic elements, remove nil, cons with depth.
                (curres (mapcar #'(lambda (x) (cons x depth))
                                (remove-if #'null
                                           (remove-if-not #'atom pat)))))
            (if no-dups
              (delete-duplicates (append curres recres))
              (append curres recres)))))))
    (rechelper
      (if with-ops (list pattern) (filter-ops pattern maxdepth))
      0)))

