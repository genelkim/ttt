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

(declaim (ftype (function (index) (simple-array hash-table *)) index))
(defclass index ()
    ((index :accessor index)))
(defun make-index ()
  (let ((new-index (make-instance 'index)))
    (setf (index new-index) (make-array 3))
    (loop for n from 0 to 2 do
         (setf (aref (index new-index) n)
               (make-hash-table :test #'eq)))
    new-index))
(defmethod fastmap (key (idx index))
  "used as an accessor by build-tree to reverse the list"
  (gethash (car key) (aref (index idx) (cdr key))))
(defmethod add-to-index (key value (idx index))
  "add key/value pair to index.  does not check for duplication of values.
   values may be stored in any order."
  (push value (gethash (car key) (aref (index idx) (cdr key)))))

(declaim (ftype (function ((or list symbol number)) list) filter-ops))


(defun extract-keys (pattern &key (no-ops nil) (no-dups t) (maxdepth 2))
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
  (labels
    ((rechelper (pat depth)
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
              (append curres recres))))
         (t (error "Unknown recursive considition for extract-keys~%")))))
    (rechelper
      (if no-ops (list pattern) (filter-ops pattern maxdepth)) ; tODO: this seems backwards.
      0)))

;;; TODO: clean up this monstrosity of formatting code organization.
;;;       it's really just an if statement with each clause containing
;;;       a large append which should be factored into variables.
;(defun extract-keys (pattern &key (no-ops nil) (no-dups t))
;  "return a list of keys, where each key is a cons of the form
;  (token . depth)
;  => (extract-keys 'X)
;  ((X . 0)
;  => (extract-keys '(S (NP (NN He)) (VP (VBZ Ran)) (|.| |.|)))
;  ((S . 1) (NP . 2)  (VP . 2) (|.| . 2))
;  => (extract-keys '(<> X Y (S (NP (NN He)) (VP (VBZ Ran)) (|.|  |.|))))
;  ((X . 0) (Y . 0) (S . 1) (NP . 2) (VP . 2) (|.| . 2))"
;  (format t "======extract-keys=======~%")
;  (format t "pattern: ~s~%" pattern)
;  (labels
;    ((clean-and-add-depth (depth lst)
;       ; Remove nil and duplicates. Then add the depth level.
;       (mapcar #'(lambda (x) (cons x depth))
;               (let ((nonil (delete nil lst)))
;                 ;; Remove duplicates if in argument.
;                 (if no-dups
;                   (delete-duplicates nonil)
;                   nonil))))
;     ) ; end of labels defn.
;
;    ; labels body.
;    (let ((lvl0 (clean-and-add-depth
;                  0
;                  (mapcar
;                    #'(lambda (x) (if (atom x) x))
;                    (if no-ops (list pattern) (filter-ops pattern)))))
;          (lvl1 (clean-and-add-depth
;                  1
;                  (the list
;                       (reduce #'append
;                               (mapcar
;                                 (lambda (y) (if (listp y)
;                                               (mapcar
;                                                 (lambda (x)
;                                                   (if (atom x) x)) y)))
;                                 (if no-ops (list pattern) (filter-ops pattern)))))))
;          (lvl2 (clean-and-add-depth
;                  2
;                  (the list
;                       (reduce
;                         #'append
;                         (mapcar
;                           (lambda (z)
;                             (if (listp z)
;                               (delete
;                                 nil
;                                 (the list
;                                      (reduce
;                                        #'append
;                                        (mapcar
;                                          (lambda (y) (if (listp y)
;                                                        (mapcar
;                                                          (lambda (x)
;                                                            (if (atom x) x)) y))) z))))))
;                           (if no-ops (list pattern) (filter-ops pattern))))))))
;      (append lvl0 lvl1 lvl2))))

