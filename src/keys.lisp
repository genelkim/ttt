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
    ((index :accessor index)))
(defun make-index ()
  (let ((new-index (make-instance 'index)))
    (setf (index new-index) (make-array 3))
    (loop for n from 0 to 2 do
	 (setf (aref (index new-index) n) (make-hash-table :test #'eq)))
    new-index))
(defmethod fastmap (key (idx index))
  "used as an accessor by build-tree to reverse the list"
  (gethash (car key) (aref (index idx) (cdr key))))
(defmethod add-to-index (key value (idx index))
  "add key/value pair to index.  does not check for duplication of values.
   values may be stored in any order."
  (push value (gethash (car key) (aref (index idx) (cdr key)))))

 (defun extract-keys (pattern &key (no-ops nil) (no-dups t))
  "return a list of keys, where each key is a cons of the form
    (token . depth)
    => (extract-keys 'X)
    ((X . 0)
    => (extract-keys '(S (NP (NN He)) (VP (VBZ Ran)) (|.| |.|)))
    ((S . 1) (NP . 2)  (VP . 2) (|.| . 2))
    => (extract-keys '(<> X Y (S (NP (NN He)) (VP (VBZ Ran)) (|.|  |.|))))
    ((X . 0) (Y . 0) (S . 1) (NP . 2) (VP . 2) (|.| . 2))"
  (if no-dups
      (append 
       (mapcar 
	(lambda (x) (cons x 0))
	(delete-duplicates 
	 (delete 
	  nil 
	  (mapcar 
	   (lambda (x) (if (atom x) x))
	   (if no-ops (list pattern) (filter-ops pattern))))))
       (mapcar 
	(lambda (x) (cons x 1)) 
	(delete-duplicates 
	 (delete nil 
		 (reduce #'append 
			 (mapcar 
			  (lambda (y) (if (listp y) 
					  (mapcar 
					   (lambda (x) 
					     (if (atom x) x)) y))) 
			  (if no-ops (list pattern) (filter-ops pattern)))))))
       (mapcar 
	(lambda (x) (cons x 2)) 
	(delete-duplicates 
	 (delete 
	  nil 
	  (reduce 
	   #'append 
	   (mapcar 
	    (lambda (z) 
	      (if (listp z) 
		  (delete 
		   nil 
		   (reduce 
		    #'append 
		    (mapcar 
		     (lambda (y) (if (listp y) 
				     (mapcar 
				      (lambda (x) 
					(if (atom x) x)) y))) z))))) 
	    (if no-ops (list pattern) (filter-ops pattern))))))))
      (append
       (mapcar 
	(lambda (x) (cons x 0)) 
	(delete 
	 nil 
	 (mapcar 
	  (lambda (x) (if (atom x) x)) 
	  (if no-ops (list pattern) (filter-ops pattern)))))
       (mapcar 
	(lambda (x) (cons x 1)) 
	(delete 
	 nil 
	 (reduce 
	  #'append 
	  (mapcar 
	   (lambda (y) 
	     (if (listp y) 
		 (mapcar (lambda (x) (if (atom x) x)) y))) 
	   (if no-ops (list pattern) (filter-ops pattern))))))
       (mapcar 
	(lambda (x) (cons x 2)) 
	(delete 
	 nil 
	 (reduce 
	  #'append 
	  (mapcar
	   (lambda (z) 
	     (if (listp z) 
		 (delete 
		  nil 
		  (reduce 
		   #'append 
		   (mapcar 
		    (lambda (y) 
		      (if (listp y) 
			  (mapcar (lambda (x) (if (atom x) x)) y))) z))))) 
	   (if no-ops (list pattern) (filter-ops pattern)))))))))
