(in-package :ttt)
(defclass free-seq (pattern has-pos-args)
  ()
  (:documentation
   "Free standing sequence of form: 
    (<> P1 P2 ... Pp)"))
(defmethod compile-pattern ((patt free-seq))
  (compile-helper patt))

(defclass permuted-seq (pattern has-pos-args has-neg-args) 
  ()
  (:documentation 
   "Permuted sequence of form: 
   ({} P1 P2 ... Pp)"))

(defmethod compile-pattern ((patt permuted-seq))
  (compile-helper patt))

(defclass general-patt (pattern has-pos-args) 
  ()
  (:documentation
  "General tree pattern:
   (_+ X Y _! Z)"))

(defmethod compile-pattern ((patt general-patt))
  (compile-helper patt))


(defstruct h-state 
  pseq tseq binds)
			     
(defun compile-helper (patt)
  "Compile general tree, free-standing, and permutation patterns."
  (let* ((cn (class-name (class-of patt))))
    (unless (member cn '(free-seq permuted-seq general-patt))
      (error "combine-helper should not be called on class ~A~%" cn))
    (when (not (initialized? patt))
      (setf (min-width patt) (patt-expr-min-width (to-expr patt))
	    (max-width patt) (patt-expr-max-width (to-expr patt))
	    (pos-args patt)  (mapcar #'build-pattern 
				     (patt-expr-pos-args (to-expr patt))))

      (unless (eq (class-name (class-of patt)) 'general-patt)
	(setf (var patt) (patt-expr-get-var (to-expr patt))))
      (if (and (= (min-width patt) 1)
	       (= (max-width patt) 1))
	  (setf (min-height patt) (reduce #'min (cons 0 (mapcar 
							 #'min-height 
							 (pos-args patt))))
		(max-height patt) (reduce #'max (cons most-positive-fixnum 
						      (mapcar 
						       #'max-height 
						       (pos-args patt))))))
      (setf (initialized? patt) t))
    
    (setf 
     (match-fn patt)
     (compile 
      nil
      (eval 
       `(wrap-sticky-check 
	 ',(and (not (eq cn 'general-patt)) (var patt))
	 (lambda (tree-seq bindings )
	   (when (or (and ,(eq cn 'general-patt) 
			  (consp tree-seq) 
			  (null (cdr tree-seq))
			  (listp (children (car tree-seq))))
		     ,(not (eq cn 'general-patt)))
	     (let (stack
		   (start (make-h-state 
			   :pseq ',(pos-args patt)
			   :tseq (if ,(eq cn 'general-patt)
				     (children (car tree-seq))
				     tree-seq)
			   :binds bindings)))
	       (push start stack)

	       (loop while stack do 
		    (let ((current (pop stack)))
		      (if (null (h-state-pseq current))
			  (if (null (h-state-tseq current))
			      (return 
				(if ,(eq cn 'general-patt)
				    (h-state-binds current)
				    (add-binding 
				     (mk-binding 
				      ',(if (not (eq cn 'general-patt)) 
					    (var patt)
					    nil)
				      tree-seq)
				     (h-state-binds current)))))
			  ;; pick an operator 
			  (loop for i
			     from 0 
			     to (if ,(eq cn 'permuted-seq)
				    (1- (length (h-state-pseq current)))
				    0) do 
			       (let ((p 
				      (if ,(eq cn 'permuted-seq)
					  (nth i (h-state-pseq current))
					  (car (h-state-pseq current)))))
				 ;; pick a prefix of tree-seq to 
				 ;; match to the first argument
				 (loop for split 
				    from (min-width p)
				    to (min 
					(max-width p) 
					(length (h-state-tseq current)))
				    do
				      (let ((b 
					     (match  p 
					       (subseq 
						(h-state-tseq current) 
						0 split)
					       (h-state-binds current)
					       )))
					;; foreach prefix and valid arg... 
					(if b 
					    (push 
					     (make-h-state
					      :pseq 
					      (if 
					       ,(eq cn 'permuted-seq)
					       (nconc ;; all but ith
						(subseq 
						 (h-state-pseq current) 
						 0 i)
						(subseq 
						 (h-state-pseq current) 
						 (1+ i)))
					       (rest (h-state-pseq current)))
					      ;; all but first n
					      :tseq  
					      (subseq 
					       (h-state-tseq current) 
					       split)
					      :binds b)
					     stack)))))))))))))))))
  (setf (compiled? patt) t)
  patt)
