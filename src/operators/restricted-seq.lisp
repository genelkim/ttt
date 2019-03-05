(in-package :ttt)
;; optimizations:
;; option 1: patterns check min/max height, width and abandon early 
;; option 2: patterns never check and always assume it's possible to succeed
;;           and that they have never been called when destined to fail.
;; option2 - seems to be faster
(defclass restricted-seq (pattern  
			  has-iter-constraints 
			  has-pos-args
			  has-neg-args)
  ()
  (:documentation 
  "Restricted sequences of general forms:
   (![n] P1 ... Pp ~ N1 ... Nn)
   (+[n] P1 ... Pp ~ N1 ... Nn)   
   (?[n] P1 ... Pp ~ N1 ... Nn)
   (*[n-m] P1 ... Pp ~ N1 ... Nn)

   The iterative constraints and negated arguments are optional. 
   When there are no negated arguments, the ~ should be omitted.
   A special case is: (! ~ N1 ... Nn), which matches any tree
   which does not match any of the negated arguments."))
  

(defstruct rs-state
  n  ;; min-iter
  m  ;; max-iter
  a  
  tseq
  binds)


(defmethod compile-pattern ((patt restricted-seq))
  "Set the match-fn for restricted sequence pattern patt."
  (when (not (initialized? patt))
    (setf 
     (var patt) (patt-expr-get-var (to-expr patt))
     (pos-args patt) (mapcar 
		      #'build-pattern 
		      (patt-expr-pos-args (to-expr patt)))
     (neg-args patt) (mapcar 
		      #'build-pattern 
		      (patt-expr-neg-args (to-expr patt)))
     (min-width patt) (patt-expr-min-width (to-expr patt))
     (max-width patt) (patt-expr-max-width (to-expr patt)))

    (case (patt-expr-get-op (to-expr patt))
      ((!)   
       (setf (min-iter patt)  (if (get-/n (car (to-expr patt))) 
				  (get-/n (car (to-expr patt))) 
				  1)
	     (max-iter patt)  (if (get-/n (car (to-expr patt))) 
				  (get-/n (car (to-expr patt))) 
				  1)))
      ((+)
       (setf (min-iter patt)  (if (get-/n (car (to-expr patt))) 
				  (get-/n (car (to-expr patt))) 
				  1)
	     (max-iter patt)  most-positive-fixnum))
      ((*)
       (setf (min-iter patt) (if (get-/n (car (to-expr patt))) 
				 (get-/n (car (to-expr patt))) 
				 0)
	     (max-iter patt) (if (get-/m (car (to-expr patt))) 
				 (get-/m (car (to-expr patt)))
				 most-positive-fixnum)))
      ((?)
       (setf (min-iter patt) 0
	     (max-iter patt) (if (get-/n (car (to-expr patt))) 
				 (get-/n (car (to-expr patt))) 
				 1)))
      (otherwise
       (error "unexpected operator parsed while compiling ~A~%" 
	      (to-expr patt))))

    (if (and (= (min-width patt) 1)
	     (= (max-width patt) 1))
	(setf (min-height patt) (reduce #'min (cons (min-height patt)
						    (mapcar 
						     #'min-height 
						     (pos-args patt))))
	      (max-height patt) (reduce #'max (cons (max-height patt)
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
       ',(var patt)
       (lambda (tree-seq bindings )
	 (if ,(and (null (pos-args patt))
		   (not (null (neg-args patt))))
	     ;; (* ~ X1 ... Xn) (! ~ X1 ... Xn)
	     ;; (+ ~ X1 ... Xn) (? ~ X1 ... Xn)

	     ;; check length
	     ;; check each expression
	     (if (and (>= (length tree-seq) ,(min-iter patt))
		      (<= (length tree-seq) ,(max-iter patt)))
		 
		 (if (not 
		      (dolist (tr tree-seq)
			(if 
			 (dolist (n ',(neg-args patt))
			   (if (match n (list tr) bindings) 
			       (return t)))
			 (return t))))
		     (add-binding (mk-binding ',(var patt) tree-seq)
				  bindings))
		 nil)
	     (let (stack   ;; not (! ~ N))
		    (start 
		     (make-rs-state 
		      :n ,(min-iter patt)
		      :m ,(max-iter patt)
		      :binds bindings
		      :tseq tree-seq)))
	       (push start stack)
	       (loop while stack do
		    (let ((current (pop stack)))
		      ;; goal-p: 
		      ;;  (and (= min-iter 0) (null tseq))
		      (if (and (= (rs-state-n current) 0)
			       (null (rs-state-tseq current)))
			  (return (add-binding 
				   (mk-binding ',(var patt) tree-seq)
				   (rs-state-binds current))))
		     
		      ;; min-iter > 0 OR tseq not null 
		      ;; if (max-iter current)  > 0
		      (if (> (rs-state-m current) 0)
			  (dolist (p ',(pos-args patt))
			     (loop for split 
				  ;; so that we're guaranteed to make progress
				from (max 1 (min-width p))
				to (min (max-width p) 
					(length (rs-state-tseq current))) do 
				  (let ((b (match p
					     (subseq (rs-state-tseq current) 
						     0 split)
					     (rs-state-binds current))))
				    (when b ;; found a matching prefix
				      (if (not
					   (dolist (n ',(neg-args patt))
					     (if (match n
						   (subseq (rs-state-tseq current)
							   0 split)
						   (rs-state-binds current))
						 (return t))))
					  (let ((state
						 (make-rs-state 
						  :n (max 
						      0 (1- (rs-state-n current)))
						  :m (1- (rs-state-m current))
						  :binds b
						  :tseq (subseq 
							 (rs-state-tseq current) 
							 split))))
					    (push state stack)))))))))))))))))
					    
  (setf (compiled? patt) t)
  patt)
