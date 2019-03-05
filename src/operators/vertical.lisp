(in-package :ttt)
(defclass vertical-patt (pattern  has-pos-args has-neg-args)
  ()
  (:documentation 
   "Vertical path of form: 
   (^@ P1 P2 ... Pp ~ N1 N2 ... Nn)"))

(defstruct v-state
  pargs
  tseq
  binds
  (leading-vars nil))
  
(defmethod compile-pattern ((patt vertical-patt))
  (when (not (initialized? patt))
    (if (patt-expr-neg-args (to-expr patt)) 
	(error 
	 "negative arguments in vertical patterns currently unsupported."))
    (setf (var patt) (patt-expr-get-var (to-expr patt))
	  (pos-args patt) (mapcar #'build-pattern 
				  (patt-expr-pos-args (to-expr patt))))
    (setf (initialized? patt) t))

  (setf 
   (match-fn patt)
   (compile 
    nil
    (lambda (tree-seq bindings )
      (let (stack 
	    current
	    (start (make-v-state 
		    :pargs (pos-args patt)
		    :tseq tree-seq
		    :binds bindings)))
      (if (and (pos-args patt) ;; (^@) - always fails
	       ;; ^@ - always matches a sequence of exactly one tree
	       (= (length tree-seq) 1)) 
	  (push start stack))
      (loop while stack do 
	   (setf current (pop stack))
	   (if (null (v-state-pargs current))  ;; goal? 
	       (return (add-binding 
			(mk-binding (var patt) tree-seq) 
			(v-state-binds current))) 
	       (let ((leading-patt (first (v-state-pargs current)))) 
		 (if (and (= (min-width leading-patt) 1)
			  (= (max-width leading-patt) 1))
		     (let ((result-binds 
			    (match
			      ;; ensure-compiled only compiles when uncompiled
			      (ensure-compiled leading-patt)  
			      (v-state-tseq current)
			      (v-state-binds current)
			      )))
		       (when result-binds
			 ;; update bindings
			 (setf result-binds 
			       (add-binding 
				(mk-binding 
				 (var leading-patt) 
				 (v-state-tseq current)) 
				result-binds))
			 (dolist (v (v-state-leading-vars current))
			   (setf result-binds 
				 (add-binding 
				  (mk-binding v (v-state-tseq current)) 
				  result-binds)))
			 ;; early goal check
			 (if (null (rest (v-state-pargs current))) 
			     (return (add-binding 
				      (mk-binding (var patt) tree-seq) 
				      result-binds)))
			 ;; push next states after matching the leading pattern
			 (if (and (get-binding '@ result-binds)
				  (not (equal (get-binding '@ 
					       (v-state-binds current))
					      (get-binding '@
					       result-binds))))
			     (push (make-v-state 
				    :pargs (rest (v-state-pargs current))
				    :tseq (get-binding '@ result-binds)
				    :binds result-binds)
				   stack)
			     (if (consp 
				  (children (first (v-state-tseq current))))
				 (dolist (c (children 
					     (first (v-state-tseq current))))
				   (push (make-v-state 
					  :pargs (rest (v-state-pargs current))
					  :tseq  (list c)
					  :binds result-binds)
					 stack))))))
		     ;; leading pattern must be a sequence operator, 
		     ;; so push next states
		     (mapcar (lambda (vstate) (push vstate stack)) 
			     (next-v-states current))))))))))
  (setf (compiled? patt) t)
  patt)


(defun next-v-states (vstate)
  (let (result 
	(leading-patt (first (v-state-pargs vstate))))
    (case (class-name (class-of leading-patt))
      ((unrestricted-seq) 
       (if (= (min-iter leading-patt) 0)        
	   (push (make-v-state 
		  :pargs (rest (v-state-pargs vstate))
		  :tseq  (v-state-tseq vstate)
		  :binds (let ((b (add-binding 
				   (mk-binding (var leading-patt) nil) 
				   (v-state-binds vstate))))
			   (dolist (v (v-state-leading-vars vstate))
			     (setf b 
				   (add-binding 
				    (mk-binding v nil) 
				    b)))
			   b))
		 result))
       (if (> (max-iter leading-patt) 0)
	   (push (make-v-state 
		  :pargs
		  (cons 
		   (make-instance 
		    'unrestricted-seq 
		    :min-width 1
		    :max-width 1
		    :min-iter 1
		    :max-iter 1
		    :var (var leading-patt)
		    :initialized? t)
		   (if (> (max-iter leading-patt) 1)
		       (cons 
			(make-instance 
			 'unrestricted-seq  ;; var nil, min-w 0, max-w inf
			 :min-width (max 0 (1- (min-iter leading-patt)))
			 :max-width (1- (max-iter leading-patt))
			 :min-iter (max 0 (1- (min-iter leading-patt)))
			 :max-iter (1- (max-iter leading-patt))
			 :var nil
			 :initialized? t)
			 (rest (v-state-pargs vstate)))
		       (rest (v-state-pargs vstate))))
		  :tseq (v-state-tseq vstate)
		  :binds (v-state-binds vstate)) 
		 result)))
      ((restricted-seq)  
       (if (= (min-iter leading-patt) 0)
	   (push (make-v-state 
		  :pargs (rest (v-state-pargs vstate))
		  :tseq (v-state-tseq vstate)
		  :binds (add-binding 
			  (mk-binding (var leading-patt) nil) 
			  (v-state-binds vstate)))
		 result))
       (if (> (max-iter leading-patt) 0)
	   (dolist (parg (pos-args leading-patt))
	     (push (make-v-state 
		    :leading-vars  ;; not sure about this
		    (cons (var leading-patt)
			  (v-state-leading-vars vstate))
		    :pargs 
		    (cons 
		     parg ;; expected to be compiled
		     (if (> (max-iter leading-patt) 1)
			 (cons 
			  (make-instance 
			   'restricted-seq
			   :pos-args (pos-args leading-patt)
			   :min-iter (max 0 (1- (min-iter leading-patt)))
			   :max-iter (1- (max-iter leading-patt))
			   :min-width (if (= (min-iter leading-patt) 0)
					  0
					  (- (min-width leading-patt)
					     (/ (min-width leading-patt)
						(min-iter leading-patt))))
			   :max-width (if (= (max-iter leading-patt) 0)
					  0
					  (- (max-width leading-patt)
					     (/ (max-width leading-patt)
						(max-iter leading-patt))))
			   :neg-args (neg-args leading-patt)
			   :var nil
			   :initialized? t)
			  (rest (v-state-pargs vstate)))
			 (rest (v-state-pargs vstate))))
		    :tseq (v-state-tseq vstate)
		    :binds (v-state-binds vstate))
		   result))))
      ((free-seq)  
       (push (make-v-state
	      :leading-vars
	      (cons (var leading-patt)
		    (v-state-leading-vars vstate))
	      :pargs 
	      (if (> (length (pos-args leading-patt)) 0)
		  (cons (first (pos-args leading-patt))
			(if (> (length (pos-args leading-patt)) 1)
			    (cons (make-instance 
				   'free-seq
				   :min-width  (- (min-width leading-patt) 
						  (min-width 
						   (first 
						    (pos-args leading-patt))))
				   :max-width  (- (max-width leading-patt) 
						  (max-width 
						   (first 
						    (pos-args leading-patt))))
				   :initialized? t
				   :pos-args (rest (pos-args leading-patt)))
				  (rest (v-state-pargs vstate)))
			    (rest (v-state-pargs vstate))))
		  (rest (v-state-pargs vstate)))
	      :tseq (v-state-tseq vstate)
	      :binds (v-state-binds vstate))
	     result))
      ((permuted-seq) 
       ;; could reduce permutations by taking into account max height
       (if (> (length (pos-args leading-patt)) 0)
	   (loop for n from 0 to (1- (length (pos-args leading-patt))) do
		(push 
		 (make-v-state
		  :pargs 
		  (cons (nth n (pos-args leading-patt))
		       (if (> (length (pos-args leading-patt)) 1)
			   (cons 
			    (make-instance 
			     'permuted-seq
			     :min-width (- (min-width leading-patt) 
					   (min-width 
					    (nth n (pos-args leading-patt))))
			     :max-width (- (max-width leading-patt) 
					   (max-width 
					    (nth n (pos-args leading-patt))))
			     :pos-args (append 
					(subseq (pos-args leading-patt) 0 n)
					(subseq (pos-args leading-patt) 
						(1+ n)))
			    :initialized? t)
			    (rest (v-state-pargs vstate)))
			   (rest (v-state-pargs vstate))))
		  :tseq (v-state-tseq vstate)
		  :binds (v-state-binds vstate)
		  :leading-vars 
		  (cons (var leading-patt)
			(v-state-leading-vars vstate)))
		 result))
	   (push (make-v-state
		  :pargs nil
		  :tseq (v-state-tseq vstate)
		  :binds (let ((b (add-binding 
				   (mk-binding (var leading-patt) nil)
				   (v-state-binds vstate))))
			   (dolist (v (v-state-leading-vars vstate))
			     (setf b (add-binding (mk-binding v nil) b)))
			   b))
		 result)))
      (otherwise 
       (error "unrecognized sequence pattern: ~A" leading-patt)))
    (nreverse result)))


(defclass point-of-attachment (pattern) ())
(defmethod compile-pattern ((patt point-of-attachment))
  (setf (var patt) (patt-expr-get-var (to-expr patt)))
  (setf (match-fn patt) 
	(compile
	 nil
	 (eval
	  `(lambda (tree-seq bindings )
	     (if (and (consp tree-seq) (null (cdr tree-seq)))
		 (add-binding 
		  (mk-binding ',(var patt) tree-seq)
		  bindings))))))
  (setf (compiled? patt) t)  
  patt)
