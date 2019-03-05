(in-package :ttt)
(defclass descendant-patt (pattern has-pos-args has-neg-args 
				   has-depth-constraints)
  ()
  (:documentation 
   "Descendant pattern of form:
   (^ P1 P2 ... Pp ~ N1 N2 ... Nn)
   (^* P1 P2 ... Pp ~ N1 N2 ... Nn)
   (^+ P1 P2 ... Pp ~ N1 N2 ... Nn)
   (^n P1 P2 ... Pp ~ N1 N2 ... Nn)
   (^^ P1 P2 ... Pp ~ N1 N2 ... Nn)"))
(defstruct d-state
  depth 
  node)
(defmethod compile-pattern ((patt descendant-patt))
  "Compile patterns with operators ^ ^^ ^* ^n"
  
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

    (case (get-op (car (to-expr patt)))
      ((^)  (if (get-/n (car (to-expr patt)))
		(setf (min-depth patt) (get-/n (car (to-expr patt)))
		      (max-depth patt) (get-/n (car (to-expr patt))))
		(setf (min-depth patt) 
		      (count #\^ (symbol-name (car (to-expr patt))))
		      (max-depth patt) 
		      (count #\^ (symbol-name (car (to-expr patt)))))))
					      
      ((^*) (if (and (get-/n (car (to-expr patt)))
		     (get-/m (car (to-expr patt))))
		(setf (min-depth patt) (get-/n (car (to-expr patt)))
		      (max-depth patt) (get-/m (car (to-expr patt))))
		(setf (min-depth patt) 0
		      (max-depth patt) most-positive-fixnum)))
      ((^+) (if (and (get-/n (car (to-expr patt)))
		     (get-/m (car (to-expr patt))))
		(setf (min-depth patt) (get-/n (car (to-expr patt)))
		      (max-depth patt) (get-/m (car (to-expr patt))))
		(setf (min-depth patt) 1
		      (max-depth patt) most-positive-fixnum)))
      (otherwise
       (error "unusual operator cannot be compiled as descendant ~A~%" 
	      (to-expr patt))))
    
    (setf (initialized? patt) t))
  (setf (match-fn patt) 
	(compile 
	 nil
	 (eval
	  `(wrap-sticky-check
	    ',(var patt)
	    (lambda (tree-seq bindings )
	      (if (and (consp tree-seq) 
		       (null (cdr tree-seq)))
		  (let (stack
			(start (make-d-state :depth 0 :node (car tree-seq))))
		    (push start stack)
		    (block search
		      (loop while stack do 
			   (let ((current (pop stack)))
			     (if (and (>= (d-state-depth current) 
					  ,(min-depth patt))
				      (<= (d-state-depth current) 
					  ,(max-depth patt)))
				 (dolist (p ',(pos-args patt))
				   (let ((b (match p 
					      (list (d-state-node current)) 
					      bindings
					      )))
				     (when b 
				       (let (failed)
					 (dolist (neg ',(neg-args patt))
					   (when 
					       (match 
						neg 
						(list (d-state-node current)) 
						bindings
						)
					     (setf failed t)
					     (return)))
					 (if (not failed) 
					     (return-from 
					      search 
					       (add-binding 
						(mk-binding 
						 ',(var patt) 
						 tree-seq)
						b))))))))
			     (unless (or (leaf? (d-state-node current))
					 (= (d-state-depth current) 
					    ,(max-depth patt)))
			       (dolist (c (children (d-state-node current)))
				 (push (make-d-state 
					:depth (1+ (d-state-depth current)) 
					:node c)
				       stack)))))))))))))
  (setf (compiled? patt) t)
  patt)
