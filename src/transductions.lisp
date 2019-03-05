(in-package :ttt)
(defclass transduction (pattern)
  ((lhs :accessor lhs)
   (rhs :accessor rhs))
  (:documentation   
   "Returns special bindings. 
    one transduction operator per rule
    no named variables     (no sticky)
    cannot be in iterated context 
    must match sequence of length 1
    must replace with sequence of length 1"))
(defstruct t-bind
  parent
  parent-idx
  template-expr)

(defmethod compile-pattern ((patt transduction))
  (unless (initialized? patt)
    (if (not (= (length (to-expr patt)) 3))
	(error "transduction operator requires exactly three arguments ~A~%"
	       (to-expr patt)))
    (setf (min-width patt) 1
	  (max-width patt) 1
	  (var patt) '/
	  (lhs patt) (build-pattern (nth 1 (to-expr patt)))
	  (rhs patt) (nth 2 (to-expr patt))
	  (initialized? patt) t))
  (setf 
   (match-fn patt)
   (compile
    nil 
    (eval 
     `(lambda (tree-seq bindings )
	(if (and (consp tree-seq) (null (cdr tree-seq)))
	    (let ((b (match ,(lhs patt) tree-seq bindings )))
	      (if b
		  (add-binding 
		   (mk-binding 
		    '/ 
		    (make-t-bind
		     :parent (parent (car tree-seq))
		     :parent-idx (parent-idx (car tree-seq))
		     :template-expr ',(rhs patt)))
		   b))))))))
  (setf (compiled? patt) t)
  patt)

(defun apply-rules (rules tree-expr &key 
				      (rule-order :slow-forward)
				      (trace nil)
				      (shallow nil) 
				      (max-n most-positive-fixnum))
  "Apply each of the rules in list rules to a tree expression
   until each rule is no longer applicable. The rules list is only
   processed once. Returns a new tree expression.

   shallow    limits rule applications to root of tree
   max-n      limits the maximum number of edits to a tree
   trace      when t, displays debugging info to stdout
              otherwise, when non-nil write debugging info to file
              appending to the file if it already exists
              
              trace format is one 4-tuple of lines per transduction:
              <rule expression>
              <tree before transduction>
              <tree after transduction>
              <blank line>

   rule-order
   :slow-forward   - apply each rule until that rule no longer
                     applies, possibly repeating the entire sequence
   :earliest-first - always apply the first rule in the list that 
                     is applicable, repeat until no rules are applicable
                     the rule list may be processed multiple times
   :fast-forward   - apply each rule at most once, in order, repeating
                     the list until convergence"

  (let ((tr (if shallow
		(build-tree tree-expr :index-subtrees nil)
		(build-tree tree-expr :index-subtrees t)))
	(trace-file (if trace (if (eq trace t) 
				  t
				  (open trace 
					:direction :output
					:if-exists :append
					:if-does-not-exist :create))))
	(compiled-rules (mapcar #'build-pattern rules))
	(converged nil)
	(prev tree-expr)
	(n 0))

    (case rule-order
      (:slow-forward
       (loop while (not converged) do
	    (setf converged t)
	    (dolist (r compiled-rules)
	      (let ((b (if shallow 
			   (match r (list tr) t)
			   (deep-match r tr)))
		    (converged2 nil))
		(loop while (and (not converged2) b (< n max-n)) do 
		     (setf tr (do-transduction tr (get-binding '/ b) b))
		     (if trace-file
			 (format trace-file "~a~%~a~%~a~%~%" 
				 (to-expr r) 
				 prev
				 (to-expr tr)))
		     (incf n)
		     (if (equal prev (to-expr tr))
			 (setf converged2 t)
			 (setf b 
			       (if shallow
				   (match r (list tr) t)
				   (deep-match r tr))
			       prev (to-expr tr)
			       converged nil)))))))

      (:earliest-first
       (loop while (not converged) do
	    (setf converged t)
	    (dolist (r compiled-rules)
	      (let ((b (if shallow
			   (match r (list tr) t)
			   (deep-match r tr))))
		(when b
		  (setf prev (to-expr tr))
		  (setf tr (do-transduction tr (get-binding '/ b) b))
		  (if trace-file
		      (format trace-file "~a~%~a~%~a~%~%"
			      (to-expr r)
			      prev
			      (to-expr tr)))
		  (if (not (equal prev (to-expr tr)))
		      (setf converged nil))
		  (return))))))


      (:fast-forward
       (loop while (not converged) do
	    (setf converged t)
	    (dolist (r compiled-rules)
	      (let ((b (if shallow 
			   (match r (list tr) t)
			   (deep-match r tr))))
		(when b 
		  (setf prev (to-expr tr))
		  (setf tr (do-transduction tr (get-binding '/ b) b))
		  (if trace-file
		      (format trace-file "~a~%~a~%~a~%~%"
			      (to-expr r)
			      prev
			      (to-expr tr)))
		  (if (not (equal prev (to-expr tr)))
		      (setf converged nil)))))))

      (otherwise (error "unrecognized option for rule-order")))
      
    (if (and trace-file (not (eq trace-file t))) (close trace-file))
    (to-expr tr)))

(defun apply-rule (rule-expr tree-expr &key 
					 (shallow nil) 
					 (trace nil)
					 (max-n most-positive-fixnum))
  "Apply a single rule to a tree expression until converged.
   Returns a new tree expression.

   shallow    limits rule applications to root of tree
   max-n      limits the maximum number of edits to a tree
   trace      when t, displays debugging info to stdout
              otherwise, when non-nil write debugging info to file
              appending to the file if it already exists
              format is (one triple of lines per transduction):
              <tree before transduction> 
              <tree after transduction> 
              <blank line>"


  (let ((tr (build-tree tree-expr :index-subtrees t))
	(compiled-rule (build-pattern rule-expr))
	(trace-file (if trace (if (eq trace t) 
				  t
				  (open trace 
					:direction :output
					:if-exists :append
					:if-does-not-exist :create))))
	(prev tree-expr)
	(converged nil)
	(n 0))
    (let ((b 
	   (if shallow
	       (match compiled-rule (list tr) t)
	       (deep-match compiled-rule tr))))
      (loop while (and (not converged) b (< n max-n)) do 
	   (setf tr (do-transduction tr (get-binding '/ b) b))
	   (if trace-file
	       (format trace-file "~a~%~a~%~%" prev (to-expr tr)))
	   (incf n)
	   (if (equal prev (to-expr tr))
	       (setf converged t)
	       (setf b 
		     (if shallow
			 (match compiled-rule (list tr) t)
			 (deep-match compiled-rule tr))
		     prev (to-expr tr)))))
    (if (and trace-file (not (eq trace-file t))) (close trace-file))
    (to-expr tr)))


(defun do-transduction (tree t-binding bindings)
  "Destructively modifies tree to implement the results of 
   a previously bound transduction operator. 
   Returns the modified tree."
  (let ((new-subtree  
	 (template-to-tree (t-bind-template-expr t-binding) bindings t))
	(par (t-bind-parent t-binding))
	(par-idx (t-bind-parent-idx t-binding)))
    (if (not (= (length new-subtree) 1))
	(error "transduction rhs cannot return more than one tree.")
	(setf new-subtree (build-tree (car new-subtree) :index-subtrees t)))
    (cond 
      ((null par) ;; replace root
       (setf (children tree) (children new-subtree)
	     (nchildren tree) (nchildren new-subtree)
	     (to-expr tree) (to-expr new-subtree)
	     (height tree) (height new-subtree)
	     (keys tree) (keys new-subtree)
	     (parent tree) nil
	     (parent-idx tree) nil)
       (dotimes (n (nchildren tree))
	 (setf (parent (nth n (children tree)))  tree
	       (parent-idx (nth n (children tree))) n)))
      (t
       (setf (parent new-subtree) par)
       (setf (children par) 
	     (append
	      (subseq (children par) 0 par-idx)
	      (cons 
	       new-subtree
	       (subseq (children par) (1+ par-idx)))))
       (dotimes (n (nchildren par))
	 (setf (parent-idx (nth n (children par))) n))
       (let ((ancestor par))
	 (loop while ancestor do
	      (setf (to-expr ancestor)
		    (mapcar 

		     #'to-expr
		     (children ancestor)))
	      (setf (keys ancestor)  ;; not the most efficient
		    (extract-keys (to-expr ancestor) :no-ops t))
	      (setf ancestor (parent ancestor))))
       )))
  (update-subtree-index tree) ;; not the most efficient
  (update-dfs-order tree)
  tree)
