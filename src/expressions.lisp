(in-package :ttt)
;; These functions are used only by build-pattern and build-tree
;; to construct the respective objects from expressions.


(defun patt-expr-neg-args (expression)
  "Given patt = '(op X1 X1 ... Xm ~ Y1 Y2 ... Yn) return '(Y1 Y2 ... Yn)"
  (let ((pos (position (intern "~") expression)))
    (if pos (subseq expression (1+ pos)))))

(defun patt-expr-pos-args (expression)
  "Given patt = '(op X1 X2 ... Xm ~ Y1 Y2 ... Yn) return '(X1 X2 ... Xm)"
  (if (and (get-op (car expression))
	   (op-requires-args? (get-op (car expression))))
      (subseq expression 1 (position (intern "~") expression))
      expression))

(defun expr-height (expression)
  (if (atom expression) 
      0
      (1+ (reduce #'max (mapcar #'expr-height expression)))))

(defun patt-expr-min-width (expression)
  (if (patt-expr-get-op expression)
      (if (listp expression)
	  (let ((n (get-/n (car expression))) (m (get-/m (car expression))))
	    (case (patt-expr-get-op expression)
	      ('! 
	       (if (patt-expr-pos-args expression) 
		   (if n 
		       (* n (reduce 
			     #'min 
			     (mapcar 
			      #'patt-expr-min-width 
			      (patt-expr-pos-args expression)))) 
		       (reduce 
			#'min 
			(mapcar 
			 #'patt-expr-min-width 
			 (patt-expr-pos-args expression)))) 1))
	      ('+ 
	       (if (patt-expr-pos-args expression) 
		   (if n 
		       (* n 
			  (reduce 
			   #'min 
			   (mapcar 
			    #'patt-expr-min-width (patt-expr-pos-args expression)))) 
		       (reduce 
			#'min 
			(mapcar 
			 #'patt-expr-min-width (patt-expr-pos-args expression)))) 1))
	      ('? 0)
	      ('* 
	       (if (and n m) 
		   (* n 
		      (reduce 
		       #'min 
		       (mapcar 
			#'patt-expr-min-width (patt-expr-pos-args expression)))) 
		   0))
	      ('<> 
	       (reduce 
		#'+ 
		(mapcar 
		 #'patt-expr-min-width (patt-expr-pos-args expression))))
	      ('{} 
	       (reduce 
		#'+ 
		(mapcar 
		 #'patt-expr-min-width (patt-expr-pos-args expression))))
	      ('^ 1)
	      ('^^ 1)
	      ('^* 1)
	      ('^+ 1)
	      ('^n 1)
	      ('^@ 1)
	      ('/ 1)
	      ('literal 1)
	      ('general 1)
	      (otherwise 
	       (when (not (op-is-pred? expression)) 
		 (print 'op-error1) 
		 (print (patt-expr-get-op expression)) 
		 0))))
	  (let ((n (get-/n expression)))
	    (case (patt-expr-get-op expression)
	      ('_! (if n n 1))
	      ('_+ (if n n 1))
	      ('_? 0)
	      ('@ 1)
	      ('_* (if n n 0))
	      ('literal 1)
	      (otherwise 
	       (when (not (op-is-pred? expression))
		 (print 'op-error2) 
		 (print (patt-expr-get-op expression)))
	       1))))
      (if (sticky? expression) 0 1)))
  

(defun patt-expr-max-width (expression)
  (if (patt-expr-get-op expression)
      (if (listp expression)
	  (let ((n (get-/n (car expression))) (m (get-/m (car expression))))
	    (case (patt-expr-get-op expression)
	      ('! 
	       (if (patt-expr-pos-args expression)
		   (if n 
		       (* n 
			  (reduce 
			   #'max 
			   (mapcar 
			    #'patt-expr-max-width (patt-expr-pos-args expression))))
		       (reduce 
			#'max 
			(mapcar 
			 #'patt-expr-max-width (patt-expr-pos-args expression))))
		   1))
	      ('+
	       most-positive-fixnum)
	      ('? 
	       (if n 
		   (* n 
		      (reduce 
		       #'max 
		       (cons 1
		       (mapcar 
			#'patt-expr-max-width (patt-expr-pos-args expression)))))
		   (reduce 
		    #'max 
		    (cons 1
			  (mapcar 
			   #'patt-expr-max-width (patt-expr-pos-args expression))))))
	      ('* 
	       (if m 
		   (* m 
		      (reduce 
		       #'max 
		       (mapcar 
			#'patt-expr-max-width (patt-expr-pos-args expression)))) 
		   most-positive-fixnum))
	      ('<> 
	       (reduce 
		#'+ 
		(mapcar 
		 #'patt-expr-max-width (patt-expr-pos-args expression))))
	      ('{} 
	       (reduce 
		#'+ 
		(mapcar 
		 #'patt-expr-max-width (patt-expr-pos-args expression))))
	      ('^ 1)
	      ('^^ 1)
	      ('^* 1)
	      ('^+ 1)
	      ('^n 1)
	      ('/ 1)
	      ('^@ 1)
	      ('literal 1)
	      ('general 1)
	      (otherwise 
	       (when (not (op-is-pred? expression)) 
		 (print 'op-error3) 
		 (print (patt-expr-get-op expression)) 
		 most-positive-fixnum))))
	  (let ((n (get-/n expression)) (m (get-/m expression)))
	    (case (patt-expr-get-op expression)
	      ('_! (if n n 1))
	      ('_+ most-positive-fixnum)
	      ('@ 1)
	      ('_? (if n n 1))
	      ('_* (if (and n m) m most-positive-fixnum))
	      ('literal 1)
	      (otherwise 
	       (when (not (op-is-pred? expression)) 
		 (print 'op-error4) 
		 (print (patt-expr-get-op expression)))
	       most-positive-fixnum))))
      (if (sticky? expression) 
	  most-positive-fixnum 
	  1)))


(defun patt-expr-get-op (expression)
    "If an operator exists which handles the particular pattern, 
     then return the proper operator, 
     otherwise return literal for tree literals or general for nonliterals."
    (if (consp expression)
	(cond ((and (get-op (car expression))
		    (op-requires-args? (get-op (car expression))))
	       (get-op (car expression)))
	      ((equal (filter-ops expression) (list expression))  'literal)
	      (t 'general))
	(if (op-requires-args? (get-op expression))
	    (if (sticky? (get-var expression)) 'sticky-check)
	    (get-op expression))))


(defun patt-expr-get-var (expression)
  "If the top-level pattern should be bound, return the variable."
  (when (listp expression) 
    (return-from patt-expr-get-var 
      (let ((op (get-op (car expression))))
	(if (and op (op-requires-args? op))
	    (get-var (car expression))))))
  (let ((op (get-op expression)))
    (unless (op-requires-args? op)
      (get-var expression))))

(defun filter-ops (expression)
  "return filter-ops + finite-required-sets-of-ors for the same depth

   => (filter-ops   '((! PP-IN PP-WITH) _*))
   (PP-IN PP-WITH)

   ((! S SBAR) _+)   - fire only when S or SBAR encountered
   ((! S _+)   _+)   - fire on every tree

   for patterns with choices (! ... +) 

   (! if this can only match symbols from a finite set, return that set
         otherwise, return the empty set)

   (+ if there exists a finite subset of symbols such that this rule 
         must match a symbol from within that set then return that set


   what this buys us: 
   if (null (intersection (patt-keys expression) (tree-keys tree)))
      then the pattern will fail to match tree

   so that we can map from trees to applicable patterns: 
   
   (foreach sub-tree in tree do
      (dolist (key (tree-keys sub-tree))
           (dolist (pattern (gethash key *rules-by-key*))
                   (let ((bindings (match pattern sub-tree)))
                        (if bindings (print 'successful-match))))))

   
   re-ranker parses of Brown have an average of 2.85 keys per sub-tree

   return a list of the freestanding elements
   => (filter-ops '(<> X Y)) 
   (X Y)

   => (filter-ops '(X Y))
   ((X Y))
   could extend to filter-all - return ALL such sets to any depth"
     
  (if (atom expression)
      (if (not (or (eq (get-op expression) 'literal) (numberp expression)))
	  (keys-to-filter-ops (get expression 'pred-keys))  ;; for predicates
	  (list expression))
      (case (get-op (car expression))
	((+ !)
	 (unless (member nil (mapcar #'filter-ops (rest expression)))
	   (reduce #'append (mapcar #'filter-ops (rest expression)))))
	((^@ /) (filter-ops (nth 1 expression)))
	((* ? ^ ^*) nil)
	((<> {}) (reduce #'append (mapcar #'filter-ops expression)))
	(otherwise (list (reduce #'append (mapcar #'filter-ops expression)))))))



(defun keys-to-filter-ops (keys)
  "for handling predicates.  a crutch. 
   switch from ((A.0) (B.0) (C.1) (D.2) (E.2)) to (A B (C) (D E))"
  (mapcar (lambda (key) (listify (cdr key) (car key))) keys))

(defun listify (n expr)
  (if (= n 0) 
      expr
    (listify (1- n) (list expr))))
