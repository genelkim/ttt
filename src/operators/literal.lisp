(in-package :ttt)
(defclass literal-patt (pattern)
  ()
  (:documentation
  "Tree-literal pattern (e.g, a pattern involving no operators)"))
(defmethod compile-pattern ((patt literal-patt))
  "Tree is a tree in Lisp expression form: E.g, (X (Y) (Z))
   tree-seq is a list of instances of class tree, results of build-tree
   
   For efficiency, compile-tree-literal expects tree objects to have
   constant time access to their underlying expression through to-expr.
   Similarly, pattern is expected to be an instance of class pattern, 
   and to represent a tree literal.  The underlying literal should be
   able to be accessed in constant time through to-literal."
  (when (not (initialized? patt))
    (setf (min-width patt) 1
	  (max-width patt) 1
	  (min-height patt) (expr-height (to-expr patt))
	  (max-height patt) (expr-height (to-expr patt)))
    (setf (initialized? patt) t))
  
  (setf (match-fn patt)
	(compile
	 nil
	 (eval 
	  `(lambda (tree-seq bindings )
	     (if (and (consp tree-seq)  
		      (null (cdr tree-seq))
		      (= (height (car tree-seq)) ,(max-height patt))
		      (if ,(atom (to-expr patt))
			  (eq ',(to-expr patt)
			      (to-expr (car tree-seq)))
			  (equal ',(to-expr patt)
				 (to-expr (car tree-seq)))))
		 bindings)))))
  (setf (compiled? patt) t)
  patt)
  
