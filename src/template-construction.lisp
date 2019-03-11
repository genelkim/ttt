(in-package :ttt)
;; construct.lisp 
;; This file handles instantiation of transduction RHS templates. 
;; constructive functions should always return sequences of tree expressions
;; they should accept free-standing sequences of arguments, 
;; with bound variables replaced with their tree-expression representations
;; functions should always be in the first position in a list

(defun template-to-tree (template-expr bindings return-expr)
  "Build a new tree expression from template-expression and bindings. 
   When return-expr is nil, a tree object is returned instead."
  (cond 
    (;; is template-expr a function?
     (and (consp template-expr)
	  (symbolp (car template-expr))
	  (fboundp (intern (symbol-name (car template-expr))))
	  (equal "!" (subseq (symbol-name (car template-expr))
			     (1- 
			      (length 
			       (symbol-name (car template-expr)))))))
     (list
     (funcall
      (if return-expr #'identity #'build-tree)
      (apply (intern (symbol-name (car template-expr)))
	     (reduce 
	      #'append
	      (mapcar 
	       (lambda (arg)
		 (template-to-tree arg bindings t))
	       (cdr template-expr)))))))
    ((consp template-expr)
      (list
       (reduce 
	#'append
	(mapcar 
	 (lambda (x) 
	   (template-to-tree x bindings return-expr ))
	 template-expr))))
    ((and (bound? template-expr bindings)
	  (not (eq (char (string template-expr) 0) #\/)))
     (mapcar 
      (if return-expr 
	  #'identity 
	  #'build-tree)
      (mapcar 
       #'to-expr
       (get-binding template-expr bindings))))
    (t ;; atomic literal
     (list 
      (funcall 
       (if return-expr 
	   #'identity 
	   #'build-tree) 
       template-expr)))))

(defun apply! (x &rest y) 
  "Applies the first argument to the rest of the arguments.
   This is a syntactic shortcut to avoid defining duplicates
   of lisp functions, but with the names ending in '!'."
  (apply x y))
     
(defun join-with-dash! (&rest args)
  "Takes a free-standing sequence of symbols as arguments. 
   Returns the symbol which is the concatenation of their symbol names, 
   with dashes in between.  Ex: (join-with-dash! 'X 'Y 'Z) => X-Y-Z"
  (if args 
      (let ((cur (car args)))
	(when (listp cur) 
	  (format t "error: join-with-dash! expects only symbol arguments") 
	  (return-from join-with-dash!))
	(setf cur (symbol-name cur))
	(dolist (sym (rest args))
	  (when (listp sym) 
	    (format t "error: join-with-dash! expects only symbol arguments") 
	    (return-from join-with-dash!))
	  (setf cur (concatenate 'string cur "-" (symbol-name sym))))
	(let ((s (intern cur)))
	  (return-from join-with-dash! (list s))))))

(defun concatenate-syms! (&rest args)
  "Takes a free-standing sequence of symbols as arguments. 
   Returns the symbol which is the concatenation of their symbol names, 
   with dashes in between.  Ex: (join-with-dash! 'X 'Y 'Z) => X-Y-Z"
  (if args 
      (let ((cur (car args)))
	(when (listp cur) 
	  (format t "error: concatenate-syms! expects only symbol arguments") 
	  (return-from concatenate-syms!))
	(setf cur (symbol-name cur))
	(dolist (sym (rest args))
	  (when (listp sym) 
	    (format t "error: concatenate-syms! expects only symbol arguments") 
	    (return-from concatenate-syms!))
	  (setf cur (concatenate 'string cur (symbol-name sym))))
	(let ((s (intern cur)))
	  (return-from concatenate-syms! (list s))))))



(defparameter *subst-new-next-int* 
  ;;"Simple global counter for new symbol integer extensions."
  0)
  
(defun subst-new! (sym expr)		
  "Replaces every occurence of symbol sym in expr with a single new symbol, 
   which is created by appending a period and an integer onto the symbol-name 
   of sym.   Ex: (subst-new! 'X (A X (X))) => (A X.1 (X.1))"
  (when (not (symbolp sym)) 
    (format t "error: subst-new! expects first argument to be a symbol.")
    (return-from subst-new!))
  (let ((newsym (intern (concatenate 'string 
				     (symbol-name sym) 
				     "." 
				     (write-to-string 
				      (incf *subst-new-next-int*))))))
    (deep-substitute newsym sym expr)))


(defun deep-substitute (newitem olditem sequence)
  "Recursively replace all occurences of olditem in sequence with newitem."
  (if (listp sequence) 
      (mapcar (lambda (x) (deep-substitute newitem olditem x)) sequence) 
      (if (eq olditem sequence) newitem sequence)))

