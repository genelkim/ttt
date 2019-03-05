(in-package :ttt)
(defclass stuck-patt (pattern ) ()
  (:documentation 
   "A class for matching free-standing sticky variables associated with 
    operators which would typically require arugments and which can be sticky.
    I.e., ! + * ? <> {} ^ ^* ^@)."))

(defmethod compile-pattern ((patt stuck-patt))
  (unless (atom patt) 
    (error "instances of stuck-patt must be atoms, got ~A~%" (to-expr patt)))
  (when (not (initialized? patt))
    (setf (var patt) (get-var (to-expr patt)))
    (setf (initialized? patt) t))
  ;; it's min-width, max-width, min-height, max-height, and keys 
  ;; all depend on the current bindings
  ;; somehow need to update these when the correct variable is bound.
  ;; a global sticky variables bindings table?
  ;; the unhappy quick solution is to use the worst-case values
  ;; and optimize later if lots of sticky variables are used
  (setf (match-fn patt)
	(compile 
	 nil
	 (eval 
	  `(lambda (tree-seq bindings)
	     (if (bound? ',(var patt) bindings)
		 (if (equal 
		      (mapcar #'to-expr (get-binding ',(var patt) bindings))
		      (mapcar #'to-expr tree-seq))
		     bindings)
	       (error "cannot match unbound sticky pattern without args: ~A~%" 
		      ',(to-expr patt)))))))
  (setf (compiled? patt) t)
  patt)

(declaim (inline wrap-sticky-check))
(defun wrap-sticky-check (var function)
  (if (sticky? var)
      (lambda (tree-seq bindings)
	(if (bound? var bindings)
	    (if (equal 
		 (mapcar #'to-expr (get-binding var bindings))
		 (mapcar #'to-expr tree-seq))
		bindings)
	    (funcall function tree-seq bindings)))
      function))
