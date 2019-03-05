(in-package :ttt)
;; possible optimizations: 
;;   lazy computation of (length tree-seq)
;;   cache that computation
(defclass unrestricted-seq (pattern  has-iter-constraints)
  ()
  (:documentation 
   "Unrestricted sequences of general forms: 
   _![n] _+[n] _?[n] _*[n-m]
   with stickied, named syntax:
   _!.[n]var _+.[n]var _?.[n]var _*.[n-m]var

  matching a sequence of length
  exactly n, at least n, at most n, 
  and between n and m, respectively."))


(defmethod compile-pattern ((patt unrestricted-seq))
  "Set the match-fn for unrestriced sequence pattern patt."
  (when (not (initialized? patt))
    (if (not (and (atom (to-expr patt))
		  (member (get-op (to-expr patt)) '(_! _* _? _+))))
	(error 
	 "bad expression as initial unrestricted-seq ~A~%" (to-expr patt)))
    
    (let* ((op (get-op (to-expr patt)))
	   (v (get-var (to-expr patt)))	 
	   (n (get-/n (to-expr patt)))
	   (m (get-/m (to-expr patt))))

      (case op
	((_!) (setf n (if (get-/n (to-expr patt)) (get-/n (to-expr patt)) 1)
		    m (if (get-/n (to-expr patt)) (get-/n (to-expr patt)) 1)))
	((_+) (setf n (if (get-/n (to-expr patt)) (get-/n (to-expr patt)) 1)
		    m most-positive-fixnum))
	((_*) (setf n (if (get-/n (to-expr patt)) 
			  (get-/n (to-expr patt)) 
			  0)
		    m (if (get-/m (to-expr patt)) 
			  (get-/m (to-expr patt)) 
			  most-positive-fixnum)))
	((_?) (setf n 0 
		    m (if (get-/n (to-expr patt)) (get-/n (to-expr patt)) 1)))
	(otherwise 
	 (error "unexpected condition while compiling ~A~%" (to-expr patt))))
      (setf (min-iter patt) n)
      (setf (max-iter patt) m)
      (setf (min-width patt) n)
      (setf (max-width patt) m)
      (setf (var patt) v))
    (setf (initialized? patt) t))

  (setf 
   (match-fn patt)
   (compile 
    nil
    (eval
     `(wrap-sticky-check 
       ',(var patt)
       (lambda (tree-seq bindings ) 
	 (if 
	  (and (>= (length tree-seq) ,(min-iter patt))
	       (<= (length tree-seq) ,(max-iter patt)))
	  (add-binding (mk-binding ',(var patt) tree-seq)
		       bindings)))))))
  (setf (compiled? patt) t)
  patt)
