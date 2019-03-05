(in-package :ttt)
;; A symbol can have an operator, 
;; a variable (which may also have a sticky dot), 
;; and one or two iterative constraints, referred to as n and m. 

(defparameter *operation-table* nil)


(defun add-op (op &key (predicate) (requires-args))
  "Insert op into the operation table.  Associate the operation with match-fn.  
   Optionally set predicate or requires-args booleans.  (both default to nil)"
  (setf op (intern (string op) :ttt))
  (setf (get op 'op) op)      
  (setf (get op 'predicate) predicate)
  (setf (get op 'requires-args) requires-args)
  (push op *operation-table*))


(add-op '_!)
(add-op '_?)
(add-op '_*)
(add-op '_+)
(add-op '* :requires-args t)
(add-op '+ :requires-args t)
(add-op '! :requires-args t)
(add-op '? :requires-args t)
(add-op '<> :requires-args t)
(add-op '{} :requires-args t)
(add-op '^ :requires-args t)
(add-op '^* :requires-args t)
(add-op '^+ :requires-args t)
(add-op '^@ :requires-args t)
(add-op '@ )
(add-op '/ :requires-args t)




(defun get-op (sym)   
  "Return the operation associated with sym.  
   If sym is a variable then return the symbol which corresponds 
   to the variable with additional binding  information stripped. 
   The operation is stored in sym's property list to avoid
   future textual searching. If sym is a list or represents a literal then 
   return nil."
  (cond ((consp sym) nil)
	((numberp sym) 'literal)
	((get sym 'op) (get sym'op))
	(t 
	 (when (op-is-pred? sym)
	   (setf (get sym 'op) sym)
	   (return-from get-op sym))
	 (loop for i from 1 to (min 2 (length (symbol-name sym))) do
	      (let* ((temp-op (intern (subseq (symbol-name sym) 0 i) :ttt)))
		(when (and (get temp-op 'op) 
			   (valid-op-suffix (subseq (symbol-name sym) i)))
		  (setf (get sym 'op) (get temp-op 'op))
		  (let ((lbracket-pos (search "[" (symbol-name sym)))
			rbracket-pos)
		    (when lbracket-pos
		      (setf rbracket-pos (search "]" (symbol-name sym) 
						 :start2 (1+ lbracket-pos)))
		      (multiple-value-bind (n a)
			  (parse-integer 
			   (subseq (symbol-name sym) 
				   (1+ lbracket-pos) 
				   rbracket-pos)
			   :junk-allowed t)
			(when n 
			  (setf (get sym '/n) n)
			  (when (equal "-" (subseq (symbol-name sym) 
						   (+ 1 lbracket-pos a) 
						   (+ 2 lbracket-pos a)))
			    (let ((m (parse-integer (subseq 
						     (symbol-name sym) 
						     (+ 2 lbracket-pos a)) 
						    :junk-allowed t)))
			      (when m (setf (get sym '/m) m))))))))
		  (return-from get-op (get sym 'op)))))
	 (setf (get sym 'op) 'literal))))

(defparameter *invalid-suffix-chars*
  (list #\! #\* #\+ #\_ #\? #\^ #\< #\> #\{ #\} #\@ #\Space))
(defun valid-op-suffix (string)
  "Added so that ** isn't parsed as an operator with suffix *"
  (loop for n from 0 to (1- (length string)) do
       (if (member (char string n) *invalid-suffix-chars*)
	   (return-from valid-op-suffix nil)))
  t)
	   
(defun sticky? (op)
  "Return true if the atomic symbol op represents a sticky variable; 
   memoize the result."
  (if (get-op op)
      (if (get op 'stickysearch)
	  (get op 'sticky)
	  (progn
	    (setf (get op 'stickysearch) t)
	    (if (search "." (symbol-name op))
		(setf (get op 'sticky) t) 
		(setf (get op 'sticky) nil))))))
(defun get-/n (sym)
  (if (get-op sym)
      (get sym '/n)))
(defun get-/m (sym)
  (if (get-op sym)
      (get sym '/m)))

(defun mk-var (op &optional bind)
  (let ((sym (gensym "SPECIALSYM")))
    (setf (get sym 'nobind) (null bind))
    (setf (get sym 'op) op)
    (setf (get sym 'forcebind) bind)
    sym))
(defun get-var (sym)
  "returns the thing which should be bound-to in the case of variables 
   (and predicates); returns nil in the case of a variable, a literal, 
   or a list"
  (if (not (listp sym))
      (if (and (get-op sym) (not (eq (get-op sym) 'literal)))
	  sym)))

(defun op-is-pred? (op)
  "Return t if the operation is a predicate.  Nil otherwise."
  (if (and (symbolp op)
	   (or (get op 'predicate)
	       (pred-sym-p op)))
      t))

(defun op-requires-args? (op)
  "Return t if the operation requires args (!,+,*,?,{},<>,^,^n,^*,^+,^@).  
   Nil otherwise (_!,_+,_*,_?)."
  (get op 'requires-args))

(defun purge-op (op)
  "Purge op from the operation table."
  (delete op *operation-table*))

(defun literal? (sym)
  "return t if sym can only match itself"
  (get (get-op sym) 'literal))

(defun variable? (sym)
  "return t if sym is an operator with a variable"
  (and (not (listp sym))
       (not (literal? sym))))



(defun pred-sym-p (sym) 
  "Check to see if the symbol is a predicate optionally followed by digits."
  (if (symbolp sym) 
      (let* ((str (string sym)) 
	     (pos (position #\? str))) 
	(and pos 
	     (fboundp (read-from-string (subseq str 0 (1+ pos))))
	     (not (find t (mapcar (lambda (x) (not (digit-char-p x))) 
				  (coerce (subseq str (1+ pos)) 'list))))))))
