;; Added 1-18-2008.
;; trivial proof-of-concept example of parsing with a non-weighted grammar
(defparameter cf-grammar 
  "car is always interpreted as a non-terminal.  
  terminal symbols must be enclosed in (t _!). 
   rules are S->XY; X->left; Y->right"
  '((S X Y) (X (t left)) (Y (t right))))

(defun parse-cfg (cfg-rules sentence)
  "converts cfg-rules to ttt rules and then apply to sentence until convergence"
  (let* ((ttt-rules (mapcar (lambda (r)	
			      `(/ (_*1 ,(cons '<> (mapcar (lambda (sym) (if (atom sym) (list sym '_*) sym)) (rest r))) _*2)
				  (_*1 (,(first r) <>) _*2))) cfg-rules))
	 (intermediate (mapcar (lambda (sym) (transduce '(/ _! (T _!)) sym)) sentence)) 
	 (previous intermediate) tmp)
    (loop  
       (setf intermediate (apply-rules-dest ttt-rules intermediate :deep nil))
       (if (tree-equal intermediate previous)
	   (return)
	   (setf previous intermediate)))
    intermediate))

(defun parse-cfg-not-working (cfg-rules sentence)
  "This doesn't work because (/ lhs rhs) lhs and rhs are required to be single trees.
   The function above does work, though.
   I may add the handle such rule types later to TTT."
  (let* ((ttt-rules (mapcar (lambda (r)	
			      `(/ ,(cons '<> (mapcar (lambda (sym) (if (atom sym) (list sym '_*) sym)) (rest r)))
				  (,(first r) <>))) cfg-rules))
	 (intermediate (mapcar (lambda (sym) (transduce '(/ _! (T _!)) sym)) sentence)) 
	 (previous intermediate))
    (loop  
       (setf intermediate (mapcar (lambda (tree) (apply-rules ttt-rules tree)) intermediate))
       (if (tree-equal intermediate previous)
	   (return)
	   (setf previous intermediate)))
    intermediate))
