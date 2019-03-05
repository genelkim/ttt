(in-package :ttt)
(defun search-tb (treebank-stream patt-expr &key (verbose nil))
  "Patt-expr should be a basic s-expression.  treebank-file should be an open steam."
  (let ((*print-pretty* nil)
	(patt (build-pattern patt-expr))
	(n-searched 0)
	(n-matched 0))
    (loop for tree = (read-line treebank-stream nil)  
       while tree do
	 (incf n-searched)
	 (let ((binds (deep-match patt (build-tree (lispify-parser-output tree) :index-subtrees t))))
	   (when binds	
	     (incf n-matched)
	     (format t "~a~%" tree))))
    (if verbose (format t "nmatched: ~a/~a.~%" n-matched n-searched))))
