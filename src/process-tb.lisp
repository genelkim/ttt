(in-package :ttt)
(defun process-tb (ruleset treebank-file &key inspect-fn inspect-show-src inspect-show-res disp-summary new-tb-fn)
  "Apply a set of rules to a treebank. 
   treebank-file should be consist of one parse tree per line.
   ruleset should be a list of TTT rules in expression form
   note: Lisp will change (CD 0600) to (CD 600)."

  (let ((tree-n 0)
	(mod-trees 0)
	(mod-this-tree nil)
	(*print-pretty* nil)
	(treebank-fh (open treebank-file))
	(inspection-fh (if inspect-fn (open inspect-fn
						       :direction :output
						       :if-exists :supersede)))
	(new-treebank-fh (if new-tb-fn 
			     (open new-tb-fn 
				   :direction :output
				   :if-exists :supersede)
			     *standard-output*)))
    (loop for tree = (read-line treebank-fh nil)   
       while tree do
	 (let* ((tree-expr (lispify-parser-output tree))
		(source-tree tree-expr)
		(prev source-tree)
		rules-applied)
	   (incf tree-n)
	   (setf mod-this-tree nil)
	   (loop for rule-n from 0 to (1- (length ruleset)) do
		(setf tree-expr (apply-rule (nth rule-n ruleset) tree-expr))
		(when (not (equal tree-expr prev))
		  (when (not mod-this-tree)
		    (incf mod-trees)
		    (setf mod-this-tree t))
		  (push rule-n rules-applied)
		  (setf prev tree-expr)))
	   (setf rules-applied (nreverse rules-applied))
	   (when (and inspection-fh rules-applied)
	     (format inspection-fh "~a " tree-n)
	     (format inspection-fh "~{~a~^ ~}~%" rules-applied)
	     (if inspect-show-src
		 (format inspection-fh "~a~%" source-tree))
	     (if inspect-show-res
		 (format inspection-fh "~a~%" tree-expr)))
	   (if mod-this-tree
	       (format new-treebank-fh "~a~%" tree-expr))))
    (when disp-summary
      (format t "~a trees modified~%" mod-trees)
      (format t "~a trees processed~%" tree-n))
    (dolist (fh (list treebank-fh inspection-fh new-treebank-fh))
      (if fh (close fh)))))
