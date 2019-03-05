#! /p/lisp/acl/linux/latest/alisp -#!
;; AUTHOR: Adam Purtee  <apurtee@cs.rochester.edu>
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence
;; Should eventually make this portable across Lisp implementations.


(when (not (>= (length (sys:command-line-arguments)) 2))
  (format t "USAGE:  ttt-apply  (-f ruleset | \"RULE\") treebank~%~%")
  (format t "  o see doc/example-ruleset for example of rulesets file~%")
  (format t "  o a single rule should be a single transduction, e.g., \"(/ THIS THAT)\"")
  (format t "  o treebank should contain one tree per line. ~%")
  (format t "    (i.e., charniak-parser output)")
  (exit))

(load "/p/nl/tools/ttt/src/load")
(in-package :ttt)

					
(let* ((ruleset (if (string-equal (nth 1 (sys:command-line-arguments)) "-f")
		    (with-open-file (fh (nth 2 (sys:command-line-arguments) ))
		      (eval (eval (read fh))))
		    (list (read-from-string (nth 1 (sys:command-line-arguments))))))
       (treebank-filename (if (string-equal (nth 1 (sys:command-line-arguments)) "-f")
			      (nth 3 (sys:command-line-arguments))
			      (nth 2 (sys:command-line-arguments)))))
  (process-tb ruleset treebank-filename))


		  
	   
	     


