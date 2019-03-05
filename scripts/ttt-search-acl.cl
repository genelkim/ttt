#! /p/lisp/acl/linux/latest/alisp -#!
;; AUTHOR: Adam Purtee  <apurtee@cs.rochester.edu>
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence
;; Should eventually make this portable across Lisp implementations.

(when (not (>= (length (sys:command-line-arguments)) 2))
  (format t "USAGE: ttt-search pattern treebank~%")
  (format t "   o  pattern should be in quotation marks ~%")
  (format t "   o  treebank should contain one tree per line. ~%")
  (format t "      (i.e., charniak-parser output)")
  (exit))

(load "/p/nl/tools/ttt/src/load")
(in-package :ttt)


(let* ((use-stdin (= (length (sys:command-line-arguments)) 2))
       (treebank-file-handle (if use-stdin *standard-input*
				 (open (nth 2 (sys:command-line-arguments)))))
       (pattern (lispify-parser-output (nth 1 (sys:command-line-arguments)))))
  
  (search-tb treebank-file-handle pattern :verbose t)
  (if (not use-stdin) (close treebank-file-handle)))
	   			  


