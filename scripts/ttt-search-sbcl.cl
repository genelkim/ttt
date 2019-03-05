#! /usr/bin/sbcl --script
;; AUTHOR: Adam Purtee  <apurtee@cs.rochester.edu>

(when (not (= (length sb-ext:*posix-argv*) 3))
  (format t "USAGE: ttt-search.cl pattern treebank~%")
  (format t "pattern should be in quotation marks ~%")
  (format t "treebank should contain one tree per line. ~%")
  (format t "  (i.e., charniak-parser output) ~%")
  (exit))

(load "load")
(in-package :ttt)
(defun search-tb (treebank-file patt-expr)
  (format t "pattern: ~a~%" patt-expr)
  (format t "treebank: ~a~%" treebank-file)
  (let ((*print-pretty* nil)
	(n-searched 0)
	(n-matched 0))
    (with-open-file (treebank-fh treebank-file)
      (loop for tree = (read-line treebank-fh nil)  
	 while tree do
	 (incf n-searched)
	 (print patt-expr)
	 (print tree)
	 (let ((binds (match-expr patt-expr (lispify-parser-output tree))))
	   (when binds	
	     (incf n-matched)
	     (format t "~a~%" tree)))))
    ;;(format t "~a trees searched.~%" n-searched)
    ;;(format t "~a trees matched.~%" n-matched)
    ))
(compile 'search-tb)


(handler-case 
 (search-tb (nth 2 sb-ext:*posix-argv*)
	    (lispify-parser-output (nth 1 sb-ext:*posix-argv*)))
 (sb-sys:interactive-interrupt () (sb-ext:quit)))
