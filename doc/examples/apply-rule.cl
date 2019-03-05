#! /usr/staff/bin/alisp -#!
;; usage:   ./apply-rule.cl "(/ lhs rhs)" treebank.lisp



;; I could write this to use strings (raw parser output).
(when (not (>= (length (sys:command-line-arguments)) 3))
  (format t "usage:   ./apply-rule.cl \"(/ lhs rhs)\" treebank.lisp~%")
  (format t "treebank must be lisp-readable~%")
  (format t "only trees which match transduction are displayed~%")
  (format t "this script is intended for rule debugging~%")
  (format t "got args: ~S~%" (sys:command-line-arguments))
  (exit))

;; It might be nicer if I just had a single ttt.lisp sourcefile
(load "/p/nl/tools/ttt/src/util.lisp")
(load "/p/nl/tools/ttt/src/tree-rep.lisp")
(load "/p/nl/tools/ttt/src/match.lisp")
(load "/p/nl/tools/ttt/src/transduce.lisp")
(load "/p/nl/tools/ttt/src/construct.lisp")


;; need a more elegant way of passing function defs
(load "/p/nl/tools/ttt/examples/caption-funcdefs.lisp")

(let ((transduction (read-from-string (second (sys:command-line-arguments)))))
  (dolist (treebank-file (cddr (sys:command-line-arguments)))
    (with-open-file (fh treebank-file)
      (loop for tree = (read fh nil)
	 while tree do
	   (let ((tmp (apply-rule  transduction tree)))
	     (unless (tree-equal tmp tree)
	       (format t "ORIGINAL:~%")
	       (print tree)
	       (format t "~%TRANSFORMED:~%")
	       (print tmp)
	       (format t "~%---------------~%")))))))


  
	