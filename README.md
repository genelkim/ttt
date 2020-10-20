# TTT (Tree-to-Tree Transduction Language)

[![Build Status](https://travis-ci.com/genelkim/ttt.svg?branch=master)](https://travis-ci.com/genelkim/ttt)
[![Coverage Status](https://coveralls.io/repos/github/genelkim/ttt/badge.svg?branch=master)](https://coveralls.io/github/genelkim/ttt?branch=master)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

TTT is a language for transparently mapping between tree structures expressed in s-expressions. At a high-level it fills same role for trees as regex for strings. This is the copy of TTT maintained by Gene Louis Kim (<gkim21@cs.rochester.edu>), primarily for SBCL. The library was originally written by Adam Purtee, accompanied by the paper 
[TTT: A tree transduction language for syntactic and semantic processing](http://aclweb.org/anthology/W12-0803). Being the primary user of this library, I now maintain it and make extensions as I see fit. See a segment of the original README(below) for how to use it. The API has not changed save for a few additional keyword arguments and some new utility functions. First, load the library with Quicklisp `(ql:quickload :ttt)`.
```
Quick Summary of TTT features 
-----------------------------
Switch to the package:
(in-package :ttt)

To match a single pattern expression against a single tree expression, do: 
(match-expr pattern-expression tree-expression)

To apply a rule to a tree-expression until converged, do: 
(apply-rule rule-expr tree-expr)

To apply a list of rules, do: 
(apply-rules rule-expr-list  tree-expr)

Both apply-rules and apply-rule have the keyword arguments:

:shallow   - when t, only apply rules to the root of tree-expr (default nil)
:max-n     - when supplied, limit the rule application to n iterations. 
             E.g, to apply a rule at most once, do 
             (apply-rule rule-expr tree-expr :max-n 1)
             or
             (apply-rules (list rule-expr) tree-expr :max-n 1)

:trace     - when t, displays debugging info to stdout
             otherwise, when non-nil write debugging info to a specified file, 
	     appending to the file if it already exists
              
             trace format is a tuple of lines per transduction:
              <rule expression>
              <tree before transduction>
              <tree after transduction>
              <blank line>
             
             apply-rule does not include the rule expression in the trace 
	     output, since the rule is determined at function call time.


apply-rules additionally supports the keyword argument :rule-order, with values
chosen among:

:slow-forward   - apply each rule until that rule no longer
                  applies, possibly repeating the entire sequence
:earliest-first - always apply the first rule in the list that 
                  is applicable, repeat until no rules are applicable
                  the rule list may be processed multiple times
:fast-forward   - apply each rule at most once, in order, repeating
                  the list until convergence


The exported symbols are:
 match-expr, 
 apply-rule,
 apply-rules, and 
 store-pred. 

You can use these without being in the TTT package in your repl by prefixing the functions with ttt:. 
For example,  (ttt:match-expr '(a (! pattern tree)) '(a tree)). 
	   
To define a predicate which is a named TTT pattern, do: 
(mk-pred-ttt pred-name patt-expr)
<See pred-defs.lisp for examples>

To define a predicate name the predicate with ending 
symbol "?" and define it to accept a single tree 
expression as input.
Example: 
(defun binary? (tree)
  (if (atom tree)  ;; a leaf is a binary tree
      t
      (and (= (length tree) 2) ;; two children
	   (binary? (nth 0 tree))
	   (binary? (nth 1 tree)))))
(match-expr 'binary? '(X Y))
 => T
(match-expr 'binary? '(X Y Z))
 => nil
(match-expr '(H binary? K) (H (X (Y Z)) K))
 => T

If you define a predicate outside the TTT package, you must explictly 
call ttt:store-pred afterward in order for TTT to be aware of it. 


Ruleset syntax and example (for applying sets of rules):
(defparameter *rulset-name*
  (list
   rule-1
   rule-2
   ..
   rule-n
   ))

(defparameter *example-ruleset*
  '(
    (/ 
     (_* (NP _+ (PP _+1)) _*1)
     (_* (NP _+) (PP _+1) _*1))
    (/ 
     (_* (PP _+ (PP _+1)) _*1)
     (_* (PP _+) (PP _+1) _*1))
    (/ 
     (_* (VP _+ (PP _+1)) _*1)
     (_* (VP _+) (PP _+1) _*1))
    (/ 
     (_* (ADJP _+ (PP _+1)) _*1)
     (_* (ADJP _+) (PP _+1) _*1))
    (/ 
     (_* (WHNP _+ (PP _+1)) _*1)
     (_* (WHNP _+) (PP _+1) _*1))
    (/ 
     (_* ((! ~ NP PP VP ADJP WHNP) _+ (PP _+1)) _*1)
     (_* (! _+) (PP _+1) _*1))
    ))
```

## Tests
Run tests with the `ttt/tests` package.
```
* (ql:quickload :ttt/tests)
* (in-package :ttt/tests)
* (run)
```
