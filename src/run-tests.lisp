(ql:quickload :ttt/tests)
(in-package :ttt/tests)
(run)
; Run a few selected tests with debug functionality.
; Delete built patterns since the pattern building includes some debug statements.
(setf ttt::*ttt-debug-level* 3)
(setf ttt::*built-patterns* (make-hash-table :test #'equal))
(lisp-unit::run-tests '(simple-matches tiny-rules))
