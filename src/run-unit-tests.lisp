;; Runs the unit tests defined through lisp-unit in test/
;(load "load")
(ql:quickload :ttt)
(loop for test-file in (directory "test/*.lisp")
      do (load test-file))
(in-package :ttt)
(lisp-unit:run-tests)