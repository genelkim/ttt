(ql:quickload :ttt)
(ql:quickload :uiop)
(in-package :ttt)
(load-tests-all)
(format t "Original TTT tests...~%") 
(compile-tests)
(run-tests)
(format t "New unit tests...~%")
(loop for test-file in (directory "test/*.lisp")
      do (load test-file))
(lisp-unit:run-tests)'
