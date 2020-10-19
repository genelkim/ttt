(ql:quickload :ttt/tests)
(in-package :ttt/tests)
(run)
; Run again with debug messages on.
(setf ttt::*ttt-debug-level* 3)
(run)
