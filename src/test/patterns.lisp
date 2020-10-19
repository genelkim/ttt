
(in-package :ttt/tests)

(define-test simple-matches
  (:tag :patterns)
  ;; Only keep a few small examples here since this test will also be used for
  ;; checking the debug print functionality.
  (assert-true (ttt:match-expr 'A 'A))
  (assert-false (ttt:match-expr 'A 'B))
  (assert-true (ttt:match-expr '(! A B) 'A))
  (assert-false (ttt:match-expr '(! A B) '(A B))))

