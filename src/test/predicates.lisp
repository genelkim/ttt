
(in-package :ttt/tests)

(in-package :lisp-unit)
(defun test-fn? (x) (and (listp x) (= (length x) 2)))
(in-package :ttt/tests)

(define-test store-pred
  (:tag :predicates)

  ;; Define a function in another package, check that it doesn't work at first,
  ;; but then with store-pred, it succeeds.  All the functions in the current
  ;; package are automatically recognized.
  (assert-false (ttt:match-expr 'test-fn? '(A B)))
  (assert-true (ttt:match-expr 'lisp-unit::test-fn? '(A B)))
  ;; Store the predicate in this package too.
  (ttt:store-pred 'test-fn? #'lisp-unit::test-fn?)
  (assert-true (ttt:match-expr 'test-fn? '(A B)))
  (assert-false (ttt:match-expr 'test-fn? '(A B C))))

(define-test mk-pred-ttt
  (:tag :predicates)

  ;; Predicate names (not the patterns) are currently always interned into the
  ;; TTT package.
  (assert-false (ttt:match-expr 'test-pred '(A B)))
  (ttt::mk-pred-ttt 'test-pred '(a b))
  (assert-false (ttt:match-expr 'test-pred '(a b)))
  (assert-true (ttt:match-expr 'ttt::test-pred '(a b)))
  (assert-false (ttt:match-expr 'ttt::test-pred '(ttt::a ttt::b))))

