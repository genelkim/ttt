
(in-package :ttt/tests)

(define-test built-in-templates
  (:tag :templates)
  
  (assert-equal '(A (A-B-C))
                (ttt:apply-rule '(/ (A B C) (ttt::join-with-dash! A B C))
                                '(A (A B C))))

  (assert-equal '(A (ABC))
                (ttt:apply-rule '(/ (A B C) (ttt::concatenate-syms! A B C))
                                '(A (A B C))))

  (assert-equal '(B (A X.1 (X.1)))
                (ttt:apply-rule '(/ (A X (X))
                                    (ttt::subst-new! X (A X (X))))
                                '(B (A X (X))))))

