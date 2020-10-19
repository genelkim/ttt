
(in-package :ttt/tests)

(define-test vertical
  (:tag :operator :vertical)
  ;; Example from paper.
  ;; (ˆ@ (+ (@ *)) X) -- matches any tree with leftmost leaf X
  (let ((pattern '(^@ (+ (@ _*)) X)))
    (assert-true (ttt:match-expr pattern '(X)))
    (assert-true (ttt:match-expr pattern '(X Y Z)))
    (assert-true (ttt:match-expr pattern '((X Y) Z)))
    (assert-true (ttt:match-expr pattern '(((((((((X) Y))))))) Z)))
    
    (assert-false (ttt:match-expr pattern '(Y)))
    (assert-false (ttt:match-expr pattern '(A (Y X))))
    (assert-false (ttt:match-expr pattern '((Y X) Z)))
    (assert-false (ttt:match-expr pattern '(A (B (C (D ((Y X))) Z))))))

  (let ((pattern '(^@ _! (C _*) E)))
    (assert-true (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C D E))))
    
    (assert-false (ttt:match-expr pattern '(A B (C D) F)))
    (assert-false (ttt:match-expr pattern '(A B (D E) F)))
    (assert-false (ttt:match-expr pattern '(C D E)))))
