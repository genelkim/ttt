
(in-package :ttt/tests)

(defun pred? (x) (member x '(one.n person.n)))
(defun lex-tense? (x) (member x '(pres past)))
(defun tense-or-aspect? (x) (member x '(pres past perf)))

(define-test tiny-rules
  (:tag :transductions)
  ;; Tiny rule application tests for testing debug functionality.
  (assert-equal 'c (ttt:apply-rule '(/ a c) 'a))
  (assert-equal 'c (ttt:apply-rules '((/ a c)) 'a)))

(define-test apply-rule
  (:tag :transductions :apply-rule)

  (assert-equal 'c
    (ttt:apply-rule '(/ (_!1 ((+ (tense-or-aspect? a) b) _!2)) _!2)
                    '(b ((pres a) c))))

  (assert-equal 'c
    (ttt:apply-rule '(/ (_!1 ((! (tense-or-aspect? a) b) _!2)) _!2)
                    '(b ((pres a) c))))

  (assert-equal 'c
    (ttt:apply-rule '(/ (_!1 ((? (tense-or-aspect? a) a) _!2)) _!2)
                    '(b ((pres a) c))))
  (assert-equal 'c
    (ttt:apply-rule '(/ (_!1 ((* (tense-or-aspect? a) b) _!2)) _!2)
                    '(b ((pres a) c))))

  (assert-equal '((some.d person.n) ((past be.v) (= (a.d one.n))))
                (ttt:apply-rule '(/ (((!1 some.d a.d an.d no.d) (!2 pred?))
                                     (((!3 lex-tense?) be.v)
                                      (= ((!4 some.d a.d an.d) (!5 pred?)))))
                                    ((!1 !5) ((!3 be.v) (= (!4 !2)))))
                                '((some.d one.n) ((past be.v) (= (a.d person.n))))))
  )


(define-test earliest-first
  (:tag :transductions :apply-rules)
  (let ((rules '((/ A B)
                 (/ A D)
                 (/ B C))))
    (assert-equal '(C C C)
                  (ttt:apply-rules rules '(A A A)
                                   :max-n 100 :rule-order :earliest-first))))

(define-test fast-forward
  (:tag :transductions :apply-rules)
  (let ((rules '((/ A B)
                 (/ A D)
                 (/ B C))))
    (assert-equal '(C D C)
                  (ttt:apply-rules rules '(A A A)
                                   :max-n 100 :rule-order :fast-forward))))

