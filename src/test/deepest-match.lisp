;;; Gene Louis Kim, 4-6-2020
;;; Unit tests for verifying more exhaustive fix for apply.

(in-package :ttt/tests)

(define-test basic-deepest-match
  "Basic tests for deepest match."
  (:tag :deepest-match)
  (let* ((sentence
           '((SUB (AT.P (WHAT.D PLACE.N)) 
                 ((THE.D (|Target| BLOCK.N)) 
                  ((PAST BE.V) [*H] 
                   (ADV-E (BEFORE.P (KE (IT.PRO ((PAST BE.V)
                                                 (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) [?]))
         (term-pattern '(!1 (what.d place.n)
                           |Target|
                           (the.d (|Target| block.n))
                           it.pro
                           |Starbucks|
                           (the.d (|Starbucks| block.n))
                           (ke (it.pro ((past be.v) (on.p (the.d (|Starbucks| block.n))))))))
         (pred-pattern '(!2 (at.p (what.d place.n))
                           place.n
                           block.n
                           (|Target| block.n)
                           be.v
                           (past be.v)
                           (|Starbucks| block.n)
                           (on.p (the.d (|Starbucks| block.n)))
                           ((past be.v) (on.p (the.d (|Starbucks| block.n))))
                           (before.p (ke (it.pro ((past be.v) (on.p (the.d (|Starbucks| block.n)))))))
                           ((past be.v) [*h]
                                        (adv-e (before.p (ke (it.pro ((past be.v) (on.p (the.d (|Starbucks| block.n)))))))))))
         (bad-inf
           '((SUB (AT.P (WHAT.D PLACE.N)) 
                 ((THE.D (|Target| BLOCK.N)) 
                  (WAS.V [*H] 
                   (ADV-E (BEFORE.P (KE (IT.PRO ((PAST BE.V)
                                                 (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) [?]))
         (good-inf
           '((SUB (AT.P (WHAT.D PLACE.N)) 
                 ((THE.D (|Target| BLOCK.N)) 
                  (WAS.V [*H] 
                   (ADV-E (BEFORE.P (KE (IT.PRO (WAS.V
                                                 (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) [?]))

         inf-rule)
    (declare (ignore bad-inf))
    (defun local-conj! (pat)
      (cond
        ((atom pat) pat)
        ((equal pat '(past be.v)) 'was.v)
        (t (mapcar #'local-conj! pat))))
    (defun action-verb? (x)
      (member x '(move.v)))

    (setf inf-rule
          (list '/
                (list term-pattern pred-pattern)
                '(!1 (local-conj! !2))))

    ; GK: Apparently fixing the key extraction algorithm made this inference work...
    ; So now, we don't have a deepest match test. See that the test below works now.
    ;(assert-false (equal good-inf (apply-rules (list inf-rule) sentence :max-n 100
    ;                                       :rule-order :slow-forward)))
    (assert-equal good-inf (apply-rules (list inf-rule) sentence :max-n 100
                                        :rule-order :slow-forward))

    (assert-equal good-inf (apply-rules (list inf-rule) sentence :max-n 100
                                           :rule-order :slow-forward :rule-depth :deepest))
    (assert-equal 'move.v
                  (apply-rules '((/ (^* action-verb?) action-verb?))
                               '((SUB ((NQUAN (HOW.MOD-A MANY.A)) (PLUR BLOCK.N))
                                      ((PAST DO.AUX-S) I.PRO (MOVE.V *H))) ?)
                               :shallow t))))

