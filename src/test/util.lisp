
(in-package :ttt/tests)

(define-test hide-ttt-ops
  (:tag :util)

  (assert-equal '((he.pro (past go.v)) [!])
                (ttt:hide-ttt-ops '((he.pro (past go.v)) !)))
  (assert-equal '((he.pro (past go.v)) ttt::[!])
                (ttt:hide-ttt-ops '((he.pro (past go.v)) !) :ttt))
  (assert-equal '(i.pro ((pres like.v) (np+preds (the.d (plur symbol.n))
                                                 (= (set-of [<>] [{}])))))
                (ttt:hide-ttt-ops
                  '(i.pro ((pres like.v) (np+preds (the.d (plur symbol.n))
                                                   (= (set-of <> {}))))))))

(define-test unhide-ttt-ops
  (:tag :util)
  (assert-equal '((he.pro (past go.v)) !)
                (ttt:unhide-ttt-ops '((he.pro (past go.v)) [!])))
  (assert-equal '((he.pro (past go.v)) ttt::!)
                (ttt:unhide-ttt-ops '((he.pro (past go.v)) [!]) :ttt))
  (assert-equal '(i.pro ((pres like.v) (np+preds (the.d (plur symbol.n))
                                                 (= (set-of <> {})))))
                (ttt:unhide-ttt-ops
                  '(i.pro ((pres like.v) (np+preds (the.d (plur symbol.n))
                                                   (= (set-of [<>] [{}]))))))))

(define-test ttt-all-rule-results
  (:tag :util)

  (assert-equal '((b (ki (a ma))) (b ma))
                (ttt:ttt-all-rule-results
                  '(/ (A _!) (B _!))
                  '(wow a (b) (a (ki (a ma)))))))

(define-test ttt-apply-rule-possibilities
  (:tag :util)
  ;; TODO: change these tests to be order-agnostic. Just pair the counts the
  ;; results and then check membership of each and length of total number of
  ;; results.
  (multiple-value-bind (results counts)
                       (ttt:ttt-apply-rule-possibilities '(/ A B)
                                                         '(A A A))
    (assert-equal '((A A A) (A A B) (A B A) (B A A))
                  results)
    (assert-equal '(0 1 1 1)
                  counts))
  (multiple-value-bind (results counts)
                       (ttt:ttt-apply-rule-possibilities '(/ A B)
                                                         '(A A A)
                                                         :min-per-tree 1)
    (assert-equal '((A A B) (A B A) (B A A))
                  results)
    (assert-equal '(1 1 1)
                  counts))
  (multiple-value-bind (results counts)
                       (ttt:ttt-apply-rule-possibilities '(/ A B)
                                                         '(A A A)
                                                         :min-per-tree 3
                                                         :max-per-tree 3)
    (assert-equal '((B B B)) results)
    (assert-equal '(3) counts)))


(define-test lispify-parser-output
  (:tag :util)
  (assert-equal '(A B (|.| |.|))
                (ttt::lispify-parser-output "(A B (. .))")))

(define-test preslash-unsafe-chars
  (:tag :util)
  (assert-equal "don\\'t\\; stop \\: use this fn \\#\\'equal and this command \\`(run\\|test)\\`\\."
                (ttt::preslash-unsafe-chars "don't; stop : use this fn #'equal and this command `(run|test)`.")))

