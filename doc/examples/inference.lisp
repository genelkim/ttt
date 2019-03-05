;; Inference example with TTT



;(setq *rule*  '(/ ((most _!.x (_!.x (!.p pred?))  (_!.x (!.q pred?))) and (most _!.x (_!.x !.q) (_!.x (!.r pred?)))) (many _!.x (_!.x !.p) (_!.x !.r))))

;(defun pred? (f) (match '(! dog.n furry.a cute.a) f))   ;predicates require arugments to be lists
;(add-op 'pred? #'pred? :predicate t)


;(setq *kb* '((most x (x dog.n) (x furry.a)) and (most x (x furry.a) (x cute.a))))


;; Snagged from email from Jonathan
(defun pred? (f)
  (match '(! dog.n furry.a cute.a small.a) f))
(add-op 'pred? #'pred? :predicate t)

(defun formula? (f)
  (listp f))
(add-op 'formula? #'formula? :predicate t)

(setf *kb* '((most x (x dog.n) (x furry.a))
             (most x (x cat.n) (x furry.a))
             (most x (x furry.a) (x cute.a))
             (most x (x cute.a) (x small.a))))

(setf *rules* '((/ (^1 _!.x _* _!.y _*) (_!.x and !.y))
                (/ ((most _!.x (_!.x (!.p pred?))
                          (_!.x (!.q pred?)))
                    and
                    (most _!.x (_!.x !.q)
                          (_!.x (!.r pred?))))
                 (many _!.x (_!.x !.p) (_!.x !.r)))
                ))


(setf *folded-rule*
      '(/ ( _* (most _!.x (_!.x (!.p pred?)) (_!.x (!.q pred?)))
           _* (most _!.x (_!.x !.q) (_!.x (!.r pred?))) _*)
        (many _!.x (_!.x !.p) (_!.x !.r)))
(setf *folded-rule* 
      '(/ ( _* (most _!.x (_!.x (!.p pred?)) (_!.x (!.q pred?)))
	   _* (most _!.x (_!.x !.q) (_!.x (!.r pred?))) _*)
	(many _!.x (_!.x !.p) (_!.x !.r))))

      