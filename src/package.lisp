(defpackage :ttt
  (:documentation "the TTT (template to template transduction) package!")
  (:use :cl)
  (:shadow :run-tests)
  (:export :match-expr
           :apply-rules
           :apply-rule
           :store-pred
           ;; Utilities
           :hide-ttt-ops
           :unhide-ttt-ops
           :ttt-all-rule-results
           :ttt-apply-rule-possibilities))

