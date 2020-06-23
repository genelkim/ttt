(defpackage ttt
  (:documentation "the TTT (template to template transduction) package!")
  (:use :cl :lisp-unit)
  (:shadow :run-tests)
  (:export :match-expr
           :apply-rules
           :apply-rule
           :store-pred))

