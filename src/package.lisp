(defpackage ttt
  (:documentation "the TTT (template to template transduction) package!")
  (:use :common-lisp)
  (:export :match-expr
	   :apply-rules
	   :apply-rule
	   :store-pred))
;;(in-package ttt)
;;(defconstant +load-path+ (system-relative-pathname 'epilog ""))
