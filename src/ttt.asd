(asdf:defsystem :ttt
    :name "Tree-to-Tree Transduction Language"
    :serial t
    :version "2.0.0"
    :description "A language for transparent modifications of s-expression based trees."
    :author "Adam Purtee and Gene Louis Kim <gkim21@cs.rochester.edu>"
    :license "GPLv3"
    :depends-on ()
    :components ((:file "package")
                 (:file "types")
                 (:file "operators")
                 (:file "expressions")
                 (:file "keys")
                 (:file "bindings")
                 (:file "trees")
                 (:file "patterns")
                 (:file "operators/unrestricted-seq")
                 (:file "operators/restricted-seq")
                 (:file "operators/fpseq-gen")
                 (:file "operators/descendant")
                 (:file "operators/vertical")
                 (:file "operators/literal")
                 (:file "sticky")
                 (:file "predicates")
                 (:file "template-construction")
                 (:file "transductions")
                 (:file "util")
                 (:file "process-tb")
                 (:file "search-tb"))
    :around-compile (lambda (next)
                      ; For development use (debug 3) (safety 3) (space 1) (speed 1)
                      (proclaim '(optimize (debug 0) (safety 2) (space 1) (speed 3)))
                      (funcall next))
    :in-order-to ((test-op (test-op :ttt/tests))))

(asdf:defsystem :ttt/tests
  :serial t
  :description "Tests for the TTT library"
  :author "Adam Purtee and Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "GPLv3"
  :depends-on (:ttt :lisp-unit)
  :components ((:file "test/package")
               (:file "tests")
               (:file "test/deepest-match")
               (:file "test/transductions")
               (:file "test/operators")
               (:file "test/templates")
               (:file "test/predicates")
               (:file "test/patterns")
               (:file "test/util"))
  :perform (test-op (o c) (symbol-call :ttt/tests :run)))

