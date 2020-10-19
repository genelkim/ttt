(defpackage #:ttt/tests
  (:use #:cl #:lisp-unit #:ttt)
  (:shadow :run-tests)
  (:export #:run))

(in-package :ttt/tests)

;; These functions are borrowed from the cl-util library. This is defined here
;; since cl-util currently depends on TTT so we can't import these functions.
;; TODO: Move TTT-dependant code to this repo and change dependency order.
;; Macro for pre- and post- interning symbols.
;; bgnval: initial symbol or s-expr of symbols
;; midval: name of variable storing bgnvar after it is interned into the ulf-lib package
;; callpkg: optional argument for the package that the output should be interned to
;;          if nil, it defaults to the value *ulf-lib-caller-pkg*
;; body: body of the code (using midval)
;; outval: immediate output of the body
(defmacro inout-intern ((bgnval midval inpkg &key (callpkg nil)) &body body)
  `(let* ((,midval (intern-symbols-recursive ,bgnval ,inpkg))
          (outval (multiple-value-list (progn ,@body))))
     (values-list
       (cond
         (,callpkg (intern-symbols-recursive outval ,callpkg))
         (*intern-caller-pkg* (intern-symbols-recursive outval *intern-caller-pkg*))
         (t outval)))))
;; Same as inout-intern macro but only performs the pre- interning portion.
;; Interns the incoming symbols and stores it in midval before evaluating
;; the body.
(defmacro in-intern ((bgnval midval inpkg) &body body)
  `(let* ((,midval (intern-symbols-recursive ,bgnval ,inpkg)))
     ,@body))

(defun run (&key tests tags
                 ;; lisp-unit verbosity parameters.
                 (print-failures t)
                 (print-errors t)
                 (print-summary t)
                 (summarize-results t))
  "Run all TTT tests.
  
  Optional arguments:
    tests:  list of test names, defaults to running all tests
    tags:   list of test tags

  `tests` and `tags` should not both be set. The `tags` argument will be
  ignored if that is the case.
  "

  (format t "Original TTT tests...~%")
  (clear-tests)
  (load-tests-all)
  (compile-tests)
  (run-tests)

  (format t "New unit tests...")
  (setf ttt::*subst-new-next-int* 0)
  (let ((*print-failures* print-failures)
        (*print-errors* print-errors)
        (*print-summary* print-summary)
        (*summarize-results* summarize-results))
    ;; Run tests.
    (cond
      ;; Specified tests.
      (tests
        (when tags
          (warn (concatenate 'string
                             "Both the :tags and :tests keyword "
                             "arguments are given for ttt/tests:run. "
                             "Ignoring the :tags argument...")))
        (in-intern (tests pkgtests :ttt/tests)
          (lisp-unit:run-tests pkgtests :ttt/tests)))
      ;; Specified tags.
      (tags (run-tags tags :ttt/tests))
      ;; Default, all tests.
      (t (lisp-unit:run-tests :all :ttt/tests)))))

