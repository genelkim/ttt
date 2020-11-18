(in-package :ttt)

(defun hide-ttt-ops (expr &optional (all-pkg nil))
;; AUTHOR: Len Schubert 
;; Generalized by Gene Kim
;~~~~~~~~~~~~~~~~~~~~~~~~
; Wrap [..] around symbols like /, !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in wff-patterns involving *, **, @, or ~), but can't
; because of their special meanings in TTT. We're assuming that
; the exprs we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
; The optional all-pkg argument allows full control of the package
; in which the symbols are interned. Otherwise, the function defaults
; to the home package of each symbol. Due to symbol inheritence, the
; default functionality may not always work.
;
  (let (str chars pkg)
       (declare (type list chars))
       (cond ((symbolp expr)
              (setf pkg (if all-pkg all-pkg (symbol-package expr)))
              (setq str (string expr))
              (setq chars (coerce str 'list))
              (cond ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~ #\/))
                     (intern (concatenate 'string "[" str "]") pkg))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]") pkg))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]") pkg))
                    (t expr)))
             ((atom expr) expr)
             (t (cons (hide-ttt-ops (car expr) all-pkg)
                      (hide-ttt-ops (cdr expr) all-pkg))))
 )); end of hide-ttt-ops


(defun unhide-ttt-ops (expr &optional (all-pkg nil))
;; AUTHOR: Len Schubert 
;; Generalized by Gene Kim
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the square brackets that have been added around ttt symbols
; in expr by 'hide-ttt-ops':
;
 (let (str chars pkg)
      (declare (type list chars))
      (cond ((symbolp expr)
             (setf pkg (if all-pkg all-pkg (symbol-package expr)))
             (setq str (string expr))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) expr)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) expr)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~ #\/))
                             (intern str pkg))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str pkg))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str pkg))
                            (t expr)))))
            ((atom expr) expr)
            (t (cons (unhide-ttt-ops (car expr) all-pkg)
                     (unhide-ttt-ops (cdr expr) all-pkg))))
 )); end of unhide-ttt-ops


(defun ttt-all-rule-results (rule tree)
;`````````````````````````````````````
; TTT operator ^* doesn't always work, so this is intended for finding
; alls subtrees of 'tree' to which 'rule' applies (yielding something different
; from 'tree' itself and from nil), and returning the results
;
; e.g. (ttt-all-rule-results '(/ (A _!) (B _!)) '(wow a (b) (a (ki (a ma)))))
;      -> ((b (ki (a ma))) (b ma))
;
; These inferences return exactly the consequent (without substituting for the
; antecedent in the original formula).
  (let (result recurd)
    (setq result (ttt:apply-rule rule tree :shallow t :max-n 1))
    (cond
      ((atom tree) (if (and result (not (equal result tree)))
                     (list result)
                     nil))
      (t
        (setq recurd
              (concatenate 'list
                           (ttt-all-rule-results rule (car tree))
                           (ttt-all-rule-results rule (cdr tree))))
        (remove-duplicates
          (if (and result (not (equal result tree)))
            (cons result recurd)
            recurd)
          :test #'equal))))) ; end of ttt-all-rule-results


(defun ttt-apply-rule-possibilities (rule tree &key (max-per-tree 1) (min-per-tree 0))
;`````````````````````````````````````````
; Generates all possible results of applying ttt rule `rule` to `tree`.
; `max-per-tree` indicates the maximum number of times `rule` could be applied
; for each returned tree. This overcomes the limitations of ttt:apply-rule which
; has a fixed order of applying the rules and we can only limit the number of them
; that are applied. This allows the rule to only be applied in deeper contexts.
;
; Returns two values,
;  1. a list of results of applying the rule
;  2. a corresponding list of application counts
;
; e.g.
; (ttt-apply-rule-possibilities '(/ A B) '(A A A))
;  -> '((A A A) (B A A) (A B A) (A A B)) '(0 1 1 1)
; (ttt-apply-rule-possibilities '(/ A B) '(A A A) :min-per-tree 1)
;  -> '((B A A) (A B A) (A A B)) '(1 1 1)
; (ttt-apply-rule-possibilities '(/ A B) '(A A A) :max-per-tree 3 :min-per-tree 3)
;  -> '((B B B)) '(3)
;
; These inferences are in-place, that is the result of the inference is
; substituted back into the original formula for the antecedent expression.
  (declare (type fixnum min-per-tree max-per-tree))
  (labels
    (;; Smaller recursive call used by 'helper'
     ;; Recurses into tr, car and cdr, with 'helper' function and combines them
     ;; while keeping track of count. Assumes that 'tr' is a tree.
     (carcdr-rec (tr apply-count)
       (declare (type fixnum apply-count))
       (let ((lrec (helper (car tr) apply-count))
             (rrec (helper (cdr tr) apply-count)))
         ; For every combination of lrec and rrec, combine if their combined
         ; count doesn't exceed max-per-tree.
         (apply #'append
                (loop for (ltr lcount)
                      of-type (tree-expr fixnum)
                      in lrec collect
                      (loop for (rtr rcount)
                            of-type (tree-expr fixnum)
                            in rrec
                            when (<= (the fixnum (+ lcount rcount))
                                     max-per-tree)
                            collect (list (cons ltr rtr)
                                          (the fixnum (+ lcount rcount))))))))

     ;; Helper function doing the heavy lifting.
     ;; Returns the possibilities of applying the rule to a tr, with counts of
     ;; how many times it was applied.
     ;;  rule: '(/ A B)
     ;;  max-per-tree: 2
     ;;  tr: '(A A)
     ;;  apply-count: 0
     ;;  -> '(((A A) 0) ((B A) 1) ((A B) 1) ((B B) 2))
     (helper (tr apply-count)
       (declare (type tree-expr tr)
                (type fixnum apply-count))
       (cond
         ;; Base case, just return the tree in a list.
         ((or (>= apply-count max-per-tree) (null tr))
          (list (list tr apply-count)))
         ;; Base case 2, not a list. Try to apply rule. Return results.
         ((not (listp tr))
          (let ((result (ttt:apply-rule rule tr :shallow t :max-n 1)))
            (declare (type tree-expr result))
            (append (list (list tr apply-count))
                    (if (and (not (equal tr result)) (not (null result)))
                      (list (list result (1+ apply-count)))))))
         ;; Return a non-modified tr, tr with rule applied to it shallowly (if different from tr),
         ;; and the version where we recurse into both tr and the modified one.
         (t
           (let* ((result (ttt:apply-rule rule tr :shallow t :max-n 1))
                  cur-rec applied-n-count applied-rec)
             (declare (type tree-expr result))
             ;; Add result to return value and recurse into result.
             (when (and (not (equal tr result)) (not (null result)))
               (setf applied-n-count (list (list result (1+ apply-count))))
               (setf applied-rec
                     (if (< (1+ apply-count) max-per-tree)
                       (carcdr-rec result (1+ apply-count)))))

             ;; Add current count, and recurse.
             (setf cur-rec (carcdr-rec tr apply-count))
             (append cur-rec applied-n-count applied-rec)))))
     ) ; end of labels defs.
    ;; Main body.
    ;; Call helper function and filter results.  max-per-tree is handled in the
    ;; recursive function so we don't compute more than necessary.
    (let ((helper-res
            (remove-if #'(lambda (x)
                           (< (the fixnum (second x)) min-per-tree))
                       (helper tree 0))))
      (values (mapcar #'first helper-res)
              (mapcar #'second helper-res)))))


;; copy of lispify-parser-output.lisp -- just to avoid having to load
;; I did not write these two functions
(defun lispify-parser-output (char-string)
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; convert a sentence parse, as a string, to a LISP S-expression
;
  (read-from-string (preslash-unsafe-chars char-string)) )

(defun preslash-unsafe-chars (char-string)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Prefix "\" to unsafe characters # ` ' : ; , . \ | in 'aString'.
;
  (let ((chars (coerce char-string 'list)) result)
       (dolist (ch chars)
           (cond ((alphanumericp ch) (push ch result))
                 ((member ch '(#\( #\) #\")) (push ch result)); unbalanced "
                 ((member ch
                   '(#\# #\` #\' #\: #\; #\, #\. #\\ #\|) )
                  (push #\\ result) (push ch result) )
                 (T (push ch result)) ))
        (coerce (reverse result) 'string)
 )); end of preslash-unsafe-chars

