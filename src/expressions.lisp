(in-package :ttt)
;; These functions are used only by build-pattern and build-tree
;; to construct the respective objects from expressions.

(defun patt-expr-neg-args (expression)
  "Given patt = '(op X1 X1 ... Xm ~ Y1 Y2 ... Yn) return '(Y1 Y2 ... Yn)"
  (declare (type list expression))
  ; intern any atomic symbols so it matches with '~ declared here.
  (let* ((symbol-interned
           (mapcar #'(lambda (x) (if (symbolp x) (intern (symbol-name x) :ttt) x))
                   expression))
         (pos (position '~ symbol-interned)))
    ; Return original package symbols.
    (if pos (subseq expression (1+ pos)))))

(defun patt-expr-pos-args (expression)
  "Given patt = '(op X1 X2 ... Xm ~ Y1 Y2 ... Yn) return '(X1 X2 ... Xm)"
  (declare (type list expression))
  (if (and (get-op (car expression))
           (op-requires-args? (get-op (car expression))))
      (let ((symbol-interned
               (mapcar #'(lambda (x) (if (symbolp x) (intern (symbol-name x) :ttt) x))
                       expression)))
        (subseq expression 1 (position '~ symbol-interned)))
      expression))

(declaim (ftype (function (tree-expr) fixnum) expr-height))
(defun expr-height (expression)
  (if (atom expression)
      0
      (1+ (the fixnum (apply #'max
                             (mapcar #'expr-height expression))))))

(declaim (ftype (function (tree-expr) fixnum) patt-expr-min-width patt-expr-max-width))
(defun patt-expr-min-width (expression)
  ; Assume my fixnum declarations are correct in this function. I.e. there won't be overflow.
  (declare (optimize (safety 0)))
  (let ((expr-op (patt-expr-get-op expression)))
    (cond
      ;; List expression with an operator.
      ((and expr-op (listp expression))
       (let ((n (get-/n (car expression)))
             (m (get-/m (car expression))))
         (case expr-op
           ;; First all the operators that requires some computation.
           ((! + *)
            (let* ((pos-args (patt-expr-pos-args expression))
                   (recmin (if pos-args
                             (apply #'min
                                    (mapcar #'patt-expr-min-width pos-args)))))
              (declare (type fixnum recmin))
              (case expr-op
                ((! +) (cond
                         ((and pos-args n) (the fixnum (* n recmin)))
                         (pos-args recmin)
                         (t 1)))
                (* (if (and n m)
                     (the fixnum (* n recmin))
                     0)))))
           ((<> {})
            (reduce #'+ (mapcar #'patt-expr-min-width
                                (patt-expr-pos-args expression))))
           (? 0)
           ((^ ^^ ^* ^+ ^n ^@ / literal general) 1)
           (otherwise
             (when (not (op-is-pred? expression))
               (print 'op-error1)
               (print (patt-expr-get-op expression))
               0)))))
      ;; Symbol expression with operator.
      (expr-op
          (let ((n (get-/n expression)))
            (case (patt-expr-get-op expression)
              ((_! _+) (if n n 1))
              (_? 0)
              (_* (if n n 0))
              ((@ literal) 1)
              (otherwise
               (when (not (op-is-pred? expression))
                 (print 'op-error2)
                 (print (patt-expr-get-op expression)))
               1))))
      ;; Sticky expression.
      ((sticky? expression) 0)
      ;; No operators and not sticky.
      (t 1))))


(defun patt-expr-max-width (expression)
  ; Assume my fixnum declarations are correct in this function. I.e. there won't be overflow.
  (declare (optimize (safety 0)))
  (let ((expr-op (patt-expr-get-op expression)))
    (cond
      ;; List expression with operator.
      ((and expr-op (listp expression))
       (let ((n (get-/n (car expression)))
             (m (get-/m (car expression))))
         (case expr-op
           ;; First take care of operators that require similar computation.
           ((! ? *)
            (let* ((pos-args (patt-expr-pos-args expression))
                   (recmax (if pos-args
                             (apply #'max
                                    (mapcar #'patt-expr-max-width pos-args)))))
              (declare (type fixnum recmax))
              (case expr-op
                (! (cond
                     ((and pos-args n)
                      (the fixnum
                           ;; Make sure we don't overflow
                           (if (> recmax (floor most-positive-fixnum n))
                             most-positive-fixnum
                             (* n recmax))))
                     (pos-args recmax)
                     (t 1)))
                (? (let ((locmax (max 1 recmax)))
                     (the fixnum
                          (cond
                            ;; Avoid overflow.
                            ((and n (> locmax (floor most-positive-fixnum n)))
                             most-positive-fixnum)
                            ;; Otherwise compute.
                            (n (* n locmax))
                            ;; No n.
                            (t locmax)))))
                (* (the fixnum
                        (if (and m (<= recmax (floor most-positive-fixnum m)))
                          (* m recmax)
                          ;; Avoid overflow.
                          most-positive-fixnum))))))
           ((<> {})
            (reduce #'+
                    (mapcar #'patt-expr-max-width (patt-expr-pos-args expression))))
           (+ most-positive-fixnum)
           ((^ ^^ ^* ^+ ^n / ^@ literal general) 1)
           (otherwise
             (when (not (op-is-pred? expression))
               (print 'op-error3)
               (print (patt-expr-get-op expression))
               most-positive-fixnum)))))

       ;; Symbol expression with operator.
       (expr-op
         (let ((n (get-/n expression))
               (m (get-/m expression)))
           (case (patt-expr-get-op expression)
             ((_! _?) (if n n 1))
             (_+ most-positive-fixnum)
             (_* (if (and n m) m most-positive-fixnum))
             ((@ literal) 1)
             (otherwise
               (when (not (op-is-pred? expression))
                 (print 'op-error4)
                 (print (patt-expr-get-op expression)))
               most-positive-fixnum))))
       ;; Sticky expression.
       ((sticky? expression) most-positive-fixnum)
       ;; No operators and not sticky.
       (t 1))))


(defun patt-expr-get-op (expression)
    "If an operator exists which handles the particular pattern,
     then return the proper operator,
     otherwise return literal for tree literals or general for nonliterals."
    (if (consp expression)
        (cond ((and (get-op (car expression))
                    (op-requires-args? (get-op (car expression))))
               (get-op (car expression)))
              ((equal (filter-ops expression) (list expression)) 'literal)
              (t 'general))
        (if (op-requires-args? (get-op expression))
            (if (sticky? (get-var expression)) 'sticky-check)
            (get-op expression))))


(defun patt-expr-get-var (expression)
  "If the top-level pattern should be bound, return the variable."
  (when (listp expression)
    (return-from patt-expr-get-var
      (let ((op (get-op (car expression))))
        (if (and op (op-requires-args? op))
            (get-var (car expression))))))
  (let ((op (get-op expression)))
    (unless (op-requires-args? op)
      (get-var expression))))

(declaim (ftype (function (tree-expr &optional fixnum) (values list &optional)) filter-ops))
(defun filter-ops (expression &optional (maxdepth most-positive-fixnum))
  "return filter-ops + finite-required-sets-of-ors for the same depth

   => (filter-ops   '((! PP-IN PP-WITH) _*))
   (PP-IN PP-WITH)

   ((! S SBAR) _+)   - fire only when S or SBAR encountered
   ((! S _+)   _+)   - fire on every tree

   for patterns with choices (! ... +)

   (! if this can only match symbols from a finite set, return that set
         otherwise, return the empty set)

   (+ if there exists a finite subset of symbols such that this rule
         must match a symbol from within that set then return that set


   what this buys us:
   if (null (intersection (patt-keys expression) (tree-keys tree)))
      then the pattern will fail to match tree

   so that we can map from trees to applicable patterns:

   (foreach sub-tree in tree do
      (dolist (key (tree-keys sub-tree))
           (dolist (pattern (gethash key *rules-by-key*))
                   (let ((bindings (match pattern sub-tree)))
                        (if bindings (print 'successful-match))))))


   re-ranker parses of Brown have an average of 2.85 keys per sub-tree

   return a list of the freestanding elements
   => (filter-ops '(<> X Y))
   (X Y)

   => (filter-ops '(X Y))
   ((X Y))
   could extend to filter-all - return ALL such sets to any depth

   maxdepth stops this process at a specific depth. If an operator set includes
   subtrees beyond the depth, the whole pattern is omitted.
   "
  (when (< maxdepth 0)
    (return-from filter-ops nil))
  (if (atom expression)
      (if (not (or (eq (get-op expression) 'literal) (numberp expression)))
          (keys-to-filter-ops (get expression 'pred-keys))  ;; for predicates
          (list expression))
      (case (get-op (car expression))
        ((+ !)
         (unless (member nil (mapcar #'(lambda (x) (filter-ops x (1- maxdepth)))
                                     (rest expression)))
           (reduce #'append (mapcar #'(lambda (x) (filter-ops x (1- maxdepth)))
                                    (rest expression)))))
        ((^@ /) (filter-ops (nth 1 expression) (1- maxdepth)))
        ((* ? ^ ^*) nil)
        ((<> {}) (reduce #'append (mapcar #'(lambda (x) (filter-ops x (1- maxdepth)))
                                          expression)))
        (otherwise (let ((recres
                           (reduce #'append (mapcar #'(lambda (x) (filter-ops x (1- maxdepth)))
                                                    expression))))
                     (if recres (list recres) recres))))))



(defun keys-to-filter-ops (keys)
  "for handling predicates.  a crutch.
   switch from ((A.0) (B.0) (C.1) (D.2) (E.2)) to (A B (C) (D E))"
  (mapcar (lambda (key) (listify (cdr key) (car key))) keys))

(declaim (ftype (function (fixnum tree-expr) tree-expr) listify))
(defun listify (n expr)
  (declare (type fixnum n))
  (if (= n 0)
      expr
    (listify (the fixnum (1- n)) (list expr))))

