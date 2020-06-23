(in-package :ttt)
(defclass transduction (pattern)
  ((lhs :accessor lhs)
   (rhs :accessor rhs))
  (:documentation
   "Returns special bindings.
    one transduction operator per rule
    no named variables     (no sticky)
    cannot be in iterated context
    must match sequence of length 1
    must replace with sequence of length 1"))
(defstruct t-bind
  parent
  parent-idx
  template-expr)

(defmethod compile-pattern ((patt transduction))
  (unless (initialized? patt)
    (if (not (= (length (to-expr patt)) 3))
        (error "transduction operator requires exactly three arguments ~A~%"
               (to-expr patt)))
    (setf (min-width patt) 1
          (max-width patt) 1
          (var patt) '/
          (lhs patt) (build-pattern (nth 1 (to-expr patt)))
          (rhs patt) (nth 2 (to-expr patt))
          (initialized? patt) t))
  (setf
   (match-fn patt)
   (compile
    nil
    (eval
     `(lambda (tree-seq bindings )
        (if (and (consp tree-seq) (null (cdr tree-seq)))
            (let ((b (match ,(lhs patt) tree-seq bindings )))
              (if b
                  (add-binding
                   (mk-binding
                    '/
                    (make-t-bind
                     :parent (parent (car tree-seq))
                     :parent-idx (parent-idx (car tree-seq))
                     :template-expr ',(rhs patt)))
                   b))))))))
  (setf (compiled? patt) t)
  patt)

(defun get-matches (pattern tree rule-depth)
  "Runes the compiled pattern agains a compiled tree object with the given
  rule-depth option. This is a helper function for apply-rule and apply-rules
  and does not check any of the inputs.

  Returns a list of matched subtrees."
  (let ((raw-match (case rule-depth
                     (:shallow (list (match pattern (list tree) t)))
                     (:deepest (deepest-matches pattern tree))
                     (:default (list (deep-match pattern tree)))
                     (otherwise (error "Unknown rule-depth option: ~s~%" rule-depth)))))
    (if (equal raw-match '(nil)) nil raw-match)))

(defun apply-rules (rules tree-expr &key
                                      (rule-order :slow-forward)
                                      (trace nil)
                                      (shallow nil)
                                      (deepest nil)
                                      (max-n most-positive-fixnum)
                                      (rule-depth :default))
  "Apply each of the rules in list rules to a tree expression
   until each rule is no longer applicable. The rules list is only
   processed once. Returns a new tree expression.

   shallow    limits rule applications to root of tree (alternative to using
              :rule-depth :shallow)
   deepest    allows multiple recursive matches into the tree at each step
              (alternative to using :rule-depth :deepest)
   max-n      limits the maximum number of edits to a tree
   trace      when t, displays debugging info to stdout
              otherwise, when non-nil write debugging info to file
              appending to the file if it already exists

              trace format is one 4-tuple of lines per transduction:
              <rule expression>
              <tree before transduction>
              <tree after transduction>
              <blank line>

   rule-order
   :slow-forward   - apply each rule until that rule no longer
                     applies, possibly repeating the entire sequence
   :earliest-first - always apply the first rule in the list that
                     is applicable, repeat until no rules are applicable
                     the rule list may be processed multiple times
   :fast-forward   - apply each rule at most once, in order, repeating
                     the list until convergence

   rule-depth
   :default   Rule application can occur at any depth, but only one match is
              returned at each step.
   :shallow   Rule application can only occur at the root of the tree.
   :deepest   Rule application can occur at any depth, and all matches are
              returned at each step. Setting a rule-depth value is recommended
              when using this option to enable reasonable runtimes. This rule
              may only be used with rule-order :slow-forward. Behavior is
              undefined for other rule-order options."

  (declare (type list rules))

  (when (not (member rule-depth '(:default :shallow :deepest)))
    (error "Invalid rule-depth option (~s). Must be one of '(:default :shallow :deepest)."
           rule-depth))
  (when (not (member rule-order '(:slow-forward :earliest-first :fast-forward)))
    (error "Invalid rule-order option (~s). Must be one of '(:slow-forward :earliest-first :fast-forward)."
           rule-depth))
  (when (and (eql rule-depth :default) shallow)
    (setf rule-depth :shallow))
  (when (and (eql rule-depth :default) deepest)
    (setf rule-depth :deepest))

  (let ((tr (if (eql rule-depth :shallow)
                (build-tree tree-expr :index-subtrees nil)
                (build-tree tree-expr :index-subtrees t)))
        (trace-file (if (or (eq trace t) (null trace))
                      *standard-output*
                      (open trace
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)))
        (compiled-rules (mapcar #'build-pattern rules))
        (converged nil)
        (prev tree-expr)
        (n 0))
    (declare (type fixnum n max-n)
             (type stream trace-file))

    (when (> *ttt-debug-level* 0)
      (progn
        (format t "===apply-rules===~%")
        (format t "rules:     ~s~%" rules)
        (format t "tree-expr: ~s~%~%" tree-expr)))

    (case rule-order
      (:slow-forward
       (loop while (not converged) do
            (setf converged t)
            ; TODO(gene): run the TTT tests after this change to verify correctness
            ; TODO(gene): probable memoize the deepest match so that when we come across the same thing we don't re-compute
            (dolist (r compiled-rules)
              (let ((bs (get-matches r tr rule-depth))
                    (converged2 nil)
                    b)
                (loop while (and (not converged2) bs (car bs) (< n max-n)) do
                     (setf b (car bs))
                     (setf bs (cdr bs))
                     (setf tr (do-transduction tr (get-binding '/ b) b))
                     (if trace
                         (format trace-file "~a~%~a~%~a~%~%"
                                 (to-expr r)
                                 prev
                                 (to-expr tr)))
                     (incf n)
                     (if (and (equal prev (to-expr tr))
                              (not (eql rule-depth :deepest)))
                         (setf converged2 t)
                         (setf bs
                               (get-matches r tr rule-depth)
                               prev (to-expr tr)
                               converged nil)))
                ))))

      (:earliest-first
       (loop while (not converged) do
            (setf converged t)
            (dolist (r compiled-rules)
              (let* ((bs (get-matches r tr rule-depth))
                     (b (car bs)))
                (when b
                  (setf prev (to-expr tr))
                  (setf tr (do-transduction tr (get-binding '/ b) b))
                  (if trace
                      (format trace-file "~a~%~a~%~a~%~%"
                              (to-expr r)
                              prev
                              (to-expr tr)))
                  (if (not (equal prev (to-expr tr)))
                      (setf converged nil))
                  (return))))))


      (:fast-forward
       (loop while (not converged) do
            (setf converged t)
            (dolist (r compiled-rules)
              (let* ((bs (get-matches r tr rule-depth))
                     (b (car bs)))
                (when b
                  (setf prev (to-expr tr))
                  (setf tr (do-transduction tr (get-binding '/ b) b))
                  (if trace
                      (format trace-file "~a~%~a~%~a~%~%"
                              (to-expr r)
                              prev
                              (to-expr tr)))
                  (if (not (equal prev (to-expr tr)))
                      (setf converged nil)))))))

      (otherwise (error "unrecognized option for rule-order")))

    (if (and trace-file (not (eq trace-file *standard-output*)))
      (close trace-file))
    (to-expr tr)))

(defun apply-rule (rule-expr tree-expr &key
                                         (shallow nil)
                                         (trace nil)
                                         (max-n most-positive-fixnum)
                                         (rule-depth :default))
  "Apply a single rule to a tree expression until converged.
   Returns a new tree expression.

   shallow    limits rule applications to root of tree
   max-n      limits the maximum number of edits to a tree
   trace      when t, displays debugging info to stdout
              otherwise, when non-nil write debugging info to file
              appending to the file if it already exists
              format is (one triple of lines per transduction):
              <tree before transduction>
              <tree after transduction>
              <blank line>

   rule-depth
   :default   Rule application can occur at any depth, but only one match is
              returned at each step.
   :shallow   Rule application can only occur at the root of the tree.
   :deepest   Rule application can occur at any depth, and all matches are
              returned at each step. Setting a rule-depth value is recommended
              when using this option to enable reasonable runtimes."

  (declare (type list rule-expr))
  (when (and (eql rule-depth :default) shallow)
    (setf rule-depth :shallow))
  (let ((tr (build-tree tree-expr :index-subtrees t))
        (compiled-rule (build-pattern rule-expr))
        (trace-file (if (or (eq trace t) (null trace))
                      *standard-output*
                      (open trace
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)))
        (prev tree-expr)
        (converged nil)
        (n 0))
    (declare (type fixnum n max-n)
             (type stream trace-file))
    (let* ((bs (get-matches compiled-rule tr rule-depth))
           (b (car bs)))
      (loop while (and (not converged) bs b (< n max-n)) do
           (setf tr (do-transduction tr (get-binding '/ b) b))
           (if trace (format trace-file "~a~%~a~%~%" prev (to-expr tr)))
           (incf n)
           (if (and (equal prev (to-expr tr))
                    (not (eql rule-depth :deepest)))
               (setf converged t)
               (setf bs
                     (get-matches compiled-rule tr rule-depth)
                     prev (to-expr tr)))))
    (if (and trace-file (not (eq trace-file *standard-output*)))
      (close trace-file))
    (to-expr tr)))


(defun do-transduction (tree t-binding bindings)
  "Destructively modifies tree to implement the results of
   a previously bound transduction operator.
   Returns the modified tree."
   (if (> *ttt-debug-level* 0)
    (progn
      (format t "===do-transduction===~%")
      (format t "tree:      ~s~%" tree)
      (format t "t-binding: ~s~%" t-binding)
      (format t "bindings:  ~s~%~%" bindings)))
  (let ((new-subtree-raw
         (template-to-tree (t-bind-template-expr t-binding) bindings t))
        (par (t-bind-parent t-binding))
        (par-idx (t-bind-parent-idx t-binding))
        (fixnum-par-idx -1)
        new-subtree)
    (declare (type fixnum fixnum-par-idx)
             (type list new-subtree-raw))
    (if (not (= (length new-subtree-raw) 1))
        (error "transduction rhs cannot return more than one tree.")
        (setf new-subtree (build-tree (car new-subtree-raw) :index-subtrees t)))
    (cond
      ((null par) ;; replace root
       (setf (children tree) (children new-subtree)
             (nchildren tree) (nchildren new-subtree)
             (to-expr tree) (to-expr new-subtree)
             (height tree) (height new-subtree)
             (keys tree) (keys new-subtree)
             (parent tree) nil
             (parent-idx tree) nil)
       (dotimes (n (nchildren tree))
         (declare (type fixnum n))
         (setf (parent (nth n (children tree))) tree
               (parent-idx (nth n (children tree))) n)))
      (t
       (setf fixnum-par-idx par-idx)
       (setf (parent new-subtree) par)
       (setf (children par)
             (append
              (subseq (children par) 0 par-idx)
              (cons
               new-subtree
               (subseq (children par) (1+ par-idx)))))
       (dotimes (n (nchildren par))
         (setf (parent-idx (nth n (children par))) n))
       (let ((ancestor par))
         (loop while ancestor do
              (setf (to-expr ancestor)
                    (mapcar

                     #'to-expr
                     (children ancestor)))
              (setf (keys ancestor)  ;; not the most efficient
                    (extract-keys (to-expr ancestor) :no-ops t))
              (setf ancestor (parent ancestor))))
       )))
  (update-subtree-index tree) ;; not the most efficient
  (update-dfs-order tree)
  tree)

