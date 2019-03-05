(in-package :ttt)
;;  bindings.lisp

;;    an individual binding is a pair:  (cons bind-variable bind-sequence),. 
;;    an standard variable may be bound more than once; 
;;       the most recently added binding will always be the binding returned.
;;    a bind-variable is an atom
;;    a bind-sequence is any sequence 
;;      (of lisp expressions, nodal trees, or paths). 
;;      (or in the case of transduction bindings, a structure)
(defun add-binding (binding bindings)
  "Nondestructively add the binding to bindings."
  (let ((bvar (bind-var binding)) (bseq (bind-seq binding)))
    (cond  ((and (eq bindings t) binding) 
	    (add-binding binding (make-hash-table)))
	   ((or (null bvar) (null bindings)) 
	    bindings)
	   (t 
	    (let ((newbinds (make-hash-table)))
	      (maphash 
	       (lambda (k v) 
		 (setf (gethash k newbinds) 
		       (gethash k bindings)))
	       bindings)
	      (push bseq (gethash bvar newbinds))
	      newbinds)))))

(defun nadd-binding (binding bindings)
  "Destructively add the binding to bindings."
  (let ((bvar (bind-var binding)) (bseq (bind-seq binding)))
    (cond  ((and (eq bindings t) binding) 
	    (add-binding binding (make-hash-table)))
	   ((or (null bvar) (null bindings)) 
	    bindings)
	   (t (push bseq (gethash bvar bindings))
	      bindings))))

(defun bound? (var bindings) 
  "Return t if variable is bound, nil otherwise."
  (if (hash-table-p bindings) (not (null (gethash var bindings)))))
(defun get-binding (bvar bindings)
  "Return the binding for variable bvar in bindings."
  (cond 
    ((hash-table-p bindings)
     (car (gethash bvar bindings)))
    ((listp bindings)
     ;; (cdr nil) = nil, so doesn't have to check for nil
     (cdr (find-if (lambda (b) (eq (car b) bvar)) bindings)))
    ((eq bindings t)
     nil)
    (t 
     (format t "bad bindings detected: ~s~%" bindings))))

(defun bind-var (binding)
  (car binding))
(defun bind-seq (binding)
  (cdr binding))
(defun mk-binding (variable target)
  (cons variable target))
(defun mk-tmp (op &optional bind)
  (let ((sym (gensym "TTT-TMP-OP")))
    (setf (get sym 'op) op)
    (setf (get sym 'nobind) (null bind))
    (when bind
	(setf (get sym 'forcebind) bind))
    sym))

(defun satisfies-constraints (binds constraints)
  "STUB"
  (dolist (c constraints)
    (if (funcall c binds) (return nil)))
  t)
