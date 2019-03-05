(in-package :ttt)
(defparameter *built-patterns* (make-hash-table :test #'equal))
(defparameter *ttt-debug-level* 0)
(defclass pattern ()
  ((min-width :accessor min-width :initform 0 :initarg :min-width)
   (max-width :accessor max-width 
	      :initform most-positive-fixnum 
	      :initarg :max-width)
   (min-height :accessor min-height :initform 0)
   (max-height :accessor max-height :initform most-positive-fixnum )
   (keys :accessor keys)
   (compiled? :accessor compiled? :initform nil)
   (initialized? :accessor initialized? :initarg :initialized? :initform nil)
   (to-expr :accessor to-expr)
   (match-fn :accessor match-fn)
   (var :accessor var :initform nil :initarg :var))
  (:documentation 
   "General class representing properties common to all patterns, 
   such as min/max matchable tree sequence width, and operator type.
   This is a purely abstract class, not meant to be instantiated directly.

   A pattern is a static object and should not be modified 
   after construction.

   The constructors for descendant patterns should initialze match-fn
   to either a generic (interpreted) function or compiled function which
   is unique to the pattern object."))
  
(defclass has-iter-constraints ()
  ((min-iter :accessor min-iter :initarg :min-iter)
   (max-iter :accessor max-iter :initarg :max-iter)))
(defclass has-depth-constraints ()
  ((min-depth :accessor min-depth :initform 0)
   (max-depth :accessor max-depth :initform most-positive-fixnum)))
(defclass has-pos-args ()
  ((pos-args :accessor pos-args :initform nil :initarg :pos-args)))
(defclass has-neg-args ()
  ((neg-args :accessor neg-args :initform nil :initarg :neg-args)))
  
(defun build-pattern (expression)
  "Builds a pattern object according to expression."
  (if (gethash expression *built-patterns*)
      (return-from build-pattern (gethash expression *built-patterns*)))
  (if (op-is-pred? (patt-expr-get-op expression)) 
      (get-pred-instance expression)
      (let* ((pattern-type 
	      (case (patt-expr-get-op expression)
		((_! _+ _* _?) 'unrestricted-seq)
		((! + * ?) 'restricted-seq)
		((<>) 'free-seq)
		(({}) 'permuted-seq)
		((^ ^* ^+ ^^ ^n) 'descendant-patt)
		((^@) 'vertical-patt)
		((@) 'point-of-attachment)
		;; stuck-patt only used for operators requiring args 
		;; but which appear without them
		(sticky-check 'stuck-patt)     
		((/) 'transduction)
		(literal 'literal-patt)
		(general 'general-patt)
		(otherwise 
		 (error 
		  (format nil "problem in build-patt, unrecognized expression: ~s:~s~%" (patt-expr-get-op expression) expression)))))
	     (pattern (make-instance pattern-type)))
	(if (> *ttt-debug-level* 0)
	    (format t "building pattern: ~s (type= ~s)~%" expression pattern-type))
	(setf (to-expr pattern) expression)
	;; could do this in compile-pattern
	(setf (keys pattern) (extract-keys expression))   
	;; compile-pattern returns modified pattern
	(prog1 
	 (setf (gethash expression *built-patterns*) 
	       (compile-pattern pattern))
	  (if (> *ttt-debug-level* 0)
	      (format t "minWidth= ~s, maxWidth= ~s~%" (min-width pattern) (max-width pattern)))))))
	



(declaim (inline ensure-compiled))    
(defmethod ensure-compiled ((patt pattern))
  (if (compiled? patt) 
      patt 
      (compile-pattern patt)))


(declaim (inline match))
(defmethod match ((patt pattern) tree-seq bindings)
  "Generic method to match a compiled pattern against a sequence of trees"
  (if (> *ttt-debug-level* 0)
      (format t "match: [patt=~s,tree-seq=~s]~%" (to-expr patt) (mapcar #'to-expr tree-seq)))
  (funcall (match-fn patt) tree-seq bindings))
(defun match-expr (pattern-expr tree-expr &key (bind-form :hash-table))
  "Added because the suffix expr is more meaningful than encap."
  (match-encap pattern-expr tree-expr :bind-form bind-form))

(defun match-encap (pattern-expr tree-expr &key (bind-form :hash-table))
  "Match a pattern expression against a tree expression."
  (let ((b (match (build-pattern pattern-expr) (list (build-tree tree-expr)) t )))
    (if (or (eq b t) (eq b nil))
	b
	(let ((result-binds)
	      binds)
	  (case bind-form
	    ((:hash-table)
	     (setf result-binds (make-hash-table))
	     (maphash 
	      (lambda (k v)
		(unless (eq k '/)
		  (setf binds t)
		  (setf (gethash k result-binds) 
			(mapcar 
			 (lambda (binds)
			   (mapcar #'to-expr binds))
			 v))))
	      b))
	    ((:expr)
	     (maphash 
	      (lambda (k v)
		(unless (eq k '/)
		  (setf binds t)
		  (push (cons k (if (car v) (mapcar #'to-expr (car v)) nil)) result-binds)))
	      b)
	     (setf result-binds (nreverse result-binds))))
	  (if (and result-binds binds)
	      result-binds
	      t)))))


(defmethod deep-match ((patt pattern) tree )
  "Match a compiled pattern against a compiled tree object, 
   use keys to avoid matching when guaranteed to fail."
  (if (keys patt)
      (dolist (key (keys patt))
	(dolist (subtree (fastmap key (to-subtrees tree)))
	  (if (and (>= (height subtree) (min-height patt))
		   (<= (height subtree) (max-height patt)))
	      (let ((b (match patt (list subtree) t )))
		(if b (return-from deep-match b))))))
      (deep-match-brute patt tree )))

(defun deep-match-brute (pattern tree )
  "Matched a compiled pattern against a compiled tree object, 
   each subtree is tested."
  (let ((b (match pattern (list tree) t )))
    (if b 
	b
	(unless (leaf? tree)
	  (dolist (c (children tree))
	    (setf b (deep-match-brute pattern c ))
	    (if b (return b)))))))
