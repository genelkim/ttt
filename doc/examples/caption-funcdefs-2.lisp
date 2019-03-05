;; Predicates required for caption repairs
;; 8-16-2012 - changed to work with revised TTT and so that the functions
;;             can be called directly from Lisp
;; 6-12-2012 - temporarily added 'finite-required-top to facilitate hashing
;;             from trees to applicable rules;
;; 1-18-2012 - added rough comments, cleaned up code.  

(load "/p/nl/tools/ttt/examples/pp-suffixes.lisp")
(load "/p/nl/tools/ttt/examples/male-names.lisp")    
(load "/p/nl/tools/ttt/examples/female-names.lisp")

(defparameter *knext-names*
  (make-hash-table))
(dolist (name (cons 'HALLE (append *male-names* *female-names*)))
  (setf (gethash name *knext-names*) t))
(defparameter *knext-pp-suffixes*
  (make-hash-table))
(dolist (suffix (cons 'PP *pp-suffixes*))
  (setf (gethash suffix *knext-pp-suffixes*) t))
(defparameter *locative-prep-ht*
  (make-hash-table))
(defparameter *locative-preps*
  ;; locative prep's taken from http://www.eslgold.com/grammar/prepositions_location.html
  '(IN ON AT BY NEAR NEARBY ABOVE BELOW OVER UNDER UP DOWN AROUND THROUGH 
    INSIDE BETWEEN BESIDE BEYOND WITHIN BENEATH UNDERNEATH AMONG ALONG 
    AGAINST WITH))
(dolist (prep *locative-preps*)
  (setf (gethash prep *locative-prep-ht*) t))

(defun PP? (tree-expr)
  "Return t if tree-expr is a single PP symbol. (ex: PP-IN"
  (if (gethash tree-expr *knext-pp-suffixes*) t))
(setf (get 'PP? 'finite-required-top)  *pp-suffixes*)

(defun PPT? (tree-expr)
  "Return t if tree-expr is a a single PP tree. 
   (ex: (PP (IN TO) (NP (DET THE) (NN STORE))))"
  (and (consp tree-expr)
       (PP? (car tree-expr))))
(setf (get 'PPT? 'finite-required-top) 
      (list *pp-suffixes*))

(defun locative-p? (tree-expr)
  "Return t if tree-expr is a locatative preposition tree (ex: (IN ABOVE))."
  (and (consp tree-expr)
       (eq (car tree-expr) 'IN)
       (gethash (cadr tree-expr) *locative-prep-ht*)))
(setf (get 'locative-p? 'finite-required-top)
      (list *locative-preps*))
(defun nn-human? (tree-expr)
  "Return t if tree-expr is a list containing a single tree which is a NN with human. 
   Ex: (NN TANYA)" 
  (and (consp tree-expr)
       (member (car tree-expr)  '(NN NNP))
       (gethash (cadr tree-expr) *knext-names*)))
(setf (get 'nn-human? 'finite-required-top)
      (list (append *male-names* *female-names*)))

(defun nn-nonhuman? (tree-expr)
  "Return t if tree-expr is a list containing a single tree which represents a nonhuman noun.
   Ex: (NN HORSE), (NN TRIVIA)"
  (and (consp tree-expr)
       (member (car tree-expr) '(NN NNP))
       (not (nn-human? tree-expr))))
(setf (get 'nn-nonhuman? 'finite-required-top)
      '((NN NNP)))

