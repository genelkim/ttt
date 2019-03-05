;; Predicates required for caption repairs
;; 6-12-2012 - temporarily added 'finite-required-top to facilitate hashing
;;             from trees to applicable rules;
;; 1-18-2012 - added rough comments, cleaned up code.  


(defun PP? (sequence)
  "Return t if sequence is a list containing a single PP symbol."
  (match '(! PP-WITH PP-AT PP-ON PP-IN PP) sequence))
(add-op 'PP? #'PP? :predicate t)
(setf (get 'PP? 'finite-required-top) '(PP-WITH PP-AT PP-ON PP-IN PP))

(defun PPT? (sequence)
  "Return t if sequence is a list containing a single PP tree."
  (match '(! (PP? _+)) sequence))
(add-op 'PPT? #'PPT? :predicate t)
(setf (get 'PPT? 'finite-required-top) 
      '((PP-WITH PP-AT PP-ON PP-IN PP)))

(defun locative-p? (sequence)
  "Return t if sequence is a list containing a single locatative preposition tree (ex: (IN ABOVE))."
  ;; locative prep's taken from http://www.eslgold.com/grammar/prepositions_location.html
  (match '(IN (! IN ON AT BY NEAR NEARBY ABOVE BELOW OVER UNDER UP DOWN AROUND THROUGH INSIDE BETWEEN BESIDE BEYOND WITHIN BENEATH UNDERNEATH AMONG ALONG AGAINST WITH)) sequence))
(add-op 'locative-p? #'locative-p? :predicate t)
(setf (get 'locative-p? 'finite-required-top)
      '((IN IN ON AT BY NEAR NEARBY ABOVE BELOW OVER UNDER UP DOWN AROUND THROUGH INSIDE BETWEEN BESIDE BEYOND WITHIN BENEATH UNDERNEATH AMONG ALONG AGAINST WITH)))

(defun nn-human? (sequence)
  "Return t if sequence is a list containing a single tree which is a NN with human. 
   Ex: (NN HALLE)" 
  (match '((! NN NNP) (! HALLE TANYA SISTER)) sequence))
(add-op 'nn-human? #'nn-human? :predicate t)
(setf (get 'nn-human? 'finite-required-top)
      '((NN NNP HALLE TANYA SISTER) ))

(defun nn-nonhuman? (sequence)
  "Return t if sequence is a list containing a single tree which represents a nonhuman noun.
   Ex: (NN HORSE), (NN TRIVIA)"
  (match '((! NN NNP) (! ~ HALLE TANYA SISTER)) sequence))
(add-op 'nn-nonhuman? #'nn-nonhuman? :predicate t)
(setf (get 'nn-nonhuman? 'finite-required-top)
      '((NN NNP)))
