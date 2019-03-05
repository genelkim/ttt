(in-package :ttt)

;;     This was primarily used during inital development.   
;;     It is certainly not comprehensive in testing combinations of pattern operators.
;;     It only tests match pass/fail.    It does not test transductions, constructions, or even inspect bindings.
;;     It does not test the iterative constraints or sticky bindings.
;;     The trees searched are mostly hand-written and not as complex as actual parse trees.


;;     To use:  user between tests
;;     when mode is fail, only the tests which were failed are displayed
;;     when disp-counts is true, the overall pass/fail counts are displayed



;; 2011-12-13 - added sticky tests


;(format t "testing hasn't been completely rewritten for oo version")

(defparameter *pattern-matching-tests*
  nil)
(defun match-f (pattern tree)
  (match pattern (list tree) t))

(defun compile-tests ()
  (let (tmp)
    (dolist (pair *pattern-matching-tests*)
      (push (list (build-pattern (test-getpat pair))
		  (build-tree (test-getexpr pair))
		  (test-getbind pair))
	    tmp))
    (setf *pattern-matching-tests* (nreverse tmp))))
  

(defparameter matchfn 'match-f)

(defun run-tests (&key (multi-line t) (mode 'fail)  (prompt nil) (disp-counts t))
  (if (not multi-line)
      (format t "~20a~t ~t~20a ~t~20a ~t ~a~%" "pattern" "expression" "bindings" "status"))
  (let ((count-pass 0) (count-fail 0))
    (dolist (test *pattern-matching-tests*)
      (when (eq mode 'all)
	(if multi-line
	    (format t "----------------~%patt: ~s~%expr: ~s~%" (to-expr (test-getpat test)) (to-expr (test-getexpr test)))
	  (format t "~20s~t ~t~20s ~t " (test-getpat test) (test-getexpr test))))
      (let ((result (funcall matchfn (test-getpat test) (test-getexpr test))))
	(if (or (and result (test-getbind test))
		(and (null result) (null (test-getbind test))))
	    (incf count-pass)
	  (incf count-fail))
	(when (or (and (eq mode 'fail) 
		       (not (or (and result (test-getbind test))
				(and (null result) (null (test-getbind test))))))
		  (and (eq mode 'pass)
		       (or (and result (test-getbind test))
			   (and (null result) (null (test-getbind test)))))
		  (eq mode 'all))
	  (unless (eq mode 'all)
	    (if multi-line
		(format t "----------------~%patt: ~s~%expr: ~s~%result: ~s~%" (to-expr (test-getpat test)) (to-expr (test-getexpr test)) result)
	      (format t "~20s~t ~t~20s ~t~20a ~t " (to-expr (test-getpat test)) (to-expr (test-getexpr test)) result)))
	  (cond ((or (and result (test-getbind test))
		     (and (null result) (null (test-getbind test))))
		 (format t "PASS~t~a~%" result))
		(t (format t "FAIL~t~a~%" result)))
	  (if prompt
	      (let ((action (read-char)))
		(if (equalp (intern (string-upcase  action) *package*) 's)
		    (return-from run-tests nil)))))))
    (if disp-counts
	(format t "pass: ~a~tfail:  ~a~%" count-pass count-fail))))


(defun disp-test (test &key (multi-line t))
  (let ((result (funcall matchfn (test-getpat test) (test-getexpr test))))
    (if multi-line
	(format t "P: ~s~%E: ~s~%R: ~s~%S: " (test-getpat test) (test-getexpr test) result)
	(format t "~20s~t ~t~20s ~t~20a ~t " (test-getpat test) (test-getexpr test) result))
    (if (or (and result (test-getbind test))
	    (and (null result) (null (test-getbind test))))
	(format t "PASS~%" result)
	(format t "FAIL~%" result))))



(defun add-test (patt expr bindings)
  (setf *pattern-matching-tests* (cons (list patt expr bindings) *pattern-matching-tests*)))
(defun test-getpat (test)
  (first test))
(defun test-getexpr (test)
  (second test))
(defun test-getbind (test)
  (third test))


(defun run-simple-tests (&key (prompt) (mode 'all) nodals)
  (clear-tests)
  (load-tests-simple)
  (format t "individually testing match pass/fail for _!, _?, _+, _*, !, and ?:~%")
  (run-tests :disp-counts 't :prompt prompt :mode mode :nodals nodals)
)
  


(defun clear-tests ()
  (setf *pattern-matching-tests* nil))

	    
	  


(defun load-tests-nva ()
  (add-test '(X) '(X) 't)
  (add-test '(Y) '(X) nil)
  (add-test '(X Y) '(X Y) 't)
  (add-test '(Y X) '(Y X) t)
  (add-test '(X)  '() nil)
  (add-test '(X)  '(X X) nil)
  (add-test '(X Y Z) '(X Y Z) 't))



(defun load-tests-_! () 	

  ; at least one
  (add-test '(_!) '() nil)
  (add-test '(X _!) '(X) nil)
  (add-test '(_! X) '(X) nil)
  (add-test '(X _! X) '(X X) nil)
  (add-test '(X _! Y) '(X Y) nil)
  ; at most one
  (add-test '(_!) '(X X) nil) 
  ; typical useages


  ; try each of first, middle, last, succeed & fail
  (add-test '(_!) '(X) 't)
  (add-test '(_!) '(X Y) nil)
  (add-test '(_!) '(X X) nil)
  (add-test '(_! X) '(X X) 't)
  (add-test '(_! X) '(X Y) nil)
  (add-test '(X _!) '(Y X) nil)
  (add-test '(X _!) '(X X) 't)

  (add-test '(X _! X) '(X Y X) 't)
  (add-test '(X _! Z) '(X Y Z) 't)
  (add-test '(X X X) '(X X X) 't)
  (add-test '(X _! Y) '(Y Y Y) nil)
  (add-test '(Y _! X) '(Y X Y) nil)
  (add-test '(Y _! X) '(Y X X) 't))
  
(defun load-tests-! ()
  (add-test '((! X)) '(X) 't)
  (add-test '((! X)) '(Y) nil)
  (add-test '((! X Y)) '(X) 't)
  (add-test '((! Y X)) '(X) 't)
  (add-test '((! X Y)) '(X Y) nil)
  (add-test '((! Y X)) '(Y X) nil)
  (add-test '((! X ~ Z)) '(X) 't)
  (add-test '((! X ~ Z)) '(Y) nil)
  (add-test '((! X ~ Z)) '(Z) nil)
  (add-test '(X (! X Y ~ Z)) '(X Y) 't)
  (add-test '(X (! X Y ~ Z)) '(X X) 't)
  (add-test '(X (! X Y ~ Z)) '(X Z) nil)

  (add-test '(X (! X Y ~ Z) X) '(X Y) nil)
  (add-test '(X (! X Y ~ Z) Y) '(X X) nil)
  (add-test '(X (! X Y ~ Z) Z) '(X Z) nil)

  (add-test '(X (! X Y ~ Z) X) '(X Y X) 't)
  (add-test '(X (! X Y ~ Z) Y) '(X X Y) 't)
  (add-test '(X (! X Y ~ Z) Z) '(X Z X) nil)
  (add-test '(X (! X Y ~ Z) Z) '(X Z Z) nil)
  (add-test '(X (! X Y ~ Z) Z) '(X Y Z) 't)
  (add-test '(X (! X Y ~ Z) Z) '(X X Z) 't))



(defun load-tests-_? () 
  (add-test '(_?) '(X) 't)
  (add-test '(_?) '() 't)
  (add-test '(_?) '(X Y) nil)
  (add-test '(X _?) '(X Y) 't)
  (add-test '(_? X) '(X Y) nil)
  (add-test '(X _?) '(Y X) nil)
  (add-test '(_? X) '(Y X) 't))

(defun load-tests-? ()
  ; general pattern form: '( ... (? X1 ... Xm ~ Y1 ... Yn) ... )
  ; - anywhere a ! should be true, the corresponding ? should be true
  (add-test '((? X)) '(X) 't)
  (add-test '((? X)) '() 't)
  (add-test '((? X)) '(X) 't)
  (add-test '((? X)) '(Y) nil)
  (add-test '((? X Y)) '(X) 't)
  (add-test '((? Y X)) '(X) 't)
  (add-test '((? X Y)) '(Y) 't)
  (add-test '((? Y X)) '(Y) 't)
  (add-test '((? X Y)) '(X Y) nil)
  (add-test '((? Y X)) '(Y X) nil)
  (add-test '((? X ~ Z)) '(X) 't)
  (add-test '((? X ~ Z)) '(Y) nil)
  (add-test '((? X ~ Z)) '(Z) nil)
  (add-test '(X (? X Y ~ Z)) '(X Y) 't)
  (add-test '(X (? X Y ~ Z)) '(X X) 't)
  (add-test '(X (? X Y ~ Z)) '(X Z) nil)

  (add-test '(X (? X Y ~ Z) X) '(X Y) nil)
  (add-test '(X (? X Y ~ Z) Y) '(X X) nil)
  (add-test '(X (? X Y ~ Z) Y) '(X Y) 't)
  (add-test '(X (? X Y ~ Z) X) '(X X) 't)
  (add-test '(X (? X Y ~ Z) Z) '(X Z) 't)

  (add-test '(X (? X Y ~ Z) X) '(X Y X) 't)
  (add-test '(X (? X Y ~ Z) Y) '(X X Y) 't)
  (add-test '(X (? X Y ~ Z) Z) '(X Z X) nil)
  (add-test '(X (? X Y ~ Z) Z) '(X Z Z) nil)
  (add-test '(X (? X Y ~ Z) Z) '(X Y Z) 't)
  (add-test '(X (? X Y ~ Z) Z) '(X X Z) 't))

(defun load-tests-_* ()
  (add-test '(_*) '() 't)
  (add-test '(X _*) '() nil)
  (add-test '(X _*) '(X) 't)
  (add-test '(X _*) '(X Y) 't)
  (add-test '(X _*) '(Y X) nil)
  (add-test '(X _*) '(X Y Z) 't)
  (add-test '(X _*) '(Y X Z) nil)
  (add-test '(_* X) '() nil)
  (add-test '(_* X) '(X) 't)
  (add-test '(_* X) '(X Y) nil)
  (add-test '(_* X) '(Y X) 't)
  (add-test '(_* X) '(X Y Z) nil)
  (add-test '(_* X) '(Y X Z) nil)
  (add-test '(_* X) '(Y Z X) 't)
  (add-test '(X _* Y) '() nil)
  (add-test '(X _* Y) '(X) nil)
  (add-test '(X _* Y) '(X Y) 't)
  (add-test '(X _* Y) '(Y X) nil)
  (add-test '(X _* Y) '(X Y Z) nil)
  (add-test '(X _* Y) '(Y X Z) nil))

(defun load-tests-* ()
  (add-test '((* X)) '(X) 't)
  (add-test '((* X)) '() 't)
  (add-test '((* X)) '(Y) nil)
  (add-test '((* X Y)) '(X) 't)
  (add-test '((* Y X)) '(X) 't)
  (add-test '((* X Y)) '(X Y) 't)
  (add-test '((* Y X)) '(Y X) 't)
  (add-test '((* X Y)) '(X Y Y Y X X X X Y X) 't)
  (add-test '((* Y X)) '(Y Y X X Y  X) 't)

  (add-test '((* X ~ Z)) '(X) 't)
  (add-test '((* X ~ Z)) '(Y) nil)
  (add-test '((* X ~ Z)) '(Z) nil)
  (add-test '(X (* X Y ~ Z)) '(X Y) 't)
  (add-test '(X (* X Y ~ Z)) '(X X) 't)
  (add-test '(X (* X Y ~ Z)) '(X Z) nil)

  (add-test '(X (* X Y ~ Z) X) '(X Y) nil)
  (add-test '(X (* X Y ~ Z) Y) '(X X) nil)
  (add-test '(X (* X Y ~ Z) Y) '(X Y) 't)
  (add-test '(X (* X Y ~ Z) X) '(X X) 't)
  (add-test '(X (* X Y ~ Z) Z) '(X Z) 't)

  (add-test '(X (* X Y ~ Z) X) '(X Y X) 't)  
  (add-test '(X (* X Y ~ Z) X) '(X Y Y X) 't)
  (add-test '(X (* X Y ~ Z) X) '(X Y X X Y X Y X) 't)
  (add-test '(X (* X Y ~ Z) X) '(X Y X X Y X Y Z X) nil)
  (add-test '(X (* X Y ~ Z) X) '(X Y X X Y X Y X Y) nil)
  (add-test '(X (* X Y ~ Z) X) '(X Y X X Y X Y Z X Y) nil)

  (add-test '(X (* X Y ~ Z) Y) '(X X Y) 't)
  (add-test '(X (* X Y ~ Z) Z) '(X Z X) nil)
  (add-test '(X (* X Y ~ Z) Z) '(X Z Z) nil)
  (add-test '(X (* X Y ~ Z) Z) '(X Y Z) 't)
  (add-test '(X (* X Y ~ Z) Z) '(X X Z) 't)
  (add-test '(X (* X Y ~ Z) Z) '(X Y X Z) 't)
  (add-test '(X (* X Y ~ Z) Z) '(X X Y Y X Z) 't))
(defun load-tests-+ ()
  (add-test '((+ X)) '(X) 't)
  (add-test '((+ X)) '() nil)
  (add-test '((+ X)) '(Y) nil)
  (add-test '((+ X Y)) '(X) 't)
  (add-test '((+ Y X)) '(X) 't)
  (add-test '((+ X Y)) '(X Y) 't)
  (add-test '((+ Y X)) '(Y X) 't)
  (add-test '((+ X Y)) '(X Y Y Y X X X X Y X) 't)
  (add-test '((+ Y X)) '(Y Y X X Y  X) 't)

  (add-test '((+ X ~ Z)) '(X) 't)
  (add-test '((+ X ~ Z)) '(Y) nil)
  (add-test '((+ X ~ Z)) '(Z) nil)
  (add-test '(X (+ X Y ~ Z)) '(X Y) 't)
  (add-test '(X (+ X Y ~ Z)) '(X X) 't)
  (add-test '(X (+ X Y ~ Z)) '(X Z) nil)

  (add-test '(X (+ X Y ~ Z) X) '(X Y) nil)
  (add-test '(X (+ X Y ~ Z) Y) '(X X) nil)
  (add-test '(X (+ X Y ~ Z) Y) '(X Y) nil)
  (add-test '(X (+ X Y ~ Z) X) '(X X) nil)
  (add-test '(X (+ X Y ~ Z) Z) '(X Z) nil)

  (add-test '(X (+ X Y ~ Z) X) '(X Y X) 't)  
  (add-test '(X (+ X Y ~ Z) X) '(X Y Y X) 't)
  (add-test '(X (+ X Y ~ Z) X) '(X Y X X Y X Y X) 't)
  (add-test '(X (+ X Y ~ Z) X) '(X Y X X Y X Y Z X) nil)
  (add-test '(X (+ X Y ~ Z) X) '(X Y X X Y X Y X Y) nil)
  (add-test '(X (+ X Y ~ Z) X) '(X Y X X Y X Y Z X Y) nil)

  (add-test '(X (+ X Y ~ Z) Y) '(X X Y) 't)
  (add-test '(X (+ X Y ~ Z) Z) '(X Z X) nil)
  (add-test '(X (+ X Y ~ Z) Z) '(X Z Z) nil)
  (add-test '(X (+ X Y ~ Z) Z) '(X Y Z) 't)
  (add-test '(X (+ X Y ~ Z) Z) '(X X Z) 't)
  (add-test '(X (+ X Y ~ Z) Z) '(X Y X Z) 't)
  (add-test '(X (+ X Y ~ Z) Z) '(X X Y Y X Z) 't))
  

(defun load-tests-_+ ()
  (add-test '(_+) '() nil)
  (add-test '(X _+) '() nil)
  (add-test '(X _+) '(X) nil)
  (add-test '(X _+) '(X Y) 't)
  (add-test '(X _+) '(Y X) nil)
  (add-test '(X _+) '(X Y Z) 't)
  (add-test '(X _+) '(Y X Z) nil)
  (add-test '(_+ X) '() nil)
  (add-test '(_+ X) '(X) nil)
  (add-test '(_+ X) '(X Y) nil)
  (add-test '(_+ X) '(Y X) 't)
  (add-test '(_+ X) '(X Y Z) nil)
  (add-test '(_+ X) '(Y X Z) nil)
  (add-test '(_+ X) '(Y Z X) 't)
  (add-test '(X _+ Y) '() nil)
  (add-test '(X _+ Y) '(X) nil)
  (add-test '(X _+ Y) '(X Y) nil)
  (add-test '(X _+ Y) '(Y X) nil)
  (add-test '(X _+ Y) '(X Y Z) nil)
  (add-test '(X _+ Y) '(Y X Z) nil))











(defun load-tests-nested ()
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(X X Y Y Y X X) t)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(X  Y Y Y X X) nil)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(X X Y Y Y  X) nil)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(X X Y Y Y X X X) nil)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(X ) nil)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(Y ) nil)
  (add-test '((+ (+ (<> X X)) (+ (<> Y Y Y)))) '(Y Y) nil)

  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '() nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(X X) t)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(X X X X) t)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y Y Y) t)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y Y Y Y Y Y) t)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(X X X) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(X ) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y ) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y  Y) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y Y Y Y Y) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(X X Y Y Y) nil)
  (add-test '((! (+ (<> X X)) (+ (<> Y Y Y)))) '(Y Y Y X X) nil)
  (add-test '((<> X)) '(X) t)
  (add-test '(<> X) 'X t)
  (add-test '(<> X) '(X) nil)
  (add-test '((+ (<> X))) '(X X X) t)
  (add-test '((+ (<> X))) '() nil)
  (add-test '((+ (<> X))) '(X) t)
  (add-test '((+ (<> X))) 'X nil)
  (add-test '(+ (<> X)) 'X t)
  (add-test '(+ (<> X)) nil nil)
  (add-test '(+ (<> X)) '(X) nil)
  (add-test '((! (+ A) (+ B))) '()  nil)
  (add-test '((! (+ A) (+ B))) '(A) 't)
  (add-test '((! (+ A) (+ B))) '(A A A) 't)
  (add-test '((! (+ A) (+ B))) '(B) 't)
  (add-test '((! (+ A) (+ B))) '(B B B) 't)
  (add-test '((! (+ A) (+ B))) '(A B) nil)
  (add-test '((! (+ A) (+ B))) '(B A) nil)
  (add-test '(X (Y (Z))) '(X (Y (Z))) 't)
  (add-test '(X (Y (Z))) '(X Y Z) nil)
  (add-test '(X Y Z) '(X (Y (Z))) nil)
  (add-test '(X (_!) Z) '(X (Y) Z) t)
  (add-test '(X (_!) Z) '(X () Z) nil)
  (add-test '(X (_!) Z) '(X Y Z) nil)
  (add-test '(X (_!) Z) '(X  Z) nil)
  (add-test '(X (_?) Z) '(X (Y) Z) t)
  (add-test '(X (_?) Z) '(X () Z) t)
  (add-test '(X (_?) Z) '(X Y Z) nil)
  (add-test '(X (_?) Z) '(X  Z) nil)
  (add-test '(X () Z) '(X () Z) t)
  
  (add-test '(X (X)) '(X (X)) t)
  (add-test '(X (X)) '((X) X) nil)
  (add-test '(X (X)) '(X (X)) t)
  (add-test '((X) X) '((X) X) t)

  (add-test '(_!) '(X) t)
  (add-test '_! '(X) t)
  (add-test '(_!) '((X)) t)
  (add-test '(_!) 'X nil)
  (add-test '_! '() t)
  (add-test '(X _! Z) '(X (Y) Z) t)
  (add-test '(X _! Z) '(X () Z) t)
  (add-test '(X (! (Y)) Z) '(X (Y) Z) t)
  (add-test '(_?) '(X) t)
  (add-test '_? '(X) t)
  (add-test '(_?) '((X)) t)
  (add-test '(_?) 'X nil)
  (add-test '_? '() t)
  (add-test '(X _? Z) '(X (Y) Z) t)
  (add-test '(X _? Z) '(X () Z) t)
  (add-test '(X (? (Y)) Z) '(X (Y) Z) t)
  (add-test '(X (? (Y)) Z) '(X  Z) t)


  (add-test '((^ X)) 'X nil)
  (add-test '((^ X)) '(X) nil)
  (add-test '((^ X)) '((X)) t)
  (add-test '(^ X) 'X nil)
  (add-test '(^ X) '(X) t)
  
  (add-test '(^^ X) 'X nil)
  (add-test '(^^ X) '(X) nil)
  (add-test '(^^ X) '((X)) t)
  (add-test '((^^ X)) '(X) nil)
  (add-test '((^^ X)) '((X)) nil)
  (add-test '((^^ X)) '(((X))) t)
  
  (add-test '(^* X) 'X t)
  (add-test '(^* X) '(X) t)
  (add-test '(^* X) '((X)) t)
  (add-test '(^* X) '(((X))) t)
  (add-test '(^* X) '(Y (Z (X))) t)
  (add-test '(^* X) '((((((K (Y (Z (X))))))))) t)
  (add-test '((^* X)) 'X nil)
  (add-test '((^* X)) '(X) t)
  (add-test '((^* X)) '((X)) t)
  (add-test '((^* X)) '(((X))) t)
  (add-test '((^* X)) '(Y (Z (X))) nil)
  (add-test '((^* X)) '((((((K (Y (Z (X))))))))) t))



(defun load-tests-live ()
  (print 'live-tests-should-be-deep)
  (add-test
   '(_+ (NP (NP _+1) (CC and) (NP (NP _+2) (PP _+3))) _*)
   '(S1 (NP (NP (NNP NOAH FRIEDLAND)) (|,| |,|) (NP (JJ UPPER) (NN RIGHT)) (|,| |,|) (PP-WITH (IN WITH) (NP (NP (NNS FRIENDS)) (CC AND) (NP (NP (NN FAMILY)) (PP  (IN AT)(NP (DETP (NP (PRP HE)) (POS |'S|)) (NN BOOK) (NN SIGNING))))))(\. \.)))
   t) ; recons (_+ (NP (NP _+1) (CC and) (NP _+2)) (PP _+3) _*)
  
  (add-test
   '(_+ (PP (IN _!) (NP (NP _+1) (PP _+2))) _*)
   '(S1 (FRAG (NP (NNP TANYA)) (|,| |,|) (PP-IN (IN IN) (NP (DETP (NP (PRP SHE)) (POS |'S|)) (JJ BLACK) (NN DRESS)))(|,| |,|)   (PP-WITH (IN WITH)(NP (NP (NN FAMILY)) (PP-AT (IN AT)(NP (DETP (NP (PRP SHE)) (POS |'S|)) (NN GRADUATION)(NN PARTY))))) (\. \.)))
   t) ; recons(_+ (PP (IN _!) (NP _+1)) (PP _+2) _*)

  (add-test
   '(_+ (NP (NP _+1) (CC and) (NP (NP _+2) (PP _+3))) _*)
   '(S1
     (NP
      (NP (NP (NNP BEN)) (CC AND)
       (NP (DETP (NP (PRP HE)) (POS |'S|)) (NN FRIEND) (NNP REGGI)))
      (|,| |,|) (PP-AT (IN AT) (NP (DT THE) (NN PARK))) (\. \.)))
					; '(_+ (NP (+ (<> (NP _+1) (? (, |,|)))) (CC and) 
					;       (NP (NP _+2) (! (PP locative-p? _+3)))) _*)
   t) ; recons   (_+ (NP <> (CC and) (NP _+2)) ! _*)


  (add-test
   '(_+ (NP (+ (<> (NP _+1) (? (|,| |,|)))) (CC and) 
	 (NP (NP _+2) (! (PP locative-p? _+3)))) _*)
   '(S1
     (NP
      (NP (NP (NNP BEN)) (CC AND)
       (NP (DETP (NP (PRP HE)) (POS |'S|)) (NN FRIEND) (NNP REGGI)))
      (|,| |,|) (PP-AT (IN AT) (NP (DT THE) (NN PARK))) (\. \.)))
   t) 

  (add-test
   '(_+ (PP (IN _!) (NP (NP _+1) (PP _+2))) _*)
   '(S1
     (NP (NP (NNP HALLE))
      (PRN (-LRB- -LRB-) (PP-ON (IN ON) (NP (DT THE) (NN RIGHT)))
       (-RRB- -RRB-))
      (CC AND)
      (NP (NNP SAUL)
       (PRN (-LRB- -LRB-) (PP-ON (IN ON) (NP (DT THE) (NN LEFT)))
	    (-RRB- -RRB-))
       (PP-WITH (IN WITH)
		(NP (NP (NNP MRS. KLAUSER))
		    (PP-AT (IN AT)
			   (NP (NP (DT THE) (NN SUMMIT))
			       (PP-FOR (IN FOR) (NP (NN DINNER.))))))))))
   t) ; recons     (_+ (PP (IN _!) (NP _+1)) (PP _+2) _*)

  (add-test
   '(_+ (PP (IN _!) (NP (NP _* ((! NNP NNPS) _+1)) (PP _+2))) _*1)
   '(S1
     (NP (NP (NNP HALLE))
      (PRN (-LRB- -LRB-) (PP-ON (IN ON) (NP (DT THE) (NN RIGHT)))
       (-RRB- -RRB-))
      (CC AND)
      (NP (NNP SAUL)
       (PRN (-LRB- -LRB-) (PP-ON (IN ON) (NP (DT THE) (NN LEFT)))
	    (-RRB- -RRB-))
       (PP-WITH (IN WITH)
		(NP (NP (NNP MRS. KLAUSER))
		    (PP-AT (IN AT)
			   (NP (NP (DT THE) (NN SUMMIT))
			       (PP-FOR (IN FOR) (NP (NN DINNER.))))))))))

   t); recons     (_+ (PP (IN _!) (NP _* (NNP _+1))) (PP _+2) _*1)

  (add-test
   '(FRAG (NP _+) (? (|,| |,|)) (PP _+1) _*)
   '(S1
     (FRAG (NP (NNP HALLE)) (|,| |,|)
      (PP-IN (IN IN)
       (NP
	(NP (DETP (NP (PRP SHE)) (POS |'S|)) (JJ NEW) (JJ BLUE)
	    (NNP BEATLES) (NN SHIRT))
	(|,| |,|) (CC AND)
	(NP (NP (DETP (NP (PRP SHE)) (POS |'S|)) (JJ BIG) (NN SISTER))
	    (|,| |,|) (NP (NNP TANYA)))))
      (\. \.)))

   t); (FRAG (NP (NP _+) ? (PP _+1)) _*)
  (add-test
   '(_+ (PP (IN _!) (NP (NP _* nn-nonhuman?) (|,| |,|) (CC and) (NP (NP _*1 nn-human?) _+1))) _*2)
   '(S1
     (FRAG (NP (NNP HALLE)) (|,| |,|)
      (PP-IN (IN IN)
       (NP
	(NP (DETP (NP (PRP SHE)) (POS |'S|)) (JJ NEW) (JJ BLUE)
	    (NNP BEATLES) (NN SHIRT))
	(|,| |,|) (CC AND)
	(NP (NP (DETP (NP (PRP SHE)) (POS |'S|)) (JJ BIG) (NN SISTER))
	    (|,| |,|) (NP (NNP TANYA)))))
      (\. \.)))

   t); (_+ (PP (IN _!) (NP _* nn-nonhuman?)) (|,| |,|) (CC and) (NP (NP _*1 nn-human?) _+1) _*2)

  (add-test
   '(_+ (NP _* (NNP _+1))  (? (|,| |,|)) (^@ (* (NP @)) ; any number of levels of NP embedding the conjunction
					     (NP (NP _*1 (NNP _+3)) (*1 ($ (NP _+4) (?1 (|,| |,|))))
						 (CC (! and or)) (NP _*2 (NNP _+5)) )))
   '(S1
     (NP (NP (NNP TANYA)) (|,| |,|)
	 (NP
	  (NP (NP (NNP JOSH)) (|,| |,|) (NP (NNP HALLE)) (CC AND)
	      (NP (NP (JJ LITTLE) (NNP BEN))
		  (PP-ON (IN ON)
			 (NP (NP (DT THE) (JJ FIRST) (NN DAY))
			     (PP-OF (IN OF) (NP (NN SCHOOL)))))))
	  (|,| |,|) (NP (CD 2009)))
	 (\. \.)))
   t); (_+ ((NP _* (NNP _+1)) ? (NP _*1 (NNP _+3)) *1 (CC (! and or)) (NP _*2 (NNP _+5))))

  (add-test
   '(_! _+ (^@ (* (_+1 @)) (! np-time?)) (!1 np-time?))
   '(S1
     (NP (NP (NNP TANYA)) (|,| |,|)
	 (NP
	  (NP (NP (NNP JOSH)) (|,| |,|) (NP (NNP HALLE)) (CC AND)
	      (NP (NP (JJ LITTLE) (NNP BEN))
		  (PP-ON (IN ON)
			 (NP (NP (DT THE) (JJ FIRST) (NN DAY))
			     (PP-OF (IN OF) (NP (NN SCHOOL)))))))
	  (|,| |,|) (NP (CD 2009)))
	 (\. \.)))
   t);
  (add-test
   '(!2 (!3 (^@ (* (_+1 @)) (! np-time?))) (!1 np-time?))
   '(S1
     (NP (NP (NNP TANYA)) (|,| |,|)
	 (NP
	  (NP (NP (NNP JOSH)) (|,| |,|) (NP (NNP HALLE)) (CC AND)
	      (NP (NP (JJ LITTLE) (NNP BEN))
		  (PP-ON (IN ON)
			 (NP (NP (DT THE) (JJ FIRST) (NN DAY))
			     (PP-OF (IN OF) (NP (NN SCHOOL)))))))
	  (|,| |,|) (NP (CD 2009)))
	 (\. \.)))
   t);

  (add-test
   '(_! _* (^@ (* (_+1 @)) (! np-time?)) (!1 np-time?))
   '(S1
     (NP (NP (NNP TANYA)) (|,| |,|)
	 (NP
	  (NP (NP (NNP JOSH)) (|,| |,|) (NP (NNP HALLE)) (CC AND)
	      (NP (NP (JJ LITTLE) (NNP BEN))
		  (PP-ON (IN ON)
			 (NP (NP (DT THE) (JJ FIRST) (NN DAY))
			     (PP-OF (IN OF) (NP (NN SCHOOL)))))))
	  (|,| |,|) (NP (CD 2009)))
	 (\. \.)))
   t);

  (add-test
   '(! (_!. (!1 (_!. _+))))
   '(S1
     (NP (NP (NNP TANYA)) (|,| |,|)
	 (NP
	  (NP (NP (NNP JOSH)) (|,| |,|) (NP (NNP HALLE)) (CC AND)
	      (NP (NP (JJ LITTLE) (NNP BEN))
		  (PP-ON (IN ON)
			 (NP (NP (DT THE) (JJ FIRST) (NN DAY))
			     (PP-OF (IN OF) (NP (NN SCHOOL)))))))
	  (|,| |,|) (NP (CD 2009)))
	 (\. \.)))
   t);
  

  (add-test
   '(_+ (ADJP _+1 (<> (PP _+2) (? (|,| |,|)))) _*)
   '(S1
     (NP (NP (NNP BEN)) (|,| |,|)
      (VP (VBG LOOKING)
       (NP (NP (NN DAPPER))
	   (PP-IN (IN IN)
		  (NP (DETP (NP (PRP HE)) (POS |'S|)) (JJ GREEN) (NN SHIRT)))))
      (|,| |,|)
      (PP-WITH (IN WITH)
       (NP (NP (NNP GRANDMA LILLIAN))
	   (PP-AT (IN AT) (NP (DT THE) (NN SUMMIT)))))
      (\. \.)))
   t);recons (_+ (ADJP _+1) <> _*)

) 

(defun load-tests-^@ ()
  (add-test '(^@ X) 'X t)
  (add-test '((^@ X)) '(X) t)
  (add-test '((^@ X)) 'X nil)

  (add-test '(^@ (_* X _*) (_* Y _*) (Z))
	    '(X (Y (Z))) t)  

  (add-test '(^@ (_* X _*) (_* Y _*) (Z))
	    '(A B X C (D E Y K (Z) L) N) t)

  (add-test '(^@ (_* X _* (_* Y _*) (Z)))
	    '(A B X C (D E Y K (Q Z T) L) N) nil)

  (add-test '(^@ (_* X _* (_* Y _*) (Z)))
	    '(A B X C (D E Y K Z L) N) nil)

  (add-test '(^@ (A (B C) D) (B C) C) 
	    '(A (B C) D) t)
  (add-test '(^@ (+ (X _! Y))) '(X Z Y) t)
  (add-test '(^@ (+ (X _! Y))) '(X (X Z Y) Y) t)
  (add-test '(^@ (+ (X _! Y))) '(X (X (X Z Y) Y) Y) t)
  (add-test '(^@ (! ((+ X) _!) (_! (+ Y))) (Z _* Z) (X))
	    '(X X X (Z K T ( X ) P S Z)) t)
  (add-test '(^@ (! ((+ X) _!) (_! (+ Y))) (Z _* Z) (X))
	    '((Z K T ( X ) P S Z) Y Y Y Y) t)
  (add-test '(^@ (+ (@ _*)) X)
	    '(X) t)
  (add-test '(^@ (+ (@ _*)) X)
	    'X nil)
  (add-test '(^@ (+ (@ _*)) X)
	    '((((X A) B) C) D) t)
  (add-test '(^@ (+ (X (_! @ _+) _*)) X)
	    '(X (Y (X (Z (X (T X Q) ) R) ) S) ) t)
  (add-test '(^@ (A (B _!)) A)
	    '(A (B A)) t)
  (add-test '(^@ (A (B _!)) A)
	    '(A (B C)) t)

  (add-test '(^@ (A @ D) (B @) C)
	    '(A (B C) D) t)
  (add-test '(^@ ) '() t)
  (add-test '(^@ _!) '() nil)
)

  



(defun load-tests-simple ()
  (load-tests-_!)
  (load-tests-!)
  (load-tests-_?)
  (load-tests-?)
  (load-tests-_*)
  (load-tests-_+)
  (load-tests-novars)
  (load-tests-*)
  (load-tests-+)
)

(defun load-tests-novars ()
  (add-test 'X 'X t)
  (add-test 'X '(X) nil)
  (add-test '(X) 'X nil)
  (add-test '(X) '(X) t)
  (add-test '() '() t)
  (add-test '() '(()) nil)
  (add-test '(()) '() nil)
  (add-test '(()) '(()) t)
  (add-test '(X) '(Y) nil)
  (add-test '(X Y) '(X Y) t)
  (add-test '(X Y) '(Y X) nil)
)  




(defun load-tests-sticky()
  (add-test '((!. X Y) !.) '(X X) t)
  (add-test '((!. X Y) !.) '(X Y) nil)
  (add-test '((!. X Y) !. (!.a a b c) !.a) '(X X a a) t)
  (add-test '((!. X Y) !. (!.a a b c) !.a) '(X Y a b) nil)
  (add-test '(_!. _!.) '(X X) t)
  (add-test '(_!. _!.) '(X Y) nil)
  (add-test '(_!. _!.2 (! _!. _!.2))  '(X Y X) t)
  (add-test '(_!. _!.2 (! _!. _!.2))  '(X Y Y) t)
  (add-test '(_!. _!.2 (! _!. _!.2))  '(X X X) t)
  (add-test '(_!. _!.2 (! _!. _!.2))  '(X Y Z) nil)
  (add-test '(_!. (! _!.)) '(X Y) nil)
  (add-test '(_!. (! _!.)) '(X X) t)
)


(defun load-tests-all ()
  (load-tests-nva)
  (load-tests-_!)
  (load-tests-!)
  (load-tests-_?)
  (load-tests-?)
  (load-tests-_*)
  (load-tests-*)
  (load-tests-_+)
  (load-tests-+)
  (load-tests-novars)
  (load-tests-sticky)
;  (load-tests-live)
  (load-tests-nested))


;;(apply-rule '(^@ (A @ _! B) (/ X Y)) '(A X X B))
;;(A Y X B)
;;CL-USER(251): (apply-rule '(^@ (A @ _! B) (/ X Y)) '(A C X B))
;(A C X B)








