(in-package :ttt)
(defun hide-ttt-ops (expr);
;; AUTHOR: Len Schubert
;~~~~~~~~~~~~~~~~~~~~~~~~
; Wrap [..] around symbols like !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in expr-patterns involving *, **, @, or ~), but can't 
; because of their special meanings in TTT. We're assuming that
; the expr's we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
  (let (str chars)
       (cond ((symbolp expr)
              (setq str (string expr))
              (setq chars (coerce str 'list))
              (cond ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]")))
                    (t expr)))
             ((atom expr) expr)
             (t (cons (hide-ttt-ops (car expr)) (hide-ttt-ops (cdr expr)))))
 )); end of hide-ttt-ops


(defun unhide-ttt-ops (expr);
;; AUTHOR: Len Schubert
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the square brackets that have been added around ttt symbols
; in expr by 'hide-ttt-ops':
;
 (let (str chars)
      (cond ((symbolp expr)
             (setq str (string expr))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) expr)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) expr)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~))
                             (intern str))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str))
                            (t expr)))))
            ((atom expr) expr)
            (t (cons (unhide-ttt-ops (car expr)) (unhide-ttt-ops (cdr expr)))))
 )); end of hide-ttt-ops


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
