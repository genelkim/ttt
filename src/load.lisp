;; ASDF system definition for TTT
;; Original version of this file written for Epilog.
(require 'asdf)

;; avoids saving compiled files in special local cache.
(let f (and (setq f (fboundp (find-symbol "DISABLE-OUTPUT-TRANSLATIONS" 'asdf)))
	    (funcall f)))

;; from http://www.cliki.net/asdf
;;; If the fasl was stale, try to recompile and load (once). Since only SBCL
;;; has a separate condition for bogus fasls we retry on any old error
;;; on other lisps. Actually, Allegro has a similar condition, but it's
;;; unexported.  Works nicely for the ACL7 upgrade, though.
;;; CMUCL has an invalid-fasl condition as of 19c.
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #+cmu ext:invalid-fasl
     #-(or sbcl allegro lispworks cmu) error ()
     (asdf:perform (make-instance 'asdf:compile-op) c)
     (call-next-method))))

;; Adds the directory containing load.lisp to the "PATH" of asdf, so
;; we can load EPILOG2 from any directory.
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)

;; compiler settings
(proclaim '(optimize (speed 3) (safety 3) (space 0) (debug 3)))

;; Load TTT Choose between the following two lines depending on
;; whether you want the files compiled into FASLs or not:
(asdf:operate 'asdf:load-op 'ttt) ;; Compile and load as necessary
;(asdf:operate 'asdf:load-source-op 'ttt) ;; Doesn't compile


