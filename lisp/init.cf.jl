
;;;; Copyright (c) 1994, 1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: init.cf.jl,v 2.2 1997/05/30 00:28:04 jaw Exp $

(set! .hash_table_size 7)    ; most often a small table will suffice
			     ; this was determined empirically
(set! .box_size 1024)        ; allocate this many cells at a time
(set! .gc_thresh (* .box_size 50))     ; keep threshhold high


(define load-path
  "list of directories in which to search for lisp files"
  (list
   "%LOCALLISP%"
   "%LISPDIR%"
   "%ETCDIR%"         ; start grasping at straws
   "%SRCDIR%/lisp"
   "%SRCDIR%/jlisp"
   "%SRCDIR%/lib" ))

(define load-extensions
  "list of extensions to try for lisp files"
  (list
   ".jl" 
   ".jlisp"))

(define load:echo    #t)  ; echo filenames as loaded
(define load:verbose #f)  ; echo each exp of the file as it is read

(define *builtin-load* load)
(define *current-file* "init")

;; basic error handler - will be overridden if debug is loaded
(define error-handler (lambda (fnc obj msg)
			(throw 'error (list fnc obj msg *current-file* .lineno))
			(for-each display (list "\nERROR: " fnc ": while loading file \""
						*current-file* "\" near line " .lineno ": ``"
						obj "'' -- " msg ?\n))
			(throw 'abort-load)
			(throw 'repl)))

;;; redefine load, the builtin is just a minimal stub
;;; this is a macro, as it must execute in the current env frame
(define load
  "(load file) load a file"
  (macro (file)
	  (let* ((efn (eval file))
		 (fp (cond
		      ((inputportp efn) efn)
		      ((stringp efn) (let ((foo ())
					   bar
					   baz
					   (l (append '(()) load-path))           ; try as given first
					   (e ()))
				       ;; search for the file
				       (while (and (nullp foo) (not (nullp l)))
					 (set! bar (if (stringp (car l))
						       (strcat (car l) "/" efn)
						     efn))
					 (set! e (append '(()) load-extensions))  ; as given first
					 (while (and (nullp foo) (not (nullp e)))
					   (set! baz (car e))
					   ;; saved in file so we can access it later if need be
					   (set! file (if (stringp baz)	
							  (strcat bar baz)
							bar))
					   (set! foo (open:read file))
					   (set! e (cdr e)))
					 (set! l (cdr l)))
				       foo))
		      (#t
		       (funcall error "load" efn "WTA: filename or port p")))))
	    (if (nullp fp)
		(funcall error "load" efn "Could not open"))
	    (if (or load:echo
		    (and (definedp 'mritool) (debug-flag 1 1)))  ;  lisp, echo
		(progn (display "Loading: ") (display file) (display ?\n)))
	    ;; the following will be executed in the calling env
	    `(unwind-protect
		 (progn
		   (set! .lineno 1)
		   (set! *current-file* ,file)
		   (catch 'abort-load
		     (catch 'eof
		       (if (or load:verbose
			       (and (definedp 'mritool) (debug-flag 1 0)))  ; lisp, verbose
			   (while #t
			     (eval (let ((foo (read ,fp)))
				     ;; (display ";; ")
				     (display foo) (display ?\n)
				     foo)))
			 (while #t
			   (eval (read ,fp)))))))

	       (close ,fp)
	       (set! *current-file* ,*current-file*)  ; restore filename
	       (set! .lineno ,.lineno)))))            ; restore lineno

;; load more
(load "lib.jl")        ; required
(load "readmacros.jl") ; required
(load "pred.jl")       ; required
(load "cmdline.jl")    ; required
(load "repl.jl")       ; required
(load "signal.jl")     ; required if we have signals
(load "debug.jl")      ; recommended - cannot be autoloaded
(load "autoload.jl")   ; recommended - cannot be autoloaded (obviously)
(load "math.jl")       ; recommended
(load "unistd.jl")     ; optional
(load "r4rs.jl")       ; optional - for some scheme compat
(load "slib.jl")       ; optional - for some scheme compat
(load "cl.jl")         ; optional - for some common lisp compat
; (load "objsys.jl")     ; optional - object system - autoloaded below
(load "expand.jl")     ; optional - expands ~ in filenames - autoloaded below
(load "format.jl")     ; optional - formatted output - autoloaded below

(autoload expand-filename "expand.jl"   "expand ~ in filenames")
(autoload roman           "roman.jl"    "return the roman numeral represenation of the number")
(autoload english         "roman.jl"    "return the english represenation of the number")
(autoload format          "format.jl"   "formatted output")
(autoload pp              "pp.jl"       "pretty print")
(autoload apropos         "all-syms.jl" "what function? by keyword")
(autoload time            "time.jl"     "how long does it take?")
(autoload abusage         "usage.jl"    "report on system use")
(autoload defclass        "objsys.jl"   "define a class")

(if (definedp 'mritool)
    (progn
      (load "mritool.jl")
      (define mri:background #t)    ; used internally by initliaztion in c code
      (define mri:windows    #t)    ; ditto
      (define mri:winsys     'x)))  ; what window system are we using


(defmac %init% ()
  "entry point from internals"
  `(progn
     ,%parse-cmd-line%
     ,%repl%
     (quit 0)))


