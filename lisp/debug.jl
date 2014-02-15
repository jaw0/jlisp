
;;;; Copyright (c) 1994,1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: debug.jl,v 2.3 1998/06/18 20:12:10 jaw Exp $

;; debugging stuff

(defvar show-trace-on-error #f
  "should we get a stack trace on error?")

(defvar debug-on-error #f
  "should we enter the debugger on error?")

(defvar debug:auto-step #f
  "cause debugger to automatically step through the function being debugged
equivalent to repeatedly hitting the 's' key")

(defvar *error-object* ()
  "the object which caused the error")

(defvar *debugger:level* 0
  "used to keep track of nested debbugging")

(defun dbg:truncate (obj len)
  (let* ((str (strcpy ""))
	 (sp  (open:string str)))
    (write obj sp)
    (if (> (length str) len)
	(set! str (strcat (substr str 0 len) "...")))
    str))

(defun show-trace (&optional how)
  "(show-trace) display a backtrace of calls made
if given an argument, will be more verbose"
  (let ((bt (backtrace)))
    (while (nnullp bt)
      (let ((fn (cond
		((nullp (caar bt))
		 (strcat "#<signal #" (number->string (nth (car bt) 1)) ">"))
		((functionp (caar bt))
		 "#<anonymous function>")
		((macrop (caar bt))
		 "#<anonymous macro>")
		(#t
		 (caar bt)))))
	(if (nth (car bt) 3)
	    (display "* ")    ; debug flag is set
	  (display "  "))
	(if (boundp how)
	    (display (dbg:truncate (cons fn (nth (car bt) 2)) 70))
	  (display fn)))
      (newline)
	   (set! bt (cdr bt)))))

(defun debugger-called-from-offset (bt)
  (let* ((b bt) (nd 1) (ne 1))
    (while (and (nnullp bt) (not (eq (caar bt) 'debugger)))
      (set! bt (cdr bt))
      (set! nd (1+ nd)))
    (if (nullp bt)
	(set! nd #f))
    (set! bt b)
    (while (and (nnullp bt) (not (eq (caar bt) 'error-handler)))
      (set! bt (cdr bt))
      (set! ne (1+ ne)))
    (if (nullp bt)
	(set! ne #f))
    (cond
     ((and ne nd) (max nd ne))
     (ne ne)
     (nd nd)
     (#t #f))))


(defun debugger-called-from ()
  "(debugger-called-from) who called the debugger?"
  (let* ((bt (backtrace))
	 (n (debugger-called-from-offset bt))
	 (fr (if n (nth bt n) #f)))
    (if (consp fr)
	(cons (nth fr 0) (nth fr 2))
      (cons "???" ()))))

(defun debugger-called-from-from-flag ()
  "(debugger-called-from-flag) debug flag of the function that called the function that called the debugger"
  (let* ((bt (backtrace))
	 (n (debugger-called-from-offset bt))
	 (fr (if n (nth bt n) #f)))
    (if (consp fr)
	(nth fr 3)
	#f)))

(defun debugger-show-env (level)
  (let ((e (nth (current-enviornment) level)))
    (for-each (lambda (sc)
		(while (nnullp sc)
		  (display ?\t)
		  (write sc)
		  (print " -> " (dbg:truncate (symbol-value sc) 60) ?\n)
		  (set! sc (symbox-chain-next sc))))
	      e)))

(defmac dbg:set-at-level! (s n v)
  (let ((env (nth (current-envoirnment) (+ n 1))))
    (if (nnullp env)
	`(with-current-enviornment ,(list env)
				   (set! ,s ,v)))))

(defun error (&optional fnc obj huh &rest therest)
  "(error fnc obj descr) signal an error"
  (throw 'error (list fnc obj huh *current-file* .lineno))
  (set! *error-object* obj)
  (print "ERROR: " fnc ": in file \"" *current-file*   "\" near line " .lineno ": ``" obj "'' -- " huh)
  (newline)
  (if show-trace-on-error
      (show-trace #t))
  (if debug-on-error
      (debugger 'error ())
    (throw 'repl:error (list fnc obj huh *current-file* .lineno therest))))

(define error-handler error
  "called by the internal error handling code to handle errors")

;; when=0 => before call; when=1 => after call
(defun debugger (&optional when retval &rest therest)
  "(debugger) entry point into the debugging routines from the internals
the following commands are available at the [debugger] prompt:
    c   continue execution
    q   quit to the repl
    s   step
    n   next
    t   show backtrace
    T   show backtrace more verbosely
    r o set the return value for the function
    p e print the value of the expression
    e   show symbol bindings for what called the debugger
    E n show symbol bindings for n levels up past what called the debugger
    h   does not print this message"
  (let* ((debug-on-next-call #f)
	 (*debugger:level* (+ 1 *debugger:level*)))
    ;; (display "Entering debugger")(newline)
    (if (!= when 1)
	(print ">> " (dbg:truncate (debugger-called-from) 70))
      (print "<< " (dbg:truncate (debugger-called-from) 70))
      (print "\n\t=> " retval))
    (newline)
    (if debug:auto-step
	(set! debug-on-next-call (if (eq when 1)
				     (debugger-called-from-from-flag)
				   #t))
      (debug:repl))
    (set! .debug-on-next-call debug-on-next-call)
    retval))


(defun debug:repl ()
  ;; cons up an input map and pass control to the repl
  (let ((prompt (lambda ()
		  (print (makestr *debugger:level* ?[) (if (debugger-called-from-from-flag) ?* ?\s)
			    "debugger" (makestr *debugger:level* ?]) " "))))
    (if (catch 'debug:quit
	  (repl (list
		 (cons 's (lambda ()
			    (set! debug-on-next-call #t)
			    (repl:quit)))
		 (cons 'c (lambda ()
			    (let ((i 1))
			      ;; clear all pending debug on exit flags
			      (for-each (lambda (n)
					  (set-debug-back-frame i #f)
					  (set! i (+ i 1)))
					(backtrace)))
			    (repl:quit)))
		 (cons 'n (lambda () (repl:quit)))	
		 (cons 't (lambda () (show-trace)))
		 (cons 'T (lambda () (show-trace #t)))
		 (cons 'q (lambda ()
			    (set! .already-debugging #f)
			    (throw 'debug:quit)))
		 (cons 'r (lambda ()
			    (if (eq when 0)
				(display "r command is valid only while _exiting_ a function\n")
			      (set! retval (eval (read))))))
		 (cons 'h (lambda ()
			    (print (docstr 'debugger) ?\n)))
		 (cons 'e (lambda ()
			    (debugger-show-env 6)))    ; XXX - there is not currently a better  
		 (cons 'E (lambda ()                   ; way than hardcoding 6 ....
			    (debugger-show-env (+ 6 (read)))))
		 
		 ))
	  #f)
	;; caught debug:quit
	(throw 'repl:continue))))



;; define debugging sections and fields
;; from include/debugging.h
(define SECTION_MRI     0)
(define SECTION_LISP    1)
(define SECTION_INIT    2)
(define SECTION_SNAKE   3)
(define SECTION_SEGM    4)
(define SECTION_WINSYS  5)

(define DBG_VERBOSE     0)
(define DBG_ECHO        1)
(define DBG_RESULT      2)
(define DBG_SAVE        3)
(define DBG_THINK       4)
(define DBG_WORK        5)
(define DBG_SAVE_X      6)
(define DBG_STATS       7)

(if (not (definedp 'set-debug-flag!))
    (defun set-debug-flag! (sect fld)
      ()))

(if (not (definedp 'debug-flag))
    (defun debug-flag (sect fld)
      #f))


