
;;;; Copyright (c) 1994,1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: cmdline.jl,v 2.2 1997/05/30 00:27:56 jaw Exp $

(defvar *cmdline:norc* #f
  "if set, do not load user rc file")

(define %parse-cmd-line%
  "(parse-cmd-line) parse the command line args"
  '(eval (let ((x ())
	       (andexit #f)
	       (userrc #t)
	       (sysrc #t)
	       (ipmode #f)
	       carg
	       foo
		;; separate cmd line args
	       (argvl (cdr (vector->list *argv*))))

	   (run-hooks 'parse-cmd-line-hooks)
	   (while (nnullp argvl)
	     (set! carg (car argvl))
	     (cond
	      ((eqv "-f" carg)		             ; load a file
	       (set! argvl (cdr argvl))
	       (set! andexit #t)
	       (set! userrc #f)
	       (set! x (append x (list `(load ,(car argvl))))))
	      ((and (definedp 'mritool)
		    (eqv "-fore" carg))		     ; run in foreground
	       (set! mri:background #f))
	      
	      ((eqv "-f" (substr carg 0 2))	     ; load a file
	       (set! andexit #t)
	       (set! userrc #f)
	       (set! x (append x (list `(load ,(substr carg 2 (- (length carg) 2)))))))
	      
	      ((and (definedp 'mritool)
		    (or
		     (eqv "-nw" carg)		     ; run in interactive, no-window mode
		     (eqv "-nowin" carg)))
	       (set! mri:windows #f)
	       (set! mri:winsys  #f)
	       (set! mri:background #f))
	      
	      ((and (definedp 'mritool)
		    (eqv "-X" (substr carg 0 2)))
	       (mri:parse-X carg))
						     ; set debugging level
	      ((eqv "-norc" carg)
	       (set! userrc #f))
	      
	      ((eqv "-nosysrc" carg)
	       (set! sysrc #f))

	      ((eqv "-ip" carg)
	       (set! ipmode #t))
	      
	      ((eqv "-e" carg)			     ; eval an expr
	       (set! argvl (cdr argvl))
	       (set! foo (read (open:string (car argvl))))
	       (set! x (append x (list foo))))

	     ((eqv "-E" carg)		; eval expr (by adding to before-main-loop-hooks
	      (set! argvl (cdr argvl))
;;	      (print argvl ?\n)
	      (add-hook 'before-main-loop-hooks
			(eval `(lambda ()
			      ,(read (open:string (car argvl))))))))
	     
	     (set! argvl (cdr argvl)))

;;	   (print x ?\n)
	   (if (definedp 'mritool)
	       (progn
		 (if ipmode (progn
			      (if userrc (set! x (cons '(catch 'error
							  (load (expand-filename "~/.iprc")))
						       x)))
			      (set! x (cons '(load "ip.jl")
					    x))))
		 (if userrc (set! x (cons '(catch 'error
					     (load (expand-filename "~/.mrirc")))
					  x)))
		 (if sysrc (set! x (cons '(catch 'error
					    (load "mrirc.jl"))
					 x)))))

	   (if (and (not *cmdline:norc*) userrc)
	       (set! x (cons '(catch 'error
				(load (expand-filename "~/.jlisprc")))
			     x)))

	   (set! x (cons 'progn x))
	   (if andexit
	       (set! x (append x (list '(quit)))))
	   ;; (print "x: " x ?\n)
	   x)))




