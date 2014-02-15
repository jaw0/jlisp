
;;;; Copyright (c) 1994, 1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: repl.jl,v 2.3 1998/06/18 20:12:14 jaw Exp $

(defun repl ((inputmap ()))
  "(repl [input-map]) read eval print loop "
  ;; catch eof and exit
  (set! .lineno 1)
  (set! *current-file* "user input")
  (let* (repl:line
	 repl:val
	 repl:cmd
	 repl:result
	 (repl:quit (lambda () (throw 'repl:quit)))
	 (repl:inmap (append inputmap (list
				       ;; and several commands to the inputmap
				       (cons 'quit  repl:quit)
				       (cons 'exit  repl:quit)
				       (cons 'bye   repl:quit)
				       (cons 'help  help)
				       (cons 'q     (lambda () (print "use 'quit' to exit\n")))
				       (cons '!     (lambda () (let ((n (read)))
								 (print ";; " (nth history n) ?\n)
								 (if (numberp n)
								     (display (eval (nth repl:history n)))
								   (error "repl" n "invalid history recall"))
								 (display ?\n))))
				       )))
	 (repl:history (makevector 1024))
	 (repl:histlen (length repl:history))
	 (repl:histrcl (lambda (n)
			 (nth repl:history (- n repl:histlow)))))
    (catch 'repl:quit			; quit repl
      (while #t
	(catch 'repl:continue		; continue, thrown to get back to a repl
	  (if (catch 'repl:error	; thrown on error to get back to a repl
		;; display a prompt...
		(prompt)
		;; cannot use let here
		(set! repl:line .lineno)
		(set! repl:val (read))
		(set! repl:cmd (assq repl:val repl:inmap))
		(if repl:cmd
		    ((cdr repl:cmd))
		  (set! repl:result (eval repl:val))
		  (if (boundp repl:result)
		      (print repl:result ?\n)))
		(set-nth! repl:history (% repl:line repl:histlen) repl:val)
		#f)
	      (display "repl: Error detected\n")))))))

(defun prompt ()
  "(prompt) displays the repl prompt"
  (print "jl(" .lineno ") > "))

(define %repl% '(progn (ungetc ?\n)  ; to make line numbers look right...
		       (catch 'eof (repl))))


