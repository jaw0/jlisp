
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: bind.jl,v 2.2 1997/05/30 00:28:05 jaw Exp $

;;; just like the postscript function of the same name
;;; goes thru' the function and looks up all calls it makes

;; does not yet handle case, cond


(defun bind:body ()
  (while (nnullp body)
    (set! form (car body))
    (set! body (cdr body))
    (set! newval (if (consp form)
		     (bind-form form deep)
		   form))
    (set! newbody (append! newbody (cons newval ())))))

(defun bind-form (body &optional deep)
  (if (not (consp body))
      body
    (let* ((form (car body))
	   ;; bind car of list
	   
	   (newval (if (consp form)
		       (bind-form form deep)
		     (if (and (symbolp form) (definedp form))
			 (symbol-value form)
		       form)))
	   
	   (newbody (cons 
		     (if (and (eq deep #t)
			      (or (functionp newval) (macrop newval)))
			 ;; recurse into new function ?
			 (bind newval #t)
		       newval) ())))
      (set! body (cdr body))
      
      (cond
       ;; things to leave alone (for now...)
       ((memq newval '(quote backquote))
	(set! newbody (cons newval body)))

       ((memq newval '(case))
	(let ((item (car body))
	      (cbody (cdr body)))
	  (set! newbody (cons newval
			      (mapcar (lambda (ci)
					(if (nconsp ci)
					    ci
					  (cons (car ci)
						(bind-form (cdr ci) deep))))
				      cbody)))))
       
       ;; bind let, let*
       ((memq newval '(let let*))
	(let ((vars (car body))
	      (lbody (cdr body))
	      (newvars ()))
	  ;; do let vars
	  (while (nnullp vars)
	    (set! form (car vars))
	    (set! vars (cdr vars))
	    (if (nconsp form)
		(set! newvars (append! newvars (cons form ())))
	      (set! newvars
		    (append! newvars
			     (cons (list
				    (car form)
				    (car (bind-form (cdr form) deep)))
				   ())))))
	  (set! newbody (append! newbody (cons newvars ())))
	  ;; do let body
	  (set! body lbody)
	  (bind:body)))
       
       ;; bind lambda, macro
       ((memq newval '(lambda macro))
	(let ((arglist (car body))
	      (lbody (cdr body)))
	  (set! newbody (append! newbody (cons arglist ())))
	  (set! body lbody)
	  (bind:body)))
       
       (#t
	(bind:body)))
      newbody)))


(defvar *bind:beenthere* ())
(defun bind (fnc &optional deep)
  "(bind func [deep]) bind all function calls in func"

  ;; currently bad things will happen if try to go deep on something recursive...

  (let* ((arglist (get-param-list fnc))
	 (body (get-body-list fnc))
	 (newbody (bind-form body deep))
	 (newfnc (cons (if (functionp fnc)
			   lambda
			 macro)
		       (cons arglist newbody))))
    
    ;; (print "Result: " newfnc ?\n)
    (eval newfnc)))




