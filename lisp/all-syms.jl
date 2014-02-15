
;;;; Copyright (c) 1994, 1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: all-syms.jl,v 2.2 1997/05/30 00:28:05 jaw Exp $

(set! debug-on-error #t)
;;; gets all intered symbols (symboxes)
(defun all-syms ()
  (let* ((ce (current-enviornment))
	 (sym-list ()))
    (for-each (lambda (ev)
		;; traverse the env vector
		(for-each
		 (lambda (sc)
		   ;; follow the symbox chain
		   (while (nnullp sc)
		     (set! sym-list (cons sc sym-list))
		     (set! sc (symbox-chain-next sc))))
		 ev))
	      ce)
    sym-list))

;;; get all docstrings
(defun all-docs (sl)
  (let ((dl ()))
    (for-each (lambda (sym)
		(let ((ds (assq 'docstring (get-props sym))))
		  (if ds
		      (set! dl (acons sym (cdr ds) dl)))))
	      sl)
    dl))

(defvar *all-docs* #f)

(defun apropos (huh)
  "(appropos keyword) find a function dealing with keyword"
  (let ((al ())
	(dl (or *all-docs* (set! *all-docs* (all-docs (all-syms))))))
    (for-each (lambda (sd)
		(if (and (consp sd) (or (match huh (symbol->string (car sd)))
					(match huh (cdr sd))))
		    ;(set! al (cons sd al))))
		    (format #t "~S ~A\n\n" (car sd) (cdr sd))))
	      dl)
    al))
(define appropos apropos)      ; too hard to spell

(defun mk-help-txt (filename)
  "(mk-help-txt filename) create help file"
  (let ((dl (or *all-docs* (set! *all-docs* (all-docs (all-syms)))))
	(f  (open:write filename))
	i s)
    (dolist (l dl)
      (if (consp l)
	  (progn
	    (set! s (strcpy (cdr l)))
	    (while (set! i (strindex s ?\n))
	      (set-nth! s i ?\001))	; replace \n -> ^A
	    (format f "~20S  ~A~%" (car l) s))))))

