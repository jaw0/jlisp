
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: readmacros.jl,v 2.2 1997/05/30 00:28:09 jaw Exp $

;;;; code to assist reader
;;;; handles various #x and {} []

(defvar macro-dispatch-alist
  '(("#+" . mh-X+)
    ("#-" . mh-X-)
    ("#S" . mh-XS)
    ("#M" . mh-XM)
    ("#." . mh-Xdot)
    ("#!" . mh-Xbang)
    ("#(" . mh-Xopen)
    ("#|" . mh-Xbar)
    ("#<" . mh-X<)))

;;; entry from reader
(defun macro-handler (port c1 c2)
  (let* ((str (strcat c1 (if (charp c2) c2 "")))
	 (mda (assv str macro-dispatch-alist)))
    (if mda
	(funcall (cdr mda) port)
      (error "macro-handler" str "could not dispatch"))))

;;; test for feature
(defun mh-X+ (port)
  (let ((feat (read port))
	(expr (read port)))
    (if (featurep feat)
	expr
      (unspecified-object))))

;;; test for absence of feature
(defun mh-X- (port)
  (let ((feat (read port))
	(expr (read port)))
    (if (featurep feat)
	(unspecified-object)
      expr)))

;;; read time eval
(defun mh-Xdot (port)
  (eval (read port)))

;;; #! is a comment on the first line of a script
(defun mh-Xbang (port)
  (if (!= .lineno 1)
      (error "read" "#!" "unreadable syntax")
    (while (!= ?\n (getc port)))))

;;; #<...> is unreadable - signal an error
(defun mh-X< (port)
  (while (!= ?> (getc port)))
  (error "read" (unspecified-object) "unreadable syntax"))

;;; read a vector
(defun mh-Xopen (port)
  (let ((h ())
	(t ()))
    (catch 'read:parenclose
      (while #t
	(let ((v (read port)))
	  (if (nullp t)
	      (progn
		(set! t (cons v ()))
		(set! h t))
	    (set-cdr! t (cons v ()))
	    (set! t (cdr t))))))
    (list->vector h)))

;;; suck out a comment
(defun mh-Xbar (port)
  (let ((c ()))
    (while (!= c ?#)
      (while (!= c ?|)
	(set! c (getc port))
	(cond
	 ((= c ?#)
	  (set! c (getc port))
	  (if (= c ?|)
	      (progn
		(mh-Xbar port)
		(set! c (getc port)))))
	 ((= c ?\n)
	  (set! .lineno (1+ .lineno)))
	 ((eq c (eof-object))
	  ;; what should we do?
	  (throw 'eof))))
      (set! c (getc port))))
  (unspecified-object))
	 
	  

;;; #S(name elem value elem value ...) -> (method (name) 'set elem value ...)
(defun mh-XS (port)
  (let ((s (read port)))
    (apply method ((car s)) 'set (cdr s)))) ; ((car s)) - create an object

;;; #M(obj name) -> (method obj 'get name)
;;; #M(obj (fnc args...)) -> (method obj 'fnc args...)
(defun mh-XM (port)
  (let ((s (read port)))
    (if (nonsp (cadr s))
	(method (car s) 'get (cadr s))
      (apply method (car s) (kwote (cadr s)) (cddr s)))))



	
    