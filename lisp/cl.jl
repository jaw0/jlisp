
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: cl.jl,v 2.2 1997/05/30 00:28:07 jaw Exp $

(defun return (&optional val tag)
  (throw tag val))

(define setq set!)
(define aref nth)
(define nconc append!)
(define rplaca set-car!)
(define rplacd set-cdr!)

(define makebroadcaststream open:broadcast)
(define makeechostream open:echo)
(define makeconcatstream open:concat)
(define maketwowaystream open:twoway)
(define makesynonymstream open:synonym)

(defmac when (test &rest body)
  "(when test body...) evaluate body iff test is #t"
  `(if ,test
       (progn
	 ,@body)
     #f))

(defmac unless (test &rest body)
  "(unless test body...) evaluate body unless test is #t"
  `(if ,test
       #f
     ,@body))

(defmac dotimes (vtr &rest body)
  "(dotimes (var count [result]) body...) do body count times,
iterating var from 0 to count-1
the value of evaling result at the completion of iterating, is returned"
  (let ((var (car vtr))
	(times (eval (cadr vtr)))
	(result (nth vtr 2)))
    `(let ((,var 0))
       (catch (unspecified-object)
	 (while (< ,var ,times)
	   ,@body
	   (set! ,var (+ ,var 1)))
	 ,result))))

(defmac dolist (vlr &rest body)
  "(dolist (var list [result]) body...) do body for each item of the list
binding the item to var, returns the result of evaling result at completion"
    (let ((var (car vlr))
	  (lst (eval (cadr vlr)))
	  (result (nth vlr 2)))
    `(catch (unspecified-object)
       (for-each (lambda (,var)
		   ,@body)
		 ',lst)
       (let ((,var ()))
	 ,result))))


(defmac do (specs test &rest body)
  "(do (specs...) (endtest [result]) body...) general iterative loop
specs: (var [init [step]]) | var, vars are initialized to init
the body and steps are repeatedly evaluated until endtest is true
result is then evaluted and returned"
  (let ((ll (mapcar (lambda (x)		; the vars and init vals passed to let
		      (cond
		       ((consp x)
			(list (car x) (cadr x)))
		       (#t x)))
		    specs))
	(sl (let ((foo ()))             ; the step forms placed at the end of the while
	      (for-each (lambda (x)
			  (cond
			   ((and (consp x) (= 3 (length x)))
			    (set! foo (append (list (list set! (car x) (nth x 2))) foo)))
			   (#t)))
			specs)
	      foo))
	(endtest (car test))
	(result (cadr test)))
    `(let ,ll
       (catch (unspecified-object)
	 (while ,endtest
	   ,@body
	   ,@sl)
	 ,result))))

(defmac do* (specs test &rest body)
  "(do* (specs...) (endtest [result]) body...) general iterative loop
specs: (var [init [step]]) | var, vars are initialized to init
the body and steps are repeatedly evaluated until endtest is true
result is then evaluted and returned"
  (let ((ll (mapcar (lambda (x)		; the vars and init vals passed to let
		      (cond
		       ((consp x)
			(list (car x) (cadr x)))
		       (#t x)))
		    specs))
	(sl (let ((foo ()))             ; the step forms placed at the end of the while
	      (for-each (lambda (x)
			  (cond
			   ((and (consp x) (= 3 (length x)))
			    (set! foo (append (list (list set! (car x) (nth x 2))) foo)))
			   (#t)))
			specs)
	      foo))
	(endtest (car test))
	(result (cadr test)))
    `(let* ,ll
       (catch (unspecified-object)
	 (while ,endtest
	   ,@body
	   ,@sl)
	 ,result))))




