 
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"
 
;;;; $Id: r4rs.jl,v 2.2 1997/05/30 00:28:06 jaw Exp $

;;;; R4RS scheme compatibility

;;;  I /think/ this covers all of the essential procedures
;;;  (nothing with continuations/...)
;;;  named let is 'nlet'

;; should these be documented?

(define quasiquote backquote)
(define unquote-splicing unquote-splice)

(define eq? eq)
(define eqv? eqv)
(define equal? equal)
(define pair? consp)
(define list? listp)
(define null? nullp)
(define zero? zerop)
(define boolean? booleanp)

(define begin progn)
(define else #t)
(define letrec let*)

(defun list-tail (x k)
  ;; R4RS p. 17
  (if (zero? k)
      x
    (list-tail (cdr x) (- k 1))))

(define list-ref nth)

(define symbol? symbolp)
(define number? numberp)
(define integer? intp)
(define char? charp)
(define string? stringp)

(define exact? intp)
(defun inexact? (a)
  (or (floatp a) (doublep a)))

(defun positive? (x)
  (> x 0))
(defun negitive? (x)
  (< x 0))

(defun odd? (x)
  (nzerop (& x 1)))
(defun even? (x)
  (zerop (& x 1)))

(defun ash (x y)
  (if (< x 0)
      (>> x (- y))
    (<< x y)))

(define ceiling ceil)

(define char=? =)
(define char<? <)
(define char>? >)
(define char<=? <=)
(define char>=? >=)

(defun char-ci=? (a b)
  (char=? (char-upcase a) (char-upcase b)))
(defun char-ci<? (a b)
  (char<? (char-upcase a) (char-upcase b)))
(defun char-ci>? (a b)
  (char>? (char-upcase a) (char-upcase b)))
(defun char-ci<=? (a b)
  (char<=? (char-upcase a) (char-upcase b)))
(defun char-ci>=? (a b)
  (char>=? (char-upcase a) (char-upcase b)))

(defun char-alphabetic? (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))))

(defun char-numeric? (c)
  (and (>= c ?0) (<= c ?9)))

(defun char-whitespace? (c)
  (and (memq c '(?\s ?\t ?\r ?\n ?\f)) #t))

(defun char-upper-case? (c)
  (and (>= c ?A) (<= c ?Z)))

(defun char-lower-case? (c)
  (and (>= c ?a) (<= c ?z)))

(define char->integer char->int)
(define integer->char int->char)


(define make-string makestr)
(define string-length length)
(define string-ref nth)
(define string-set! set-nth!)

(define string=? eqv)

(defun string-upcase! (s)
  (let ((i 0)
	(l (length s)))
    (while (!= i l)
      (set-nth! s i (char-upcase (nth s i)))
      (set! i (+ 1 i)))
    s))

(defun string-downcase! (s)
  (let ((i 0)
	(l (length s)))
    (while (!= i l)
      (set-nth! s i (char-downcase (nth s i)))
      (set! i (+ 1 i)))
    s))

(defun string-upcase (s)
  (string-upcase! (strcpy s)))

(defun string-downcase (s)
  (string-downcase! (strcpy s)))

(defun string-compare-helper (a b)
  (let* ((i 0)
	(al (length a))
	(bl (length b))
	(l (min al bl)))
    (while (and (char=? (nth a i) (nth b i)) (!= i l))
      (set! i (+ 1 i)))
    (cond
     ((and (= i l) (= al bl))
      0)
     ((= i l)
      (if (> al bl)
	  1
	-1))
     (#t
      (if (> (nth a i) (nth b i))
	  1
	-1)))))

(defun string<? (a b)
  (eq (string-compare-helper a b) -1))

(defun string>? (a b)
  (eq (string-compare-helper a b) 1))

(defun string-ci=? (a b)
  (string=? (string-upcase a) (string-upcase b)))

(defun string-ci<? (a b)
  (string<? (string-upcase a) (string-upcase b)))

(defun string-ci>? (a b)
  (string>? (string-upcase a) (string-upcase b)))

(defun substring (s start end)
  (substr s start (- end start)))

(define string-append strcat)
(define string-copy strcpy)

(defun string->list (a)
  (vector->list
   (let* ((l (length a))
	  (v (makevector l)))
     (while (<= 0 l)
       (set-nth! v l (nth a l))
       (set! l (- l 1)))
     v)))

(defun list->string (a)
  (let* ((v (list->vector a))
	 (l (length v))
	 (s (makestr l)))
    (while (<= 0 l)
      (set-nth! s l (nth v l))
      (set! l (- l 1)))
    s))


(define vector? vectorp)
(define vector-ref nth)
(define vector-length length)
(define vector-set! set-nth!)

(defun make-vector (l &optional f)
  (let ((v (makevector l)))
    (while (<= 0 l)
      (set-nth! v l f)
      (set! l (- l 1)))
    v))


(define procedure? procedurep)

(define input-port? inputportp)
(define output-port? outputportp)
(define open-input-file open:read)
(define open-output-file open:write)

(define close-input-port close)
(define close-output-port close)

(define read-char getc)
(define write-char putc)

(defun peek-char (&optional port)
  (ungetc (getc port) port))

(defun call-with-input-file (filename thunk)
  (let ((*stdin_port* (open:read filename)))
    (funcall thunk)))

(defun call-with-output-file (filename thunk)
  (let ((*stdout_port* (open:write filename)))
    (funcall thunk)))

(define with-input-from-file call-with-input-file)
(define with-output-to-file call-with-output-file)

(defun current-output-port ()
  *stdout_port*)

(defun current-input-port ()
  *stdin_port*)

(defun eof-object? (a)
  (eq a (eof-object)))

(defmac delay (x)
  "(delay thunk) delay the execution of thunk until it is forced\n[see also: force]"
  `(list #f (current-enviornment) ',x))

(defun force (x)
  "(force promise) force the execution of the promise\n[see also: delay]"
  (if (car x)
      (cdr x)
    (set-car! x #t)
    (with-current-enviornment (cons (car (current-enviornment)) (cadr x))
			      (set-cdr! x (eval (caddr x))))
    (cdr x)))

;; this could be fun...
;; this is now in C!
;;(defmac closure (args &rest body)
;;  "(closure args body...) create a closure"
;;  `(lambda ,args
;;     (with-current-enviornment (cons (car (current-enviornment))
;;				     ',(cdr (current-enviornment)))
;;			       ,@body)))


;;; named let
(defmac nlet (name varlist &rest body)
  "named let"
  (let* ((nvl (cons '&optional varlist))
	(clo (eval `(closure ,nvl ,@body))))
    (eval `(define ,name ,clo))
    `(funcall ,clo)))


;;; redefine builtin define
;;; handles (define (fnc a b . c) body...)
(defmac define (sym &rest body)
  (if (consp sym)
      (if (stringp (car body))
	  ;; doc string
	  `(#.define ,(car sym) ,(car body) (closure ,(cdr sym) ,@(cdr body)))
	;; no docstring
	`(#.define ,(car sym) (closure ,(cdr sym) ,@body)))
    `(#.define ,sym ,@body)))

