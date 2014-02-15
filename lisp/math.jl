
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: math.jl,v 2.4 1998/06/22 20:15:52 jaw Exp jaw $
;; supplemental math routines/extensions

;;; we use read time eval (#.) to speed these up

(defvar $+$ +)
(defvar $-$ -)
(defvar $*$ *)
(defvar $/$ /)
(defvar abs fabs)

#- minimal
(defvar pi (* 4 (atan 1))
  "the ratio of the circumference to the diameter of a circle")


;;; the builtins only handle 2 args
;;; these will take any number of args

(defmac listop (op iac)
  `(lambda argl
     (,let ((acc ,iac))
       (,while (,nnullp argl)
	 (,set! acc (,op acc (,car argl)))
	 (,set! argl (,cdr argl)))
       acc)))

(define + (listop $+$ 0)
  "(+ n ...) add numbers")

(define * (listop $*$ 1)
  "(* n ...) multiply numbers")

(defun - argl
  "(- n ...) subtract numbers"
  (#.cond
   ((#.nullp argl) 0)
   ((#.nullp (#.cdr argl)) (#.$-$ 0 (#.car argl)))
   (#t
    (#.$-$ (#.car argl) (#.apply #.+ (#.cdr argl))))))

(defun / argl
  "(/ n ...) divide numbers"
  (#.cond
   ((#.nullp argl) 1)
   ((#.nullp (#.cdr argl)) (/ 1 (#.car argl)))
   (#t
    (#.$/$ (#.car argl) (#.apply #.* (#.cdr argl))))))

(defun extreme (op argl)
  "(extreme ...) internal"
  (#.if (#.nullp argl) ()
    (#.let ((a (#.car argl))
	  (l (#.cdr argl)))
      (#.while (nnullp l)
	(#.set! a 
	      (#.if (op a (#.car l))
		  a
		(#.car l)))
	(#.set! l (#.cdr l)))
      a)))

(defun max argl
  "(max n ...) return the maximum of a bunch of numbers"
  (#.extreme #.> argl))

(defun min argl
  "(min n ...) return the minimum of a bunch of numbers"
  (#.extreme #.< argl))

(defun truncate (x)
  "(truncate n) round towards 0"
  (#.if (#.< x 0)
      (#.$-$ (#.floor (#.$-$ 0 x)))
    (#.floor x)))

(defun round (x)
  "(round n) round towards nearest integer"
  (#.floor (#.$+$ x .5)))

(defun ->exact (x)
  "(->exact n) convert to integral form"
  (#.cond
   ((#.intp x) x)
   ((#.bignump x) x)
   (#t (#.->bignum x))))

(defun gcd (a b)
  "(gcd a b) find the greatest common denominator"
  (#.cond ((#.zerop a) b)
	(#t (gcd (#.% b a) a))))

(defun lcm (a b)
  "(lcm a b) find the least common multiple"
  (#.abs (#.$/$ (#.$*$ a b) (#.gcd a b))))

(defun quotient (a b)
  "(quotient a b) the integer division of a b"
  (#.truncate (#.$/$ a b)))


;; the % operator is not garunteed to be either modulo or remainder
;; so lets play ...

(defun remainder (a b)
  "(remainder a b) the remainder of integer division (has the sign of a)"
  (#.% a (#.abs b))) ;; is this garunteed to be right?
;; (- a (* b (quotient a b)))

;;; note: I don't think I spelled "garunteed" correctly
;;; but ispell was no help....

(defun modulo (a b)
  "(modulo a b) the modulous of a b (has sign of b)"
  (#.let ((c (#.% a b)))
	 (#.cond
	  ((#.or (#.and (#.< b 0) (#.> c 0))
		 (#.and (#.> b 0) (#.< c 0)))
	   (#.$+$ c b))
	  (#t c))))


