
;;;; Copyright (c) 1994,1997 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: pred.jl,v 2.2 1997/05/30 00:28:01 jaw Exp $

(define nconsp atomp
  "(nconsp obj) An atom?")

(defun listp (x)
  "(listp obj) A list?"
  (#.or (#.consp x) (#.nullp x)))

(defun numberp (x)
  "(numberp obj) A number?"
  (#.or (#.intp x)
      (#.bignump x)
      (#.floatp x)
      (#.doublep x)))

(defun integerp (x)
  "(integerp obj) is obj an integral type"
  (#.or (#.intp x) (#.bignump x)))

(defun booleanp (x)
  "(booleanp obj) A boolean?"
  (#.or (#.eq x #t) (#.eq x #f)))

(defun nboundp (x)
  "(nboundp x) Not been bound a value?"
  (#.not (#.boundp x)))

(defun ndefinedp (x)
  "(ndefinedp x) Not been defined?"
  (#.not (#.definedp x)))

(defun nzerop (x)
  "(nzerop x) Not zero?"
  (#.not (#.zerop x)))

(defun nnullp (x)
  "(nnullp x) Not ()?"
  (#.not (#.nullp x)))

(defun nfalsep (x)
  "(nfalsep x) Not #f?"
  x)

(defun truep (x)
  "truep x) is x #t?"
  (#.eq x #t))

(define bcdp ccodep)
(define dptr consp)


