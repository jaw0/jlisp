
;; $Id: slib.jl,v 2.2 1997/05/30 00:28:03 jaw Exp $
;; some code borrowed from slib / jaffer



;; from slib/jaffer
(defun remove (i l)
  "(remove item list) remove item (equal) from list"
  (cond
   ((nullp l) l)
   ((equal i (car l))
    (remove i (cdr l)))
   (#t (cons (car l) (remove i (cdr l))))))

(defun remq (i l)
  "(remq item list) remove item (eq) from list"
  (cond
   ((nullp l) l)
   ((eq i (car l))
    (remq i (cdr l)))
   (#t (cons (car l) (remq i (cdr l))))))

(defun remv (i l)
  "(remv item list) remove item (eqv) from list"
  (cond
   ((nullp l) l)
   ((eqv i (car l))
    (remv i (cdr l)))
   (#t (cons (car l) (remv i (cdr l))))))


(defun delete! (i l)
  "(delete! item list) destructively delete item (equal) from list"
  (cond
   ((nullp l) ())
   ((equal i (car l)) (delete! i (cdr l)))
   (#t (set-cdr!  l (delete! i (cdr l)))
       l)))

(defun delq! (i l)
  "(delq! item list) destructively delete item (eq) from list"
  (cond
   ((nullp l) ())
   ((eq i (car l)) (delq! i (cdr l)))
   (#t (set-cdr!  l (delq! i (cdr l)))
       l)))

(defun delv! (i l)
  "(delv! item list) destructively delete item (eqv) from list"
  (cond
   ((nullp l) ())
   ((eqv i (car l)) (delv! i (cdr l)))
   (#t (set-cdr!  l (delv! i (cdr l)))
       l)))

;;; "sort.scm" Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(defun sort:merge! (a b less?)
  (defun loop (r a b)
    (if (less? (car b) (car a))
	(begin
	 (set-cdr! r b)
	 (if (null? (cdr b))
	      (set-cdr! b a)
	   (loop b a (cdr b)) ))
      ;; (car a) <= (car b)
      (begin
       (set-cdr! r a)
       (if (null? (cdr a))
	   (set-cdr! a b)
	 (loop a (cdr a) b)) )) )
  (cond
   ((null? a) b)
   ((null? b) a)
   ((less? (car b) (car a))
    (if (null? (cdr b))
	(set-cdr! b a)
      (loop b a (cdr b)))
    b)
   (#t ; (car a) <= (car b)
    (if (null? (cdr a))
	(set-cdr! a b)
      (loop a (cdr a) b))
    a)))


;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(defun sort:sort! (seq less?)
  (defun step (n)
    (cond
     ((> n 2)
      (let* ((j (quotient n 2))
	     (a (step j))
	     (k (- n j))
	     (b (step k)))
	(sort:merge! a b less?)))
     ((= n 2)
      (let ((x (car seq))
	    (y (cadr seq))
	    (p seq))
	(set! seq (cddr seq))
	(if (less? y x) (begin
			 (set-car! p y)
			 (set-car! (cdr p) x)))
	(set-cdr! (cdr p) '())
	p))
     ((= n 1)
      (let ((p seq))
	(set! seq (cdr seq))
	(set-cdr! p '())
	p))
     (#t
      '()) ))
  (if (vector? seq)
      (let ((n (vector-length seq))
	    (vec seq))
	(set! seq (vector->list seq))
	(do ((p (step n) (cdr p))
	     (i 0 (+ i 1)))
	    ((null? p) vec)
	  (vector-set! vec i (car p)) ))
    ;; otherwise, assume it is a list
    (step (length seq)) ))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(defun sort:sort (seq less?)
  (if (vector? seq)
      (list->vector (sort:sort! (vector->list seq) less?))
    (sort:sort! (append seq '()) less?)))

(define sort sort:sort)
(define sort! sort:sort!)

