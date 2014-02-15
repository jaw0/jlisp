
;;;; Copyright (c) 1997 Jeff Weisberg
;;;; see the file "License"
 
;;;; $Id: pp.jl,v 2.3 1998/06/18 20:12:13 jaw Exp $

;;;; pretty printer
;;;; (pp object [outputfunction] [maxwidth])


;;; there should be no need to change this
(defvar *pp:quotes*
  "how to pretty print quoted things"
  '( (quote          . "'")
     (backquote      . "`")
     (unquote        . ",")
     (unquote-splice . ",@")))

;;; these may be over-ridden adding a property of pp:how on the symbol
;;; (set-props! foo '((pp:how . method) ...
(defvar *pp:how*
  "default methods for pretty printing things"
  '( (let      . pp:let)
     (let*     . pp:let)     ; symbol of function to use
     (nlet       3 0 0 2)    ; or we can also use pp:shape params
     (while    . pp:while)
     (define   . pp:define)
     (defun    . pp:define)
     (defmac   . pp:define)
     (defvar   . pp:define)
     (let      . pp:let)
     (unwind-protect . pp:unwind)
     (case     . pp:case)
     (catch    . pp:case)
     (if       . pp:if)
     (list     . pp:list)
     (atom     . pp:list)
     (mapcar   . pp:while)
     (for-each . pp:while)
     (map      . pp:while)
     (lambda   . pp:lambda)
     (closure  . pp:lambda)
     (macro    . pp:lambda)))

;;; decompose a function into a list
;;; so that (pp fnc) pretty-prints the function definition, not just "#<function>"
(defun pp:fnc->list (l)
  (if (ccodep l)
      (pp:atom->string l)   ; what? you'd expect us to print out the original C?
    (let ((type (cond
		 ((functionp l) 'lambda)
		 ((macrop l)    'macro)
		 ((closurep l)  'closure)
		 (#t            'unknown)))
	  (argl (get-param-list l))
	  (body (get-body-list l)))
      (cons2 type argl body))))
 
;;; covert atom into string representation
(defun pp:atom->string (a)
  (let* ((str (strcpy ""))
	 (fp  (open:string str)))
    (cond
     ((or (stringp a) (charp a))
      (write a fp))
     (#t
      (display a fp)))
    str))

;;; find the name of a builtin in function, so we can indent it correctly....
(defun pp:ccodename (c)
  (let* ((str (pp:atom->string c))
	 (len (length str))
	 (skip (length "#<builtin-function:")))
    (substr str skip (- len skip 1))))
  
;;; find total length of atoms
(defun pp:totallength (l)
  (apply + (mapcar (lambda (a)
		     (cond
		      ((stringp a) (length a))
		      ((consp a)   (+ (pp:getprop a 'length #f #t)            ; length of sublist
				      (length (pp:getprop a 'openparen "("))  ; any extra chars in front (such as quotes)
				      1))                                     ; terminating paren
		      (#t          (error "pp:totallength" a "unexpected"))))
		   l)))

;;; find total number of elems
(defun pp:totalitems (l)
  (apply + (mapcar (lambda (a)
		     (cond
		      ((stringp a) (length a))
		      ((consp a)   (pp:getprop a 'nitems 0))
		      (#t          (error "pp:totalitems" a "unexpected"))))
		   l)))

#|  known props:
      length    - total length of component atoms, sublists, and parens (not spaces)
      nitems    - total number of atoms
      openparen - initial paren chars including any quotes or such
      initsym   - initial symbol of list, so we can indent special things differently
|#
   
;;; add (prop . value) to the alist in l
(defun pp:addprop! (l prop value)
  (set-car! l (acons prop value (car l)))
  l)

;;; get the prop from l if it exists
;;; if it doesn't exist - default? error? huh?
(defun pp:getprop (l prop (defaultvalue #f) (errorifnotfound #f))
  (let ((e (assq prop (car l))))
    (if (consp e) (cdr e)
      (if errorifnotfound
	  (error "pp:getprop" prop "property not found")
	defaultvalue))))

(defun pp:quoted? (l)
  (and (assq (car l) *pp:quotes*) #t))

(defun pp:measure-quoted (obj)
  (let ((l (pp:measure (cadr obj)))
	(qc (cdr (assq (car obj) *pp:quotes*))))
    (if (nconsp l)
	(strcat qc l)  ; convert -> "'foo"
      (pp:addprop! l 'openparen (strcat qc "("))
      (pp:addprop! l 'initsym 'quote)
      l)))

;;; mapcar that handles dotted lists (a b c . d)
;;; converts above -> (a b c '. d)
(defun pp:mapcar-spec (fnc l)
  (let ((c l)
	(r ())
	(h ())
	(t ()))
    (while (consp c)
      (set! r (fnc (car c)))
      (if (nullp t)
	  (progn
	    (set! t (cons r ()))
	    (set! h t))
	(set-cdr! t (cons r ()))
	(set! t (cdr t)))
      (set! c (cdr c)))
    (if (nnullp c)
      ;; dotted list
      (set-cdr! t (cons (fnc '.) (cons (fnc c) ()))))
    h))

;;; our work function
;;; converts the list into list of strings,
;;; and keeps track of lengths and suchlikes about the list and sublists...
(defun pp:measure (obj)
  (cond
   
   ((vectorp obj)
    (let ((l (pp:measure (vector->list obj))))
      (pp:addprop! l 'openparen "#(")
      (pp:addprop! l 'initsym 'vector)
      l))
   
   ((atomp obj)
    (pp:atom->string obj))
   
   (#t
    (if (and (pp:quoted? obj)
	     (consp (cdr obj)))  ; leave 'unusual' quoted things alone
	(pp:measure-quoted obj)
      (let ((is (car obj))
	    (nl (cons () (pp:mapcar-spec pp:measure obj))))
	(pp:addprop! nl 'initsym (cond
				  ((symbolp is)   is)
				  ((consp   is)   'list)
				  ((ccodep   is)   (string->symbol (pp:ccodename is)))
				  (#t             'atom)))
	(pp:addprop! nl 'openparen "(")
	(pp:addprop! nl 'length (pp:totallength (cdr nl)))
	(pp:addprop! nl 'nitems (pp:totalitems (cdr nl)))
	nl)))))

;;; newline and indent if necc.
(defun pp:freshindent (outfnc)
  (if (> *pp:pos* *pp:left*)
      (progn
	(outfnc "\n")
	(set! *pp:pos* 0)))
  (if (< *pp:pos* *pp:left*)
      (progn
	(outfnc (makestr (- *pp:left* *pp:pos*) ?\s))
	(set! *pp:pos* *pp:left*))))

;;; output horizontally
(defun pp:horiz (l outfnc)
  (cond
   ((atomp l)
    (outfnc l)
    (set! *pp:pos* (+ *pp:pos* (length l))))
   (#t
    (let ((props (car l))
	  (items (cdr l)))
      (pp:horiz (pp:getprop l 'openparen "(") outfnc)

      (pp:horiz (car items) outfnc)
      (for-each (lambda (i)
		  (pp:horiz " " outfnc)
		  (pp:horiz i outfnc))
		(cdr items))
      (pp:horiz ")" outfnc))))
  #t)

;;; output in some arbitrary shape
;;; ontop number of things on top
;;; innies number of things indented by innyindent
;;; the rest indednted by outyindent
;;;
;;; (ontop ontop ontop
;;;     inny
;;;     inny
;;;   outy
;;;   outy)
(defun pp:shape (l outfnc ontop innies innyindent outyindent)
  (let ((nitm (1- (length l)))
	(props (car l))
	(items (cdr l))
	(oleft *pp:left*)
	(innymax (+ ontop innies))
	(n 1))
    (pp:horiz (pp:getprop l 'openparen "(") outfnc)
    (set! *pp:left* (1+ *pp:left*))
    (pp:outdriver (car items) outfnc)  ; or pp:horiz
    (for-each (lambda (i)
		(cond
		 ((< n ontop)
		  (pp:horiz " " outfnc)
		  (set! *pp:left* *pp:pos*))  ; in case ontops wrap over
		 ((< n innymax)
		  (set! *pp:left* (+ oleft innyindent))
		  (pp:freshindent outfnc))
		 (#t
		  (set! *pp:left* (+ oleft outyindent))
		  (pp:freshindent outfnc)))
		(set! n (1+ n))
		(pp:outdriver i outfnc))
	      (cdr items))
    (pp:horiz ")" outfnc)
    (set! *pp:left* oleft))
  #t)


;;; define a bunch'a useful formats:

;; (a
;;  b
;;  c)
(defun pp:list (l outfnc)
  (pp:shape l outfnc 1 0 0 1))

;; (lambda args
;;   body
;;   body)
(defun pp:lambda (l outfnc)
  (pp:shape l outfnc 2 0 0 2))

;; (if cond
;;     then
;;   else
;;   else)
(defun pp:if (l outfnc)
  (pp:shape l outfnc 2 1 4 2))

;; (case obj
;;   stuff
;;   stuff)
(defun pp:case (l outfnc)
  (pp:shape l outfnc 2 0 0 2))

;; (unwind-protect
;;     a
;;   b
;;   c)
(defun pp:unwind (l outfnc)
  (pp:shape l outfnc 1 1 4 2))

;; (let (vars)
;;   body)
(defun pp:let (l outfnc)
  (pp:shape l outfnc 2 0 0 2))

;; ;;; ((props
;; ;;;   ...))
;; (define a b
;;   c)
(defun pp:define (l outfnc)
;; uncomment to pretty-print symbol properties along with defin
;;  (let* ((s (caddr l))
;;	   (f (if (stringp s) (string->symbol s)))
;;	   (p (if (symbolp f)
;;		  (get-props f)
;;		#f)))
;;    (if p
;;	  (progn
;;	    (outfnc "\n;;; ")
;;	    (pp p (closure (s)
;;			   (if (eqv s "\n")
;;			       (outfnc "\n;;; ")
;;			     (outfnc s)))
;;		*pp:maxwidth*)
;;	    (outfnc "\n")))
;;    (pp:shape l outfnc 3 0 0 2)))
  (pp:shape l outfnc 3 0 0 2))

;; (while cond
;;   body)
(defun pp:while (l outfnc)
  (pp:shape l outfnc 2 0 0 2))

;;; figure out how to output stuff
(defun pp:outdriver (l outfnc)
  (let ((len (if (consp l)
		 (+ (pp:getprop l 'length) (pp:getprop l 'nitems))
	       (length l)))
	(spaceleft (- *pp:maxwidth* *pp:pos*)))

    (cond
     ((atomp l)         (pp:horiz l outfnc))
     ((> spaceleft len) (pp:horiz l outfnc)) ; if there is room - go horiz
     (#t
      ;; dispatch out special forms...
      (let* ((is (pp:getprop l 'initsym 'list))
	     (syshow (assq is *pp:how*))
	     (userhow (if (definedp is)                  ; if there is a prop on the symbol - use it
			  (assq 'pp:how (get-props is))  ; otherwise use from alist up top, or default 
			#f))
	     (how (cdr (or userhow syshow '(foo . pp:list)))))
	(if (or (symbolp how) (procedurep how))
	    (funcall how l outfnc)
	  (apply pp:shape l outfnc how)))))))

;;; interface from the outside world
(defun pp (l (outfnc display) (maxwidth 80))
  "(pp form [outfunction maxwidth]) pretty-print"
  (let ((*pp:left* 0)
	(*pp:pos* 0)
	(*pp:maxwidth* maxwidth))
    (pp:outdriver (pp:measure (if (procedurep l) (pp:fnc->list l) l)) outfnc)))


;;; for testing...
(defun pp:file (fn)
  (let ((fp (open:read fn)))
    (catch 'eof
      (while #t
	(pp (read fp))
	(display "\n")))))


