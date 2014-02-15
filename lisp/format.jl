 
;;;; Copyright (c) 1994,1997 Jeff Weisberg
;;;; see the file "License"
 
;;;; $Id: format.jl,v 2.2 1997/05/30 00:27:59 jaw Exp $
 
;;; an inplementation of format for jlisp
;;; from spec in CLtL2

;;; ~e ~g ~$ ~<~> ~:^ are not yet implemented
;;; ~f is not quite up to spec
;;; ~T is just flat out wrong

;;; the docstring on format:spec details it all...

(defvar *format:debug* #f)

(defun format (port fspec &rest args)
  "(format how format-spec args...) print out the args according to the format-spec
   how: a port to write to,
        a string to append to
        #t to standard out
        #f return a new string
  [see the documentation for format:spec for details]"

  (let* ((string (strcpy ""))
	(format-port (cond
		      ((outputportp port) port)
		      ((stringp port)
		       (let ((p (open:string port)))
			 (seek p (length port))
			 p))
		      ((truep   port) *stdout_port*)
		      ((falsep  port) (open:string string))
		      ((ndefinedp port) *stdout_port*)
		      (#t (error "format" port "port ought be a string, port, #t, or #f")))))
    ;; do the formatting
    (catch 'format:error
      (catch 'format:hat
	(format:do-specstr format-port fspec 0 0 args)))
    
    ;; return desired result
    (cond
     ((outputportp port) #t)
     ((stringp port) port)
     ((truep   port) #t)
     ((falsep  port) string))))

;; some helper functions
(defun format:debug argl
  (if *format:debug*
      (apply print argl)))
    
(defun format:string-upcase (s)
  (let ((i 0)
        (l (length s)))
    (while (!= i l)
      (set-nth! s i (char-upcase (nth s i)))
      (set! i (+ 1 i)))
    s))
(defun format:string-downcase (s)
  (let ((i 0)
        (l (length s)))
    (while (!= i l)
      (set-nth! s i (char-downcase (nth s i)))
      (set! i (+ 1 i)))
    s))
(defun format:string-capitalize (s)
  (let ((i 0)
	(w #t)
        (l (length s)))
    (while (!= i l)
      (if w
	  (set-nth! s i (char-upcase (nth s i)))
	(set-nth! s i (char-downcase (nth s i))))
      (if (format:char-is-white (nth s i))
	  (set! w #t)
	(set! w #f))
      (set! i (+ 1 i)))
    s))
(defun format:string-capitalize1 (s)
  (let ((i 0)
	(w #t)
	(ww #t)
        (l (length s)))
    (while (!= i l)
      (if (and w ww)
	  (progn
	    (set-nth! s i (char-upcase (nth s i)))
	    (set! ww #f))
	(set-nth! s i (char-downcase (nth s i))))
      (if (format:char-is-white (nth s i))
	  (set! w #t)
	(set! w #f))
      (set! i (+ 1 i)))
    s))
  
(defun format:char-is-white (c)
  (and (memq c '(?\s ?\t ?\r ?\n ?\f)) #t))

(defun format:->string (fnc obj)
  (let* ((str (strcpy ""))
	(fp  (open:string str)))
    (fnc obj fp)
    str))

(defun format:error argl
  (apply print "FORMAT ERROR: " argl)
  (newline)
  (print ?\t ?" fspec ?" ?\n)
  (print ?\t (makestr spec-index ?\s) ?\s ?^ ?\n)
  (throw 'error)
  (throw 'format:error))

(defun format:garg (dfl (cvt #f))
  ;; get next param
  (let ((foo (if (nnullp params)
		 (let ((foo (car params)))
		   (set! params (cdr params))
		   foo)
	       ())))
    (if (nnullp foo) (if cvt (cvt foo) foo) dfl)))

(defun format:format-string (obj fnc mincol colinc minpad padchar ovchar gravity format-port)
  (let* ((str (format:->string fnc obj))
	 (leng (+ (length str) minpad))
	 (width (if (nnullp mincol) (->int (abs mincol)) 0))
	 (ci (if (nullp colinc) 1 colinc))
	 (padamt (* (/ (+ (- width leng) ci -1) ci) ci))
	 (maxcolp (and (nnullp mincol) (< mincol 0))))

    (format:debug "format:format-string: " leng ?- width ?- ci ?- padamt ?- maxcolp ?\n)
    ;; add minpad padchars
    (set! str (case gravity
		((r) (strcat (makestr minpad padchar) str))
		((l) (strcat str (makestr minpad padchar)))))
    (cond
     ((= leng width)
      ;; is proper size -- nop
      )
     ((> leng width)
      ;; is long -- chop or leave ?
      (if maxcolp
	  (progn
	    (set! str (substr str 0 ( - width 1)))
	    (set! str (case gravity
			    ((r) (strcat ovchar str))
			    ((l) (strcat str ovchar))
			    (#t  (strcat str ovchar)))))))

     ((< leng width)
      ;; is short -- pad?
      (set! str (case gravity
		  ((r) (strcat (makestr padamt padchar) str))
		  ((l) (strcat str (makestr padamt padchar)))
		  ((c) (let* ((rpl (->int (/ (- width leng) 2)))
			      (lpl (- width rpl)))
			 (strcat (makestr lpl padchar) str (makestr rpl padchar))))))))
    (display str format-port)))

(defun format:format-string-hp (obj fnc format-port)
;  (print params ?\n)
  (let* ((gravity (if got-atsign 'r 'l))
	 (mincol  (format:garg ()))
	 (colinc  (format:garg ()))
	 (minpad  (format:garg ()))
	 (padchar (format:garg ?\s int->char))
	 (ovchar  (format:garg (case gravity
			  ((r) ?<)
			  ((l) ?>)
			  (#t  ?*)) int->char)))
    (format:format-string obj fnc mincol colinc minpad padchar ovchar gravity format-port)))

;; not even close to fully compliant...
(defun format:do-float (obj how format-port)
  (let* ((w  (format:garg #f))
	 (d  (format:garg #f))
	 (k  (format:garg #f))
	 (ov (format:garg #f  int->char))
	 (pc (format:garg ?\s int->char))
	 (n  (if k (* obj (exp10 k)) obj))
	 (*output-float* (strcat (if got-atsign "+%" "%")
				 ;; (if w (number->string w) "") ;; we want to do the padding ourself
				 (if d "." "") (if d (number->string d) "")
				 "f"))
	 (*output-double* (strcat (if got-atsign "+%" "%")
				 ;; (if w (number->string w) "")
				 (if d "." "") (if d (number->string d) "")
				 "lf"))
	 (fstr (number->string n))
	 (fsl  (length fstr))
	 (fastr (if (and w ov (> fsl w))
		    (makestr w ov)
		  (if (and w (< fsl w))
		      (strcat (makestr (- w fsl) pc) fstr)
		    fstr))))
    (display fastr format-port)))

(defun format:do-number (n base format-port)
  (let* ((mincol     (format:garg ()))
	 (padchar    (format:garg ?\s int->char))
	 (commachar  (format:garg ?, int->char))
	 (commawidth (format:garg 3))
	 ; ovchar ??? - where did I find this?
	 (ovchar (format:garg ?* int->char))
	 (sign  (if (< n 0) ?- (if got-atsign ?+ #f)))
	 (pnstr (number->string (->exact (if (< n 0) (- n) n)) base))
	 (nstr  (if (not got-colon) pnstr (format:commaify pnstr commachar commawidth)))
	 (str (if sign (strcat sign nstr) nstr)))
    (format:format-string str display mincol () () padchar ovchar 'r format-port)))

(defun format:commaify (pnstr commachar commawidth)
  (let* ((sl (length pnstr))
	 (ss (if (zerop (% sl commawidth)) commawidth (% sl commawidth)))
	 (str (substr pnstr 0 ss)))
    (while (< ss sl)
      (strappend! str commachar (substr pnstr ss commawidth))
      (set! ss (+ ss commawidth)))
    str))

(defun format:ctoi (c)
  (- (char->int c) (char->int ?0)))

(defun format:display-ntimes (c n)
  (display (makestr (if (nullp n) 1 n) c) format-port))

(defun format:parse ()
  ;; does not catch most illegal specs
  (let ((n ())
	how
	(sgn #f))
    (while (case (nth fspec spec-index)
		 ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
		  ;; handle number
		  (set! n (if (nullp n)
			      (format:ctoi (nth fspec spec-index))
			    (+ (* 10 n) (format:ctoi (nth fspec spec-index))))))
		 ((?-) (set! sgn #t))
		 ((?')
		  (++ spec-index)
		  (set! n (char->int (nth fspec spec-index))))
		 ((?,)
		  (set! params (append! params (list (if sgn (* n -1) n))))
		  (set! sgn #f)
		  (set! n ()))
		 ((?:) (set! got-colon  #t))
		 ((?@) (set! got-atsign #t))
		 ((?#)
		  ;; param from number of args left
		  (set! n (- (length args) (+ index arg-index))))
		 ((?V ?v)
		  ;; param from arg list
		  (set! n (nth args (+ index arg-index)))
		  (set! arg-index (+ 1 arg-index)))
		 (#t #f))
      (++ spec-index))
    (if (nnullp n)
	(set! params (append! params (list (if sgn (* n -1) n)))))
    (set! how (nth fspec spec-index))
    (case how
	  ;; handle perl-isms
	  ((?< ?> ?.)
	   (set! n 1)
	   (while (or (eq ?< how) (eq ?> how) (eq ?. how))
	     (++ n)
	     (++ spec-index)
	     (set! how (nth fspec spec-index)))
	   (set! params (cons n params))
	   (-- spec-index)
	   (set! how (nth fspec spec-index)))
	  (#t ()))
    how))
	   

;; yank out delimited, possibly nested, substring
(defun format:traipse (str index ich ech (tch #f) (stripp #t))
  (let ((string (strcpy ""))
	(nf 1)
	(gt #f)
	(go #f)
	(no 0)
	(sl (length str))
	(pos index))
    (while (and (< pos sl) (nzerop nf))
      (let ((cc (nth str pos)))
	(cond
	 ((and gt tch (= cc tch) (= nf 1))   ; ~; at proper level
	  (set! nf (- nf 1)))
	 ((and gt (= cc ech))                ; got end seq - ~}
	  (set! nf (- nf 1)))
	 ((and gt (= cc ich))                ; got start seq - ~{
	  (set! nf (+ nf 1)))
	 ((= cc ?~)                          ; got ~
	  (set! go #f)
	  (set! gt #t))
	 ((and gt (memq cc '(?: ?@ ?# ?v ?V ?, ?' ?0 ?1
				?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
	  (set! go #t)
	  (set! no (+ no 1))
	  (if (= cc ?')
	      ;; skip char
	      (progn
		(set no (+ no 1))
		(set! pos (+ pos 1)))))
	 (#t
	  (set! no 0)
	  (set! go #f)
	  (set! gt #f)))
	(set! pos (+ pos 1))
	(strappend! string cc)))
    (if (nzerop nf)
	;; not found
	#f
      ;; strip trailing sequence
      (if stripp
	  (substr string 0 (- (length string) (+ no 2)))
	string))))


;; go through the spec string
;; and call format:do-spec to handle each spec
;; returns number of args used 
(defun format:do-specstr (format-port fspec spindex index args)
  (format:debug "format:do-specstr: " ?" fspec ?" ?- spindex ?- index ?- args ?\n)
  (let ((spec-index spindex)
	(spec-length (length fspec))
	(arg-index 0))

    ;; iterate over spec string
    (while (!= spec-index spec-length)
      (cond
       ((eq ?~ (nth fspec spec-index))
	;; do formatting
	(++ spec-index)
	(let* ((params ())
	       (got-atsign #f)
	       (got-colon  #f)
	       (how (format:parse)))
	  (set! arg-index (+ arg-index (format:do-spec
					format-port how got-atsign got-colon
					(+ arg-index index) args)))))
       (#t
	;; not a format spec - just copy char from fspec 
	(putc (nth fspec spec-index) format-port)))
      (++ spec-index))
    arg-index))

;; do the appropriate action for this spec
;; returns the number of args used
(defun format:do-spec (format-port how got-atsign got-colon index args)
  (let ((curr-arg (nth args index)))
    (case how
      ((??)    ; indirection
       (if got-atsign
	   (+ 1 (format:do-specstr format-port curr-arg 0 (+ 1 index) args))
	 (catch 'format:hat
	   (apply format format-port curr-arg (nth args (+ index 1))))
	 2))
      ((?~)
       (format:display-ntimes ?~ (if (nnullp params) (car params) 1))
       0)
      ((?*)    ; skip args
       (- (* (if got-colon -1 1)
	     (if (nullp params)
		 (if got-atsign 0 1)
	       (car params)))
	  (if got-atsign index 0)))
      ((?T ?t)
       ;; XXX - this is wrong - we need to keep track of the output column...
       (format:display-ntimes ?\t (if (nnullp params) (car params) 1))
       0)
      ((?|)
       (format:display-ntimes ?\f (if (nnullp params) (car params) 1))
       0)
      ((?% ?&)
       (format:display-ntimes ?\n (if (nnullp params) (car params) 1))
       0)
      ((?_)
       (format:display-ntimes ?\s (if (nnullp params) (car params) 1))
       0)
      ((?\n)
       (if got-atsign (display ?\n format-port))
       (if (not got-colon)
	   (progn
	     ;; remove ws from fspec
	     (while (format:char-is-white (nth fspec spec-index))
	       (++ spec-index))
	     (-- spec-index)))
       0)
      ((?p ?P) ; pluralize
       (if (= 1 (if (not got-colon)
		    curr-arg
		  (nth args (- index 1))))
	   (if got-atsign (display "y" format-port))
	 (display (if got-atsign "ies" "s") format-port))
       (if got-colon 0 1))
      
      ((?c ?C)
       (let* ((cc (if (nullp params) curr-arg (int->char (car params))))
	     (ci (char->int cc))
	     (cv #("NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS"  "HT"  "NL"
		   "VT"  "NP"  "CR"  "SO"  "SI"  "DLE" "DC1" "DC2" "DC3" "DC4" "NAK"
		   "SYN" "ETB" "CAN" "EM"  "SUB" "ESC" "FS"  "GS"  "RS"  "US"  "SP" )))
	 (if got-colon
	     (display (if (<= ci (length cv)) (nth cv ci)
			(if (>= ci 127) (strcat "#\\\\0" (number->string ci 8)) cc))
		      format-port)
	   (if got-atsign
	       (if (or (<= ci (length cv)) (>= ci 127))
		   (display (strcat "#\\\\0" (number->string ci 8)) format-port)
		 (write cc format-port))
	     (display cc format-port)))
	 (if (nullp params) 1 0)))
      
      ((?<)
       (format:format-string curr-arg display (- (car params)) () () ?\s ?> 'l format-port)
       1)
      ((?>)
       (format:format-string curr-arg display (- (car params)) () () ?\s ?< 'r format-port)
       1)
      ((?.)
       (format:format-string curr-arg display (- (car params)) () () ?\s ?> 'c format-port)
       1)

      ((?f ?F ?g ?G ?e ?E ?$)
       (format:do-float curr-arg how format-port)
       1)
      ((?d ?D)
       (format:do-number curr-arg 10 format-port)
       1)
      ((?o ?O)
       (format:do-number curr-arg  8 format-port)
       1)
      ((?x ?X)
       (format:do-number curr-arg 16 format-port)
       1)
      ((?b ?B)
       (format:do-number curr-arg  2 format-port)
       1)
      ((?r ?R)
       (if (nullp params)
	   (cond
	    ((and got-atsign got-colon)
	     ;; old style roman
	     (format:format-string-hp (roman curr-arg #t) display format-port))
	    (got-atsign
	     ;; roman
	     (format:format-string-hp (roman curr-arg) display format-port))
	    (got-colon
	     (format:format-string-hp (english curr-arg #t) display format-port))
	    (#t
	     (format:format-string-hp (english curr-arg) display format-port)))
	 (format:do-number curr-arg (let ((radx (car params)))
				      (set! params (cdr params))
				      radx)
			   format-port))
       1)
      ((?a ?A)
       (format:format-string-hp curr-arg display format-port)
       1)
      ((?s ?S)
       (format:format-string-hp curr-arg write format-port)
       1)
      ((?q ?Q)
       ;; print out version, copyright, ...
       (display .version format-port)
       (display ?\n format-port)
       0)
      ((#\()   ; the other way confuses emacs too much...
       ;; case conv
       (let* ((string (strcpy ""))
	      (port (open:string string))
	      (indi (catch 'format:paren
		      (format:do-specstr port fspec (+ spec-index 1) index args))))
	 ;; mutilate case
	 (display
	  (if got-atsign
	      (if got-colon
		  (format:string-upcase string)
		(format:string-capitalize1 string))
	    (if got-colon
		(format:string-capitalize string)
	      (format:string-downcase string)))
		format-port)
	 (set! spec-index (car indi))
	 (- (cdr indi) index)))
      ((#\))
       ;; end of case conv
       (throw 'format:paren (cons spec-index index))
       (format:error "unbalanced ~( ~)")
       0)

      ((#\[)
       (let* ((spec (or (format:traipse fspec (+ spec-index 1) ?[ ?] #f #f)
			(format:error "~[ has no closing ~]")))
	      (got-subcolon #f)
	      (pcv (let ((i 0)
			 (sl (length spec))
			 (il ()))

		     ;; parse out the sub-pieces
		     (while (!= i sl)
		       (let ((s (format:traipse spec i ?[ ?] #\;)))
			 (if (= (nth s 0) #\;)
			     (progn
			       (set! s (substr s 1 (- (length s) 1)))
			       (set! i (+ i (length s) 3))
			       (set! got-subcolon #t))
			   (set! i (+ i (length s) 2)))
			 (set! il (cons s il))))
		     (list->vector (reverse il)))))
	 (format:debug "[] pv: " pcv ?\n)
	 (set! spec-index (+ spec-index (length spec)))

	 (cond
	  ;; I do not believe :@ is legal...
	  (got-colon
	   (+ (format:do-specstr format-port (nth pcv (if curr-arg 1 0)) 0 index args) 1))

	  (got-atsign
	   (if curr-arg
	       (format:do-specstr format-port (nth pcv 0) 0 (- index 0) args)
	     1))
	  (#t
	   (let* ((pl (length pcv))
		 (pa (if (nnullp params)
			 (car params)
		       #f))
		 (np (if (and (intp pa) (< pa pl))
			 pa
		       (if got-subcolon
			   (- pl 1)
		       #f)))
		 (na (if (and (intp curr-arg) (< curr-arg pl))
			 curr-arg
		       (if got-subcolon
			   (- pl 1)
			 #f)))
		 (au (if (or (and pa np) na)
			 (progn
			   (format:debug "[] using: " (nth pcv (or np na)) ?\n)
			   (format:do-specstr format-port (nth pcv (or np na)) 0 (+ index (if pa 0 1)) args))
		       (if pa 0 1))))
	     (+ au (if pa 0 1)))))))
      ((#\])
       (format:error "~] has no matching ~[")
       0)
      
      ((#\{)
       (let* ((spec (or (format:traipse fspec (+ spec-index 1) ?{ ?})
			(format:error "unbalanced ~{ ~}")))
	      (got-spec (nzerop (length spec)))
	      (use-arglist got-atsign)
	      (use-sublist got-colon)
	      (ltou (if got-spec
			(if use-arglist
			    (list-tail args index)
			  curr-arg)
		      ;; otherwise spec is taken from args
		      (if use-arglist
			    (list-tail args (+ index 1))
			(nth args (+ index 1)))))
	     (n 0)
	     (iter 0)
	     (esi 0)
	     (max (if (nullp params) #f (car params)))
	     (l (length ltou)))
	 (format:debug "{} starting: " spec ?- ltou ?- l ?\n)
	 (while (and (< (if use-sublist iter n) l) (or (falsep max) (nzerop max)))
	   (format:debug "{} loop: " n ?- l ?- ltou ?- use-sublist ?\n)
	   (let ((indi (catch 'format:hat
			 (format:do-specstr format-port (if got-spec spec curr-arg) 0
					    (if use-sublist 0 n)
					    (if use-sublist
						(nth ltou iter)
					      ltou)))))
	     (if max (set! max (- max 1)))
	     (set! iter (+ iter 1))
	     (set! n (+ n (if (consp indi)
			      (car indi)
			    indi)))))
	 (set! spec-index (+ spec-index (length spec) 2))
	 (+ (if use-arglist (if use-sublist iter n) 1) (if got-spec 0 1))))
      
      ((#\})
       (format:error "unbalanced ~{ ~}")
       0)

      ((?^)
       (let* ((a1 (format:garg #f))
	      (a2 (format:garg #f))
	      (a3 (format:garg #f))
	      (outp (cond
		     ((and a1 a2 a3)
		      (and (<= a1 a2) (<= a2 a3)))
		     ((and a1 a2)
		      (= a1 a2))
		     (a1
		      (zerop a1))
		     (got-colon
		      ;; need to check number of sublists remaining
		      )
		     (#t
		      (zerop (- (length args) index))))))
	 (if outp
	     (if got-colon
		 (throw 'format:hat-colon (list index arg-index))
	       (throw 'format:hat (list index arg-index)))))
       (format:debug "nothing caught hat\n")
       0)
      
      (#t
       ;; error...
       (format:error "unrecognized format spec: " ?~ how)
       1))))


(define format:spec 'format:spec ;; defined only for the docstring...
  "(format how fspec args...)

   how: a port to write to,
        a string to append to
        #t to standard out
        #f return a new string

  fspec: a string with embedded fomat directives of the form:
         ~[N]{,N}[v][#][@][:]X
         ie. a ~ optionally followed by a comma separated sequence of numbers
             a v will read the arg from the arglist
             a # will interpolate to the number of remaining args
             optional colon and at-sign modifiers
             and a single letter directive

         the directive can be one of:

            A  print as would display
            S  print as would write
                 @ will pad on left
                 params are: mincols (maxcols if <0), colinc, minpad, padchar, overflowchar

            ~ print a ~ [N ~s]
            T print a tab [N tabs]
            % print a newline [N newlines]
            & print a newline [N newlines]
            | print a formfeed [N formfeeds]
            _ print a space [N spaces]
            \n ignore the newline and any following whitespace
                 : newline is ignored, whitespace is left
                 @ newline is printed, following whitespce is ignored
            * next arg is ignored, with param, next N args are ignored
                 : back up in arg list, with param, backup N args
                 @ goto 0th arg, or with a param, Nth arg 
            ? indirect - 2 args are a format string and list of args
                 @ - 1 arg - is a format string, use args from current arglist

            P pluralize
                 @ use y/ies
                 : use previous arg

            D a number in base 10
            O a number in base 8
            X a number in base 16
            B a number in base 2
            R a number in specified radix (ie. ~7R)
                @ print leading sign
                : print with commas
                params are: mincol, padchar, commachar, commawidth, overflowchar

             without a radix specifier:
                  in english \"four\"
               :  in english, ordinal \"fourth\"
               @  in roman \"IV\"
               :@ in old roman \"IIII\"


            C a character
                @ as with write
                : spell out control chars

            ( downcase until ~)    - hello world
                @  capitalize the first word  - Hello world
                :  capitalize  - Hello World
                :@ uppercase  - HELLO WORLD

            { iteration spec until ~}
                @  use remaining args
                :  arg is list of sublists
                :@ use remaining args, which are sublists

            [ conditional spec, separated with ~; ending with ~]
                choose item specified by arg  ~:; is default item
                with a param, chhose with it instead of arg
                @ choose if not #f
                : use first item if #f, second otherwise

            ^ abort ? {} or <> if no args remain,
                or if a param is given, it is 0
                or if 2 params are given, they are equal
                or if 3 params are given, the 1st is <= 2nd <= 3rd
                : terminate an entire :{ or :@{, not just this iteration
                

     also supported are some PERL-isms:
          ~<<<<<<<   left justified
          ~>>>>>>>   right justified
          ~.......   centered (| in perl)

            ")



