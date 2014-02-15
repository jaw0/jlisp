
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: roman.jl,v 2.2 1997/05/30 00:28:04 jaw Exp $

(defun roman (num &optional oldway)
  "(roman number) return the roman numeral represenation of the number"
  (let ((rc #("/M"    "/D"   "/C"   "/L"  "/X"  "/V" ?M   ?D  ?C  ?L ?X ?V ?I))
	(rv #(1000000 500000 100000 50000 10000 5000 1000 500 100 50 10  5  1  0 0))
	(index 0)
	(str (strcpy "")))

    (while (> num 0)
      (cond
       ((>= num (nth rv index))
	(strappend! str (nth rc index))
	(set! num (- num (nth rv index))))
       ((<= num (nth rv (+ index 1)))
	(set! index (+ index 1)))
       ((and (not (truep oldway))
	     (zerop (& index 1))
	     (>= num (- (nth rv index) (nth rv (+ index 2)))))
	(strappend! str (nth rc (+ index 2)))
	(strappend! str (nth rc index))
	(set! num (+ (- num (nth rv index))
		     (nth rv (+ index 2)))))
       ((and (not (truep oldway))
	     (nzerop (& index 1))
	     (>= num (- (nth rv index) (nth rv (+ index 1)))))
	(strappend! str (nth rc (+ index 1)))
	(strappend! str (nth rc index))
	(set! num (+ (- num (nth rv index))
		     (nth rv (+ index 1)))))
       (#t
	(set! index (+ index 1)))))
    str))

(defun english:small (num &optional ordinal)
  ;; num < 1000
  (let ((units  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
		  "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
		  "seventeen" "eighteen" "nineteen"))
	(ounits #("zeroth" "first" "second" "third" "fourth" "fifth"
		  "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth"))
	(tens  #("" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))
	(otens #("" "" "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth" "seventieth"
		 "eightieth" "ninetieth"))
	n
	(str (strcpy "")))

    (set! num (->int num))
    (if (>= num 100)
	(progn 
	  (strappend! str ?\s (nth units (/ num 100)) " hundred")
	  (set! num (% num 100))
	  (if (and (truep ordinal) (zerop num))
	      (strappend! str "th"))))

    (if (>= num 20)
	(progn
	  (set! n (% num 10))
	  (if (and (truep ordinal) (zerop n))
	      (strappend! str ?\s (nth otens (/ num 10)))
	    (strappend! str ?\s (nth tens (/ num 10))))
	  (set! num n)))

    (if (> num 0)
	(if (truep ordinal)
	    (if (< num (length ounits))
		(strappend! str ?\s (nth ounits num))
	      (strappend! str ?\s (nth units num) "th"))
	  (strappend! str ?\s (nth units num))))

    (set! str (substr str 1 (- (length str) 1)))
    str))

(defun english (num &optional ordinal)
  "(english number [ordinal]) return the english representation of the number (ie 12 => twelve)
if ordinal is #t will yeild it as an ordinal number (ie 12 => twelveth"
  
  (let* ((illions  #("" "thousand" "million" "billion" "trillion" "quadrillion"
		     "quintillion" "sextillion" "septillion" "octillion" "nonillion"
		     "decillion" "undecillion" "duodecillion" "tredecillion"
		     "quattuordecillion" "quindecillion" "sexdecillion" "septdecillion"
		     "octodecillion" "novemdecillion" "vigintillion"))
	 ;;; (Latin K)-illion = 10**3(k+1)
	 (i (- (length illions) 1))
	 (b 1)
	 n
	 (minusp #f)
	 (str (strcpy "")))

    (if (< num 0)
	(progn
	  (set! minusp #t)
	  (set! num (- num))))
    (if (zerop num)
	(strappend! str
		    (if (truep ordinal) ", zeroth" ", zero")))

    (dotimes (x i)
      (set! b (* b 1000)))
      ;; (set! b (->bignum (* b 1000))))
    
    (while (nzerop num)
      ;; (set! b (exp10 (* i 3)))    ; is only exact upto ~ (exp10 22)
      (set! n (->int (/ num b)))
      (set! num (->exact (% num b)))
      (set! b (/ b 1000))
      
      ; (print "i: " i " b: " b " n: " n " num: " num ?\n)
      (cond
       ((> n 1000)
	(strappend! str ", very many " (nth illions i)))
       ((> n 0)
	(strappend! str ?, ?\s (english:small n (and (truep ordinal) (zerop num))) ?\s (nth illions i)))
       (#t))

      (set! i (- i 1)))

    (set! str (substr str 2 (- (length str) 3)))
    (if minusp (strcat "minus " str) str)))



