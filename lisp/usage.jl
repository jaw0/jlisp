
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"
 
;;;; $Id: usage.jl,v 2.2 1997/05/30 00:28:07 jaw Exp $


(autoload time:report "time")

(defvar *page-size* 4096)		; on sparc-boxen

(defun abusage:report-k (label amt)
  (abusage:report label (/ amt 1024) "K"))

(defun abusage:report (label amt units)
  (format *stderr_port* "~<<<<<<<< ~8D ~A~%"
	  label amt units))

(defun abusage (&optional (more #f))
  "report on current system (ab)use"
  (let* ((u (getrusage))
	 (user (nth u 0))
	 (sys  (nth u 1))
	 (pgm1 (- *page-size* 1))
	 (pgmk (~ pgm1))
	 (text (& (+ (nth u 17) pgm1) pgmk))  ; includes 2 extra pages
	 (data (& (+ (nth u 16) pgm1) pgmk))
	 (stck (& (+ (nth u 18) pgm1) pgmk))) ; approx, will likely be under estimate
	 
    (time:report "User CPU" '(0 0 0) user)
    (time:report "Sys  CPU" '(0 0 0) sys)

    (if more (abusage:report-k "Text" text))
    (if more (abusage:report-k "Data" data))
    (if more (abusage:report-k "Stack" stck))
    (abusage:report-k "Size" (+ text data stck))
    (if more (abusage:report-k "Max RSS" (* (nth u 2) *page-size*)))

    (if more (abusage:report "Read" (nth u 9) "Blocks"))
    (if more (abusage:report "Writes" (nth u 10) "Blocks"))
    (if more (abusage:report "Maj.Fault" (nth u 7) ""))
    (if more (abusage:report "IVCntxtSw" (nth u 15) ""))))


