
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"
 
;;;; $Id: time.jl,v 2.2 1997/05/30 00:28:06 jaw Exp $



(defmac time body
  "(time body...) how long does this take?"
  (let* ((start-wall (current-time))
	(start-use  (getrusage))
	(start-sys  (nth start-use 1))
	(start-usr  (nth start-use 0))
	end-wall end-use end-sys end-usr
	result)

    (set! result (eval (cons progn body)))

    (set! end-wall (current-time))
    (set! end-use  (getrusage))
    (set! end-sys  (nth end-use 1))
    (set! end-usr  (nth end-use 0))

    (time:report "User" start-usr  end-usr)
    (time:report "Sys"  start-sys  end-sys)
    (time:report "Wall" start-wall end-wall)

    result))


(defun time:report (label start end)

  (let* ((d_usec (- (nth end 2) (nth start 2)))
	 (d_lsec (- (nth end 1) (nth start 1)))
	 (d_hsec (- (car end)   (car start)))
	 sec)

    (if (< d_usec 0)
	(progn
	  (set! d_usec (+ d_usec 1000000))
	  (set! d_lsec (- d_lsec 1))))
    (if (< d_lsec 0)
	(progn
	  (set! d_lsec (+ d_lsec 65536))
	  (set! d_hsec (- d_hsec 1))))
    (if (< d_hsec 0)
	(print label " took negative time!???\n"))

    (set! sec (+ d_lsec (* d_hsec 65536)))

    (cond
     ((zerop sec)
      (format *stderr_port* "~<<<<<<<< ~8D microsec~:p~%" label d_usec))
     ((< sec 60)
      (format *stderr_port* "~<<<<<<<< ~8D second~:p~%" label sec))
     ((< sec 3600)
      (format *stderr_port* "~<<<<<<<< ~-5D:~-2,48D (min:sec)~%" label (/ sec 60) (% sec 60)))
     (#t
      (format *stderr_port* "~<<<<<<<< ~-5D:~-2,48D (hr:min)~%" label (/ sec 3600)
	      (/ (% sec 3600) 60))))))


      
      
    



