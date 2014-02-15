
;;;; Copyright (c) 1994 Jeff Weisberg
;;;; see the file "License"

;;;; $Id: expand.jl,v 2.2 1997/05/30 00:27:57 jaw Exp $
;; filename expansion

(defun expand-filename (flnm)
  "(expand-filename filename) expand ~ in filename"
  (let (ful)
    (cond
     ((or (equal "~/" (substr flnm 0 2))
	  (equal "~"  flnm))
      (set! ful (nth (getpwuid (getuid)) 7))
      (set! ful (strcat ful (substr flnm 1 (length flnm)))))

     ((equal ?~ (nth flnm 0))
      ;; it matches ~name | ~name/...
      (let ((user (substr flnm 1 (length flnm)))
	    i)
	;; find user name
	(set! i (or
		 (let ((foo (strindex flnm ?/)))
		   (if foo (- foo 1) foo))
		 (length flnm)))
	(set! user (substr user 0 i))
	(let ((pwent (getpwnam user)))
	  (if (vectorp pwent)
	      (set! ful (nth (getpwnam user) 7))
	    (error "expand-filename" user "No such user")))
	(set! ful (strcat ful
			  (if (!= i (length flnm))
			      (substr flnm (+ i 1) (length flnm))
			    "")))))
     (#t
      (set! ful flnm)))
    ful))

