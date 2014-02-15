

(defun notify-author (subject body)
  "drop a note to the author"

  (let ((mail (open:pipe/out (strcat "mail -s \"" subject
				  "\" jaw@op.net"))))
    (display body mail)
    (close mail)))

(defun y-or-n-p (prompt)
  (let (ans)
    (while (and (not (eq ans ?y))
		(not (eq ans ?n)))
      (if (not (eq ans ?\n))
	  (display prompt))
      (set! ans (getc)))
    (eq ans ?y)))


(if (y-or-n-p "Shall I notify Jeff for you [yn]? ")
    (progn
      (notify-author "New jlisp user"
		     (format #f "~A~%~A~%"
		      .version
		      *features*))
      (close (open:write ".notify"))))



