

(defvar LOGGER_PORT 13254)
(defvar LOGGER_FILE "/tmp/log")
(defvar LOGGER_HOST #(128 151 160 255)) ; UR-EE broadcast

(defun logger ()
  "collect inet datagrams and append them to a log file"
  (let* ((sock (ipc:open AF_INET SOCK_DGRAM))
	 (s1 (ipc:setopt sock SO_REUSEADDR))
	 (s2 (ipc:setopt sock SO_DEBUG))
	 (s3 (ipc:bind sock INADDR_ANY LOGGER_PORT)))
    
    (if (intp s3) (error "logger" s3 "bind failed"))

    (while #t
      (let ((got (ipc:recv sock))
	    (file (open:append LOGGER_FILE))
	    (time (current-time-string)))

	(format file "~A ~A: ~A~%" 
		(substr time 0 (- (length time) 1))
		(format-ip-addr (cdr got))
		(car got))
	
	(close file)))))

(defun log (msg)
  "log message to (logger) by sending an inet datagram"
  (let* ((sock (ipc:open AF_INET SOCK_DGRAM)))

    (ipc:setopt sock SO_DEBUG)
    (ipc:send sock msg LOGGER_HOST LOGGER_PORT)
    (close sock)))

(defun inet-server (port thunk)
  "create an internet stream based server
the given thunk will be eval'ed with std in/out attached
to the remote connection"
  (let* ((sock (ipc:open AF_INET SOCK_STREAM))
	 (s1 (ipc:setopt sock SO_REUSEADDR))
	 (s2 (ipc:setopt sock SO_DEBUG))
	 (s3 (ipc:bind sock INADDR_ANY port))
	 (s4 (ipc:listen sock 5)))
    (unwind-protect
	(progn
	  ;; (if (intp s1) (error "inet-server" s1 "setsockopt SO_REUSEADDR failed"))
	  ;; (if (intp s2) (error "inet-server" s2 "setsockopt SO_DEBUG failed"))
	  (if (intp s3) (error "inet-server" s3 "bind failed"))
	  (if (intp s4) (error "inet-server" s4 "listen failed"))
	  (while #t
	    (print "Waiting on port "
		   (nth (getsockname sock) 4)
		   ?\n)
	    (let* ((fdp (ipc:accept sock))
		   (frm (cdr fdp))
		   (fd  (car fdp)))
	      (if (nconsp fdp) (error "inet-server" fdp "accept failed"))
	      (print "Connection from "
		     (format-ip-addr frm)
		     ?\n)
	      (unwind-protect
		  (catch 'eof
		    (catch 'repl
		      (let ((*stdin_port*  fd)
			    ;; (*stderr_port* fd)
			    (*stdout_port* fd))
			(thunk))))
		(close fd)))))
      (close sock))))

(defun format-ip-addr (addr &optional flag)
  "return nice string from the given ip address"
  (strcat
   (gethostbyaddr addr)
   " ["
   (number->string (nth addr 0)) ?.
   (number->string (nth addr 1)) ?.
   (number->string (nth addr 2)) ?.
   (number->string (nth addr 3))
   "]"
   (if (> (length addr) 4)
       (strcat
	(if (truep flag)
	    " port " ".")
	(number->string (nth addr 4)))
     "")))

;;; possible examples for inet-server
(defun nrepl ()
  (catch 'repl
    (while #t
      (display (eval (read)))
      (newline))))
(defun serve ()
  (while #t
    (display (getline *stdin_port*) *stdout_port*)
    (newline *stdout_port*)))


(defun inet-client (host port)
  "sends a string over to a remote host/port over an inet stream"
  (let ((sock (ipc:open AF_INET SOCK_STREAM)))
    (ipc:connect sock host port)
    (display "Four score and seven years ago\n" sock)
    (catch 'eof (getline sock))
    (close sock)))

(defun mconnect (port host)
  "interactively connects to the specified host/port over an inet stream"
  (let ((sock (ipc:open AF_INET SOCK_STREAM))
	pid
	(osigint     (install-signal-handler SIGINT (lambda ()
						      (throw 'repl))))
	(s1 (ipc:connect sock host port)))

    (if (intp s1) (error "mconnect" s1 "connect failed"))
    (set! pid (fork))
    (if (zerop pid)
	;; child
	(progn
	  (install-signal-handler SIGINT (lambda ()
					   (unwind-protect
					       (progn
						 (display "kid done\n")
						 (close sock))
					     (_quit))))
	  (unwind-protect
	      (catch 'repl
		(catch 'eof
		  (while #t
		    (display (getline sock)))))
	   (kill (getppid) SIGINT)
	   (while #t (sleep 1))))	;wait till signal
	  
      ;; pappy
      (unwind-protect
	  (catch 'repl
	    (catch 'eof
	      (while #t
		(display (getline) sock))))
	(display "pappy done\n")
	(close sock)
	(install-signal-handler SIGINT osigint)
	(kill pid SIGINT)))))

 
	
		      






      


    
