
;;; these may be OS dependant...

;; option values for waitpid
(define WNOHANG 1)
(define WUNTRACED 2)

;; values for ipc routines
;; OS independant
(define AF_UNIX 1)
(define AF_INET 2)

(define SOCK_STREAM 1)
(define SOCK_DGRAM  2)
(define SOCK_RAW    3)

(define INADDR_ANY  0)			;actually any non-string...

(define SO_DEBUG       1)
(define SO_REUSEADDR   2)
(define SO_KEEPALIVE   3)
(define SO_DONTROUTE   4)
(define SO_USELOOPBACK 5)
(define SO_OOBINLINE   6)
(define SO_BROADCAST   7)


