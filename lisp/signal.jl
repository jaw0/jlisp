
;; define signal names
;; the defines are from /usr/include/signal.h
;; and thus are:
#|
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      from: @(#)signal.h      7.16 (Berkeley) 3/17/91
 *      signal.h,v 1.5 1993/06/16 22:15:06 jtc Exp
 |#

(define	SIGHUP	1	"hangup")
(define	SIGINT	2	"interrupt")
(define	SIGQUIT	3	"quit")
(define	SIGILL	4	"illegal instruction (not reset when caught)")
(define	SIGTRAP	5	"trace trap (not reset when caught)")
(define	SIGABRT	6	"abort()")
(define	SIGEMT	7	"EMT instruction")
(define	SIGFPE	8	"floating point exception")
(define	SIGKILL	9	"kill (cannot be caught or ignored)")
(define	SIGBUS	10	"bus error")
(define	SIGSEGV	11	"segmentation violation")
(define	SIGSYS	12	"bad argument to system call")
(define	SIGPIPE	13	"write on a pipe with no one to read it")
(define	SIGALRM	14	"alarm clock")
(define	SIGTERM	15	"software termination signal from kill")
(define	SIGURG	16	"urgent condition on IO channel")
(define	SIGSTOP	17	"sendable stop signal not from tty")
(define	SIGTSTP	18	"stop signal from tty")
(define	SIGCONT	19	"continue a stopped process")
(define	SIGCHLD	20	"to parent on child stop or exit")
(define	SIGTTIN	21	"to readers pgrp upon background tty read")
(define	SIGTTOU	22	"like TTIN for output if (tp->t_local&LTOSTOP)")
(define	SIGIO	23	"input/output possible signal")
(define	SIGXCPU	24	"exceeded CPU time limit")
(define	SIGXFSZ	25	"exceeded file size limit")
(define	SIGVTALRM 26	"virtual time alarm")
(define	SIGPROF	27	"profiling time alarm")
(define SIGWINCH 28	"window size changes")
(define SIGINFO	29	"information request")
(define SIGUSR1 30	"user defined signal 1")
(define SIGUSR2 31	"user defined signal 2")

(define SIG_DFL #t      "default signal handler")
(define SIG_IGN ()      "ignore signal")

(defun %initialize-signal-handlers% ()
  "initilializes the signal handlers, is called at startup by the c-code"
  (install-signal-handler SIGHUP  quit)
  (install-signal-handler SIGQUIT quit)
  (install-signal-handler SIGINT  (lambda ()
				    (display ?\n)
				    (display ?)
				    ;; back to repl, don't call it an error
				    (throw 'repl:continue)
				    (quit)))  ; in case not caught
  (run-hooks 'install-signal-handlers-hooks))

