
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: signals.c,v 2.2 1997/05/30 00:28:35 jaw Exp $";
#endif


#include <jlisp.h>
#include <signal.h>

#ifndef NO_SIGNALS

static Obj sig_thunks[32] = {
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL, 
	IC_NIL, IC_NIL, IC_NIL, IC_NIL
};

int sig_block_mask  = 0;		/* anachr. */
int signals_blocked = 0;		/* are we in a critical section */
unsigned int got_signal_vec = 0;	/* which signals need processing */
static int protected_p = 0;
extern Obj internal_gc_protect;
extern Backtrace *backtrace_list;

/*
    if we are in a critical section (signals_blocked != 0)
    we set the signal got in got_signal_vec, and process
    the signal at the end of the critical section.

    if we get more than one of the same signal while in the
    critical section, since we are not keeping a count,
    only one will be processed. since the critical sections
    are small, we assume that this is unlikely, and we will
    live with this risk...
*/

void signal_handler(int signo){
	
	got_signal_vec |= 1 << (signo - 1);

	if( signals_blocked ) return;

	signal_dispatcher();
}

void signal_dispatcher(void){
	Obj ush;
	Backtrace bt;
	int i,m;

	for(i=m=1;i<32;i++,m<<=1){
		if( got_signal_vec & m ){ 
			/* got signal i */
			/* clear it */
			got_signal_vec &= ~m;
			
			/* process signal */
			ush = sig_thunks[ i ];
			if( NNULLP( ush )){
				bt.fncname = IC_NIL;
				bt.fncdefn = MAKINT(i);
				bt.argl    = ush;
				bt.dbg_me  = 0;
				bt.next    = backtrace_list;
				backtrace_list = &bt;

				Feval( ush );
				
				backtrace_list = bt.next;
			}
		}
	}
}

DEFUN("install-signal-handler", Finstsigh, Sinstsigh, 2,2,1,0,
      "(install-signal-handler signo thunk) install a signal handler\n"
      "for the specified signal number, thunk is a procedure to be called\n"
      "which must not require any params, or it may be () to ignore the\n"
      "signal, or #t to restore the default system handler",
      (Obj sgn, Obj thnk))
{
	int t;
	int sigs;
	Obj foo, bar;
	
	if( !INUMP(sgn) || CINT(sgn) > 32 || CINT(sgn) < 1)
		return JLERROR("install-signal-handler", sgn, "WTA: intp");
	if( Fprocp(thnk)==IC_FALSE && thnk!=IC_NIL && thnk!=IC_TRUE)
		return JLERROR("install-signal-handler", thnk, "WTA: procp");

	t = CINT(sgn);
	if( NULLP(thnk)){
		signal(t, SIG_IGN);
		sig_thunks[t] = IC_NIL;
		/* sig_block_mask &= ~sigmask(t); */
	}else if( thnk==IC_TRUE){
		signal(t, SIG_DFL);
		sig_thunks[t] = IC_NIL;
		/* sig_block_mask &= ~sigmask(t); */
	}else{
		if(! protected_p){
			/* make sure these thunks are not gc'ed
			we make the above array look like a
			vector and protect it */
			Obj v = newcell();

			internal_gc_protect = Fcons(v,
				internal_gc_protect);

			DISABLE( sigs );
			CAR(v) = MAKETYPE(TPV_VECTOR) | (32 << 12);
			CDR(v) = (Obj)sig_thunks;
			protected_p = 1;
			RENABLE( sigs );
		}
		foo = Fcons(thnk, IC_NIL);
		bar = sig_thunks[t];
		DISABLE( sigs );
		sig_thunks[t] = foo;
		signal(t, signal_handler);
		/* sig_block_mask |= sigmask(t); */
		RENABLE( sigs );
	}
	return bar;
}

#endif /* NO_SIGNALS */


