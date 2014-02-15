
/*
    Copyright (c) 1997 Jeff Weisberg

    see the file "License"
*/

#ifndef _catch_h
#define _catch_h

typedef struct Unwind {
	Obj catch, envc, envl, code;
	struct Unwind *next;
} Unwind;

typedef struct {
	jmp_buf jb;
	Backtrace *bt;
	Obj catch, envc, envl;
	Unwind *up;
	Obj retval;
} CatchFrame;

extern Unwind *unwind_list;
extern Obj envcurr, envlist;
extern Backtrace *backtrace_list;
extern Obj catchlist;

#define CATCH(cf, tag, label)	{			\
	Obj acf = newcell();				\
	int sigs;					\
							\
	cf.envc  = envcurr;				\
	cf.catch = catchlist;				\
	cf.envl  = envlist;				\
	cf.bt    = backtrace_list;			\
	cf.up    = unwind_list;				\
	cf.retval= IC_UNSPEC;				\
							\
	DISABLE( sigs );				\
	CAR(acf) = MAKETYPE( TPV_CATCHFRM );		\
	CDR(acf) = (Obj)&cf;				\
	RENABLE( sigs );				\
							\
	if( setjmp( cf.jb )){				\
		if( NBOUNDP(cf.retval))			\
			cf.retval = IC_TRUE;		\
		RENABLE( 0 );				\
		goto label;				\
	}						\
	acf = Fcons(Feval(tag), acf);			\
	DISABLE( sigs );				\
	catchlist = Fcons( acf, catchlist);		\
	RENABLE( sigs );				\
	/* body follows ... */				\
}



#define CATCH_CLEANUP(cf)	{			\
	int sigs;					\
							\
	DISABLE( sigs );				\
	catchlist = cf.catch;				\
	envcurr = cf.envc;				\
	envlist = cf.envl;				\
	backtrace_list = cf.bt;				\
	unwind_list = cf.up;				\
	RENABLE( sigs );				\
}
	




/* to catch in c:

   CatchFrame cf;

   code....

   CATCH(cf, tag, caught);

   code...

 caught:
   CATCH_CLEANUP(cf);

   continue...
   if( cf.retval == something ){
         stuff
   }

*/



#endif /* !_catch_h */

