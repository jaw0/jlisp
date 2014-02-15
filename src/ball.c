
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: ball.c,v 2.2 1997/05/30 00:28:33 jaw Exp $";
#endif

#include <jlisp.h>
#include <setjmp.h>
#include <catch.h>

/*
    ball.c? Huh? What does this have to with balls?

    Well, see whats the most natural thing to do with a ball?
    Why to throw them and catch them, of course....
*/

Unwind *unwind_list = 0;
Obj catchlist = IC_NIL;

void markctfrm(Obj a){
	/* nop */
}


DEFUN("catch", Fcatch, Scatch, 1,1,0,1,
      "(catch tag body...) setup a catch frame\n"
      "the return value will be the value specified by the throw\n"
      "if the throw did not specify a value, it will default to #t\n"
      "or the result of body if no throw was caught\n"
      "[see also: throw unwind-protect]",
      (Obj args))
{
	Obj tag = CAR(args);
	Obj body= CDR(args);
	CatchFrame cf;


	CATCH( cf, tag, caught );

	cf.retval = Fprogn( body );

  caught:
	CATCH_CLEANUP(cf);

	return cf.retval;
}

DEFUN("throw", Fthrow, Sthrow, 1,2,1,0,
      "(throw tag [value]) jump out of the matching catch frame\n"
      "if a value is given, it will be the return value from the matching (eqv) catch\n"
      "if there is no matching catch, throw will return #f\n"
      "[see also: catch unwind-protect]",
      (Obj tag, Obj val))
{
	Obj m;
	CatchFrame *cf;
	Unwind *up;
	
	m = Fassv(tag, catchlist);
	
	if( m == IC_FALSE )
		/* no matching tag */
		return IC_FALSE;
	cf = (CatchFrame*)CDDR(m);
	
	cf->retval = val;
	
	/* handle unwind protects */
	for(up=unwind_list; up!=cf->up; up=up->next){
		envcurr = up->envc;
		envlist = up->envl;
		catchlist = up->catch;
		unwind_list  = up->next;
		Fprogn( up->code );
	}

	longjmp( cf->jb, 1);
}

DEFUN("unwind-protect", Funwindpro, Sunwindpro, 1,1,0,1,
      "(unwind-protect now-form later-forms...)\n"
      "make sure later-forms get eval'ed even if we throw\n"
      "while doing now-form, returns the result of now-form\n"
      "[see-also: catch throw]",
      (Obj argl))
{
	Obj now = CAR(argl);
	Obj later = CDR(argl);
	Obj retval;
	Unwind up;

	up.next  = unwind_list;
	up.catch = catchlist;
	up.envc  = envcurr;
	up.envl  = envlist;
	up.code  = later;
	unwind_list = &up;

	retval = Feval( now );

	unwind_list = up.next;
	Fprogn( later );

	return retval;
}


