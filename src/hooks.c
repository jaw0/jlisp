
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: hooks.c,v 2.2 1997/05/30 00:28:34 jaw Exp $";
#endif

#include <jlisp.h>

DEFUN("add-hook", Faddhook, Saddhook, 2,2,1,0,
      "(add-hook hooks function) add the function to the hook",
      (Obj h, Obj f))
{
	Obj foo;
	
	if(SYMBOLP(h)){
		foo = Fenvlookup(h, IC_UNSPEC);
		if(! SYMBOXP(foo)){
			foo = Finter(h, IC_UNSPEC);
			VALUE(foo) = IC_NIL;
		}
		h = foo;
	}
	
	if(! SYMBOXP(h))
		return JLERROR(Saddhook.name, h, "WTA: symbolp");

	if( Fprocp( f )==IC_FALSE)
		return JLERROR(Saddhook.name, h, "WTA: procedurep");
	
	return VALUE(h) = Fcons( f, VALUE(h));
}

DEFUN("run-hooks", Frunhooks, Srunhooks, 1,1,1,0,
      "(run-hooks hooks) run hooks",
      (Obj h))
{

	if(SYMBOLP(h) || SYMBOXP(h))
		h = getvalue(h);

	if(NCONSP(h))
		/* do not return JLERROR, we want to use internally */
		return IC_FALSE;

	for(; CONSP(h); h = CDR(h)){
		if( Fprocp( CAR(h))==IC_TRUE)
			Fcall( CAR(h));
	}

	return IC_TRUE;
}

