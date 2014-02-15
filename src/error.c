
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: error.c,v 2.2 1997/05/30 00:28:33 jaw Exp $";
#endif

#include <jlisp.h>
#include <stdio.h>

extern Obj envcurr;

EXTERN_C
Obj jlerror(char* fnc, Obj a, char* huh){
	Obj errsym;
	Obj hndlr, ehs=maksym_c("error-handler");

	/* 1st look for a user defined error handler */
	hndlr = getvalue(ehs);
	if( Fprocp( hndlr )!= IC_FALSE){
		/* hand it our args */
		funcall_3("error", ehs, makstr(fnc), a, makstr(huh));
		/* ought not return ... */
	}

	/* next try throwing an 'error */
	Fthrow(maksym_c("error"), IC_TRUE );

	/* try throwing back to the repl */
	Fthrow(maksym_c("repl:error"), IC_TRUE);
	Fthrow(maksym_c("repl"), IC_TRUE);

	/* hopefully by this point something has been taken
	   as if we return, we will likely segv... */
	
	/* lastly, just return */
	return IC_UNSPEC;
}
