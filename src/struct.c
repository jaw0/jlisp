
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: struct.c,v 2.2 1997/05/30 00:28:45 jaw Exp $";
#endif

#include <jlisp.h>

#ifndef NO_STRUCTS

Obj makstruct(int len){
	Obj foo;
	int sigs;

	foo = makvect(len);
	DISABLE( sigs );
	CAR( foo ) = MAKETYPE( TPV_STRUCT ) | (len << 12);
	RENABLE( sigs );
	return foo;
}

DEFUN("makestruct", Fmakestruct,Smakestruct, 1,1,1,0,
      "(makestruct len) create a struct of specified length",
      (Obj a))
{
	register int len;

	if(INUMP(a)) len = CINT(a);
	else if( VECTORP(a) || STRINGP(a) )
		len = CLENGTH(a);
	else{
		return JLERROR(Smakestruct.name,a, "How big?");
	}
	return makstruct(len);
}

int prnstrct(Obj a, Obj stream, int how){

	return funcall_3("#<internal:print>",
			 getvalue(maksym_c("method")),
			 a,
			 maksym_c("print"),
			 stream); 
}




#endif /* NO_STRUCTS */
