

/*
    Copyright (c) 1997 Jeff Weisberg
 
    see the file "License"
*/
 
#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: ext_init.c,v 2.2 1997/05/30 00:28:50 jaw Exp $";
#endif
 
#include <jlisp.h>

#undef DEFUN
#define DEFUN(ln, cn, sn, min, max, ep, lp, doc, pr)		\
	extern Defun_Decl sn;
#include "defun.list"
 
#undef DEFVAR
#define DEFVAR(ln, cn, doc, val)				\
	extern Obj cn;
#include "defvar.list"


void init_ext(void){
	extern int initialized;
	
        if(! initialized){
		
#undef DEFUN
#define DEFUN(ln, cn, sn, min, max, ep, lp, doc, pr)		\
                init_csyms( &sn );
#include "defun.list"

#undef DEFVAR
#define DEFVAR(ln, cn, doc, val)                                \
		foo = maksym_c(ln);                             \
		Fdefine(foo, makstr(doc), IC_UNDEF);            \
                cn = Fenvlookup(foo, IC_UNSPEC);                \
                CAR( cn ) |= SDBIT;                             \
                VALUE( cn ) = val;
#include "defvar.list"


	
	}
}

