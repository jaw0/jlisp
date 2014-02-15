
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: debug.c,v 2.2 1997/05/30 00:28:35 jaw Exp $";
#endif

#include <jlisp.h>

extern Backtrace *backtrace_list;
extern Obj sym_quote;

DEFVAR(".already-debugging", Valrdy_dbg, 
       ".already-debugging  used internally by the debugger to avoid recursing",
       IC_FALSE)
     
Obj debugger(Obj what, Obj val){
	/*
	when internally called, "what" will be:
	0  entering fnc
	1  leaving fnc
	*/
	Obj dbg, dbs;
	
	if(! FALSEP( VALUE(Valrdy_dbg))) return val;
	
	dbs = maksym_c("debugger");
	dbg = getvalue(dbs);
	if( DEFINEDP(dbg) && !FALSEP(Fprocp(dbg)) ){
		VALUE(Valrdy_dbg) = IC_TRUE;
		val = funcall_2( "#<internal:debugger>", dbs, what, val);
		VALUE(Valrdy_dbg) = IC_FALSE;
	}
	return val;
}

DEFUN("backtrace", Fbacktrc, Sbacktrc, 0,0,1,0,
      "(backtrace) return a list of the lisp calling frames\n"
      "of the form: (fncname fncdefn arglist dbgflag)",
      ())
{
	Obj head=IC_NIL, tail, nl;
	Backtrace *bt;

	for(bt=backtrace_list; bt; bt=bt->next){

		nl = Fcons(bt->fncname,
			Fcons(bt->fncdefn,
			      Fcons(bt->argl,
				    Fcons( bt->dbg_me?IC_TRUE:IC_FALSE, IC_NIL))));
		if( NULLP(head)){
			head = tail = Fcons( nl, IC_NIL );
		}else{
			CDR(tail) = Fcons(nl, IC_NIL);
			tail = CDR(tail);
		}
	}
	return head;
}

#ifndef MINIMAL 
/* for taking apart a function, these should be used only for debugging... */
DEFUN("get-param-list", Fgparaml, Sgparaml, 1,1,1,0,
      "(get-param-list function|macro) return the parameter declaration list\n"
      "[see also: get-body-list]",
      (Obj l))
{
	if(! FUNCTIONP(l) && ! MACROP(l))
		return JLERROR(Sgparaml.name, l, "WTA: function or macro p");

	return CADDR(l);
}

DEFUN("get-body-list", Fgbodyl, Sgbodyl, 1,1,1,0,
      "(get-body-l function|macro) return the body\n"
      "[see also: get-param-list]",
      (Obj l))
{
	if(! FUNCTIONP(l) && ! MACROP(l))
		return JLERROR(Sgbodyl.name, l, "WTA: function or macro p");

	return CDDDR(l);
}

DEFUN("get-env-list", Fgenvl, Sgenvl, 1,1,1,0,
      "(get-env-list closure) return the enviornment",
      (Obj l))
{
	if(! FUNCTIONP(l) && ! MACROP(l))
		return JLERROR(Sgparaml.name, l, "WTA: function or macro p");

	return CADR(l);
}


DEFUN("set-debug-back-frame", Fsdbgbt, Ssdbgbt, 1,2,1,0,
      "(set-debug-back-frame depth [flag]) set the debug flag on the function\n"
      "depth levels back on the stack frame",
      (Obj d, Obj f))
{
	int i, dpth, flg=1, was;
	Backtrace *bt;
	
	if(! INUMP(d) || CINT(d)<=0)
		return JLERROR(Ssdbgbt.name, d, "WTA: intp");

	if( f==IC_TRUE ) flg = 1;
	if( f==IC_FALSE) flg = 0;
	
	dpth = CINT(d);

	bt = backtrace_list;
	for(i=0; i++!= dpth && bt; bt=bt->next);
	if( bt ){
		was = bt->dbg_me;
		bt->dbg_me = flg;
		return was?IC_TRUE:IC_FALSE;
	}
	return IC_UNSPEC;
}

DEFUN("get-frame-back", Fgfbck, Sgfbck, 1,1,1,0,
      "(get-frame-back n) get the stack frame n levels back",
      (Obj n))
{
	int i;
	int dpth;
	Backtrace *bt = backtrace_list;

	if( !INUMP(n) || CINT(n)<=0 )
		return JLERROR(Sgfbck.name, n, "WTA: intp");

	dpth = CINT(n);
	for(i=0; i++!= dpth && bt; bt=bt->next);

	if( ! bt )
		return IC_UNSPEC;
	
	return Fcons(bt->fncname,
		Fcons(bt->fncdefn,
		      Fcons(bt->argl,
			    Fcons( bt->dbg_me?IC_TRUE:IC_FALSE, IC_NIL))));
}

DEFUN("debug-on-entry", Fdbgentry, Sdbgentry, 1,1,1,0,
      "(debug-on-entry function) enter the debugger when function is called",
      (Obj f))
{

	if(! FUNCTIONP(f) && ! MACROP(f))
		return JLERROR(Sdbgentry.name, f, "WTA: function or macro p");

	CAR(f) |= 1<<12;

	return f;
}

DEFUN("cancel-debug-on-entry", Fcncldbg, Scncldbg, 1,1,1,0,
      "(cancel-debug-on-entry function) no longer call debugger function when called",
      (Obj f))
{

	if(! FUNCTIONP(f) && ! MACROP(f))
		return JLERROR(Scncldbg.name, f, "WTA: function or macro p");

	CAR(f) &= ~(1<<12);

	return f;
}
#endif /* MINIMAL */


void btrace(void){

	Fdisplay( Fbacktrc(), stderr_port );
}

