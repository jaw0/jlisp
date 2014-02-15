
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: eval.c,v 2.2 1997/05/30 00:28:27 jaw Exp $";
#endif

#include <jlisp.h>

extern void pushenv(void), popenv(void);
extern Obj envcurr, envlist;
extern Obj sym_rest, sym_optional;
extern Obj sym_eval_function, sym_eval_macro, sym_eval_closure;
extern Obj sym_eof;
extern Obj getvalue(Obj);
extern Obj debugger(Obj, Obj);

Backtrace *backtrace_list=0;

EXTERN_C Obj eval_internal(Obj, int, char*);


#if defined(INFERIOR_CC) || defined(__GNUC__)
#	define DOT_DOT_DOT
#else
#	define DOT_DOT_DOT	...
#endif


DEFVAR(".debug-on-next-call", Vdebug_on_next_call,
       ".debug-on-next-call if not #f the debugger will be entered prior to the next call",
       IC_FALSE)
     

DEFUN("call", Fcall, Scall, 1,1,1,0,
      "(call thunk)  call the specified thunk",
      (Obj thk))
{

	return Feval( Fcons(thk, IC_NIL));
}

DEFUN("eval", Feval, Seval, 1,1, 1,0 ,
      "(eval form) evaluate form",
      (Obj argl))
{
	return eval_internal(argl, 1, Seval.name);
}

DEFUN("funcall", Ffuncall, Sfuncall, 1,1,1,1,
      "(funcall func args...) call func with args",
      (Obj argl))
{
	return eval_internal(argl, 0, Sfuncall.name);
}

EXTERN_C
Obj funcall_0(char *name, Obj f){
	return eval_internal(Fcons(f, IC_NIL), 0, name);
}

EXTERN_C
Obj funcall_1(char *name, Obj f, Obj a){
	return eval_internal(Fcons(f, Fcons( a, IC_NIL)), 0, name);
}

EXTERN_C
Obj funcall_2(char *name, Obj f, Obj a, Obj b){
	return eval_internal(Fcons(f, Fcons(a, Fcons(b, IC_NIL))), 0, name);
}	

EXTERN_C
Obj funcall_3(char *name, Obj f, Obj a, Obj b, Obj c){
	return eval_internal(Fcons(f, Fcons(a, Fcons(b, Fcons(c, IC_NIL)))), 0, name);
}

EXTERN_C
Obj funcall_4(char *name, Obj f, Obj a, Obj b, Obj c, Obj d){
	return eval_internal(Fcons(f, Fcons(a, Fcons(b, Fcons(c, Fcons(d, IC_NIL))))), 0, name);
}	




DEFUN("apply", Fapply, Sapply, 1,1,1,1,
      "(apply func args... arglist) apply func to args arglist",
      (Obj l))
{
	Obj head=l;
	Obj fnc = CAR(l);
	l = CDR(l);

	if( NULLP(l) || NULLP( CDR(l))){
		/* (f) || (f l) */

		if( NNULLP(CAR(l)) && NCONSP( CAR(l)))
			return JLERROR(Sapply.name, CAR(l), "WTA: listp");

		return Ffuncall(Fcons(fnc, CAR(l)));
	}

	/* (f a.. l) */
	while( CONSP( CDDR(l))){
		l = CDR(l);
	}
	/* join */
	CDR(l) = CADR(l);

	return eval_internal( head, 0, Sapply.name);

}
	
EXTERN_C
Obj eval_internal(Obj arg, int eval_args, char* fncname){
	Obj foo, qux, quux, retval;
	int nmin, nmax, listp, evalp;
	register int i;
	int n, typ = TYPEOFX( arg );
	Backtrace bt;
	int cnvflag = 0;
	char *conv;
	
	struct Cargs {
		Obj r[32];
	} cargs;

	bzero( (void*)&cargs, (int)sizeof(cargs));	/* to keep GC happy */
	
	switch( typ ){

	  case TPV_FREE_CELL:
	  case TPV_BOX_CELLS:
		return JLERROR(fncname, arg, "Cannot evaluate form");

	  case TPV_SYM_BOX:
		return getvalue( arg );
		/* or just return CSYM_BOX( arg )->value; */

	  case TPVF_IMMED:
		if( SYMBOLP(arg) ){
			foo = Fenvlookup( arg, IC_UNSPEC );	/* lookup in default current env */
			if( ! SYMBOXP( foo ) ){
				return JLERROR(fncname, arg, "Undefined symbol");
			}	
			return getvalue( foo );		
		}
		if( arg == IC_EOF ){
			Fthrow(sym_eof, IC_TRUE);
		}
		/* others self evaluate */
		return arg;
		
	  case TPVF_CONS:
		arg = Fcopylist(arg);
		/* set up backtrace frame and find fnc defn */
		foo = eval_internal( bt.fncname=CAR(arg), 1, fncname);
		bt.fncdefn = foo;
		bt.argl    = CDR( arg );
		bt.dbg_me  = 0;
		bt.next    = backtrace_list;
		backtrace_list = &bt;
				
		switch( TYPEOFX( foo ) ){

		  case TPV_FUNCTION:
			/* make it look like:
			(#<:internal:eval-function>  function  args)
			*/
		  case TPV_MACRO:
			/* make it look like:
			(#<:internal:eval-macro>  macro  args)
			*/
			  
			/* is this function set for debug-on-entry? */
			if( CDBGME(foo))
				bt.dbg_me = 1;

			CAR(arg) = foo;
			
			if( TYPEOFX(foo) == TPV_FUNCTION)
				foo = sym_eval_function;
			else
				foo = sym_eval_macro;

			foo = getvalue( foo ); 
			arg = Fcons(foo, arg);
			/* fall thru' */
			
		  case TPV_C_CODE:
			/* call c code */
			  
			if( bt.dbg_me || VALUE( Vdebug_on_next_call )!=IC_FALSE){
				VALUE( Vdebug_on_next_call ) = IC_FALSE;
				bt.dbg_me = 1;
				debugger( MAKINT(0), IC_NIL );
			}
			
			/* figure out how this function is called... */
			evalp = CCDECL(foo)->evalp;
			listp = CCDECL(foo)->listp;
			nmin  = CCDECL(foo)->minarg;
			nmax  = CCDECL(foo)->maxarg;
			cnvflag=CCDECL(foo)->flags & DDF_CONV;
			conv  = CCDECL(foo)->conv;
			
			n = 0;
			
			if( !evalp && !eval_args)
				return JLERROR(fncname, foo, "Incorrect type of function");
			if(! eval_args)
				evalp = 0;
			
			if( evalp ){
				/* traverse args and eval them -- but not more than nmax */
				qux = CDR(arg);
				
				while( NNULLP( qux )){
					if( listp || n < nmax){
						quux = CAR(qux);
						/* avoid the eval on self-evaluating things */
						if( NSELFEVALP(quux)){
							CAR(qux) = Feval( CAR(qux));
						}else{
							CAR(qux) = quux;
						}
					}
					qux = CDR(qux);
					n++;
				}
				qux = CDR(arg);
				
			}else{
				qux = quux = CDR( arg );

				/* and count them */
				while( NNULLP( quux ) ){
					n++;
					quux = CDR( quux );
				}
			}
			
			if( !listp && (n < nmin || n > nmax) ){
				return JLERROR(fncname, foo, "Wrong number of arguments");
			}
			
			if( listp ){
				/* call with list */
				retval = ((Obj(*)(Obj))CCFUNC( foo ))(qux);
				goto done;
			}
			
			/* set up args */
			i = 0;
			if( cnvflag ){
				/* RSN */
			}else{
				while( i < n ){
					cargs.r[i++] = CAR( qux );
					qux = CDR( qux );
				}
			}
			for(; i<32; i++)
				cargs.r[i] = IC_UNSPEC;

#if 0
			/* I'd like for this to work...*/
			retval = ((Obj(*)( DOT_DOT_DOT ))CCFUNC( foo ))( cargs );
			goto done;
#else			
			switch( nmax ){
			  case 0:
				retval = ((Obj(*)( DOT_DOT_DOT ))CCFUNC( foo ))();
				goto done;
			  case 1:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0]
				);
				goto done;
			  case 2:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1]
				);
				goto done;
			  case 3:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2]
				);
				goto done;
			  case 4:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3]
				);
				goto done;
			  case 5:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4]
				);
				goto done;
			  case 6:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5]
				);
				goto done;
			  case 7:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6]
				);
				goto done;
			  case 8:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6], cargs.r[7]
				);
				goto done;
			  case 9:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6], cargs.r[7], cargs.r[8]
				);
				goto done;
			  case 10:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6], cargs.r[7], cargs.r[8],
					cargs.r[9]
				);
				goto done;
			  case 11:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6], cargs.r[7], cargs.r[8],
					cargs.r[9], cargs.r[10]
				);
				goto done;
			  case 12:
				retval = ((Obj(*)(DOT_DOT_DOT))CCFUNC( foo ))(
					cargs.r[0], cargs.r[1], cargs.r[2],
					cargs.r[3], cargs.r[4], cargs.r[5],
					cargs.r[6], cargs.r[7], cargs.r[8],
					cargs.r[9], cargs.r[10], cargs.r[11]
				);
				goto done;

			  default:
				retval = JLERROR(fncname, MAKINT( nmax ),
					"internal error--nargs switch too small\n"
					"send in bug report");
					
				goto done;
			}
#endif			
			
			break;
		  default:
			/* what ought be done about such cases
			   (non-fnc as a fnc call: (xxx ...))
			   ideas (RSN):
			       symbol	lookup again (as emacs does)
			       vector	bytecoded fnc?
			       numbers
			       strings
			       iconsts
			   (even if only for nif value)
			*/
			   
			return JLERROR(fncname, foo, "not a function");
		}
		break;

	  default:
		
		/* all other types self evaluate */
		return arg;
	}
	return JLERROR(fncname, IC_NIL, "internal error?");

  done:
	/* restore backtrace frame */
	if( bt.dbg_me ){
		VALUE( Vdebug_on_next_call ) = IC_FALSE;
		retval = debugger(MAKINT(1), retval);
	}
	backtrace_list = bt.next;
	return retval;
}

/* evaluate lambda/macro */
DEFUN("#<:internal:eval-function>", Feval_function, Seval_function, 1,1,1,1,
      "for internal use",
      (Obj a))
{
	Obj function = CAR(a), args = CDR(a);
	Obj env = CADR(function), params = CADDR(function), body = CDDDR(function);
	Obj result, envpre, envlpre;
	Obj box, sy;
	int opt=0;

	envpre = envcurr;
	envlpre = envlist;
	/* if closure - install saved env */
	if( NNULLP(env) ){
		envlist = Fcons(envpre, envlist);
		envcurr = env;
	}

	/* if no formals - do not push new env (an optimization) */
	/* this will not do the expected thing if someone does:
	     (lambda ()
	        (define x ...
	*/
	if( NNULLP(params))
		pushenv();
	
	if(SYMBOLP(params)){
		box = Finter(params, IC_UNSPEC);
		CSYM_BOX(box)->value = args;
	}else{
		while( NNULLP(params) ){
			/* inter params into env with proper values */
			sy = CAR(params);
			if( sy==sym_optional ){
				opt = 1;
				params = CDR(params);
				continue;
			}
			if( sy==sym_rest ){
				sy = CADR(params);
				box = Finter(sy, IC_UNSPEC);
				CSYM_BOX(box)->value = args;
				params = args = IC_NIL;
			}

			if( NULLP(args)){
				if(!opt && CONSP(sy)){
					/* if next arg is list set opt */
					opt = 1;
				}
				break;
			}
			if( NCONSP(sy)){
				box = Finter(sy, IC_UNSPEC);
				CSYM_BOX(box)->value = CAR(args);
			}else{
				opt = 1;	/* so &optional can be optional */
				/* it is (sym init [sp]) arg */
				box = Finter( CAR(sy), IC_UNSPEC);
				CSYM_BOX(box)->value = CAR(args);
				if( NNULLP(CDR(sy)) && NNULLP(CDDR(sy))){
					/* it is (sy init sp) -- set sp */
					box = Finter( CADDR(sy), IC_UNSPEC);
					CSYM_BOX(box)->value = IC_TRUE;
				}
			}

			params = CDR(params);
			args = CDR(args);
		}
		if( opt && NNULLP(params)){
			/* inter the remaining optionals as unspec'd */
			while( NNULLP(params)){
				sy = CAR(params);
				if( sy==sym_rest ){
					     /* no more args for rest */
					params = CDR(params);
					sy = CAR(params);
				}
				if( NCONSP(sy)){
					box = Finter(sy, IC_UNSPEC);
					CSYM_BOX(box)->value = IC_UNSPEC;
				}else{
					/* it is (sym init [sp]) */
					box = Finter( CAR(sy), IC_UNSPEC);
					if( NNULLP(CDR(sy))){
						CSYM_BOX(box)->value = Feval( CADR(sy) );
						if( NNULLP(CDDR(sy))){
							/* it is (sy init sp) -- set sp */
							box = Finter( CADDR(sy), IC_UNSPEC);
							CSYM_BOX(box)->value = IC_FALSE;
						}
					}else{
						CSYM_BOX(box)->value = IC_UNSPEC;
					}
				}
				params = CDR(params);
			}
		}
		
		if( NNULLP(params) || NNULLP(args) ){
			return JLERROR("eval-function", CDR(a), "Wrong number of parameters");
		}
	}

	result = Fprogn( body );
	envcurr = envpre;
	envlist = envlpre;
	return result;
}

DEFUN("expand-macro", Fexp_macr, Sexp_macr, 1,1,0,1,
      "(expand-macro macro args...) Expand a macro",
      (Obj m))
{
	CAR(m) = Feval( CAR(m)); /* lookup macro */
	if(! MACROP( CAR(m) ))
		return m;
	return Feval_function(m);
}

DEFUN("#<:internal:eval-macro>", Feval_macro, Seval_macro, 1,1, 0,1,
      "for internal use",
      (Obj a))
{
	
	return Feval( Feval_function( a ));
}

void dbgpm(char *a, int n){
	int i;

	for(i=0;i<n;i++){
		printf("%2.2x ", a[i]);
	}
}

	
