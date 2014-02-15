

/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: jlib.c,v 2.2 1997/05/30 00:28:36 jaw Exp $";
#endif

/*
misc. basic functions
*/

#include <jlisp.h>
#include <string.h>

extern Obj envcurr, envlist;
extern Obj_Vtbl jlisp_vtbl[];
extern Obj sym_optional, sym_rest, sym_quote;
extern Obj sym_quote, sym_bquote, sym_bq_comma, sym_bq_comma_at;
extern Obj sym_docstring;

Obj Flength(Obj), Fequal(Obj, Obj);

DEFUN("cons", Fcons, Scons, 2, 2, 1,0,
      "(cons obj obj) create a cons cell",
      (Obj a, Obj b))
{

	Obj c = newcell();
	int sigs;

	DISABLE( sigs );
	CAR(c) = a ;
	CDR(c) = b ;
	RENABLE( sigs );
	
	return c;
}

DEFUN("car", Fcar, Scar, 1,1, 1,0,
      "(car cell) return the CAR of a cell",
      (Obj a))
{

	if( CONSP(a)) return CAR(a);
	else return IC_UNSPEC;
}

DEFUN("cdr", Fcdr, Scdr, 1,1, 1,0,
      "(cdr cell) return the CDR of a cell",
      (Obj a))
{
	
	if( CONSP(a)) return CDR(a);
	else return IC_UNSPEC;
}

DEFUN("set-car!", Fsetcar, Ssetcar, 2,2, 1,0,
      "(set-car! cell value) set the CAR of a cell",
      (Obj cell, Obj val))
{

	if( NCONSP(cell))
		return JLERROR(Ssetcar.name, cell, "WTA: consp");

	CAR(cell) = val;

	return val;
}

DEFUN("set-cdr!", Fsetcdr, Ssetcdr, 2,2, 1,0,
      "(set-cdr! cell value) set the CDR of a cell",
      (Obj cell, Obj val))
{

	if( NCONSP(cell))
		return JLERROR(Ssetcdr.name, cell, "WTA: consp");

	CDR(cell) = val;

	return val;
}

DEFUN("list", Flist, Slist, 1,1, 1,1,
      "(list objs...) create a list",
      (Obj a))
{
	return a;
}

DEFUN("vector", Fvector, Svector, 1,1,1,1,
      "(vector objs...) create a vector",
      (Obj a))
{
	return Flist_vect(a);
}

DEFUN("quote", Fquote, Squote, 1,1, 0, 0,
      "(quote arg) quote arg",
      (Obj a))
{
	return a;
}

Obj lambda_macro_common(Obj a, int typ, Obj env, char* name){
	Obj function = newcell();
	Obj s, sy, ps;
	int opt=0, rest=0;
	int sigs;
	
	/* either a param or list of params */
	if(! CONSP(a))
		return JLERROR(name, a, "WNA: no arg list");
	
	a = Fcopydeep(a); /* do we need the safety? yes...*/
	s = CAR(a);
	if(SYMBOLP(s) || SYMBOXP(s))
		;
	else{
		for(; CONSP(s); ps=s,s=CDR(s)){
			if( rest )
				return JLERROR(name, s, "WNA: too many rests?");
		  dbl_chk:
			sy = CAR(s);
			if( CONSP(sy)){
				if(! SYMBOLP(CAR(sy)) )
					return JLERROR(name, s, "Invalid parameter list");
				if( NNULLP(CDR(sy)) && NNULLP(CDDR(sy))
				   &&(! SYMBOLP(CADDR(sy)) ) )
					return JLERROR(name, s, "Invalid parameter list");

				opt = 1; /* &optional is implied */
			}else if(! SYMBOLP(sy) ){
				return JLERROR(name, s, "Invalid parameter list");
			}
			/* check &rest &optional have matching symbols */
			if( sy==sym_optional ){
				if( opt || rest )
					return JLERROR(name, s, "bad optional or rest spec");
				s = CDR(s);
				opt = 1;
				goto dbl_chk;
			}
			if( sy==sym_rest ){
				if(rest)
					return JLERROR(name, s, "bad optional or rest spec");
				s = CDR(s);
				rest = 1;
				goto dbl_chk;
			}
		}
		if( NNULLP(s)){
			/* got dotted arglist (a . b) */
			if( opt || rest || ! SYMBOLP(sy))
				return JLERROR(name, s, "Invalid parameter list");
			/* turn into (a &rest b) */
			CDR(ps) = Fcons( sym_rest, Fcons( s, IC_NIL ));
		}
	}

	DISABLE( sigs );
	CAR(function) = MAKETYPE( typ );
	CDR(function) = Fcons(env, a);
	RENABLE( sigs );
	
	return function;
}

DEFUN("lambda", Flambda, Slambda, 1,1, 0,1,
      "(lambda arglist body...) Lambda the great",
      (Obj a))
{
	return lambda_macro_common(a, TPV_FUNCTION, IC_NIL, Slambda.name);
}

DEFUN("macro", Fmacro, Smacro, 1,1, 0,1,
      "(macro arglist body...) define a macro",
      (Obj a))
{
	return lambda_macro_common(a, TPV_MACRO, IC_NIL, Smacro.name);
}

DEFUN("closure", Fclosure, Sclosure, 1,1,0,1,
      "(closure arglist body...) create a closure",
      (Obj a))
{
	return lambda_macro_common(a, TPV_FUNCTION, envcurr, Smacro.name);
}

DEFUN("set!", Fset, Sset, 2,2, 0,0,
      "(set! symbol value) set the value of a symbol",
      (Obj sym, Obj val))
{
	Obj box;
	
	if( SYMBOLP(sym) ){
		box = Fenvlookup(sym, IC_UNSPEC);
		if( NDEFINEDP( box )){
			/* box = Finter(sym, IC_UNSPEC); */
			return JLERROR(Sset.name, sym, "Undefined symbol");
		}
		sym = box;
	}
	if( !SYMBOXP(sym) )
		return JLERROR(Sset.name, sym, "WTA: symbolp");

	return CSYM_BOX(sym)->value = Feval( val );
}

DEFUN("set-props-nq!", Fsetpropsnq, Ssetpropsnq, 2,2,1,0,
      "(set-props-nq! symbol proplist) set the plist of a symbol",
      (Obj sym, Obj val))
{
	Obj box;
	
	if( SYMBOLP(sym) ){
		box = Fenvlookup(sym, IC_UNSPEC);
		if( NDEFINEDP( box )){
			return JLERROR(Ssetpropsnq.name, sym, "Undefined symbol");
		}
		sym = box;
	}
	if( !SYMBOXP(sym) )
		return JLERROR(Ssetpropsnq.name, sym, "WTA: symbolp");

	return CSYM_BOX(sym)->props = val;
}

DEFUN("set-props!", Fsetprops, Ssetprops, 2,2, 0,0,
      "(set-props! symbol proplist) set the plist of a symbol",
      (Obj sym, Obj val))
{
	Obj box;
	
	if( SYMBOLP(sym) ){
		box = Fenvlookup(sym, IC_UNSPEC);
		if( NDEFINEDP( box )){
			return JLERROR(Ssetprops.name, sym, "Undefined symbol");
		}
		sym = box;
	}
	if( !SYMBOXP(sym) )
		return JLERROR(Ssetprops.name, sym, "WTA: symbolp");

	return CSYM_BOX(sym)->props = Feval( val );
}


DEFUN("get-props", Fgetprops, Sgetprops, 1,1,1,0,
      "(get-props symbol) get the plist of a symbol",
      (Obj sym))
{
	Obj box;

	if( SYMBOLP(sym) ){
		box = Fenvlookup(sym, IC_UNSPEC);
		if( NDEFINEDP( box )){
			return JLERROR(Sgetprops.name, sym, "Unbound symbol");
		}
		sym = box;
	}
	if( !SYMBOXP(sym) )
		return JLERROR(Sgetprops.name, sym, "WTA: symbolp");

	return CSYM_BOX(sym)->props;
}

DEFUN("define", Fdefine, Sdefine, 2,3, 0,0,
      "(define symbol value [docstring]) or (define symbol [docstring] value)\n"
      "define a variable in the current scope with an optional doc-string",
      (Obj sym, Obj v1, Obj v2))
{
	Obj ds, val, ods;
	Obj box;

	
	if( !SYMBOLP(sym) ){
		return JLERROR(Sdefine.name, sym, "wrong type of argument: symbolp");
	}

	box = Finter(sym, IC_UNSPEC);

	if( NBOUNDP(v2))
		CSYM_BOX(box)->value = Feval( v1 );
	else{
		if( STRINGP(v2)){
			ds = v2;
			val = v1;
		}else if( STRINGP(v1)){
			ds = v1;
			val = v2;
		}else{
			/* default docstr then val */
			/* this should pro'ly be an error */
			val = v2;
			ds  = v1;
		}
		CSYM_BOX(box)->value = Feval( val );

#ifndef NO_DOCSTRINGS		
		ods = Fassq(sym_docstring, CSYM_BOX(box)->props);
		if( ods != IC_FALSE ){
			CDR(ods) = Feval(ds);
		}else{
			CSYM_BOX(box)->props = Fcons(
				Fcons( sym_docstring,
				      Feval(ds))
				, CSYM_BOX(box)->props);
		}
#endif		
	}
	return IC_UNSPEC;
}

DEFUN("and", Fand, Sand, 1,1,0,1, "(and value...) boolean and",
      (Obj a))
{
	Obj s;
	Obj r = IC_TRUE;

	for(s=a; NNULLP(s); s=CDR(s)){
		
		if( NCONSP(s)  )
			return JLERROR(Sand.name, s, "Invalid parameter list");

		r = Feval( CAR(s) );
		if( FALSEP(r)) break;
	}
	return r;
}

DEFUN("or", For, Sor, 1,1,0,1, "(or value...) boolean or",
      (Obj a))
{
	Obj s;
	Obj r = IC_FALSE;

	for(s=a; NNULLP(s); s=CDR(s)){
		
		if( NCONSP(s)  )
			return JLERROR(Sor.name, s, "Invalid parameter list");

		r = Feval( CAR(s) );
		if( ! FALSEP(r)) break;
	}
	return r;
}

DEFUN("not", Fnot, Snot, 1,1,1,0, "(not value) boolean not",
      (Obj a))
{

	return FALSEP(a) ? IC_TRUE : IC_FALSE;
}

DEFUN("quit", Fquit, Squit, 0,1,1,0, "(quit [value]) All done!",
      (Obj a))
{
	int ev = 0;

	Frunhooks(maksym("quit-hooks"));
	if( INUMP(a) ) ev = CINT(a);
	exit( ev );

	/* not reached */
	return IC_UNDEF;
}


DEFUN("while", Fwhile, Swhile, 1,1, 0,1,
      "(while test body...) iterative looping construct",
      (Obj a))
{
	Obj cond = CAR(a), body = CDR(a);
	Obj b;
	
	while( !FALSEP( Feval( cond ))){

		b = body;
		/* execute body */
		while( CONSP( b )){

			Feval( CAR(b) );
			b = CDR(b);
		}
	}
	return IC_FALSE;
}

DEFUN("eq", Feq, Seq, 2,2, 1,0,
      "(eq a b) are these the same?",
      (Obj a, Obj b))
{
	return (a == b)? IC_TRUE : IC_FALSE;
}

DEFUN("eqv", Feqv, Seqv, 2,2,1,0,
      "(eqv a b) are these equivalent?",
      (Obj a, Obj b))
{

	if( TYPEOFX(a) != TYPEOFX(b) ) return IC_FALSE;
	switch( TYPEOFX(a)){
	  case TPVF_CONS:
		return Feq(a,b);

	  default:
		return Fequal(a,b);
	}
}

DEFUN("equal", Fequal, Sequal, 2,2, 1,0,
      "(equal a b) are these equivalent?",
      (Obj a, Obj b))
{
	Obj (*func)(Obj,Obj);
	
	if( TYPEOFX(a) != TYPEOFX(b) ) return IC_FALSE;

	if( IMMEDP(a) ) return Feq(a, b);

	func = jlisp_vtbl[ TYPEOFX(a) ].equal;

	if(!func) return Feq(a, b);
	return func(a, b);
}

DEFUN("progn", Fprogn, Sprogn, 1,1,0,1,
      "(progn body...) eval body",
      (Obj a))
{
	Obj s, r=IC_NIL;
	
	for(s=a; NNULLP(s); s=CDR(s)){
		
		if( NCONSP(s)  )
			return JLERROR(Sprogn.name, s, "Invalid parameter list");

		r = Feval( CAR(s) );
	}
	return r;
}

Obj let_internal(Obj a, Obj env, char *fnc){
	Obj vars = CAR(a), body = CDR(a);
	Obj s, p, box, val;
	
	/* inter our new vars */
	for(s=vars; NNULLP(s); s=CDR(s)){

		if( NCONSP(s) )
			return JLERROR(fnc, vars, "WTA: consp");
		p = CAR(s);

		if( SYMBOLP(p) || SYMBOXP(p) ){
			box = Finter(p, CAR(env));
			CSYM_BOX(box)->value = IC_NIL;
		} else if( CONSP(p) && (SYMBOLP( CAR(p)) || SYMBOXP(CAR(p)))){
			if( NULLP(CDR(p)) || NNULLP( CDDR(p)))
				return JLERROR(fnc, p, "not of the form: symbol | (symbol value)");
			/* we calculate the value before we inter the symbol */
			val = Feval( CADR(p));
			box = Finter( CAR(p), CAR(env));
			CSYM_BOX(box)->value = val;
		}else{
			return JLERROR(fnc, p, "WTA: what are you thinking?");
		}
	}

	envcurr = env;
	return Fprogn( body );
}

DEFUN("let", Flet, Slet, 1,1, 0,1,
      "(let ( {(sym value)|sym}...) body...) the let form",
      (Obj a))
{
	Obj envb, envpre = envcurr, envlpre = envlist;
	Obj result;

	/* push the new env, then remove it.
	   let let_internal inter into the orphaned part while
	   looking things up without it

	   we hook up both to envlist to protect from gc
	*/
	
	pushenv();
	envlist = Fcons(envcurr, envlist);
	envb = envcurr;
	envcurr = envpre;

	result = let_internal(a, envb, Slet.name);
	envcurr = envpre;
	envlist = envlpre;
	return result;
}

DEFUN("let*", Fletstar, Sletstar, 1,1, 0,1,
      "(let* ( {(sym value)|sym}...) body...) the let* form",
      (Obj a))
{
	Obj result;
	
	pushenv();

	result = let_internal(a, envcurr, Sletstar.name);
	popenv();
	return result;
}

DEFUN("if", Fif, Sif, 1,1, 0,1,
      "(if test then else...) conditional form",
      (Obj a))
{

	if( NCONSP(a) || NCONSP( CDR(a)) )
		return JLERROR(Sif.name, a, "WTA: I see no list here");

	if( Feval( CAR(a)) != IC_FALSE )
		return Feval( CADR(a));

	return Fprogn( CDDR(a) );
}


DEFUN("length", Flength, Slength, 1,1,1,0,
      "(length obj) how long is this thing?",
      (Obj a))
{
	int l=0;
	
	if( VECTORP(a) || STRINGP(a) || ENVECP(a) || STRUCTP(a) ){
		return MAKINT( CLENGTH(a) );
	}else if( CONSP(a)){
		for(l=0; CONSP(a); a=CDR(a)) l++;
		return MAKINT(l);
	}else if(NULLP(a)) {
		return MAKINT(0);
	}else{
		return JLERROR(Slength.name, a, "WTA: string or vector p");
	}
}

DEFUN("nth", Fnth, Snth, 2,2,1,0,
      "(nth obj index) snarf the specified nth element",
      (Obj a, Obj n))
{
	int nth;

	if( ! INUMP(n) )
		return JLERROR(Snth.name, n, "WTA: intp");
	nth = CINT(n);

	if( VECTORP(a) || ENVECP(a) || STRUCTP(a)){
		if( nth >= CLENGTH(a))
			return IC_NIL;
		return CVECTOR(a)[nth];
	}
	if( STRINGP(a)){
		if( nth >= CLENGTH(a))
			return MAKCHAR(0);
		return MAKCHAR( CCHARS(a)[nth] );
	}
	
	if( NULLP(a)) return IC_NIL;
	if( CONSP(a)){
		for( ; nth && CONSP(CDR(a)); a=CDR(a)) nth--;
		if( !nth && CONSP(a)) return CAR(a);
		return IC_NIL;
	}
	return JLERROR(Snth.name, a, "WTA: indexable-type-p");
}

DEFUN("set-nth!", Fset_nth,Sset_nth, 3,3,1,0,
      "(set-nth! obj index value) set the specified element",
      (Obj a, Obj n, Obj v))
{
	int nth;
	
	if( ! INUMP(n) )
		return JLERROR(Sset_nth.name, n, "WTA: intp");
	nth = CINT(n);

	if( VECTORP(a) || ENVECP(a) || STRUCTP(a)){
		if( nth >= CLENGTH(a))
			return IC_UNSPEC;	/* or error ? */
		return CVECTOR(a)[nth] = v;
	}else if( STRINGP(a)){
		if( nth >= CRLENGTH(a))
			return JLERROR(Sset_nth.name, a, "index past end of string or read only string");
		if( !ICHARP(v))
			return JLERROR(Sset_nth.name, v, "WTA: string contains only characters");
		return CCHARS(a)[nth] = CCHAR(v);
	}else if( CONSP(a)){
		for(nth--; nth && CONSP(CDR(a)); a=CDR(a)) nth--;
		if( !nth && CONSP(a))
			return CAR(a) = v;
	}
	return IC_UNSPEC;
	
}

DEFUN("cond", Fcond, Scond, 1,1,0,1,
      "(cond (test body)...) evaluate each test until one is not #f\n"
      "then the body is evaluated and the result returned"
      "or #f is returned if none matched",
      (Obj cls))
{
	Obj cond, body, result;

	while( CONSP(cls) ){
		cond = CAAR( cls );
		body = CDAR( cls );
		result = Feval( cond );
		if( ! FALSEP(result))
			if( NNULLP( body )) return Fprogn( body );
			else return result;
		cls = CDR( cls );
	}
	return IC_FALSE;
}

DEFUN("case", Fcase, Scase, 1,1,0,1,
      "(case key ((obj...) body...)...) evaluates key, and looks for a matching (eqv)\n"
      "obj, if a match is found, the matching body is evaluted and the result returned\n"
      "if no match is found the result is unspecified, #t will match anything (else)",
      (Obj lst))
{
	Obj key = CAR(lst);
	Obj cls, objl, body;

	key = Feval(key);
	lst = CDR(lst);
	
	while( CONSP(lst)){
		cls = CAR(lst);
		objl= CAR(cls);
		body= CDR(cls);

		if( CONSP(objl)){
			if( Fmemv(key, objl)!=IC_FALSE){
			     	     /* a match */
				if( NNULLP(body))
					return Fprogn( body );
				else return IC_NIL;
			}
		}else if( Feqv(objl,key)!=IC_FALSE || objl==IC_TRUE ){
			if( NNULLP(body))
				return Fprogn( body);
			return IC_NIL;
		}
		lst = CDR(lst);
	}
	return IC_UNSPEC;
}

DEFUN("unspecified-object", Funspec_obj, Sunspec_obj, 0,0,1,0,
      "(unspecified-object) return the #<unspecified> object",
      ())
{
	return IC_UNSPEC;
}

DEFUN("undefined-object", Fundef_obj, Sundef_obj, 0,0,1,0,
      "(undefined-object) return the #<undefined> object",
      ())
{
	return IC_UNDEF;
}

DEFUN("eof-object", Feof_obj, Seof_obj, 0,0,1,0,
      "(eof-object)  return the #<EOF> object",
      ())
{
	return IC_EOF;
}

DEFUN("int->char", Fint_char, Sint_char, 1,1,1,0,
      "(int->char i) convert int to char",
      (Obj a))
{
	if(INUMP(a)) return MAKCHAR( CINT(a) );
	else return MAKCHAR( 0 );
}
DEFUN("char->int", Fchar_int, Schar_int, 1,1,1,0,
      "(char->int c) convert char to int",
      (Obj a))
{
	if(ICHARP(a)) return MAKINT( CCHAR(a) );
	else return MAKINT( 0 );
}


DEFUN("backquote", Fbquote, Sbquote, 1,1,0,0,
      "`expr mostly quote expr",
      (Obj a))
{
	Obj s,e,p;
	Obj nl = Fcopylist(a);
	
	for(s=nl; CONSP(s); s=CDR(s)){
		e = CAR(s);
		if( CONSP(e)){
			if( Feq(CAR(e), sym_bq_comma)==IC_TRUE){
				     /* strip off ',' and eval it */
				CAR(s) = Feval( CAR(CDR(e)));
			}else if( Feq(CAR(e), sym_bq_comma_at)==IC_TRUE){
				     /* strip off ',@' eval and splice */
				e = Feval( CADR(e) );
				if( CONSP(e)){
					     /* splice */
					CAR(s) = CAR(e);
					     /* find tail */

					if( NNULLP( CDR(e))){
						for(p=CDR(e);CONSP(p) && NNULLP(CDR(p));p=CDR(p));
						CDR(p) = CDR(s);
						CDR(s) = CDR(e);
						s = p;
					}
				}else{
					CAR(s) = e;
				}
			}else if( Feq(CAR(e), sym_bquote)==IC_TRUE){
				;
			}else{
				     /* recurse */
				CAR(s) = Fbquote(e);
			}

		}
	}
	return nl;
}

DEFUN("unquote", Fbq_comma, Sbq_comma, 1,1,0,0,
      ",expr unquote expr within a backquoted expr",
      (Obj a))
{
	return JLERROR(",", a, ", not within `");
}

DEFUN("unquote-splice", Fbg_comma_at, Sbq_comma_at, 1,1,0,0,
      ",@expr unquote and splice within a backquoted expr",
      (Obj a))
{
	return JLERROR(",@", a, ",@ not within `");
}

DEFUN("hashq", Fhashq, Shashq, 1,1,1,0,
      "(hashq object) returns an integer hash value\n"
      "such that if 2 objects are eq they will have the same hash value",
      (Obj obj))
{
	return MAKINT( (IntType)obj );
}

DEFUN("hashv", Fhashv, Shashv, 1,1,1,0,
      "(hashv object) returns an integer hash value\n"
      "such that if 2 objects are eqv they will have the same hash value",
      (Obj obj))
{
	int i=0, l=0;
	
	if( IMMEDP(obj)) return MAKINT( (IntType)obj);
	if( STRINGP(obj)){
		l = CLENGTH(obj);
		while(l){
			i = i << 1 ^ CCHARS(obj)[--l];
		}
		return MAKINT( i<0 ? -i : i );
	}
	if( VECTORP(obj))
		return Fhashv( CVECTOR(obj)[0] );
	if( CONSP(obj))
		return Fhashv( CAR(obj));

	return MAKINT( 918273645 );
}


void dbgprn(Obj a){
	Fdisplay(a, IC_UNSPEC);
}
