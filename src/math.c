
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: math.c,v 2.2 1997/05/30 00:28:31 jaw Exp $";
#endif


#include <jlisp.h>
#include <math.h>

extern Obj makfloat(float);
extern Obj makdbl(double);
extern Obj big_plus_minus(Obj, Obj, int), big_multiply(Obj, Obj), big_divide(Obj, Obj, int);
extern Obj big_logical(Obj, Obj, int, char*), big_shl(Obj, Obj, char*), big_shr(Obj, Obj, char*);
extern big_log(Obj, Obj);
extern Obj cdouble_to_big(double);
extern IntType big_to_cint(Obj);
extern int big_compare(Obj, Obj);
extern double big_to_cdouble(Obj);
extern Obj big_invert(Obj);
Obj cint_to_big(IntType);
Obj Fto_bignum(Obj);

double dblof(Obj a){

	if(ICHARP(a))  return CCHAR(a);
	if(INUMP(a))   return CINT(a);
	if(FLOATP(a))  return CFLOAT(a);
	if(DOUBLEP(a)) return CDOUBLE(a);
#ifndef NO_BIGNUMS	
	if(BIGNUMP(a)) return big_to_cdouble(a);
#endif
	return 0.0;
}
IntType intof(Obj a){

	if(ICHARP(a))  return CCHAR(a);
	if(INUMP(a))   return CINT(a);
	if(FLOATP(a))  return CFLOAT(a);
	if(DOUBLEP(a)) return CDOUBLE(a);
#ifndef NO_BIGNUMS
	if(BIGNUMP(a)) return big_to_cint(a);
#endif
	return 0;
}
float fltof(Obj a){

	if(ICHARP(a))  return CCHAR(a);
	if(INUMP(a))   return CINT(a);
	if(FLOATP(a))  return CFLOAT(a);
	if(DOUBLEP(a)) return CDOUBLE(a);
#ifndef NO_BIGNUMS
	if(BIGNUMP(a)) return big_to_cdouble(a);
#endif
	return 0;
}

Obj simplemath(Obj a, Obj b, int op, char *name){
	int ta = TYPEOFX(a), tb = TYPEOFX(b);
	int mt;
	IntType th, tl;
	int ck, cmp;
	float tf;
	double td;
	Obj tv, bn;
	mt = MAX(ta, tb);

	/* this depends on TYPEOFX (int) < (bignum) < (float) < (double)
	   [see jlisp.h]

	   and is contaguous: int -> bignum -> float -> double
	*/
	
	switch( mt ){
	  case TPVF_IMMED:
		  if( !INUMP(a) )
			  a = MAKINT(intof(a));
		  if( !INUMP(b))
			  b = MAKINT(intof(b));
		  
		  /* both args are int */
		switch (op ){
		  case '=':
			return a == b ? IC_TRUE : IC_FALSE;
		  case '<':
			return a < b  ? IC_TRUE : IC_FALSE;
		  case '>':
			return a > b  ? IC_TRUE : IC_FALSE;
		  case 'g':
			return a >= b ? IC_TRUE : IC_FALSE;
		  case 'l':
			return a <= b ? IC_TRUE : IC_FALSE;
		  case '+':
			tv = a + b - MAKINT(0);
			ck = tv - b + MAKINT(0) == a;
			break;
		  case '-':
			tv = a - b + MAKINT(0);
			ck = tv + b - MAKINT(0) == a;
			break;
		  case '*':
			tv = MAKINT((th=CINT(a)) * (tl=CINT(b)));
			ck = !tl || (CINT(tv)/tl == th);
			break;
		  case '%':
			if( (tl=CINT(b)) == 0)
				goto divide_by_zero;
			tv = MAKINT( (CINT(a)) % tl );
			ck = 1;
			break;
		  case '/':
			if( (tl=CINT(b)) == 0)
				goto divide_by_zero;
			tv = MAKINT( (CINT(a)) / tl );
			ck = 1;
			break;
		}
#ifndef NO_BIGNUMS
		/* check to see if we ought convert int->bignum */
		if( ! ck ){
			a = cint_to_big( CINT(a) );
			b = cint_to_big( CINT(b) );
			goto redo_as_big;
		}
#endif
		return tv;

	  case TPV_FLOAT:
		switch(op){
		  case '=':
			return fltof(a) == fltof(b) ? IC_TRUE : IC_FALSE;
		  case '<':
			return fltof(a) < fltof(b)  ? IC_TRUE : IC_FALSE;
		  case '>':
			return fltof(a) > fltof(b)  ? IC_TRUE : IC_FALSE;
		  case 'g':
			return fltof(a) >= fltof(b) ? IC_TRUE : IC_FALSE;
		  case 'l':
			return fltof(a) <= fltof(b) ? IC_TRUE : IC_FALSE;
		  case '+':
			tf = fltof(a) + fltof(b);
			break;
		  case '-':
			tf = fltof(a) - fltof(b);
			break;
		  case '*':
			tf = fltof(a) * fltof(b);
			break;
		  case '%':
			/* not a well defined operation for non-integers */
			if( (tf=fltof(b)) == 0.0)
				goto divide_by_zero;
#ifndef NO_BIGNUMS			
			return big_divide( Fto_bignum(a), Fto_bignum(b), op);
#else
			tf = intof(a) % tf;
#endif			
			break;
		  case '/':
			if( (tf=fltof(b)) == 0.0)
				goto divide_by_zero;
			tf = fltof(a) / tf;
			break;
		}
		return makfloat( tf );
		
	  case TPV_DOUBLE:
		switch (op){
		  case '=':
			return dblof(a) == dblof(b) ? IC_TRUE : IC_FALSE;
		  case '<':
			return dblof(a) < dblof(b)  ? IC_TRUE : IC_FALSE;
		  case '>':
			return dblof(a) > dblof(b)  ? IC_TRUE : IC_FALSE;
		  case 'g':
			return dblof(a) >= dblof(b) ? IC_TRUE : IC_FALSE;
		  case 'l':
			return dblof(a) <= dblof(b) ? IC_TRUE : IC_FALSE;
		  case '+':
			td = dblof(a) + dblof(b);
			break;
		  case '-':
			td = dblof(a) - dblof(b);
			break;
		  case '*':
			td = dblof(a) * dblof(b);
			break;
		  case '%':
			/* not a well defined operation for non-integers */
			if( (td=dblof(b)) == 0.0)
				goto divide_by_zero;
#ifndef NO_BIGNUMS			
			return big_divide( Fto_bignum(a), Fto_bignum(b), op);
#else
			tf = intof(a) % td;
#endif	
			break;
		  case '/':
			if( (td=dblof(b)) == 0.0)
				goto divide_by_zero;
			td = dblof(a) / td;
			break;
		}
		return makdbl( td );

#ifndef NO_BIGNUMS		
	  case TPV_BIGNUM:
	  redo_as_big:
		switch (op){
		  case '=':
		  case '<':
		  case '>':
		  case 'g':
		  case 'l':
			if( ta == tb)
				cmp = big_compare(a, b);
			else if( ta < tb)
				cmp = big_compare(cint_to_big(CINT(a)), b);
			else
				cmp = big_compare(a, cint_to_big(CINT(b)));
			switch( op ){
			  case '=':
				return cmp==0  ? IC_TRUE : IC_FALSE;
			  case '>':
				return cmp==1  ? IC_TRUE : IC_FALSE;
			  case '<':
				return cmp==-1 ? IC_TRUE : IC_FALSE;
			  case 'g':
				return cmp!=-1 ? IC_TRUE : IC_FALSE;
			  case 'l':
				return cmp!=1  ? IC_TRUE : IC_FALSE;
			}
			
		  case '+':
			if( ta == tb)
				bn = big_plus_minus(a, b, 0);
			else if( ta < tb)
				bn = big_plus_minus(cint_to_big(CINT(a)), b, 0);
			else
				bn = big_plus_minus(a, cint_to_big(CINT(b)), 0);
			break;
		  case '-':
			if( ta == tb)
				bn = big_plus_minus(a, b, 1);
			else if( ta < tb)
				bn = big_plus_minus(cint_to_big(CINT(a)), b, 1);
			else
				bn = big_plus_minus(a, cint_to_big(CINT(b)), 1);
			break;
		  case '*':
			if( ta == tb)
				bn = big_multiply(a, b);
			else if( ta < tb)
				bn = big_multiply(cint_to_big(CINT(a)), b);	
			else
				bn = big_multiply(a, cint_to_big(CINT(b)));
			break;
		  case '/':
		  case '%':
			if( ta == tb ){
				bn = big_divide(a, b, op);
			}else if( ta < tb)
				bn = big_divide(cint_to_big(CINT(a)), b, op);	
			else
				bn = big_divide(a, cint_to_big(CINT(b)), op);
			break;
		}
		/* should we convert -> int ? */
		if( CLENGTH(bn) < 2 || CLENGTH(bn)==2 && CBIGNUM(bn)[1] < 4096 )
			/* 4096 = 2**12, 12 + 16 = 28, ints have 30 bits */
			bn = MAKINT( big_to_cint(bn) );
		
		return bn;
#endif
	  case TPV_COMPLEX:

	  default:
		return JLERROR(name, Fcons(a, b), "WTA: numberp");
	}
	
  divide_by_zero:
	return JLERROR(name, b, "divide by zero");
}

DEFUN("+", Fplus, Splus, 2,2,1,0, "(+ n m) addition",
      (Obj a, Obj b))
{
	return simplemath(a,b, '+', Splus.name);
}

DEFUN("-", Fminus, Sminus, 2,2,1,0, "(- n m) subtraction",
      (Obj a, Obj b))
{
	return simplemath(a,b, '-', Sminus.name);
}

DEFUN("*", Ftimes, Stimes, 2,2,1,0, "(* n m) multiplication",
      (Obj a, Obj b))
{
	return simplemath(a,b, '*', Stimes.name);
}

DEFUN("/", Fdivide, Sdivide, 2,2,1,0, "(/ n m) division",
      (Obj a, Obj b))
{
	return simplemath(a,b, '/', Sdivide.name);
}

DEFUN("%", Fmod, Smod, 2,2,1,0,
      "(% n1 n2) n1 modulo n2",
      (Obj a, Obj b))
{
	return simplemath(a, b, '%', Smod.name);
}

DEFUN("=", Fmequal, Smequal, 2,2,1,0,
      "(= n1 n2) Are two numbers equal?",
      (Obj a, Obj b))
{
	return simplemath(a, b, '=', Smequal.name);
}
DEFUN(">", Fgreater, Sgreater, 2,2,1,0,
      "(> n1 n2) Is n1 greater than n2?",
      (Obj a, Obj b))
{
	return simplemath(a, b, '>', Sgreater.name);
}
DEFUN("<", Fless, Sless, 2,2,1,0,
      "(< n1, n2) Is n1 less than n2?",
      (Obj a, Obj b))
{
	return simplemath(a, b, '<', Sless.name);
}
DEFUN(">=", Fgrequal, Sgrequal, 2,2,1,0,
      "(>= n1 n2) Is n1 greater or equal to n2?",
      (Obj a, Obj b))
{
	return simplemath(a,b,'g',Sgrequal.name);
}
DEFUN("<=", Flsequal, Slsequal, 2,2,1,0,
      "(<= n1 n2) Is n1 less or equal than n2?",
      (Obj a, Obj b))
{
	return simplemath(a,b,'l',Slsequal.name);
}

Obj logicalmath(Obj a, Obj b, int op, char *name){
	/* bignum contagious */
	int ta = MIN( TYPEOFX(a), TPV_BIGNUM),
		tb = MIN( TYPEOFX(b), TPV_BIGNUM);
	int mt = MAX( ta, tb);
	Obj t, foo, bar;
	
	switch ( mt ){
	  case TPVF_IMMED:
		switch ( op ){
		  case '<':
			if( CINT(b) > 28 )
				goto redo_as_big;
			t = MAKINT( CINT(a) << CINT(b) );
			if( CINT(t) >> CINT(b) != CINT(a))
				goto redo_as_big;
			return t;
		  case '>':
			return MAKINT( intof(a) >> intof(b) );
		  case '&':
			return MAKINT( intof(a) & intof(b) );
		  case '|':
			return MAKINT( intof(a) | intof(b) );
		  case '^':
			return MAKINT( intof(a) ^ intof(b) );
		}
#ifndef NO_BIGNUMS		
	  case TPV_BIGNUM:
	  redo_as_big:
		if(! BIGNUMP(a)) foo = Fto_bignum(a);
		else foo = a;

		if(! BIGNUMP(b)) bar = Fto_bignum(b);
		else bar = b;

		switch ( op ){
		  case '<':
			return big_shl(foo, bar, name);
		  case '>':
			return big_shr(foo, bar, name);
		  default:
			return big_logical(foo, bar, op, name);
		}
#endif		
	}
	return JLERROR(name, a, "WTA: what the...?");
}

DEFUN("<<", Fshl, Sshl, 2,2,1,0, "(<< n m) shift left)",
      (Obj a, Obj b))
{
	return logicalmath(a, b, '<', Sshl.name);
}

DEFUN(">>", Fshr, Sshr, 2,2,1,0, "(>> n m) shift right)",
      (Obj a, Obj b))
{
	return logicalmath(a, b, '>', Sshr.name);
}

DEFUN("&", Fland, Sland, 2,2,1,0, "(& n m) bitwise and",
      (Obj a, Obj b))
{
	return logicalmath(a, b, '&', Sland.name);
}

DEFUN("|", Flor, Slor, 2,2,1,0, "(| n m) bitwise or",
      (Obj a, Obj b))
{
	return logicalmath(a, b, '|', Slor.name);
}

DEFUN("^", Flxor, Slxor, 2,2,1,0, "(^ n m) bitwise xor",
      (Obj a, Obj b))
{
	return logicalmath(a, b, '^', Slxor.name);
}

DEFUN("~", Flnot, Slnot, 1,1,1,0, "(~ n) bitwise not",
      (Obj a))
{
	if(! BIGNUMP(a))
		return MAKINT( ~ intof(a));
	return big_invert(a);
}

#if 0
DEFUN("real", Freal, Sreal, 1,1,1,0, "(real c) return the real part of a complex number",
	(Obj a))
{

	if(COMPLEXP(a)) return makdbl( CCOMPLEX(a).re );
	if(INUMP(a) || FLOATP(a) || DOUBLEP(a)) return a;

	return JLERROR(Sreal.name, a, "WTA: numberp");
}

DEFUN("imag", Fimag, Simag, 1,1,1,0, "(imag c) return the imaginary part of a complex number",
	(Obj a))
{
	
	if(COMPLEXP(a)) return makdbl( CCOMPLEX(a).im );
	if(INUMP(a) || FLOATP(a) || DOUBLEP(a)) return makdbl( (double)0.0);

	return JLERROR(Simag.name, a, "WTA: numberp");
}
#endif


Obj trancend1(Obj a, double (*fnc)(double), char* fnm){
	double d;

	if(INUMP(a)||FLOATP(a)||DOUBLEP(a)||COMPLEXP(a)||BIGNUMP(a))
		d = dblof(a);
	else
		return JLERROR(fnm, a, "WTA: numberp");

	return makdbl( fnc(d) );
}

Obj trancend2(Obj a, Obj b, double (*fnc)(double, double), char* fnm){
	double d1, d2;

	if(INUMP(a)||FLOATP(a)||DOUBLEP(a)||COMPLEXP(a)||BIGNUMP(a))
		d2 = dblof(a);
	else
		return JLERROR(fnm, a, "WTA: numberp");

	if(INUMP(b)||FLOATP(b)||DOUBLEP(b)||COMPLEXP(b)||BIGNUMP(b))
		d2 = dblof(b);
	else
		return JLERROR(fnm, b, "WTA: numberp");

	return makdbl( fnc(d1,d2) );
}

DEFUN("exp", Fexp, Sexp, 1,2,1,0,
      "(exp  d [base]) take e [or a specified base] raised to the d power",
      (Obj a, Obj b))
{
	double e, p=0.0;
	
	if( 1 || ! BIGNUMP(a)){
		e = dblof(a);
		if( BOUNDP(b))
			p = dblof(b);
		if( p==0.0)
			return makdbl( exp( e ));
		else 
			return makdbl( pow(p, e));	/* = exp( e * log(p)) */
	}
				
	/* how to handle bignums? */
	return IC_UNDEF;
}

DEFUN("log", Flog, Slog, 1,2,1,0, "(log x [base]) take the natural [or base] logarithm",
      (Obj a, Obj b))
{
	double e,l=0.0;
	
	if(! BIGNUMP(a)){
		e = dblof(a);
		if( BOUNDP(b))
			l = dblof(b);
		if( l==0.0)
			return makdbl( log( e ));
		else
			return makdbl( log( e) / log( l ));
	}

#ifndef NO_BIGNUMS	
	return big_log( a, b );
#else
	return IC_FALSE;	/* cant happen */
#endif	
}


DEFUN("floor", Ffloor, Sfloor, 1,1,1,0, "take the floor",
      (Obj a))
{
	return MAKINT( (int)CDOUBLE(trancend1(a, floor, Sfloor.name)) );
}
DEFUN("ceil", Fceil, Sceil, 1,1,1,0, "take the ceiling",
      (Obj a))
{
	return MAKINT( (int)CDOUBLE(trancend1(a, ceil, Sceil.name)) );
}

DEFUN("random", Frandom, Srandom, 0,0,1,0,
      "(random) return a random number",
      ())
{
#ifndef INFERIOR_LIBM
	return MAKINT(random() & 0xFFFFFFF);
#else
	return MAKINT(rand() & 0xFFFFFFF);
#endif	
}

DEFUN("->int", Fto_int, Sto_int, 1,1,1,0,
      "(->int n) convert to integer",
	(Obj n))
{
	return MAKINT( intof(n));
}

DEFUN("->float", Fto_float, Sto_float, 1,1,1,0,
      "(->float n) convert to single precision float",
	(Obj n))
{
	return makfloat( fltof(n));
}

DEFUN("->double", Fto_double, Sto_double, 1,1,1,0,
      "(->double n) convert to double precision float",
	(Obj n))
{
	return makdbl( dblof(n));
}

DEFUN("->bignum", Fto_bignum, Sto_bignum, 1,1,1,0,
      "(->bignum n) convert to a bignum",
      (Obj n))
{
#ifndef NO_BIGNUMS
	if( BIGNUMP(n)) return n;
	if( INUMP(n))   return cint_to_big( CINT(n));
	if( FLOATP(n))  return cdouble_to_big( CFLOAT(n));
	if( DOUBLEP(n)) return cdouble_to_big( CDOUBLE(n));

	return cint_to_big(0);
#else
	return IC_UNDEF;
#endif
}


