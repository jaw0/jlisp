
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: jlisp.c,v 2.2 1997/05/30 00:28:26 jaw Exp $";
#endif

#ifdef __GNUC__
#	define	__USE_FIXED_PROTOTYPES__
#endif

#include <jlisp.h>
#include <stdio.h>
#include <setjmp.h>
#include <string.h>

extern void gc_mark(Obj);

Obj internal_gc_protect = IC_NIL;

void mark0(Obj), marksymbx(Obj), markvect(Obj), markcdr(Obj), markport(Obj), markctfrm(Obj);
void markweak(Obj), markenv(Obj);

int free0(Obj), freecdr(Obj), freeerror(Obj), freeport(Obj), freestr(Obj);

Obj eqcdr(Obj,Obj), eqdbl(Obj,Obj), eqcmplx(Obj,Obj), eqstr(Obj,Obj), eqvect(Obj,Obj), eqbign(Obj,Obj);
Obj eqsym(Obj,Obj), eqcdr(Obj,Obj), eqsymbox(Obj,Obj), eqport(Obj,Obj), eqcons(Obj,Obj);
Obj eqweak(Obj,Obj);

extern int prnvect(Obj,Obj,int), prnbign(Obj,Obj,int), prncmplx(Obj,Obj,int),
  prnport(Obj,Obj,int), prnenvec(Obj,Obj,int), prnstrct(Obj,Obj,int), prnsym(Obj,Obj,int),
  prncons(Obj,Obj,int), prnsymbx(Obj,Obj,int), prnflt(Obj,Obj,int), prndbl(Obj,Obj,int),
  prnstr(Obj,Obj,int), prnfunc(Obj,Obj,int), prnmacr(Obj,Obj,int), prnccode(Obj,Obj,int);

DEFVAR(".version", Vversion, ".version what version are we using",
       makstr_c("Jeff's Lisp Version " QUOTIFY(VERSION_MAJOR)))
     
DEFVAR(".versno", Vversno, ".versno the version number",
       makstr_c( QUOTIFY(VERSION_MAJOR) ))

Obj_Vtbl jlisp_vtbl[] = {
	/* mark,    free,     print,    equalp   name */

	{ 0,        0,        prncons,  eqcons, "cell"		},	/* cons */
#ifdef NO_BIGNUMS
	{ 0, 	    freeerror,0, 	0     , "???"		}, 	/* no bignums */
#else	
	{ mark0,    freecdr,  prnbign,  eqbign, "bignum"	},	/* bignum */
#endif	
	{ mark0,    free0,    prnflt,   eqcdr,  "float"	 	},	/* float */
	{ mark0,    freecdr,  prndbl,   eqdbl,  "double" 	},	/* double */
	{ mark0,    freecdr,  prncmplx, 0,	"complex"       },	/* complex */
	{ mark0,    freestr,  prnstr,   eqstr,  "string"	},	/* string */
	{ markvect, freecdr,  prnvect,  eqvect,	"vector" 	},	/* vector */
	{ mark0,    freestr,  0,        0,	"symbol"  	},	/* symbol */
	{ mark0,    free0,    prnccode, eqcdr, 	"code"		},	/* c code */
	{ marksymbx,freecdr,  0,        0, 	"symbol"	},	/* sym box */	
	{ mark0,    freeerror,0,	0, 	"free"		},	/* a free cell */
	{ mark0,    freeerror,0,	0,	"box"   	},	/* box of cells */
	{ markport, freeport, prnport, 	eqport,	"port"		},	/* io ports */
	{ markcdr,  free0,    prnfunc,	0,	"lambda"	},	/* function */
	{ markcdr,  free0,    prnmacr,	0,	"macro"		},	/* macro */
	{ markctfrm,free0,    0, 	0,	"catch"  	},	/* catch frame */
#ifdef NO_WEAKS
	{0,         freeerror,0,   	0,	"???"		},	/* no weaks */
#else	
	{ markweak, freecdr,  0, 	eqweak, "weak"		},	/* weak */
#endif	
	{ markenv,  freecdr,  prnenvec, eqvect,	"env"		},	/* env */
#ifdef NO_STRUCTS	
	{ 0,	    freeerror,0, 	0,	"???"		},	/* no structs */
#else	
	{ markvect, freecdr,  prnstrct, eqvect,	"struct"	},	/* struct */
#endif	
	{ mark0, free0, 0,0, "???"},				     	/* user1 */
	{ mark0, free0, 0,0, "???"},					/* user2 */	
	{ mark0, free0, 0,0, "???"},					/* user3 */
	{ mark0, free0, 0,0, "???"},					/* user4 */
	{ mark0, free0, 0,0, "???"},					/* user5 */
	{ mark0, free0, 0,0, "???"},					/* user6 */
	{ mark0, free0, 0,0, "???"},					/* user7 */
	{ mark0, free0, 0,0, "???"},					/* user8 */
	/* ... */
	
	{0,0,0,0,"???"}
};

void mark0(Obj a){
	/* nop */
}

void markcdr(Obj a){
	gc_mark( CDR(a) );
}

void marksymbx(Obj a){
	/* mark a sym box -- */
	/* both the value cell and props */

	gc_mark( CSYM_BOX(a)->value );
	gc_mark( CSYM_BOX(a)->props );
	gc_mark( CSYM_BOX(a)->next  );
	/* gc_mark( CSYM_BOX(a)->prev  ); */	/* *must* not go back if using shallow binding */
}

void markenv(Obj a){
	/* mark env */
	markvect(a);
#if 0
	/* it should work, but... */
	int sz = CLENGTH( a );
	Obj *box = CVECTOR(a);
	Obj chn;
	int i;

	for(i=0; i< sz; i++){
		chn = box[i];
		while( NNULLP( chn )){
			gc_mark( chn );
			chn = CSYM_BOX(a)->next;
		}
	}
#endif	
}


void markvect(Obj a){
	/* mark vector */
	int sz = CLENGTH( a );
	Obj *box = CVECTOR(a);
	int i;

	for(i=0; i< sz; i++){
		gc_mark( box[i] );
	}
}

int freestr(Obj a){

	if( (CAR(a)>>12) > 0)
		free( (char*) CDR(a));
	return 1;
}

int free0(Obj a){
	/* nop */
	return 1;
}

int freecdr(Obj a){
	free( (char*) CDR(a) );
	return 1;
}

int freeerror(Obj a){
	int p = CAR(a) >> 12;
	
	printf("Error: Free: ");
	Fdisplay(a, IC_UNSPEC);
	printf("\n");
	printf("(0x%lx . 0x%lx)\n", CAR(a), CDR(a));
	printf("\t{%x, %x, %x, %x }\n",
	       (p>>7)&1,
	       (p>>6)&31,
	       (p>>1)&31,
	       p &1);

/*	JLERROR("free", a, "bad"); */
	return 0;
}

#if 0
Obj maksym(char *sym){
	Obj foo;
	int sigs;
	int l;
	
	foo = makstr(sym);
	l = CAR(foo) >> 12;
	DISABLE( sigs );
	CAR( foo ) = MAKETYPE( TPV_SYMBOL ) | (l<<12);
	RENABLE( sigs );
	
	return foo;
}

Obj maksym_c(char *sym){
	Obj foo;
	int sigs;
	int l;
	
	foo = makstr_c(sym);
	l = CAR(foo) >> 12;	/* will be < 0 */
	DISABLE( sigs );
	CAR( foo ) = MAKETYPE( TPV_SYMBOL ) | (l<<12);
	RENABLE( sigs );
	
	return foo;
}
#endif


Obj makvect(int len){
	Obj foo = newcell(), bar;
	register int i;
	int sigs;

	bar = (Obj)(Obj*)my_malloc(sizeof(Obj)*len);
	DISABLE( sigs );
	CAR( foo ) = MAKETYPE( TPV_VECTOR ) | (len << 12);
	CDR( foo ) = bar;

	for(i=0; i<len; i++){
		CVECTOR(foo)[i] = IC_UNDEF;
	}
	RENABLE( sigs );
	return foo;
}

DEFUN("makevector", Fmakevector,Smakevector,1,1,1,0,
      "(makevector len) create a vector of specified length",
      (Obj a))
{
	register int len;

	if(INUMP(a)) len = CINT(a);
	else if( VECTORP(a) || STRINGP(a) ) len = CLENGTH(a);
	/* size from a list ? */
	else{
		return JLERROR(Smakevector.name,a, "How big?");
	}
	return makvect(len);
}


Obj makfloat(float f){
	Obj foo = newcell();
	int sigs;

	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_FLOAT );
	CDR(foo) = *((Obj*)&f);
	RENABLE( sigs );
	return foo;
}
Obj makdbl(double d){
	Obj foo = newcell(), bar;
	int sigs;

	bar = (Obj)my_malloc(sizeof(double));
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_DOUBLE );
	CDR(foo) = bar;
	*(double*)CDR(foo) = d;
	RENABLE( sigs );
	return foo;
}

#if 0
Obj makcmplx(double r, double i){
	Obj foo = newcell(), bar;
	int sigs;

	bar = (Obj)my_malloc(sizeof(Complex));
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_COMPLEX );
	CDR(foo) = bar;
	RENABLE( sigs );
	
	CCOMPLEX(foo).re = r;
	CCOMPLEX(foo).im = i;
	return foo;
}

DEFUN("makecomplex",Fmakecomplex,Smakecomplex,2,2,1,0,
      "(make-complex r i) return a complex number",
      (Obj r, Obj i))
{
	double rr, ii;
	
	if(INUMP(r)) rr = CINT(r);
	else if(FLOATP(r)) rr = CFLOAT(r);
	else if(DOUBLEP(r))rr = CDOUBLE(r);
	else rr = 0.0;

	if(INUMP(i)) ii = CINT(i);
	else if(FLOATP(i)) ii = CFLOAT(i);
	else if(DOUBLEP(i))ii = CDOUBLE(i);
	else ii = 0.0;

	return IC_NIL;
	/* return makcmplx(rr,ii); */ 
}
#endif

Obj makport( FILE* fp, int rw){
	Obj foo = newcell();
	int sigs;
	
	if( ! fp ) return IC_FALSE;
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_IOPORT ) | (rw <<12);
	CDR(foo) = (Obj)fp;
	RENABLE( sigs );
	return foo;
}

Obj eqstr(Obj a, Obj b){
	int l = CLENGTH(a);

	if( l != CLENGTH(b)) return IC_FALSE;
	
	return strncmp(CCHARS(a), CCHARS(b), l) ? IC_FALSE : IC_TRUE;
}

Obj eqcdr(Obj a, Obj b){

	return CDR(a)==CDR(b) ? IC_TRUE : IC_FALSE;
}

#if 0
Obj eqcmplx(Obj a, Obj b){

	return 
		(CCOMPLEX(a).re == CCOMPLEX(b).re
		 && CCOMPLEX(a).im == CCOMPLEX(b).im) ?  IC_TRUE : IC_FALSE;
}
#endif

Obj eqdbl(Obj a, Obj b){

	return CDOUBLE(a) == CDOUBLE(b) ? IC_TRUE : IC_FALSE;
}

Obj eqvect(Obj a, Obj b){
	int l;

	if( CLENGTH(a) != (l=CLENGTH(b)) )
		return IC_FALSE;

	for(l--;l>=0; l--){
		if( CVECTOR(a)[l] != CVECTOR(a)[l])
			return IC_FALSE;
	}
	return IC_TRUE;
}

Obj eqcons(Obj a, Obj b){

	return ( Fequal(CAR(a), CAR(b)) && Fequal(CDR(a), CDR(b)) )
		? IC_TRUE : IC_FALSE;
}

DEFUN("typeof", Ftypeof, Stypeof, 1,1,1,0,
      "(typeof obj) return string containing the name of the type of object",
      (Obj a))
{
	int t = TYPEOFX(a);
	char *n;
	
	if( t==TPVF_IMMED){
		if( ICONSTP(a)){
			switch( a ){
			  case IC_NIL:		n = "nil";		break;
			  case IC_TRUE:		n = "true";		break;
			  case IC_FALSE:	n = "false";		break;
			  case IC_UNDEF:	n = "undefined";	break;
			  case IC_UNSPEC:	n = "unspecified";	break;
			  case IC_EOF:		n = "eof";		break;
			}
		}else if( ICHARP(a)){
			n = "char";
		}else if( INUMP(a)){
			n = "int";
		}else if( SYMBOLP(a)){
			n = "symbol";
		}else{
			n = "???";
		}
		return makstr_c( n );
	}else{
		return makstr_c( jlisp_vtbl[t].name );
	}
}
