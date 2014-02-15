
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

/*
  $Id: jlisp.h,v 2.2 1997/05/30 00:28:25 jaw Exp $
*/

#ifndef _jlisp_h
#define _jlisp_h

#include <jlconf.h>

#if SIZEOF_LONG == SIZEOF_PTR
 	typedef long IntType;
 	typedef unsigned long UIntType;
#else
#	if SIZEOF_INT == SIZEOF_PTR
 	    	typedef int IntType;
 	    	typedef unsigned int UIntType;
#	else
#		define x(error
#	endif
#endif

typedef IntType Obj;

typedef struct {
	Obj car, cdr;
} Cell;	

typedef struct {
	int id;
	Obj value, props, next, prev;
} Symbox;

typedef struct {
	char *name;
	Obj (*func)();
	int minarg, maxarg;
	int evalp, listp;
	char *doc;
	int flags;
#define    DDF_CONV	1
	char *conv;	/*
			  i	int
			  f	float
			  d	double
			  c	char
			  s	char*
			  v	void*
			*/
} Defun_Decl;

typedef struct {
	void (*mark)(Obj);
	int (*free)(Obj);
	int (*print)(Obj, Obj, int);
	Obj (*equal)(Obj, Obj);
	char *name;
} Obj_Vtbl;

typedef struct Backtrace {
	struct Backtrace *next;
	int dbg_me;
	Obj fncname;
	Obj fncdefn;
	Obj argl;
} Backtrace;


#define CLASS0P(x)	( (x) & 2 )
#define CLASS1P(x)	(!CLASS0P(x))

#define IMMEDP(x)	(CLASS0P(x))
#define INUMP(x)	( ((x)&7)==2 )
#define ICHARP(x)	( ((x)&0x3F)==6)
#define ICONSTP(x)	( ((x)&0x3F)==14)

#define CINT(x)		((IntType)((x) >> 3))
#define MAKINT(i)	((((IntType)(i)) << 3) | 2)
#define CCHAR(x)	((x) >> 6)
#define MAKCHAR(c)	(((c) << 6) | 6)

#define IC_NIL		(( 0 << 6) | 14)	/* () */
#define IC_TRUE		(( 1 << 6) | 14)	/* #t */
#define IC_FALSE	(( 2 << 6) | 14)	/* #f */
#define IC_UNDEF	(( 3 << 6) | 14)	/* undefined-object - to indicate symbols that are not defined */
#define IC_UNSPEC	(( 4 << 6) | 14)	/* unspecified-object - basic default value where nonelse is specified */
#define IC_EOF		(( 5 << 6) | 14)	/* end-of-file-object - gotten at the end of a file */

#define CSYMID(x)	((x) >> 6)
#define SYMIDP(x)	(((x)&0x3F)==22)
#define SYMBOLP(x)	SYMIDP(x)
#define MAKSYM(x)	(((x) << 6) | 22)

#define NULLP(x)	((x)==IC_NIL)
#define NNULLP(x)	((x)!=IC_NIL)
#define BOUNDP(x)	((x)!=IC_UNSPEC)
#define NBOUNDP(x)	((x)==IC_UNSPEC)
#define DEFINEDP(x)	((x)!=IC_UNDEF)
#define NDEFINEDP(x)	((x)==IC_UNDEF)
#define FALSEP(x)	((x)==IC_FALSE)


#define CAR(x)		( ((Cell*)(x))->car)
#define CDR(x)		( ((Cell*)(x))->cdr)
#define CDDR(x)		CDR( CDR(x))
#define CAAR(x)		CAR( CAR(x))
#define CDAR(x)		CDR( CAR(x))
#define CADR(x)		CAR( CDR(x))
#define CADDR(x)	CAR( CDDR(x))
#define CDDDR(x)	CDR( CDDR(x))
#define CONSP(x)	(CLASS1P(x) && ( !(CAR(x)&1)))
#define NCONSP(x)	(! CONSP(x))

#define TYPEMASK	2041
#define TYPEOF(x)	( (CAR(x)&TYPEMASK) >>3)
#define MAKETYPE(t)	(((t)<<3) | 1)
#define SDBIT		4
#define GCBIT		2

#define TPV_BIGNUM	1		/* bignum < float < double < complex */
#define TPV_FLOAT	2		/* makes the math routines simpler... */
#define TPV_DOUBLE	3
#define TPV_COMPLEX	4
#define TPV_STRING	5
#define TPV_VECTOR	6
/* #define TPV_SYMBOL	7 */
#define TPV_C_CODE	8
#define TPV_SYM_BOX	9
#define TPV_FREE_CELL	10
#define TPV_BOX_CELLS	11
#define TPV_IOPORT	12
#define TPV_FUNCTION	13
#define TPV_MACRO	14
#define TPV_CATCHFRM	15
#define TPV_WEAK	16
#define TPV_ENVEC	17
#define TPV_STRUCT	18

#define TPV_USER_BASE	19
#define TPV_USER1	(TPV_USER_BASE + 0)
#define TPV_USER2	(TPV_USER_BASE + 1)
#define TPV_USER3	(TPV_USER_BASE + 2)
#define TPV_USER4	(TPV_USER_BASE + 3)
#define TPV_USER5	(TPV_USER_BASE + 4)
#define TPV_USER6	(TPV_USER_BASE + 5)
#define TPV_USER7	(TPV_USER_BASE + 6)
#define TPV_USER8	(TPV_USER_BASE + 7)
#define TPV_USER9	(TPV_USER_BASE + 8)
#define TPV_USER10	(TPV_USER_BASE + 9)
#define TPV_USER11	(TPV_USER_BASE + 10)
#define TPV_USER12	(TPV_USER_BASE + 11)
/* ... */

#define TPV_LAST	(TPV_USER_BASE + 11)

/* pair of fake values to simplify things */
#define TPVF_IMMED	-1
#define TPVF_CONS	0
/* and an extended typeof */
#define TYPEOFX(x)	(IMMEDP(x) ? TPVF_IMMED :( CONSP(x) ? TPVF_CONS : TYPEOF(x)))

#define FLOATP(x)	(TYPEOFX(x)==TPV_FLOAT)
#define DOUBLEP(x)	(TYPEOFX(x)==TPV_DOUBLE)
#define COMPLEXP(x)	(TYPEOFX(x)==TPV_COMPLEX)
#define STRINGP(x)	(TYPEOFX(x)==TPV_STRING)
#define VECTORP(x)	(TYPEOFX(x)==TPV_VECTOR)
#define BIGNUMP(x)	(TYPEOFX(x)==TPV_BIGNUM)
/* #define SYMBOLP(x)	(TYPEOFX(x)==TPV_SYMBOL) */
#define SYMBOXP(x)	(TYPEOFX(x)==TPV_SYM_BOX)
#define CCODEP(x)	(TYPEOFX(x)==TPV_C_CODE)
#define FREECELLP(x)	(TYPEOFX(x)==TPV_FREE_CELL)
#define BOXCELLP(x)	(TYPEOFX(x)==TPV_BOX_CELLS)
#define IOPORTP(x)	(TYPEOFX(x)==TPV_IOPORT)
#define FUNCTIONP(x)	(TYPEOFX(x)==TPV_FUNCTION)
#define MACROP(x)	(TYPEOFX(x)==TPV_MACRO)
#define ENVECP(x)	(TYPEOFX(x)==TPV_ENVEC)
#define STRUCTP(x)	(TYPEOFX(x)==TPV_STRUCT)

#define NSELFEVALP(x)	(SYMBOLP(x) || CONSP(x) || SYMBOXP(x))
#define SELFEVALP(x)	(! NSELFEVAL(x))

#define READABLE	1
#define WRITABLE	2
#define RPORTP(x)	(IOPORTP(x) && ((CAR(x)>>12) & READABLE))
#define WPORTP(x)	(IOPORTP(x) && ((CAR(x)>>12) & WRITABLE))
#define SUBPORT(x)	((CAR(x)>>14) & 15)
#define SUBSUBPORT(x)	((CAR(x)>>18) & 31)

#define CFLOAT(x)	(*((float*)&CDR(x)))
#define CDOUBLE(x)	(*((double*)CDR(x)))
#define CCHARS(x)	((char*)CDR(x))
#define CVECTOR(x)	((Obj*)CDR(x))
#define CSYM_BOX(x)	((Symbox*)CDR(x))
#define CBIGNUM(x)	((unsigned short*)CDR(x))
#define CFILEPTR(x)	((FILE*)CDR(x))
#define CSOCKET(x)	((int)CDR(x))
#define CENVVEC(x)	((Obj*)CDR(x))
#define CSTRUCT(x)	((Obj*)CDR(x))

#define CLENGTH(x)	ABS (CAR(x) >> 12)
#define CRLENGTH(x)     (CAR(x) >> 12)

#define CFREENEXT(x)	(CDR(x))

#define CBOXSIZE(x)       	(((Cell*)CDR(x))[0].car)
#define CBOXPTR(x)		((Cell*)CDR(x))
#define CBOXNEXT(x)      	(((Cell*)CDR(x))[0].cdr)
#define CBOXNFREE(x)     	(((Cell*)CDR(x))[1].car)
#define CBOXFREECELL(x)		(((Cell*)CDR(x))[1].cdr)

#define CCDECL(x)		((Defun_Decl*)CDR(x))
#define CCFUNC(x)		(((Defun_Decl*)CDR(x))->func)
#define CDBGME(x)		((CAR(x)>>12)&1)

/*
  convention:
   Cxxx(obj)	convert lispobj to C thing
   MAKxxx(x)    convert C thing to lisp obj
   xxxP(obj)    is this obj a lisp xxx
*/

#ifndef NO_DOCSTRINGS
#define DEFUN(ln, cn, sn, min, max, ep, lp, doc, pr)       			\
 	EXTERN_C Obj cn pr;							\
 	Defun_Decl sn = { ln, (Obj (*)())cn, min, max, ep, lp, doc, 0,0 };	\
 	EXTERN_C Obj cn pr
#else
#define DEFUN(ln, cn, sn, min, max, ep, lp, doc, pr)       			\
 	EXTERN_C Obj cn pr;							\
 	Defun_Decl sn = { ln, (Obj (*)())cn, min, max, ep, lp, 0, 0,0 };	\
 	EXTERN_C Obj cn pr
#endif
/*
  DEFUN:
   ln = string - the name of the function from lisp
   cn = the name of the function from C - by convention begins with F
   sn = the name of the C Defun_Decl struct - by convention begins with S
   min = the minimum number of args
   max = the maximum number of args
   ep = evaluate the args? normally 1, for special forms 0
   lp = pass args as a list of args, min and max are ignored
   doc = documention string
   pr = arg list for C funtion, including the parens
*/

	
#define DEFVAR(ln, cn, doc, val)				\
Obj cn = 0;
/*
  DEFVAR:
   ln = string - the name of the function from lisp
   cn = name of the var in C - by convention begins with V
   doc = documention string
   val = initial value
*/  

	
#define VALUE(v)	CSYM_BOX(v)->value

extern Obj stdin_port, stdout_port, stderr_port;
extern Obj sym_optional, sym_rest, sym_quote;
extern Obj *stackbase;
extern Obj internal_gc_protect;

#ifndef NO_SIGNALS
 	/* for protecting critical sections from signals */
 	extern int signals_blocked;
 	extern unsigned int got_signal_vec;
 	EXTERN_C void signal_dispatcher(void);
#	define DISABLE(x)	x = signals_blocked ++
#	define RENABLE(x)	if( !(signals_blocked = x) && got_signal_vec) signal_dispatcher()
#else
#	define DISABLE(x)	
#	define RENABLE(x)	
#endif
/*
  all critical sections (which is really only while constructing an obj)
  should be surrounded by DISABLE/RENABLE
*/  

	
#define QQUOTIFY(x)	#x
#define QUOTIFY(x)	QQUOTIFY(x)

#ifdef MINIMAL
#	define JLERROR(a, b, c)	jlerror(0, b, 0)
#else
#	define JLERROR(where, ob, desc)	jlerror(where, ob, desc)
#endif
/*
  jlerror:
  where = string naming the function where the error occured, typically use Sfunction.name
  ob = the lisp object in error
  desc = string describing the error
*/
	
#ifndef ABS
#	define ABS(x)		((x) < 0 ? -(x) : (x))
#endif
#ifndef MIN
#	define MIN(a,b)	((a)<(b) ? (a) : (b))
#endif
#ifndef MAX
#	define MAX(a,b)	((a)>(b) ? (a) : (b))
#endif

#include <jl_proto.h>

#endif /* !_jlisp_h */
