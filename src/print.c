
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: print.c,v 2.2 1997/05/30 00:28:29 jaw Exp $";
#endif

/* $Id: print.c,v 2.2 1997/05/30 00:28:29 jaw Exp $ */


#include <jlisp.h>
#include <stdio.h>
#include <math.h>

extern Obj_Vtbl jlisp_vtbl[];
void writestr(Obj port, char* s);
void writechar(Obj port, int c);
void printnum(Obj port, int val, int bacse, int len, int zc);

extern Obj sym_oradix, sym_stdout;
extern Obj sym_floatoutput, sym_doubleoutput;


char *spec_repr(int cc, int spp){
	static char buf[8];
	char *foo = 0;
	
	switch( cc ){
	  case '\n': foo = "\\n"; break;
	  case '\r': foo = "\\r"; break;
	  case '\b': foo = "\\b"; break;
	  case '\t': foo = "\\t"; break;
	  case '\f': foo = "\\f"; break;
	  case '\a': foo = "\\a"; break;
	  case 0x1B: foo = "\\e"; break;
	  case ' ' : if(!spp) foo = "\\s"; break;
	  default:
		if( cc < ' ' || cc > 126){
			sprintf(buf, "\\0%.2o", cc);
			foo = buf;
		}
		break;
	}
	return foo;
}
	

					
/* how:
       0	std. print form (no quotes)
       1	in a form that can be read back
*/

void prnobj(Obj a, Obj stream, int how){
	int typ = TYPEOFX(a);
	int (*printfnc)(Obj,Obj,int);
	char buf[8];
	char *foo;
	Obj radix;
	int base;
	
	switch( typ ){
	  case TPVF_IMMED:
		if( INUMP( a )){
			/* int */
			radix = getvalue( sym_oradix);
			if(DEFINEDP(radix)&& INUMP(radix))
				base = CINT(radix);
			else
				base = 10;
			if(how) base = 10;
			printnum(stream, CINT(a), base, 0,0);
		} else if( ICHARP( a )){
			/* char */
			foo = 0;
			if(how){
				writestr(stream, "#\\");
				foo = spec_repr( CCHAR(a), 0 );
			}
			if(foo)
				writestr(stream, foo);
			else
				writechar(stream, CCHAR(a));
		} else if( ICONSTP( a )){
			/* const sym */
			switch( a ){

			  case IC_NIL:
				writestr(stream, "()");
				break;

			  case IC_TRUE:
				writestr(stream, "#t");
				break;

			  case IC_FALSE:
				writestr(stream, "#f");
				break;

			  case IC_UNDEF:
				writestr(stream, "#<undefined>");
				break;

			  case IC_UNSPEC:
				writestr(stream, "#<unspecified>");
				break;

			  case IC_EOF:
				writestr(stream, "#<EOF>");
				break;

			  default:
				writestr(stream, "#<<send in bug report> IC_0x");
				printnum(stream, a, 16,0,0);
				writestr(stream, "?>");
				break;
			}
		} else if ( SYMBOLP( a )){
			writestr(stream, symbolname(a) );
		} else {
			writestr(stream, "#<<send in bug report> IMM_0x");
			printnum(stream, a, 16,0,0);
			writestr(stream, "?>");
		}
		break;
#if 0
	  case TPV_SYMBOL:
		writestr(stream, CCHARS(a));
		break;
#endif
	  case TPV_SYM_BOX:
		if(how)
			writestr(stream, symbolname( MAKSYM(CSYM_BOX(a)->id )));
		else{
			writestr(stream, "#<[");
			writestr(stream, symbolname( MAKSYM(CSYM_BOX(a)->id )));
			writestr(stream, "]>");
		}
		break;

	  default:
		printfnc = jlisp_vtbl[ typ ].print;

		if( !printfnc || ! printfnc(a, stream, how) ){
			writestr(stream, "#<_");
			printnum(stream, typ, 10,0,0);
			writestr(stream, "_0x");
			printnum(stream, a, 16, 0,0);
			writestr(stream, ">");
		}
		break;
	}
}

int prn_func_macr(Obj a, Obj stream, char* which){
	Obj env  = CADR(a);
	Obj args = CADDR(a);
	Obj body = CDDDR(a);
	
	writestr(stream, "(");
	writestr(stream, which);

	writestr(stream, " ");
	prnobj( args, stream, 1);
	writestr(stream, " ");
	
	a = body;
	while( NNULLP( a )){
		if( NCONSP( a )){
			writestr(stream, " . ");
			prnobj(a, stream, 1);
			break;
		}
		writestr(stream, " ");
		prnobj( CAR(a), stream, 1);
		a = CDR( a );
	}
	writestr(stream, ")");
	return 1;
}

int prnfunc(Obj a, Obj stream, int how){
	char n;

	if( NNULLP( CADR(a))){
		if( how) return prn_func_macr(a, stream, "closure");
		else writestr(stream, "#<closure>");
	}else{
		if( how) return prn_func_macr(a, stream, "lambda");
		else writestr(stream, "#<lambda>");
	}
	return 1;
}
int prnmacr(Obj a, Obj stream, int how){

	if( how) return prn_func_macr(a, stream, "macro");
	else writestr(stream, "#<macro>");
	return 1;
}

int prnccode(Obj a, Obj stream, int how){

	writestr(stream, "#<builtin-function:");
	writestr(stream, CCDECL(a)->name);
	writestr(stream, ">");
	return 1;
}

int prnstr(Obj a, Obj stream, int how){
	int i, c;
	char *foo, buf[8];
	
	if(how){
		writestr(stream, "\"");
		for(i=0; i< CLENGTH(a); i++){
			c = CCHARS(a)[i];
			foo = spec_repr(c, 1);
			if(foo)
				writestr(stream, foo);
			else
				writechar(stream, c);
		}
		writestr(stream, "\"");
	}else{
		for(i=0; i< CLENGTH(a); i++)
			writechar(stream, CCHARS(a)[i]);
	}
	return 1;
}

int prnvect(Obj a, Obj stream, int how){
	int i;
	
	writestr(stream, "#(");
	if( CLENGTH(a)) prnobj( CVECTOR(a)[0], stream, how);
	for(i=1; i< CLENGTH(a); i++){
		writestr(stream, " ");
		prnobj( CVECTOR(a)[i], stream, how);
	}
	writestr(stream, ")");
		
	return 1;
}

int prncmplx(Obj a, Obj stream, int how){
	return 0;
}
	
int prncons(Obj a, Obj stream, int how){
	
	writestr(stream, "(");
	prnobj(CAR(a), stream, how);
	a = CDR(a);
	while( NNULLP( a )){
		if( NCONSP( a )){
			writestr(stream, " . ");
			prnobj(a, stream, how);
			break;
		}
		writestr(stream, " ");
		prnobj( CAR(a), stream, how );
		a = CDR( a );
	}
	writestr(stream, ")");
	return 1;
}

DEFUN("display", Fdisplay, Sdisplay, 1, 2, 1,0,
      "(display obj [port]) Display the object",
      (Obj a, Obj stream))
{

	if( NBOUNDP( stream )) stream = getvalue(sym_stdout);

	if( NULLP(stream)) return IC_UNSPEC;
	
	if( ! WPORTP( stream )){
		return JLERROR("display", stream, "WTA: outputportp");
	}

	prnobj(a, stream, 0);
	return IC_UNSPEC;
}

DEFUN("write", Fwrite, Swrite, 1, 2, 1,0,
      "(write obj [port]) Display the object in read form",
      (Obj a, Obj stream))
{

	if( NBOUNDP( stream )) stream = getvalue(sym_stdout);

	if( NULLP(stream)) return IC_UNSPEC;

	if( ! WPORTP( stream )){
		return JLERROR("write", stream, "WTA: outputportp");
	}

	prnobj(a, stream, 1);
	return IC_UNSPEC;
}


void printnum(Obj port, int val, int base, int len, int zc){
	int c;
	int vv=1;
	int foo;
	if(!zc) zc = '0';
	
	if(val<0){
		val = -val;
		writechar(port, '-');
	}
	if(!val && !len){
		writechar(port, '0');
		return;
	}
	if(len) vv = pow(base, len);
	else{
		foo = val;
		while (foo >= base){
			foo /= base;
			vv *= base;
		}
	}
	
	while(vv){

		c = val / vv;
		if(!c)
			writechar(port, zc);
		else{
			if(c>=0 && c<=9) c+= '0';
			else c += 'A' - 0xA;
			writechar(port, c);
			zc = '0';
		}
		val %= vv;
		vv /= base;
	}
}

int prnflt(Obj a, Obj stream, int how){
	char *format;
	char buffer[512];
	Obj ofmt;

	ofmt = getvalue( sym_floatoutput );
	if(DEFINEDP(ofmt)&& STRINGP(ofmt))
		format = CCHARS(ofmt);
	else	
		format = "%f";
	
	if (how)
		format = "%.10f";
	
	sprintf(buffer, format, CFLOAT(a));
	writestr(stream, buffer);
	return 1;
}

int prndbl(Obj a, Obj stream, int how){
	char *format;
	char buffer[512];
	Obj ofmt;
	
	ofmt = getvalue( sym_doubleoutput );
	if(DEFINEDP(ofmt)&& STRINGP(ofmt))
		format = CCHARS(ofmt);
	else
		format = "%lf";

	if (how)
		format = "%.20lf";
	
	sprintf(buffer, format, CDOUBLE(a));
	writestr(stream, buffer);
	return 1;
}

int prnenvec(Obj a, Obj stream, int how){

	writestr(stream, "#<ENV_0x");
	printnum(stream, a, 16, 0,0);
	writestr(stream, ">");
	return 1;
}

