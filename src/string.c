
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: string.c,v 2.2 1997/05/30 00:28:32 jaw Exp $";
#endif


#include <jlisp.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>

int str_alloc_size( int );
void str_append(Obj str, int off, int c);

#define STR_ALLOC( x )	((char*)my_malloc(str_alloc_size(x)))

/*
   the allocated space for a string is _always_ > than the
   required amount, this allows us to null terminate it,
   and simplify growing it
*/

Obj makstrn(char* str, int n){
	Obj foo = newcell(), bar;
	int sigs;

	bar = (Obj) STR_ALLOC( n );
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_STRING ) | (n<<12);
	CDR(foo) = bar;
	if(str && n) memcpy( CCHARS(foo), str, n);
	CCHARS(foo)[n] = 0;
	RENABLE( sigs );
	
	return foo;
}
Obj makstrn_c(char* str, int n){
	Obj foo = newcell();
	int sigs;

	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_STRING ) | (-n<<12);
	CDR(foo) = (Obj) str;
	RENABLE( sigs );
	
	return foo;
}

Obj makstr(char* str){
	return makstrn(str, str?strlen(str):0);
}
Obj makstr_c(char* str){
	return makstrn_c(str, str?strlen(str):0);
}

DEFUN("makestr", Fmakstr, Smakstr, 1,2,1,0,
      "(makestr len [char]) make an empty string of specified length",
      (Obj a, Obj c))
{
	int l, i=0;
	Obj s;

	if( INUMP(a)) l = CINT(a);
	else l = CINT( Flength(a));

	s = makstrn(0, l);

	if( ICHARP(c))
		for( ;i<l; i++) CCHARS(s)[i] = CCHAR(c);
	CCHARS(s)[i] = 0;
	return s;
}

DEFUN("strcat", Fstrcat, Sstrcat, 1,1,1,1,
      "(strcat strings...) Concatenate strings",
      (Obj strs))
{
	Obj cstr, nstr, cs;
	int tl=0, i,j=0;

	/* find total length of strings */
	for(cstr=strs; CONSP(cstr); cstr=CDR(cstr)){
		cs = CAR(cstr);
		if(ICHARP(cs))
			tl ++;
		else{
			if(!STRINGP(cs))
				return JLERROR(Sstrcat.name, cs, "WTA: string or char p");
			tl += CLENGTH(cs);
		}
	}
	nstr = makstrn("", tl);
	for(cstr=strs; CONSP(cstr); cstr=CDR(cstr)){
		cs = CAR( cstr );
		if(ICHARP(cs)){
			CCHARS(nstr)[j++] = CCHAR(cs);
		}else{
			for(i=0; i<CLENGTH(cs); i++)
				CCHARS(nstr)[j++] = CCHARS(cs)[i];
		}
	}
	CCHARS(nstr)[j] = 0;
	return nstr;
}

DEFUN("strcpy", Fstrcpy, Sstrcpy, 1,1,1,0,
      "(strcpy string) return a copy of a string",
      (Obj a))
{

	if(!STRINGP(a))
		return JLERROR(Sstrcpy.name, a, "WTA: stringp");
	return makstrn( CCHARS(a), CLENGTH(a) );
}


DEFUN("substr", Fsubstr, Ssubstr, 3,3,1,0,
      "(substr string start len) extract substring",
      (Obj str, Obj strt, Obj len))
{
	int length, st;
	
	if(!STRINGP(str))
		return JLERROR(Ssubstr.name, str, "WTA: stringp");
	if(!INUMP(strt))
		return JLERROR(Ssubstr.name, strt, "WTA: intp");
	if(!INUMP(len))
		return JLERROR(Ssubstr.name, len, "WTA: intp");
	
	length = CINT(len);
	st = CINT(strt);
	if(st<0) st = CLENGTH(str) - st;
	if(length<0) length = length;	/* ??? */
	
	if(length+st > CLENGTH(str))
		length = CLENGTH(str) - st;
	
	return makstrn( CCHARS(str)+CINT(strt), length);
}

DEFUN("strindex", Fstrindex, Sstrindex, 2,2,1,0,
      "(strindex str c) return offset of first occurance of c in string",
      (Obj str, Obj c))
{
	int i;
	
	if(! STRINGP(str))
		return JLERROR(Sstrindex.name, str, "WTA: stringp");
	if(! ICHARP(c))
		return JLERROR(Sstrindex.name, c, "WTA: charp");

	for(i=0; i<CLENGTH(str); i++)
		if( CCHARS(str)[i] == CCHAR(c)) return MAKINT(i);
	return IC_FALSE;
}


DEFUN("getenv", Fgetenv, Sgetenv, 1,1,1,0,
      "(getenv string) get value from the enviornment",
      (Obj a))
{
	
	if(!STRINGP(a))
		return JLERROR(Sgetenv.name, a, "WTA: stringp");

	return makstr( getenv( CCHARS(a) ));
}

DEFUN("symbol->string", Fsym_str, Ssym_str, 1,1,1,0,
      "(symbol->string symbol) return the name of the symbol as a string",
      (Obj a))
{

	if(SYMBOLP(a)) return makstr( symbolname(a) );
	if(SYMBOXP(a)) return makstr( symbolname( MAKSYM(CSYM_BOX(a)->id )));

	return JLERROR(Ssym_str.name, a, "WTA: symbolp");
}

DEFUN("string->symbol", Fstr_sym, Sstr_sym, 1,1,1,0,
      "(string->symbol string) returns the symbolwith the look of string",
      (Obj a))
{
	if( STRINGP(a)) return maksym( CCHARS(a));

	return JLERROR(Sstr_sym.name, a, "WTA: stringp");
}
	
#ifndef NO_REGEXPS
DEFUN("match", Fmatch, Smatch, 2,2,1,0,
      "(match regex string) does the string match the regex?",
      (Obj r, Obj s))
{
	char *err;
	extern char* re_comp(char*);	/* ?.h */
	extern char* re_exec(char*);
	
	if(! STRINGP(r))
		return JLERROR(Smatch.name, r, "WTA: stringp");
	if(! STRINGP(s))
		return JLERROR(Smatch.name, s, "WTA: stringp");

	if( (err=re_comp( CCHARS(r))))
		return JLERROR(Smatch.name, r, err);

	return re_exec( CCHARS(s)) ? IC_TRUE : IC_FALSE;
}
#endif /* NO_REGEXPS */

#ifndef MINIMAL
DEFUN("strsplit", Fstrsplit, Sstrsplit, 2,2,1,0,
      "(strsplit str (chars...)) split string at the specified chars",
      (Obj str, Obj cl))
{
	int i;
	int ini=0;
	Obj head=IC_NIL, tail=IC_NIL;
	Obj nstr;
	
	if(! STRINGP(str))
		return JLERROR(Sstrsplit.name, str, "WTA: stringp");
	if(! CONSP(cl))
		return JLERROR(Sstrsplit.name, cl, "WTA: listp");

	for(i=0; i<CLENGTH(str); i++){
		
		if( Fmemq( MAKCHAR(CCHARS(str)[i]), cl)!=IC_FALSE){
			/* add to list */
			nstr = Fsubstr(str, MAKINT(ini), MAKINT(i - ini));
			ini = i + 1;

			if(! CLENGTH(nstr)) continue;	/* don't add null strings */
			
			if( NULLP(head)){
				head = tail = Fcons(nstr, IC_NIL);
			}else{
				CDR(tail) = Fcons(nstr, IC_NIL);
				tail = CDR(tail);
			}
		}
	}

	nstr = Fsubstr(str, MAKINT(ini), MAKINT(i - ini));
	if( NULLP(head)){
		head = tail = Fcons(nstr, IC_NIL);
	}else{
		CDR(tail) = Fcons(nstr, IC_NIL);
		tail = CDR(tail);
	}
	return head;
}

#endif /* MINIMAL */

DEFUN("strappend!", Fstrappend, Sstrappend, 1,1,1,1,
      "(strappend! string chars...) append chars to end of string",
      (Obj loc))
{
	Obj c;
	Obj str = CAR(loc);

	loc = CDR(loc);
	
	if(! STRINGP(str))
		return JLERROR(Sstrappend.name, str, "WTA: stringp");
	
	while( CONSP(loc)){
		c = CAR(loc);
		loc = CDR(loc);

		if( ICHARP(c))
			str_append(str, CLENGTH(str), CCHAR(c));
		else if( STRINGP(c)){
			int i, l = CLENGTH(c);

			for(i=0; i<l; i++)
				str_append(str, CLENGTH(str), CCHARS(c)[i]);
		}else
			return JLERROR(Sstrappend.name, c, "WTA: char or string p");
	}

	CCHARS(str)[ CLENGTH(str) ] = 0;
	return str;
}

/* string port routines */

int strreadc(Obj p){
	int off = CINT( CADR(p));
	Obj str = CDDR(p);
	int c;

	if(off >= CLENGTH(str)) return EOF;
	c = CCHARS(str)[off++];
	CADR(p) = MAKINT(off);
	return c;
}
void strunreadc(Obj p, int c){
	int off = CINT( CADR(p)) -1;

	if(off <0) off = 0;
	if( c != EOF)
		CADR(p) = MAKINT(off);
}
void strputc(Obj p, int c){
	int off = CINT( CADR(p));
	Obj str = CDDR(p);

	str_append( str, off++, c);
	CCHARS(str)[CLENGTH(str)] = 0;
	
	CADR(p) = MAKINT(off);
	return;
}
void strseek(Obj p, int i){
	CADR(p) = MAKINT(i);
}
Obj strtell(Obj p){
	return CADR(p);
}



int str_alloc_size(int size){
	/* the amount of space actually allocated for
	   a given length string (always more than needed)

	   we round up to a power of 2
	*/
	
	int n = 1;

	if( size < 0)  return 0;	/* constant strings have size < 0 */
	if( size < 16) return 32;
	size ++;
	while( size ){
		n <<= 1;
		size >>= 1;
	}
	return n;
}

void str_append(Obj str, int off, int c){
	/* insert char at specified offest,
	   which may be beyond the end */

	/* if we are appending to the end this function will
	   not append the null terminator, which ought exist
	   so the string can be used from C.
	*/

	if(off >= CLENGTH(str)){
		/* offset is beyond the end of the string */
		
		if( str_alloc_size(off) >= str_alloc_size( CRLENGTH(str)) ){
			/* we need more space... */
			/* using CRLENGTH, will always realloc constant strings */
			char *old = CCHARS(str);
			CDR(str) = (Obj) STR_ALLOC(off);
			memcpy( CCHARS(str), old, CLENGTH(str));
			free(old);
		}
		/* else there is enough space in the string,
		  we can adjust the length field, and continue */
		CAR(str) = MAKETYPE( TPV_STRING ) | ( (off+1) << 12 );
	}
	CCHARS(str)[off] = c;
}
	
DEFUN("char-upcase", Fchar_upcase, Schar_upcase, 1,1,1,0,
      "(char-upcase char) return upper case version of char",
      (Obj c))
{
	if(! ICHARP(c))
		return c;

	if( islower( CCHAR(c)))
		return MAKCHAR( toupper( CCHAR(c)));

	return c;
}

DEFUN("char-downcase", Fchar_downcase, Schar_downcase, 1,1,1,0,
      "(char-upcase char) return upper case version of char",
      (Obj c))
{
	if(! ICHARP(c))
		return c;

	if( isupper( CCHAR(c)))
		return MAKCHAR( tolower( CCHAR(c)));

	return c;
}

