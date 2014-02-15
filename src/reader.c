
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: reader.c,v 2.3 1998/06/18 20:12:29 jaw Exp $";
#endif

#include <jlisp.h>
#include <stdio.h>
#include <string.h>

extern Obj makfloat(float);
extern Obj str_append(Obj, int, int);
extern Obj cint_to_big(int);

EXTERN_C Obj Fread(Obj);
int readchar(Obj port);
Obj read_internal(Obj);
void unreadchar(Obj port, int c);

extern Obj sym_optional, sym_rest, sym_quote, sym_dot;
extern Obj sym_quote, sym_bquote, sym_bq_comma, sym_bq_comma_at;

extern Obj sym_iradix, sym_eof, sym_stdin;

extern Obj envcurr;


DEFVAR(".lineno", Vlineno, ".lineno the current line number", MAKINT(1))

     
void inc_lineno(){
	/* increment line number */
	VALUE( Vlineno ) += MAKINT(1) - MAKINT(0);
}
void dec_lineno(){
	/* decrement line number */
	VALUE( Vlineno ) -= MAKINT(1) - MAKINT(0);
}

void got_eof(){
	Fthrow(sym_eof, IC_TRUE);
}

int vallof(int c, int b){

	if(c>='0' && c<='9') return c - '0';
	if(c>='a' && c<='z') return c - 'a' + 0xa;
	if(c>='A' && c<='Z') return c - 'A' + 0xA;

	return 255;
}

int isvalid(int c, int b){

	return vallof(c, b) < b;
}

void eatcomment(Obj stream){
	/* eat #| comment |#
	   may be nested */
	int c=0;
	
	while( c!='#'){
		while(c!='|'){
			c = readchar(stream);
			switch(c){
			  case '#':
				c = readchar(stream);
				if(c=='|'){
					eatcomment(stream);
					c = readchar(stream);
				}
				break;
			  case EOF:
				got_eof();
				return;
			  case '\n':
				inc_lineno();
			  default:
				break;
			}
		}
		c = readchar(stream);
	}
	return;
}

int special_char(Obj stream){
	/* handle special \escaped characters */
	int c;
	int val=0, base;
	c = readchar(stream);
	switch( c ){
	  case 'a':	c = '\a';	break;	/* yes, I know that this is the ANSI C alert char... */
	  case 'n':	c = '\n';	break;
	  case 'r':	c = '\r';	break;
	  case 'b':	c = '\b';	break;
	  case 't':	c = '\t';	break;
	  case 's':	c = ' ';	break;
	  case 'f':	c = '\f';	break;
	  case 'v':	c = '\v';	break;
	  case 'e':	c = '\033';	break;
	  case '\\':	c = '\\';	break;
	  case '"':	c = '"';	break;
		
	  case '0':
		base = 8;  goto rnum;
	  case 'x':
	  case 'X':
		c = readchar(stream);
		base = 16; goto rnum;
	  case '1': case '2': case '3':
	  case '4': case '5': case '6':
	  case '7': case '8': case '9':
		base = 10; goto rnum;
	  rnum:
		while( isvalid(c, base)){
			val *= base;
			val += vallof(c, base);
			c = readchar(stream);
		}
		unreadchar(stream, c);
		c = val;
		break;

	  case EOF:
		got_eof();
		return EOF;
	  case '\n':
		inc_lineno();
	  default:
		c = c;		break;

	}
	return c;
}

int getc_skipws(Obj stream){
	int c;
	
	while( 1 ){
		c = readchar(stream);
		switch(c){
		  case ';':
			while( c!= '\n' && c!=EOF )
				c = readchar(stream);
			
			/* fall thru' */
		  case '\n':
			inc_lineno();
		  case ' ':
		  case '\t':
		  case '\r':
			continue;

		  /* this could hust as easily be done in lisp ... */
		  case '#':
			c = readchar(stream);
			if(c!='|'){
				unreadchar(stream, c);
				return '#';
			}
			eatcomment(stream);
			continue;
		}
		break;
	}
	return c;
}

Obj macrochar(Obj port, int c1, int c2){
	Obj fn, fv;

	fn = maksym_c("macro-handler");
	fv = getvalue(fn);
	
	if( DEFINEDP(fv) && !FALSEP(Fprocp(fv)) )
		return funcall_3("#<internal:macrochar>", fn, port, MAKCHAR(c1), c2?MAKCHAR(c2):IC_UNSPEC);
	return JLERROR("#<internal:macrochar>", port, "no macro-handler");
}

DEFUN("read", Fread, Sread, 0, 1, 1,0,
      "(read [port]) read in an expression",
      (Obj stream))
{
	Obj v;

	if( NBOUNDP( stream )) stream = getvalue(sym_stdin);
	if( NULLP(stream)){
		Fthrow(sym_eof, IC_EOF);
		return IC_EOF;
	}
	if( !RPORTP( stream )){
		return JLERROR(Sread.name, stream, "wrong type of argument, inputportp");
	}

	do{
		v = read_internal(stream);
	}while( v==IC_UNSPEC );

	return v;
}

Obj read_internal(Obj stream){
	int c;
	char buf[1024];
	int i;
	Obj val, frac, baseo, sym;
	Obj foo, bar, head, tail;
	int decmp, negp, base;
	Obj radix;
	Obj buffer;
	

  restart:
	c = getc_skipws( stream );
	
	switch( c ){
	  case EOF:
		got_eof();
		return IC_EOF;
		
	  case '(':
		head = tail = IC_NIL;
		while(1){
			c = getc_skipws(stream);
			if( c==')' ){
				val = head;
				break;
			}
			unreadchar(stream, c);
			val = read_internal(stream);
			if( val==IC_UNSPEC )
				continue;

			/* handle dotted list */
			if( val==sym_dot ){
				c = getc_skipws(stream);
				unreadchar(stream, c);
				if( c==')' ){
					return JLERROR(Sread.name, stream, "bad dotted list");
				}
				val = Fread(stream);
				c = getc_skipws(stream);
				if( c!=')' ){
					unreadchar(stream, c);
					return JLERROR(Sread.name, stream, "bad dotted list");
				}
				if( head==IC_NIL ){
					return JLERROR(Sread.name, stream, "bad dotted list");
				}
				CDR(tail) = val;
				val = head;
				break;
			}
			val = Fcons(val, IC_NIL);
			if( head==IC_NIL )
				head = tail = val;
			else
				tail = CDR(tail) = val;
		}
		goto bottom;
		
	  case ')':
		/* allow a macro handler to catch this before we call it an error */
		Fthrow(maksym_c("read:parenclose"), IC_TRUE);
		return JLERROR(Sread.name, stream, "unexpected ')'");

	  case '"':
		buffer = makstrn("", 0);
		i = 0;
		do {
			c = readchar(stream);
			if(c=='\\'){
				c = special_char(stream);
				str_append(buffer, i++, c);
				c = 0;
				continue;
			}
			if( c==EOF){
				got_eof();
				return IC_EOF;
			}
			if( c=='\n') inc_lineno();
			if( c!= '"')
				str_append(buffer, i++, c);
		}while( c != '"' );

		CCHARS(buffer)[i] = 0;
		val = buffer;
		goto bottom;

	  case '?':
	  rchar:
		c = readchar(stream);
		if( c == '\\' ){
			i = special_char(stream);
		}else
			i = c;
		if( i==EOF ){
			got_eof();
			return IC_EOF;
		}
		val = MAKCHAR( i );
		goto bottom;

	  case '#':
		c = getc_skipws( stream );

		switch( c ){
		  case EOF:
			got_eof();
			return IC_EOF;
		  case '\\':
			/* handle scheme-like character syntax #\x #\\n would be a newline... */
			goto rchar;
		  case 't':
		  case 'T':
			return IC_TRUE;
		  case 'f':
		  case 'F':
			return IC_FALSE;
		  case 'x':
		  case 'X':
			base = 16;	goto rnump;
		  case 'o':
		  case 'O':
			base = 8;	goto rnump;
		  case 'd':
		  case 'D':
			base = 10;	goto rnump;
		  case 'b':
		  case 'B':
			base = 2;
		  rnump:
			c = getc_skipws(stream);
			goto rnum;

		  default:
			/* handle things like #[ ... ] #{ ... } #( ... ) #+ #- #= ## #. #! ...
			   by passing it out to a lisp function
			   
			   most of the stuff that used to be in this switch, has
			   been moved out to lisp-land
			*/
			return macrochar(stream, '#', c );
		}
		break;

	  case '{':
	  case '}':
	  case '[':
	  case ']':
		/* dispatch out to lisp to handle */
		return macrochar(stream, c, 0);
		       
	  case '\'':
		val = Fcons(sym_quote, Fcons(Fread( stream ), IC_NIL));
		goto bottom;

	  case '`':
		val = Fcons(sym_bquote, Fcons(Fread( stream ), IC_NIL));
		goto bottom;

	  case ',':
		c = readchar(stream);
		if( c=='@') return Fcons(sym_bq_comma_at, Fcons(Fread( stream ), IC_NIL));
		unreadchar(stream, c);
		val = Fcons(sym_bq_comma, Fcons(Fread( stream ), IC_NIL));
		goto bottom;
		
	  default:
		radix = getvalue( sym_iradix );
		
		if( INUMP(radix))
			base = CINT(radix);
		else
			base = 10;
	  rnum:
		baseo = MAKINT(base);

		i = 0;
		while(1){
			if( c==' ' ) break;
			if( c=='\t') break;
			if( c=='\r') break;
			if( c=='\n') break;
			if( c=='(' ) break;
			if( c==')' ) break;
			if( c==';' ) break;
			if( c==',' ) break;
			if( c=='#' ) break;	/* XXX ? ought # be allowed in a symbol name? */
			if( c==EOF ) break;
			if( c=='\\'){
				c = special_char(stream);
				if( c==EOF )
					break;
			}
			buf[i++] = c;
			buf[i] = 0;
			c = readchar(stream);
		}
		unreadchar(stream, c);

		val = MAKINT(0);
		frac= makfloat(1);
		decmp = negp = 0;
		i = 0;
		
		
		if( buf[0]=='-'){
			negp = 1;
			i++;
		}
		while(buf[i]){
			if( !isvalid(buf[i], base) && buf[i]!='.'){
				/* a symbol */
				goto ret_sym;
			}
			if( buf[i]=='.' ){
				if( decmp ) return maksym( buf );
				decmp = 1;
				i++;
				val = Ftimes(val, frac);	/* frac=1.0, causes int->float */
				continue;
			}
			
			if( decmp ){
				if( FLOATP(val) && i > 6)
					/* float -> double */
					val = Fto_double(val);
				frac = Fdivide(frac, baseo);
				val  = Ftimes(val, baseo);
				val  = Fplus(val, MAKINT(vallof( buf[i], base )));
			} else {
				val = Ftimes(val, baseo);
				val = Fplus(val, MAKINT(vallof( buf[i], base )));
			}
			i++;

		}
		if(negp && i==1 || decmp && i==1){
		  ret_sym:
			val = maksym( buf );
		}else{
			if(decmp)
				val = Ftimes(val, frac);
			if(negp)
				val = Fminus(MAKINT(0), val);
		}
	}
  bottom:
	c = readchar(stream);
	if( c == ',' ){
		/* object system sugar hack
		   xlat:
		        obj,name	  -> (method obj 'get 'name)
			obj,(fnc args...) -> (method obj 'fnc args...)
		*/
		foo = Fread(stream);
		if( NCONSP( foo)){
			val = Fcons( maksym_c("method"),
				    Fcons( val,
					  Fcons( Fcons( sym_quote, Fcons( maksym_c("get"), IC_NIL)),
						Fcons( Fcons( sym_quote, Fcons( foo, IC_NIL)),
						      IC_NIL))));	
		}else{
			val = Fcons( maksym_c("method"),
				    Fcons( val, 
					  Fcons( Fcons( sym_quote, Fcons( CAR(foo), IC_NIL)),
						CDR(foo))));
		}
	}else{
		unreadchar(stream, c);
	}

	return val;
}





