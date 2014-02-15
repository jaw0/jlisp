
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: port.c,v 2.3 1998/06/18 20:12:36 jaw Exp $";
#endif


/* I/O ports */

#ifdef __GNUC__
#	define	__USE_FIXED_PROTOTYPES__
#endif

#include <jlisp.h>
#include <stdio.h>
#include <setjmp.h>
#include <fcntl.h>

#ifndef NO_UNIX
#	include <unistd.h>
#endif
#ifndef SEEK_SET
#	define SEEK_SET 0
#endif


typedef struct {
	void (*mark)(Obj);
	int (*free)(Obj);
	int (*print)(Obj,Obj,int);
	Obj (*equal)(Obj, Obj);
	int (*getc)(Obj);
	void (*ungetc)(Obj,int);
	void (*flush)(Obj);
	void (*putc)(Obj,int);
	void (*seek)(Obj, int);
	Obj (*tell)(Obj);
} PortDesc;

extern Obj sym_eof;

extern void mark0(Obj), markcdr(Obj);
extern int free0(Obj);
extern Obj eqcdr(Obj,Obj);

void funreadc(Obj,int);
int  freadc(Obj), freefile(Obj), freepipe(Obj);
int  strreadc(Obj);
void strunreadc(Obj,int);
void flflush(Obj), flputc(Obj,int), strputc(Obj,int);
void flseek(Obj,int), strseek(Obj,int);
void compflush(Obj), compputc(Obj, int), compseek(Obj, int);
int  compgetc(Obj), compfree(Obj); 
Obj fltell(Obj), strtell(Obj), comptell(Obj);

EXTERN_C Obj Fclose(Obj);
EXTERN_C Obj Fread(Obj);
EXTERN_C Obj Fflush(Obj);
EXTERN_C Obj Fgetc(Obj);
EXTERN_C Obj Fputc(Obj, Obj);

extern int freesock(Obj);
extern int sckgetc(Obj);
extern void sckputc(Obj, int);

PortDesc pdesc[] = {
#ifndef NO_FILES
	/* mark free  print  equal  getc    ungetc    flush    putc    seek    tell */ 
	{mark0, freefile, 0, eqcdr, freadc, funreadc, flflush, flputc, flseek, fltell },	/* std file */
#else
	{0,0,0,0,0,0,0,0},									/* no files */
#endif	
#ifndef NO_SOCKETS	
	{mark0, freepipe, 0, eqcdr, freadc, funreadc, flflush, flputc, flseek, fltell },	/* pipe */
#else
	{0,0,0,0,0,0,0,0},									/* no sockets => no pipes */
#endif	
	{markcdr, free0,  0, eqcdr, strreadc, strunreadc, 0, strputc, strseek, strtell },	/* string */
#ifndef NO_SOCKETS	
	{mark0, freesock, 0, eqcdr, sckgetc, 0, 0, sckputc, 0, 0 },				/*  socket */
#else
	{0,0,0,0,0,0,0,0},									/* no sockets */
#endif
#ifndef MINIMAL
	{markcdr, compfree, 0, eqcdr, compgetc, 0, compflush, compputc, compseek, comptell },	/* composite port */
#else
	{0,0,0,0,0,0,0,0},									/* no composites */
#endif
	
	{0,0,0,0,0,0,0,0}
};

#define MAK2PORTTYPE(rw, sub, subsub)	(( (rw)<<12) | ((sub)<<14) | ((subsub)<<18))

/* our subsub types for composite ports */
#define COMP_T		0
#define COMP_BCAST	1
#define COMP_ECHO	2
#define COMP_SYNO	3
#define COMP_2WAY	4
#define COMP_CONCAT	5
#define COMP_FUNC	6


/* entries to vtbl table in jlisp.c */
void markport(Obj p){
	void (*fnc)(Obj);
	int t = SUBPORT(p);

	fnc = pdesc[t].mark;

	if(fnc) fnc(p);
	else mark0(p);
}

int freeport(Obj p){
	int (*fnc)(Obj);
	int t = SUBPORT(p);

	fnc = pdesc[t].free;

	if(fnc) return fnc(p);
	else return free0(p);
}

int prnport(Obj p, Obj s, int h){
	int (*fnc)(Obj,Obj,int);
	int t = SUBPORT(p);

	fnc = pdesc[t].print;

	if(fnc)
		return fnc(p, s, h);
	else{
		writestr(s, "#<ioport:");
		if( RPORTP(p))
			writechar(s, 'R');
		if( WPORTP(p))
			writechar(s, 'W');
		if( SUBPORT(p)){
			writechar(s, '_');
			printnum(s, SUBPORT(p), 16, 0, 0);
			if( SUBSUBPORT(p)){
				writechar(s, ':');
				printnum(s, SUBSUBPORT(p), 16, 0, 0);
			}
		}
		writechar(s, '>');
		return 1;
	}
}

Obj eqport(Obj a, Obj b){
	Obj (*fnc)(Obj,Obj);
	int t = SUBPORT(a);

	fnc = pdesc[t].equal;

	if(fnc) return fnc(a, b);
	else return eqcdr(a, b);
}

#ifndef NO_FILES
/* entries for port desc table (top of this file) */
int freefile(Obj a){

	fclose( CFILEPTR( a ));
	return 1;
}
#endif
#ifndef NO_SOCKETS
int freepipe(Obj a){

	pclose( CFILEPTR( a ));
	return 1;
}
#endif

#ifndef NO_FILES
int freadc(Obj p){
	return fgetc( CFILEPTR(p) );
}

void funreadc(Obj p, int c){
	ungetc(c, CFILEPTR(p));
}

void flflush(Obj p){
	fflush( CFILEPTR(p));
}

void flputc(Obj p, int c){
	extern int errno;
	
	if( fputc(c, CFILEPTR(p)) == EOF){
		Fthrow(maksym_c("write-error"), MAKINT(errno));
	}
}

void flseek(Obj p, int i){
	fseek(CFILEPTR(p), i, SEEK_SET);
}

Obj fltell(Obj p){
	return MAKINT( ftell(CFILEPTR(p)));
}
#endif /* NO_FILES */

#ifndef MINIMAL
/* composite ports */
void compputc(Obj p, int c){
	int i, l;
	Obj cp = CDR(p);
	
	switch (SUBSUBPORT(p)){
	  case COMP_T:
		writechar( CAR( cp ), c);
		writechar( CDR( cp ), c);
		break;
	  case COMP_BCAST:
		l = CLENGTH(cp);
		for(i=0; i<l; i++)
			writechar( CVECTOR(cp)[i], c);
		break;
	  case COMP_ECHO:
	  case COMP_2WAY:
		writechar( CDR(cp), c);
		break;
	  case COMP_SYNO:
		Fputc( MAKCHAR(c), getvalue(cp) );
		break;
	  case COMP_FUNC:
		funcall_1("#<internal:compputc>", CVECTOR(cp)[1], MAKCHAR(c));
		break;
	  default:
		break;
	}
}

int compgetc(Obj p){
	int c;
	Obj cp = CDR(p), r;

	switch (SUBSUBPORT(p)){
	  case COMP_T:
	  case COMP_ECHO:
		c = readchar( CAR(cp));
		writechar( CDR(cp), c);
		break;
	  case COMP_2WAY:
		c = readchar( CAR(cp));
		break;
	  case COMP_CONCAT:
		c = readchar( CAR(cp));
		if( c == EOF ){
			/* go to next component */
			CDR(p) = CDR(cp);
			if( NULLP(CDR(p))){
				CAR(p) = IC_NIL;
				c = EOF;
			}else{
				c = compgetc(p);
			}
		}
		break;
	  case COMP_SYNO:
		r = Fgetc( getvalue(cp));
		if( r == IC_EOF )
			return EOF;
		if( !ICHARP(r) )
			return JLERROR("#<internal:compgetc>", r, "synonym-port returned a non-char");
		c = CCHAR(r);
		break;
	  case COMP_FUNC:
		r = funcall_0("#<internal:compgetc>", CVECTOR(cp)[0]);
		if( r == IC_EOF )
			return EOF;
		if( !ICHARP(r) )
			return JLERROR("#<internal:compgetc>", r, "function-port returned a non-char");
		c = CCHAR(r);
		break;
	  default:
		c = EOF;
		break;
	}
	return c;
}

void compflush(Obj p){
	Obj cp = CDR(p);
	int i, l;

	switch (SUBSUBPORT(p)){
	  case COMP_T:
	  case COMP_ECHO:
	  case COMP_2WAY:
		Fflush( CAR(cp));
		Fflush( CDR(cp));
		break;
	  case COMP_CONCAT:
		Fflush( CAR(cp));
		break;
	  case COMP_BCAST:
		l = CLENGTH(cp);
		for(i=0; i<l; i++)
			Fflush( CVECTOR(cp)[i] );
		break;
	  case COMP_SYNO:
		Fflush( getvalue(cp));
		break;
	  case COMP_FUNC:
		funcall_0("#<internal:compflush>", CVECTOR(cp)[2]);
		break;
	  default:
		break;
	}
}	
Obj comptell(Obj p){
	Obj cp = CDR(p);
	
	switch (SUBSUBPORT(p)){
	  case COMP_FUNC:
		return funcall_0("#<internal:comptell>", CVECTOR(cp)[4]);
	  default:
		break;
	}
	return MAKINT(0);
}
void compseek(Obj p, int s){
	Obj cp = CDR(p);
	
	switch (SUBSUBPORT(p)){
	  case COMP_FUNC:
		funcall_1("#<internal:compseek>", CVECTOR(cp)[3], MAKINT(s));
		break;
	  default:
		break;
	}
}

int compfree(Obj p){
	/* no, we do NOT want to close the component ports */
	return 1;
}
#endif


/* string port code is now in string.c */

#ifndef NO_FILES
Obj openport(Obj a, char *mode, int m, char*fnc){
	FILE*fp;

	if(! STRINGP(a))
		return JLERROR(fnc, a, "WTA: filename expected");

	fp = fopen( CCHARS(a), mode);

	if( !fp)
		return IC_NIL;
	return makport( fp, m );
}

#ifndef NO_SOCKETS
Obj openpipe(Obj a, char *mode, int m, char*fnc){
	FILE*fp;

	if(! STRINGP(a))
		return JLERROR(fnc, a, "WTA: filename expected");

	fp = popen( CCHARS(a), mode);

	if( !fp)
		return IC_NIL;
	return makport( fp, m + 4 );
}

DEFUN("open:pipe/in", Fopenpiperead, Sopenpiperead, 1,1,1,0,
      "(open:pipe/in command) open a pipe to read from",
      (Obj a))
{
	return openpipe(a, "r", READABLE, Sopenpiperead.name);
}

DEFUN("open:pipe/out", Fopenpipewrite, Sopenpipewrite, 1,1,1,0,
      "(open:pipe/out command) open a pipe to read from",
      (Obj a))
{
	return openpipe(a, "w", WRITABLE, Sopenpipewrite.name);
}
#endif

DEFUN("open:read", Fopenread,Sopenread, 1,1,1,0,
      "(open:read filename) Open a file for reading",
      (Obj a))
{
	return openport(a, "r", READABLE, Sopenread.name);
}

DEFUN("open:write",Fopenwrite,Sopenwrite,1,1,1,0,
      "(open:write filename) Open a file for writing",
      (Obj a))
{
	return openport(a, "w", WRITABLE, Sopenwrite.name);
}

DEFUN("open:read/write", Fopenrw, Sopenrw, 1,1,1,0,
      "(open:read/write filename) Open a file for reading and writing",
      (Obj a))
{
	return openport(a, "r+", READABLE|WRITABLE, Sopenrw.name);
}

DEFUN("open:append",Fopenappend,Sopenappend,1,1,1,0,
      "(open:append filename) Open a file for appending",
      (Obj a))
{
	return openport(a, "a", WRITABLE, Sopenappend.name);
}
#endif /* NO_FILES */

DEFUN("open:string", Fopen_str, Sopen_str, 1,1,1,0,
      "(open:string string) Open a string as an io port",
      (Obj str))
{
	Obj p = newcell(), foo;
	int sigs;

	if(! STRINGP(str))
		return JLERROR(Sopen_str.name, str, "WTA: stringp");

	foo = Fcons( MAKINT(0), str);
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | ((8+READABLE+WRITABLE) <<12);
	CDR(p) = foo;
	RENABLE( sigs );
	
	return p;
}

#ifndef MINIMAL
DEFUN("open:tee", Fopent, Sopent, 2,2,1,0,
      "(open:tee port port) create a T ouput port\n"
      "writing to a T causes output to go to both component ports\n"
      "reading a T reads from the first, and writes to the 2nd",
      (Obj a, Obj b))
{	
	Obj p = newcell(), foo;
	int sigs, type;

	if(!WPORTP(a) && !RPORTP(a))
		return JLERROR(Sopent.name, a, "WTA: portp");
	if(! WPORTP(b))
		return JLERROR(Sopent.name, b, "WTA: outputportp");

	foo = Fcons(a, b);
	type = MAK2PORTTYPE( (RPORTP(a)?READABLE:0) | (WPORTP(a)?WRITABLE:0),
			     4, COMP_T);
	
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = foo;
	RENABLE( sigs );
	
	return p;
}

/* I created T first, and then decided to do analogs to the variuos CL make*stream
 */
   

DEFUN("open:echo", Fopenecho, Sopenecho, 2,2,1,0,
      "(open:echo inport outport) writes go to outport, reads come from inport and are echoed to outport",
      (Obj a, Obj b))
{
	Obj p = newcell(), foo;
	int sigs, type;

	if(!RPORTP(a))
		return JLERROR(Sopenecho.name, a, "WTA: inputportp");
	if(! WPORTP(b))
		return JLERROR(Sopenecho.name, b, "WTA: outputportp");

	foo = Fcons(a, b);
	type = MAK2PORTTYPE( READABLE | WRITABLE, 4, COMP_ECHO);
	
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = foo;
	RENABLE( sigs );
	
	return p;
}

DEFUN("open:twoway", Fopentwo, Sopentwo, 2,2,1,0,
      "(open:twoway inport outport) create 2way port, reads come from inport, writes to outport",
      (Obj a, Obj b))
{
	Obj p = newcell(), foo;
	int sigs, type;

	if(!RPORTP(a))
		return JLERROR(Sopentwo.name, a, "WTA: inputportp");
	if(! WPORTP(b))
		return JLERROR(Sopentwo.name, b, "WTA: outputportp");

	foo = Fcons(a, b);
	type = MAK2PORTTYPE( READABLE | WRITABLE, 4, COMP_2WAY);
	
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = foo;
	RENABLE( sigs );
	
	return p;
}

DEFUN("open:broadcast", Fopenbcast, Sopenbcast, 1,1,1,1,
      "(open:broadcast outports...) creates a broadcast port, writes go to all components",
      (Obj v))
{
	Obj p = newcell();
	int sigs, type, i, l;

	v = Flist_vect(v);
	l = CLENGTH(v);
	for(i=0; i<l; i++)
		if(! WPORTP( CVECTOR(v)[i] ))
			return JLERROR(Sopenbcast.name, CVECTOR(v)[i], "WTA: outputportp");

	type = MAK2PORTTYPE( WRITABLE, 4, COMP_BCAST);
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = v;
	RENABLE( sigs );
	
	return p;
}

DEFUN("open:concat", Fopenconcat, Sopenconcat, 1,1,1,1,
      "(open:concat inports...) reads come from the first and upon EOF, continues with the next",
      (Obj l))
{
	Obj p = newcell(), c;
	int sigs, type, i;

	for(c=l; CONSP(c); c=CDR(c)){
		if( !RPORTP( CAR(c)))
			return JLERROR(Sopenconcat.name, CAR(c), "WTA: inputportp");
	}
	
	type = MAK2PORTTYPE( WRITABLE, 4, COMP_CONCAT);
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = l;
	RENABLE( sigs );
	
	return p;
}

DEFUN("open:synonym", Fopensyn, Sopensyn, 1,1,1,0,
      "(open:synonym symbol) operations are performed on the stream dynamically bound to symbol",
      (Obj s))
{
	Obj p = newcell();
	int sigs, type;

	if( !SYMBOLP(s))
		return JLERROR(Sopensyn.name, s, "WTA: symbolp");
	
	type = MAK2PORTTYPE( READABLE | WRITABLE, 4, COMP_SYNO);
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = s;
	RENABLE( sigs );
	
	return p;
}

/* not from CL - but kinda cool.... */
DEFUN("open:function", Fopenfunc, Sopenfunc, 0,5,1,0,
      "(open:function [readfunc writefunc flushfunc seekfunc tellfunc]) the component functions are used to perform the appropriate actions",
      (Obj rf, Obj wf, Obj ff, Obj sf, Obj tf))
{
	Obj p = newcell(), vf;
	int sigs, type;

	/* passed in functions are not type checked here, so there is no error if not actually used */
	
	vf = makvect(5);
	CVECTOR(vf)[0] = rf;
	CVECTOR(vf)[1] = wf;
	CVECTOR(vf)[2] = ff;
	CVECTOR(vf)[3] = sf;
	CVECTOR(vf)[4] = tf;
	
	type = MAK2PORTTYPE( READABLE | WRITABLE, 4, COMP_FUNC);
	DISABLE( sigs );
	CAR(p) = MAKETYPE( TPV_IOPORT ) | type;
	CDR(p) = vf;
	RENABLE( sigs );
	
	return p;
}


#endif

#ifndef NO_FILES
DEFUN("load", Fload, Sload, 1,1,1,0,
      "(load filename) load a lisp file",
      (Obj file))
{
	/* this is used only for the initial init file
	which then redefines load to a much more useful
	function (with more error handling...) */
	FILE *fp;
	Obj foo;
	
	if( STRINGP(file)){
		Fdefine(maksym_c("*current-file*"), file, IC_UNSPEC);
		file = Fopenread(file);
	}
	if( NULLP( file )) return IC_FALSE;

	if( ! RPORTP(file))
		return IC_FALSE;
		/* return JLERROR("load",file,"WTA: filename or input port p");  */

	fp = CFILEPTR( file );

	while( !feof( fp )){
		foo = Fread(file);
		/* Fdisplay( foo, IC_UNSPEC ); */
		Feval( foo );
	}
	Fclose( file );
	return IC_TRUE;
}
#endif

int readchar(Obj port){
	int (*fnc)(Obj);
	int t = SUBPORT(port);

	if( CAR(port) & 0x80000000){
		CAR(port) &= ~0x80000000;
		return (CAR(port) >> 23) & 0xFF;
	}
	fnc = pdesc[t].getc;

	if(fnc) return fnc(port);
	else return EOF;
}

void unreadchar(Obj port, int c){
	void (*fnc)(Obj,int);
	int t = SUBPORT(port);

	fnc = pdesc[t].ungetc;

	if(fnc) fnc(port, c);
	else{
		/* save it in CAR */
		CAR(port) &= ~0xFF800000;
		CAR(port) |= (c|0x100) << 23;
	}

}

void writechar(Obj port, int c){
	void (*fnc)(Obj,int);
	int t = SUBPORT(port);

	fnc = pdesc[t].putc;

	if(fnc) fnc(port, c);
}

void writestr(Obj port, char* s){
	void (*fnc)(Obj,int);
	int t = SUBPORT(port);

	if(! (fnc=pdesc[t].putc))
		return;
	while( *s)
		fnc(port, *s++);
}

DEFUN("getc", Fgetc, Sgetc, 0,1,1,0,
      "(getc port) Read a character from the specified port",
      (Obj p))
{
	int c;
	
	if( NBOUNDP(p)) p = stdin_port;
	if( NULLP(p)){
		Fthrow(sym_eof, IC_TRUE);
		return IC_EOF;
	}
	if(! RPORTP(p))
		return JLERROR("getc",p, "WTA: input port p");
	c = readchar(p);
	if(c==EOF){
		Fthrow(sym_eof, IC_TRUE);
		return IC_EOF;
	}
	return MAKCHAR( c );
}

DEFUN("ungetc", Fungetc, Sungetc, 1,2,1,0,
      "(ungetc char [port]) un-get a character from the specified port",
      (Obj c, Obj p))
{

	if( NBOUNDP(p)) p = stdin_port;
	if(! RPORTP(p))
		return JLERROR("ungetc",p, "WTA: input port p");
	if(! ICHARP(c)) c = MAKCHAR(0);
	unreadchar(p, CCHAR( c ));
	return c;
}

DEFUN("putc", Fputc, Sputc, 1,2,1,0,
      "(putc char [port]) Write a character to the specified port",
      (Obj c, Obj p))
{
	if( NBOUNDP(p)) p = stdout_port;
	if(! WPORTP(p))
		return JLERROR("putc",p, "WTA: output port p");

	if(! ICHARP(c))
		return JLERROR(Sputc.name, c, "WTA: charp");
	
	writechar(p, CCHAR(c));
	return IC_UNSPEC;
}


DEFUN("flush", Fflush, Sflush, 0,1,1,0,
      "(flush port) flush the buffer associated with port",
      (Obj port))
{
	void (*fnc)(Obj);
	int t;

	if( NBOUNDP(port)) port = stdin_port;
	if(! IOPORTP(port))
		return JLERROR("flush", port, "WTA: ioportp");

	t = SUBPORT(port);
	fnc = pdesc[t].flush;
	if(fnc) fnc(port);
	return IC_UNSPEC;
}

/* this ought use bignum offset */
DEFUN("seek", Fseek, Sseek, 2,2,1,0,
      "(seek port offset) move file postion",
      (Obj p, Obj  o))
{
	void (*fnc)(Obj, int);
	int t;
	
	if(! IOPORTP(p))
		return JLERROR("seek", p, "WTA: ioportp");

	if(! INUMP(o))
		return JLERROR("seek", o, "WTA: integerp");
	
	t = SUBPORT(p);
	fnc = pdesc[t].seek;
	if(fnc) fnc(p, CINT(o));

	return IC_UNSPEC;
}

DEFUN("tell", Ftell, Stell, 1,1,1,0,
      "(tell port) return the current file postion",
      (Obj p))
{
	Obj (*fnc)(Obj);
	int t;

	if(! IOPORTP(p))
		return JLERROR("seek", p, "WTA: ioportp");

	t = SUBPORT(p);
	fnc = pdesc[t].tell;
	if(fnc) return fnc(p);

	return IC_UNSPEC;
}

DEFUN("close", Fclose, Sclose, 1,1,1,0,
      "(close port) closes the port",
      (Obj p))
{
	int (*fnc)(Obj);
	int t;

	if(! IOPORTP(p))
		return JLERROR("seek", p, "WTA: ioportp");

	t = SUBPORT(p);
	fnc = pdesc[t].free;
	if(fnc){
		fnc(p);
		/* make sure it is no longer used */
		CAR(p) = CDR(p) = IC_NIL;
	}
	return IC_UNSPEC;
}
