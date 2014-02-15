
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: ipc2.c,v 2.3 1998/06/18 20:12:42 jaw Exp $";
#endif

/* a go at some interprocess communication support */

#include <jlisp.h>

#ifndef NO_SOCKETS

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <dirent.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <malloc.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>

extern int errno;

extern int freesock(Obj);
extern int sckgetc(Obj);
extern void sckputc(Obj, int);

#define SOCKET_SUBTYPE	3

#define JL_AF_UNIX	MAKINT(1)
#define JL_AF_INET	MAKINT(2)

#define JL_SK_STREAM	MAKINT(1)
#define JL_SK_DGRAM	MAKINT(2)
#define JL_SK_RAW	MAKINT(3)

#define JL_SO_DEBUG	MAKINT(1)
#define JL_SO_REUSE     MAKINT(2)
#define JL_SO_KEEPALIVE MAKINT(3)
#define JL_SO_DONTROUTE MAKINT(4)
#define JL_SO_USELOOP	MAKINT(5)
#define JL_SO_OOBINLINE	MAKINT(6)
#define JL_SO_BROADCAST	MAKINT(7)


int freesock(Obj s){

	/* shutdown( CSOCKET(s), 2); */
	close( CSOCKET(s));

	return 1;
}

int sckgetc(Obj s){
	char foo;

	if(! read(CSOCKET(s), &foo, 1)){
		return EOF;
	}
	return foo;
}

void sckputc(Obj s, int c){
	char foo = c;
	extern int errno;
	
	if( write(CSOCKET(s), &foo, 1) == -1){
		Fthrow(maksym_c("write-error"), MAKINT(errno));
	}
}

makesocket(int s, int rw, int type){
	int sigs;
	Obj foo = newcell();
	
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_IOPORT ) | (rw <<12) | (SOCKET_SUBTYPE << 14) | (type << 18);
	CDR(foo) = (Obj)s;
	RENABLE( sigs );
	return foo;
}

DEFUN("ipc:open", Fopen_socket, Sopen_socket, 2,2,1,0,
      "(ipc:open domain type) create an interprocess communication socket\n"
      " domain is AF_UNIX or AF_INET; type is SOCK_STREAM or SOCK_DGRAM\n"
      " [see also: ipc:bind ipc:connect ipc:listen ipc:accept]",
      (Obj dom, Obj typ))
{

	int s;
	int foo = 1;
	int d,t;
	
	if(! INUMP(dom))
		return JLERROR(Sopen_socket.name, dom, "WTA: intp");
	if(! INUMP(typ))
		return JLERROR(Sopen_socket.name, typ, "WTA: intp");

	switch( dom ){
	  case JL_AF_INET:	d = AF_INET;	break;
	  case JL_AF_UNIX:	d = AF_UNIX;	break;

	  default:		d = CINT(dom);	break;
	}
	switch( typ ){
	  case JL_SK_STREAM:	t = SOCK_STREAM;	break;
	  case JL_SK_DGRAM:	t = SOCK_DGRAM;		break;
	  case JL_SK_RAW:	t = SOCK_RAW;		break;

	  default:		t = CINT(typ);		break;
	}
		
	if( (s=socket( d, t, 0))==-1)
		return MAKINT(errno);
	
	setsockopt(s, SOL_SOCKET, SO_DEBUG, &foo, sizeof(foo));
	
	return makesocket(s, READABLE|WRITABLE, d);
}

Obj fill_in_sain(struct sockaddr_in *sain, Obj adr, Obj prt, char *name){

	sain->sin_family = AF_INET;

	if( STRINGP(adr)){
		/* lookup hostname */
		struct hostent *host;
			
		host = gethostbyname( CCHARS(adr));
		if(! host )
			return JLERROR(name, adr, "unknown host");
		bcopy(host->h_addr, &sain->sin_addr, host->h_length);
	}else if( VECTORP(adr)){
		/*  ip adr -- vector of octets */
		int host=0, i, l=CLENGTH(adr);
		for(i=0;i<4; i++){
			if(i<l){
				if(! INUMP(CVECTOR(adr)[i]))
					return JLERROR(name, adr, "WTA: invalid address");
				host |= (CINT( CVECTOR(adr)[i] ) & 0xFF) << ((3-i)*8);
			}
		}
		sain->sin_addr.s_addr = htonl( host );
		if( i<l ){
			/* allow #(... port) */
			sain->sin_port = htons( CINT( CVECTOR(adr)[i]) );
		}

	}else{
		/*  inaddr_any */
		sain->sin_addr.s_addr = INADDR_ANY;
	}
	if( STRINGP(prt)){
		/* lookup port no. */
		struct servent *sp;

		sp = getservbyname( CCHARS(prt), 0);
		if(! sp)
			return JLERROR(name, prt, "invalid port");
		sain->sin_port = sp->s_port;
	}else if( INUMP(prt)){
		sain->sin_port = htons( CINT(prt));
	}else if( NBOUNDP(prt)){
	}else
		return JLERROR(name, prt, "invalid port");

	return IC_UNDEF;
}

DEFUN("ipc:connect", Fconnect, Sconnect, 2,3,1,0,
      "(ipc:connect socket address [port]) establish a connection to a socket\n"
      " address is the hostname (af_inet) or pathname (af_unix)\n"
      " port is the tcp port (af_inet)\n"
      " [see also: ipc:open ipc:bind ipc:listen ipc:accept]",
(Obj sck, Obj adr, Obj prt))
{
	struct sockaddr_un saun;
	struct sockaddr_in sain;
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sconnect.name, sck, "WTA: not a socket");
	}

	switch( SUBSUBPORT(sck)){

	  case AF_UNIX:
		if(! STRINGP(adr))
			return JLERROR(Sconnect.name, adr, "WTA: stringp");
		strcpy(saun.sun_path, CCHARS(adr));
		saun.sun_family = AF_UNIX;
		if(connect( CSOCKET(sck),  &saun, strlen(saun.sun_path) + sizeof(saun.sun_family)) ==-1)
				return MAKINT(errno);

		break;
		
	  case AF_INET:
		fill_in_sain( &sain, adr, prt, Sconnect.name);

		if(connect( CSOCKET(sck), &sain, sizeof(sain))==-1)
				return MAKINT(errno);

		break;
	}

	CAR(sck) |= (READABLE|WRITABLE) << 12;
	return IC_TRUE;
}

DEFUN("ipc:bind", Fbind, Sbind, 2,3,1,0,
      "(ipc:bind socket adr [port]) bind a name to a socket\n"
      " address is the hostname (af_inet) or pathname (af_unix)\n"
      " port is the tcp port (af_inet)\n"
      " [see also: ipc:open ipc:connect ipc:listen ipc:accept]",
(Obj sck, Obj adr, Obj prt))
{
	struct sockaddr_un saun;
	struct sockaddr_in sain;
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sbind.name, sck, "WTA: not a socket");
	}

	switch( SUBSUBPORT(sck)){

	  case AF_UNIX:
		if(! STRINGP(adr))
			return JLERROR(Sbind.name, adr, "WTA: stringp");
		strcpy(saun.sun_path, CCHARS(adr));
		saun.sun_family = AF_UNIX;
		if( bind(CSOCKET(sck), &saun, strlen(saun.sun_path) + sizeof(saun.sun_family)) ==-1)
			return MAKINT(errno);
		break;
		
	  case AF_INET:
		fill_in_sain( &sain, adr, prt, Sbind.name);

		if(bind(CSOCKET(sck), &sain, sizeof(sain))==-1)
				return MAKINT(errno);

		break;
	}

	return IC_TRUE;
}

DEFUN("ipc:listen", Flisten, Slisten, 2,2,1,0,
      "(ipc:listen socket n) listen to a socket"
      " [see also: ipc:open ipc:connect ipc:bind ipc:accept]",
(Obj sck, Obj n))
{
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Slisten.name, sck, "WTA: not a socket");
	}

	if(! INUMP(n))
		return JLERROR(Slisten.name, n, "WTA: intp");

	if( listen( CSOCKET(sck), CINT(n)) ==-1)
		return MAKINT(errno);

	return IC_TRUE;
}

DEFUN("ipc:accept", Faccept, Saccept, 1,1,1,0,
      "(ipc:accept socket) accept a connection on a socket\n"
      " returns (usable-socket . from-address)\n"
      " [see also: ipc:open ipc:bind ipc:connect ipc:listen]",
(Obj sck))
{
	struct sockaddr_un saun;
	struct sockaddr_in sain;
	int fd, foo, i;
	Obj v;
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Saccept.name, sck, "WTA: not a socket");
	}

	switch( SUBSUBPORT(sck)){

	  case AF_UNIX:
		foo = sizeof(saun);
		fd = accept( CSOCKET(sck), &saun, &foo);
		if( fd==-1)
			return MAKINT(errno);
		return Fcons( makesocket(fd, 3, AF_UNIX), IC_TRUE);

	  case AF_INET:
		foo = sizeof(sain);
		fd = accept(CSOCKET(sck), &sain, &foo);
		if( fd==-1)
			return MAKINT(errno);
		v = makvect(5);
		foo = ntohl(sain.sin_addr.s_addr);
		for(i=0; i<4; i++){
			CVECTOR(v)[i] = MAKINT( (foo >> ((3-i)*8)) & 0xFF );
		}
		CVECTOR(v)[4] = MAKINT(sain.sin_port);
		return Fcons(makesocket(fd, READABLE|WRITABLE, AF_INET), v);
	}

	return IC_UNDEF;
}

DEFUN("getpeername", Fgetpeername, Sgetpeername, 0,1,1,0,
      "(getpeername [port]) return the remote address attached to this socket",
      (Obj sck))
{
	int fd = 0, i, foo;
	struct sockaddr_in sain;
	int len = sizeof(sain);
	Obj v = makvect(5);
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sgetpeername.name, sck, "WTA: not a socket");
	}

	if( getpeername( CSOCKET(sck), &sain, &len )==-1)
		return MAKINT(errno);

	foo = ntohl(sain.sin_addr.s_addr);
	for(i=0; i<4; i++){
		CVECTOR(v)[i] = MAKINT( (foo >> ((3-i)*8)) & 0xFF );
	}
	CVECTOR(v)[4] = MAKINT(sain.sin_port);
	return v;
}

DEFUN("getsockname", Fgetsockname, Sgetsockname, 0,1,1,0,
      "(getsockname [port]) return the local address attached to this socket",
      (Obj sck))
{
	int fd = 0, i, foo;
	struct sockaddr_in sain;
	int len = sizeof(sain);
	Obj v = makvect(5);
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sgetsockname.name, sck, "WTA: not a socket");
	}

	if( getsockname( CSOCKET(sck), &sain, &len )==-1)
		return MAKINT(errno);

	foo = ntohl(sain.sin_addr.s_addr);
	for(i=0; i<4; i++){
		CVECTOR(v)[i] = MAKINT( (foo >> ((3-i)*8)) & 0xFF );
	}
	CVECTOR(v)[4] = MAKINT(sain.sin_port);
	return v;
}

DEFUN("gethostbyname", Fgethostbyname, Sgethostbyname, 1,1,1,0,
      "(gethostbyname name) return (hostname . ip-address)",
      (Obj name))
{
	struct hostent *host;
	int i, foo;
	Obj v;
	
	if(! STRINGP(name))
		return JLERROR(Sgethostbyname.name, name, "WTA: stringp");
	host = gethostbyname( CCHARS(name));
		if(! host )
			return JLERROR(Sgethostbyname.name, name, "unknown host");

	v = makvect(4);
	bcopy(host->h_addr, &foo, host->h_length);
	foo = ntohl(foo);
	for(i=0; i<4; i++){
		CVECTOR(v)[i] = MAKINT( (foo >> ((3-i)*8)) & 0xFF);
	}

	return Fcons(makstr(host->h_name), v);
}

DEFUN("gethostbyaddr", Fgethostbyaddr, Sgethostbyaddr, 1,1,1,0,
      "(gethostbyaddr addr) return hostname",
      (Obj v))
{
	int hosta=0, i, l;
	struct hostent *host;
	
	if(! VECTORP(v))
		return JLERROR(Sgethostbyaddr.name, v, "WTA: vectorp");

	l=CLENGTH(v);
	for(i=0;i<MIN(l, 4); i++){
		if(i<l){
			if(! INUMP(CVECTOR(v)[i]))
				return JLERROR(Sgethostbyaddr.name, v, "WTA: invalid address");
			hosta |= (CINT( CVECTOR(v)[i] ) & 0xFF) << ((3-i)*8);
		}
	}
	hosta = ntohl(hosta);

	host = gethostbyaddr(&hosta, 4, AF_INET);

	if(! host) return MAKINT(errno);
	
	return makstr( host->h_name );
}

DEFUN("ipc:setopt", Fsetopt, Ssetopt, 2,3,1,0,
      "",
      (Obj sck, Obj opt, Obj how))
{
	int on = 1;
	int option;
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Ssetopt.name, sck, "WTA: not a socket");
	}

	if( FALSEP(how)) on = 0;

	if(! INUMP(opt))
		return JLERROR(Ssetopt.name, opt, "WTA: intp");

	switch( opt ){
	  case JL_SO_DEBUG:	option = SO_DEBUG;		break;
	  case JL_SO_REUSE:	option = SO_REUSEADDR;		break;
	  case JL_SO_KEEPALIVE:	option = SO_KEEPALIVE;		break;
	  case JL_SO_DONTROUTE:	option = SO_DONTROUTE;		break;
	  case JL_SO_USELOOP:	option = SO_USELOOPBACK;	break;
	  case JL_SO_OOBINLINE:	option = SO_OOBINLINE;		break;
	  case JL_SO_BROADCAST: option = SO_BROADCAST;		break;
	  default:
		option = CINT( opt );
	}
	if( setsockopt( CSOCKET(sck), SOL_SOCKET, option, &on, sizeof(on))==-1)
		return MAKINT(errno);

	return on ? IC_TRUE : IC_FALSE;
}

DEFUN("socketpair", Fsocketpair, Ssocketpair, 0,0,1,0,
      "(socketpair) returns a pair of ports",
      ())
{
	int fd[2];
	FILE *fp[2];
	
	if( socketpair(AF_UNIX, SOCK_STREAM, 0, fd)==-1)
		return MAKINT(errno);

	return Fcons(
		makesocket(fd[0], READABLE|WRITABLE, 0),
		makesocket(fd[1], READABLE|WRITABLE, 0));
}

DEFUN("ipc:recv", Fipcread, Sipcread, 1,1,1,0,
      "(ipc:recv s) receive a datagram, returns (string . from-address)",
      (Obj sck))
{
	char buffer[1024];
	struct sockaddr_in sa;
	int l, fl=sizeof(sa), i,foo;
	Obj v;
	
	
	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sipcread.name, sck, "WTA: not a socket");
	}

	l = recvfrom(CSOCKET(sck), buffer, 1024, 0, &sa, &fl);
	/* l = read(CSOCKET(sck), buffer, 1024); */
	if(l<0) return MAKINT(errno);

	v = makvect(5);
	foo = ntohl(sa.sin_addr.s_addr);
	for(i=0; i<4; i++){
		CVECTOR(v)[i] = MAKINT( (foo >> ((3-i)*8)) & 0xFF );
	}
	CVECTOR(v)[4] = MAKINT(sa.sin_port);

	return Fcons(makstrn(buffer, l), v);
}

DEFUN("ipc:send", Fipcwrite, Sipcwrite, 2,4,1,0,
      "",
      (Obj sck, Obj data, Obj host, Obj port))
{
	int l;
	struct sockaddr_in sa;

	if( !IOPORTP(sck) || SUBPORT(sck) != SOCKET_SUBTYPE){
		return JLERROR(Sipcwrite.name, sck, "WTA: not a socket");
	}
	if(! STRINGP(data))
		return JLERROR(Sipcwrite.name, data, "WTA: stringp");

	if( BOUNDP(host))
		fill_in_sain( &sa, host, port, Sipcwrite.name);

	if( BOUNDP(host))
		l = sendto(CSOCKET(sck), CCHARS(data), CLENGTH(data), 0, &sa, sizeof(sa));
	else
		l = write(CSOCKET(sck), CCHARS(data), CLENGTH(data));
	if(l<0) return MAKINT(errno);

	return IC_TRUE;
}

#endif /* NO_SOCKETS */
