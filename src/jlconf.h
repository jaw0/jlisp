
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

/*
  $Id: jlconf.h,v 2.2 1997/05/30 00:28:38 jaw Exp $
*/

#ifndef _jlconf_h
#define _jlconf_h

/*
Things you may want/need to define:

    PASSWD_IS_BIG	does struct passwd contain pw_expire, pw_class, & pw_change
                        probobly needed on most "newer" Unixes
			
    
    STACK_IS_UNALIGNED	if items on stack need not be aligned on word bndries.
                        probobly needed on most 386 boxen

    HAS_NO_LOG2		define this if you have an inferior math library which
                        is missing log2()

    HAS_NO_BZERO	do you have bzero & bcopy?			

    INFERIOR_UNIX	define this if you have some sort of inferior unix-like os
                        such as Solaris(Yechhhhh) or linux

    NO_UNIX		do not include unix specific functions

    MINIMAL		include only the bare minimum in the executable

    NO_BIGNUMS		do not include bignum support

    NO_SOCKETS		do not include network support

    NO_SIGNALS		do not include signal support

    NO_REGEXPS		do not include regexp support

    NO_WEAKS		do not include support for the weak type

    NO_UNEXEC		do not include support for dumping the executable
                        (this may be needed on systems that have problems
			 with with the unexec code)

    NO_DOCSTRINGS	do not keep documentation strings
			
*/



#ifdef sun
#	include <sys/param.h>
#	if ( NOFILE == 256 )
                /* SunOS 4 */
                /* nothing needed */
#	else
                /* Solaris -- Yechhhh!!! */
#		define SOLARIS_YECH
#	endif
#endif

#ifdef SOLARIS_YECH
#	define INFERIOR_UNIX
#	define INFERIOR_LIBM
#	define HAS_NO_LOG2
#	define HAS_NO_BZERO
/*	define HAS_NOTHING_AT_ALL */
#endif

#ifdef __i386__
#	define STACK_IS_UNALIGNED
#endif

#ifdef __NetBSD__
#	define PASSWD_IS_BIG
#	define HAS_NO_LOG2
#	define NO_VFORK_H
#endif

#ifdef linux
#	define INFERIOR_UNIX
#	define INFERIOR_LIBM
#	define NO_UNEXEC

#endif

#ifdef BORLAND
/* borland bcc on an MS-DOn'tS box */
#	define STACK_IS_UNALIGNED
#	define INFERIOR_LIBM
#	define INFERIOR_CC
#	define SIZEOF_INT	16
#	define SIZEOF_LONG	32
#	define SIZEOF_PTR	32
#endif


/* below here ought need not be changed */

#ifdef __cplusplus
#	define EXTERN_C	extern "C"
#else
#	define EXTERN_C
#endif

#ifdef EMBEDDED
/* configuration meant for one of Jeff's
   controler boards
*/
#	define NO_BIGNUMS
#	define NO_STRUCTS
#	define NO_SOCKETS
#	define NO_SIGNALS
#	define NO_REGEXPS
#	define NO_UNEXEC
#	define NO_WEAKS
#	define NO_UNIX
#	define NO_DOCSTRINGS
#	define NO_FILES
#endif

#ifdef MINIMAL
/* configuration for a minimal usable
   system
*/
#	define NO_BIGNUMS
#	define NO_STRUCTS
#	define NO_SOCKETS
#	define NO_SIGNALS
#	define NO_REGEXPS
#	define NO_UNEXEC
#	define NO_WEAKS
#	define NO_UNIX
#	define NO_DOCSTRINGS
#endif

#ifdef NO_UNIX
#	define NO_SOCKETS
#	define NO_UNEXEC
#endif

#ifdef INFERIOR_UNIX
#	define NO_UNEXEC
#endif


#ifdef HAS_NO_BZERO
#	define bzero(a,b)	memset(a, 0, b)
#	define bcopy(a,b,c)	memcpy(b,a,c)
#endif

#ifndef SIZEOF_INT
#	define SIZEOF_INT 32
#endif
#ifndef SIZEOF_LONG
#	define SIZEOF_LONG 32
#endif
#ifndef SIZEOF_PTR
#	define SIZEOF_PTR 32
#endif


#endif /* !_jlconf_h */
