
/*
    Copyright (c) 1994, 1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: unexec.c,v 2.2 1997/05/30 00:28:37 jaw Exp $";
#endif

#include <jlisp.h>

static int data_start = 0;

#ifndef NO_UNEXEC

/*
    borrowed from emacs src
    as it has no GPL virus attached

    original author's comment block:
*/
/* Contributed by Viktor Dukhovni.  */
/*
 * Unexec for Berkeley a.out format + SUNOS shared libraries
 * The unexeced executable contains the __DYNAMIC area from the
 * original text file,  and then the rest of data + bss + malloced area of
 * the current process.  (The __DYNAMIC area is at the top of the process
 * data segment,  we use "data_start" defined externally to mark the start
 * of the "real" data segment.)
 *
 * For programs that want to remap some of the data segment read only
 * a run_time_remap is provided.  This attempts to remap largest area starting
 * and ending on page boundaries between "data_start" and "bndry"
 * For this it to figure out where the text file is located.  A path search
 * is attempted after trying argv[0] and if all fails we simply do not remap
 *
 * One feature of run_time_remap () is mandatory:  reseting the break.
 *
 *  Note that we can no longer map data into the text segment,  as this causes
 *  the __DYNAMIC struct to become read only,  breaking the runtime loader.
 *  Thus we no longer need to mess with a private crt0.c,  the standard one
 *  will do just fine,  since environ can live in the writable area between
 *  __DYNAMIC and data_start,  just make sure that pre-crt0.o (the name
 *  is somewhat abused here) is loaded first!
 *
 */

#ifdef __GNUC__
#	define	__USE_FIXED_PROTOTYPES__
#endif

#include <sys/param.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <a.out.h>
#include <unistd.h>

/*
    verified to work on:
      Sparc / 4.1.3
      Sun 3 / 4.1.1
      i486  / NetBSD-.9
*/

#ifndef PAGSIZ
#	define PAGSIZ 		__LDPGSZ
#endif
#ifndef N_PAGSIZ
#	define N_PAGSIZ(x)	PAGSIZ
#endif
#ifndef N_BSSADDR
#	define N_BSSADDR(ex)	N_ALIGN(ex, (N_DATADDR(ex)+(ex).a_data))
#endif
#ifndef N_TRELOFF
#	define N_TRELOFF(x)	N_RELOFF(x)
#endif
#ifndef MAP_FILE
#	define MAP_FILE		0
#endif

#define XX_ALIGN(x)		((x + PAGSIZ - 1) & ~(PAGSIZ - 1))

/*
 * for programs other than emacs
 * define data_start + initialized here,  and make sure
 * this object is loaded first!
 * emacs will define these elsewhere,  and load the object containing
 * data_start (pre-crt0.o or firstfile.o?) first!
 * The custom crt0.o *must not* be loaded!
 */

extern char *getenv ();
extern caddr_t sbrk();
extern int fchmod();

static struct exec nhdr;
static int rd_only_len;
static long cookie;

extern int initialized;
extern unsigned int Brk;

extern int edata, etext, end;

extern Obj envcurr, catchlist;
extern Backtrace *backtrace_list;

void unexec (char *new_name, char *a_name, unsigned bndry, unsigned bss_start, unsigned entry){
	int fd, nnew;
	char *old;
	struct exec ohdr;	/* Allocate on the stack,  not needed in the next life */
	struct stat stat;
	extern int errno;
	

	if ((fd = open(a_name, O_RDONLY)) < 0){
		fprintf (stderr, "%s: open: ", a_name);
		perror (a_name);
		exit (1);
	}
	
	if ((nnew = open(new_name, O_WRONLY | O_CREAT, 0666)) == -1){
		fprintf (stderr, "%s: open: ", a_name);
		perror (new_name);
		exit (1);
	}

	if ((fstat(fd, &stat) == -1)){
		fprintf (stderr, "%s: ", a_name);
		perror ("fstat");
		exit (1);
	}

	/* map in original file */
	old = (char *)mmap (0, stat.st_size, PROT_READ, MAP_FILE | MAP_SHARED, fd, 0);
	if (old == (char *)-1){
		fprintf (stderr, "%s: ", a_name);
		perror ("mmap");
		exit (1);
	}
	close (fd);

	nhdr = ohdr = (*(struct exec *)old);


	/*
	* Remember a magic cookie so we know we've got the right binary
	* when remapping.
	*/
	cookie = time (0);

	Brk = (unsigned int) sbrk (0);		/* Save the break, it is reset to &_end (by ld.so?) */

	/*
	* Round up data start to a page boundary (Lose if not a 2 power!)
	*/
	data_start = ((((int)&data_start) - 1) & ~(N_PAGSIZ (nhdr) - 1)) + N_PAGSIZ (nhdr);

	/*
	* Round down read only pages to a multiple of the page size
	*/
	if (bndry)
		rd_only_len = ((int)bndry & ~(N_PAGSIZ (nhdr) - 1)) - data_start;

	/* Have to do this some time before dumping the data */
	initialized = 1;
	
	/* 
	* Handle new data and bss sizes and optional new entry point.
	* No one actually uses bss_start and entry,  but tradition compels
	* one to support them.
	* Could complain if bss_start > Brk,  but the caller is *supposed* to know
	* what she is doing.
	*/
	nhdr.a_data = XX_ALIGN( (bss_start ? bss_start : Brk) - N_DATADDR (nhdr) );
	nhdr.a_bss  = XX_ALIGN( bss_start ? Brk - bss_start : 0 );
	if (entry) 
		nhdr.a_entry = entry;

	/*
	* Write out the text segment with new header
	* Dynamic executables are ZMAGIC with N_TXTOFF==0 and the header
	* part of the text segment, but no need to rely on this.
	* So write the TEXT first,  then go back replace the header.
	* Doing it in the other order is less general!
	*/
	lseek (nnew, N_TXTOFF (nhdr), L_SET);
	write (nnew, old + N_TXTOFF (ohdr), N_TXTOFF (ohdr) + ohdr.a_text);
	
	lseek (nnew, 0L, L_SET);
	write (nnew, (char*) &nhdr, sizeof (nhdr));
	
	/*
	* Write out the head of the old data segment from the file not
	* from core, this has the unresolved __DYNAMIC relocation data
	* we need to reload
	*/
	lseek (nnew, N_DATOFF (nhdr), L_SET);
	write (nnew, old + N_DATOFF (ohdr), (int)&data_start - N_DATADDR (ohdr));
	
	/*
	* Copy the rest of the data from core
	*/
	write (nnew, (char*) &data_start, N_BSSADDR (nhdr) - (int)&data_start);
	
	/*
	* Copy the symbol table and line numbers
	*/
	lseek (nnew, N_TRELOFF (nhdr), L_SET);
	write (nnew, old + N_TRELOFF (ohdr), stat.st_size - N_TRELOFF (ohdr));
	
	fchmod (nnew, 0755);
	close (nnew);
}


DEFUN("unexec", Funexec, Sunexec, 2,2,1,0,
      "(unexec new-filename old-filename) dump out a new executable image",
      (Obj nf, Obj of))
{
	Obj cf, ef;
	Backtrace *bt;
	
	if(! STRINGP(nf))
		return jlerror(Sunexec.name, nf, "WTA: stringp");
	if(! STRINGP(of))
		return jlerror(Sunexec.name, of, "WTA: stringp");

	Fgc();

	/* make sure the catch frame list is empty when restarted */
	cf = catchlist;
	catchlist = IC_NIL;

	/* likewise the backtrace list */
	bt = backtrace_list;
	backtrace_list = 0;
	
	/* we don't want to start up inside the unexec frame */
	/* unwind to the top level */
	ef = envcurr;
	while(Flength(envcurr) != MAKINT(1))
		envcurr = CDR(envcurr);
	   
	unexec( CCHARS(nf), CCHARS(of), 0,0,0);

	catchlist = cf;
	envcurr = ef;
	backtrace_list = bt;
	
	return IC_UNSPEC;
}

#endif /* NO_UNEXEC */

DEFUN("been-dumped?", Fbeen_dumped, Sbeen_dumped, 0,0,1,0,
      "(been-dumped?) is this a dumped version?",
      ())
{
#ifdef NO_UNEXEC
	return IC_FALSE;
#else	
	return initialized ? IC_TRUE : IC_FALSE;
#endif
}

