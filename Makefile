

# $Id: Makefile,v 2.3 1998/06/18 20:12:01 jaw Exp jaw $

jlversion = 2.05

prefix = /usr/local

# where the resultant binaries are installed
bindir = $(prefix)/bin

libdir = $(prefix)/lib

# where the lispcode will be
lispdir = $(libdir)/jlisp-$(jlversion)/lisp

# where to place local lisp files
locallispdir = $(libdir)/jlisp-$(jlversion)/local-lisp

# where some helper files will be
# and helpfiles, ...
etcdir = $(libdir)/jlisp-$(jlversion)/etc

# the jlisp initialization code
jlisp_init = $(lispdir)/init.jl

# how to compile
CC       = gcc
CPP      = cpp
DEBUG    = -g
OPTIMIZE = -O2
# -pg -static
EXTRAFLAGS = -DRCSID -DNO_UNEXEC -DNO_REGEXPS
# -Wall -pg

# if your OS does not provide the nessecary regex routines
# then uncomment the following line
# it is not needed for SunOS, ...
#EXTRAOBJS= regex.o

# Solaris needs
#EXTRALIBS= -lsocket -lnsl
# NetBSD needs
#EXTRALIBS= -lcrypt

BAKEXT = .bak

INSTALL = install

LISPOBJS = lisp/init.jl

all: .notify src/jl etc/jl.help

.notify: src/jl
	./jl -f notify

# called from below
# cannot use debugger on dumped code, might as well strip it...
_jl: $(LISPOBJS)
	@echo "Dumping..."
	./xxjl -norc -e '(progn (unexec "jl" "xxjl") (quit))'
#	strip jl

# called from src/Makefile: jl
_xxjl:
	@echo '=> make xxjl'
	./bin/if-changed cp src/jl xxjl   $(MAKE) _jl
	@echo '<= make xxjl'

src/jl: src_jl
src_jl:
	@echo '=> make src/jl'
	cd src; $(MAKE) jl INIT_FILE="$(jlisp_init)" version=$(jlversion) OPTIMIZE="$(OPTIMIZE)" \
		DEBUG="$(DEBUG)" CC=$(CC) CPP=$(CPP) EXTRAFLAGS="$(EXTRAFLAGS)" EXTRALIBS="$(EXTRALIBS)" EXTRAOBJS="$(EXTRAOBJS)"
	@echo '<= make src/jl'

# some makes destroy the sed command...
etc/jl.help: 
	./jl -norc -e "(progn (load \"all-syms.jl\")(mk-help-txt \"/tmp/jl.help\")(quit))"
	sort /tmp/jl.help \
		| sed 's/\/\
	                        /g' > etc/jl.help
	rm /tmp/jl.help

lisp/init.jl: lisp/init.cf.jl configure.sed
	sed -f configure.sed lisp/init.cf.jl > lisp/init.jl

lisp/mrirc.jl: lisp/mrirc.cf.jl configure.sed
	sed -f configure.sed lisp/mrirc.cf.jl > lisp/mrirc.jl

jlisp.a: _jlispa
_jlispa:
	cd src; $(MAKE) jlisp.a version=$(jlversion) OPTIMIZE="$(OPTIMIZE)" \
		DEBUG="$(DEBUG)" CC=$(CC) CPP=$(CPP)
	./bin/if-changed cp src/jlisp.a jlisp.a

include/defproto.h: src/defproto.h
	./bin/if-changed cp src/defproto.h include/defproto.h

src/defproto.h:
	cd src; $(MAKE) defproto.h

configure.sed: 
	@-mv configure.sed configure.sed$(BAKEXT)
	@echo s@%SRCDIR%@.@g           >> configure.sed
	@echo s@%PREFIX%@$(prefix)@g   >> configure.sed
	@echo s@%BINDIR%@$(bindir)@g   >> configure.sed
	@echo s@%LIBDIR%@$(libdir)@g   >> configure.sed
	@echo s@%ETCDIR%@$(etcdir)@g   >> configure.sed
	@echo s@%LISPDIR%@$(lispdir)@g >> configure.sed
	@echo s@%LOCALLISP%@$(locallispdir)@g >> configure.sed

install: all install.dirs   install.jl   install.lisp 
uninstall: uninstall.dirs uninstall.jl uninstall.lisp

install.dirs:
	-mkdir -p $(bindir) $(libdir) $(lispdir) $(locallispdir) $(etcdir)
uninstall.dirs: uninstall.lisp
	-rm -rf $(libdir) $(lispdir) $(locallispdir) $(etcdir)

install.jl: jl
	$(INSTALL) -m 555  jl        $(bindir)/jl
	$(INSTALL) -m 555  xxjl      $(bindir)/xxjl
uninstall.jl:
	-rm -f $(bindir)/jl
	-rm -f $(bindir)/xxjl

install.lisp: lispcode
	cp lisp/*.jl $(lispdir)
uninstall.lisp:
	-rm -rf $(lispdir)

clean:
	-rm -f $(LISPOBJS) $(OBSLS) core a.out #*# include/cli-defvar.list \
		include/cli-defun.list include/defproto.h etc/jl.help
	@echo '=> make clean'
	cd src;  $(MAKE) clean
	@echo '<= make clean'

realclean: clean
	-rm -f jl xxjl jlisp.a stamp *bak configure.sed .notify
	@echo '=> make realclean'
	cd src; $(MAKE) realclean
	@echo '<= make realclean'

depends:
	@echo making depends...
	@echo '=> make depends'
	cd src; $(MAKE) depends
	@echo '<= make depends'

Makefiles:
	@echo '=> make Makefiles'
	cd src; $(MAKE) Makefile
	@echo '<= make Makefiles'

CI = ci -l2 -m'periodic'
# -f
rcs:
	for x in Makefile ToDo Describble Ack* Bugs Changes License README; do $(CI) $$x; done
	for x in `find Doc  -name RCS -prune -o -type f -print`; do $(CI) $$x; done
	for x in `find lisp -name RCS -prune -o -type f -print`; do $(CI) $$x; done
	for x in `find misc -name RCS -prune -o -type f -print`; do $(CI) $$x; done
	for x in `find src  -name RCS -prune -o -name ext -prune -o -type f \! -name \*.o \! \
		-name jl -print`; do $(CI) $$x; done
	for x in ext_init.c gen-ipl-h.l gen-ipl-h.y libc.g math2.g unistd2.g ; do $(CI) src/ext/$$x; done

stamp:
	touch stamp

dist:
	./jl -f misc/dist.jl
tar:
	cd ..; tar cvFFf - jlisp-$(jlversion) | gzip -c > jlisp-$(jlversion).tar.gz
ftp:
	cp ../jlisp-$(jlversion).tar.gz /home/renoir/ftp/pub/src


