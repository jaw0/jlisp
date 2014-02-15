
# jlisp makefile
# $Id: Makefile,v 2.2 1997/05/30 00:28:23 jaw Exp $

OBJSA = unexec.o alloc.o eval.o gc.o jlisp.o reader.o\
	symtab.o print.o jlib.o pred.o string.o \
	list.o alist.o math.o port.o unistd.o \
	ball.o hooks.o error.o signals.o debug.o \
	weak.o bignum.o ipc2.o struct.o symbol.o \
	$(EXTRAOBJS)

OBJSL = init_l.o
OBJSS = init_s.o

SRCS =  unexec.c alloc.c eval.c gc.c jlisp.c reader.c\
	symtab.c print.c jlib.c pred.c string.c \
	list.c alist.c math.c port.c unistd.c \
	ball.c hooks.c error.c signals.c debug.c \
	weak.c bignum.c ipc2.c struct.c symbol.c \
	init.c


CFLAGS = -I. $(OPTIMIZE) $(DEBUG) $(STANDALONE) -DJLISP_INIT=$(INIT_FILE) -DVERSION_MAJOR=$(version) $(EXTRAFLAGS)


all: jl jlisp.a

jl: $(OBJSA) $(OBJSS) ext_jlext.a
	@echo '=> make (src)/jl'
	$(CC)  $(CFLAGS) -o jl $(OBJSA) $(OBJSS) ext/jlext.a -lm $(EXTRALIBS)
	cd .. ; $(MAKE) _xxjl
	@echo '<= make (src)/jl'

ext_jlext.a:
	@echo '=> make (src)/ext'
	cd ext ; $(MAKE) jlext.a CC=$(CC) CPP=$(CPP) CFLAGS="$(OPTIMIZE) $(DEBUG) -I.."
	@echo '<= make (src)/ext'

init_l.o:	defun.list defvar.list defproto.h init.c
	$(CC) $(CFLAGS) -DNOT_STAND_ALONE -c init.c
	mv init.o init_l.o
init_s.o:	defun.list defvar.list defproto.h init.c
	$(CC) $(CFLAGS) -c init.c
	mv init.o init_s.o

$(OBJSA): jlisp.h jlconf.h
$(OBJSL): jlisp.h jlconf.h
$(OBJSS): jlisp.h jlconf.h

jlisp.a: $(OBJSA) $(OBJSL) ext_jlext.a
	ar rcuv jlisp.a $(OBJSA) $(OBJSL) ext/jlext.a
	ranlib jlisp.a

defun.list: defun.list.tmp
	../bin/if-changed cp defun.list.tmp defun.list
defun.list.tmp: defun
defun:
	exec > defun.list.tmp;\
	for f in $(SRCS); do \
		echo ""; \
		echo "/* $$f */"  ; \
		sed -n -f mdefun.sed $$f ; \
	done

defvar.list: defvar.list.tmp
	../bin/if-changed cp defvar.list.tmp defvar.list
defvar.list.tmp: defvar
defvar:
	exec > defvar.list.tmp;\
	for f in $(SRCS); do \
		echo ""; \
		echo "/* $$f */"  ; \
		sed -n -f mdefvar.sed $$f ; \
	done
	
defproto.h: defun.list defproto.gen
	$(CPP) -I. -C -P defproto.gen | sed -e 's/^$$//' -e 's/  */ /g' > defproto.h

math2.c: math2.list math2.gen math2.top
	./math2.gen > math2.c

depends:
	@echo making depends...
	@$(CC) -M $(CFLAGS) $(SRCS) \
	| grep -v /usr/opnet\
	| grep -v /usr/include > depends

clean:
	-rm -f $(OBJSA) $(OBJSL) $(OBJSS) jl jlisp.a defun.list defvar.list
	-rm -rf defun.list.tmp defvar.list.tmp defproto.h math2.c libm.list
	-rm -rf a.out core
	cd ext ; $(MAKE) clean

realclean: clean


Makefile: depends
	@echo making Makefile...
	@cp Makefile Makefile.BAK
	@sed -n '1,/#END_[oO]F_MAKE/p' Makefile\
		| sed "s%^srcdir =.*\$$%srcdir = `pwd`%"  >Make.new
	@cat Make.new depends > Makefile
	@rm -f Make.new

stamp:
	touch stamp


# DO NOT remove following line!
#END_OF_MAKE
