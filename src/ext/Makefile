

OBJS  = math2.o unistd2.o libc.o ext_init.o
# OBJS  = ext_init.o

ASRCS = math2.c unistd2.c libc.c
# ASRCS = 

SRCS = ext_init.c $(ASRCS)


all:  jlext.a
.SUFFIXES: $(SUFFIXES) .g

$(ASRCS): gen-ipl-h

.g.c:
	cpp -P -DAGEN_JLISP $< | ./gen-ipl-h > $@

ext_init.o: defun.list defvar.list

# ipl = Image Processing Library, the code's original purpose
gen-ipl-h-lex.c: gen-ipl-h.l
	lex gen-ipl-h.l
	mv lex.yy.c gen-ipl-h-lex.c

gen-ipl-h.c: gen-ipl-h.y
	yacc gen-ipl-h.y
	mv y.tab.c gen-ipl-h.c

gen-ipl-h: gen-ipl-h.c gen-ipl-h-lex.c
	$(CC) -o gen-ipl-h gen-ipl-h.c -ly -g

jlext.a: $(OBJS)
	ar rcuv jlext.a $(OBJS)
	ranlib jlext.a

defun.list: defun.list.tmp
	../../bin/if-changed cp defun.list.tmp defun.list
defun.list.tmp: $(SRCS)
	exec > defun.list.tmp;\
	for f in $(SRCS); do \
		echo ""; \
		echo "/* $$f */"  ; \
		sed -n -f ..//mdefun.sed $$f ; \
	done
 
defvar.list: defvar.list.tmp
	../../bin/if-changed cp defvar.list.tmp defvar.list
defvar.list.tmp: $(SRCS)
	exec > defvar.list.tmp;\
	for f in $(SRCS); do \
		echo ""; \
		echo "/* $$f */"  ; \
		sed -n -f ../mdefvar.sed $$f ; \
	done


clean:
	-rm -f $(OBJS) $(ASRCS) jlext.a defun.list defun.list.tmp defvar.list defvar.list.tmp
	-rm -f gen-ipl-h gen-ipl-h.c gen-ipl-h-lex.c

realclean: clean


