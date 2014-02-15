
/*
    Copyright (c) 1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: symbol.c,v 2.2 1997/05/30 00:28:50 jaw Exp $";
#endif

#include <jlisp.h>
#include <string.h>

int hash(char*);

typedef struct Symbol {
	char *name;
	int id;
	struct Symbol *next;
} Symbol;

static int symbol_id_counter = 0;
static int byid_len = 0;

Symbol ***byid, **byhash;

#define SYMHASHSIZE	3967
#define SYMARRAYSIZE	4096
#define SYMA2INITIAL	4090

/*
  byid -> [............]  (SYMA2INITIAL then grows)
            |
	    v
	    [......................]  (SYMARRAYSIZE)
	           |
		   v
		   {Symbol}

		   
  byhash -> [.........] (SYMHASHSIZE)
               |
	       v
	       {Symbol}->{Symbol}->{Symbol}

*/

int init_symbol(){
	int i;
	
	byhash = (Symbol**)my_malloc(SYMHASHSIZE * sizeof( Symbol* ));
	byid   = (Symbol***)my_malloc(SYMA2INITIAL * sizeof( Symbol** ));
	symbol_id_counter = 0;
	byid_len = SYMA2INITIAL;
	
	for(i=0;i<SYMHASHSIZE;i++)
		byhash[i] = 0;
	for(i=0;i<SYMA2INITIAL;i++)
		byid[i] = 0;

	return 1;
}

Obj maksym(char* p){
	return maksymbol(p, 1);
}
Obj maksym_c(char* p){
	return maksymbol(p, 0);
}

/* if it is already in the hash table return id, else insert it */
EXTERN_C
Obj maksymbol(char *p, int reallocate){
	int h, id, ida, idb;
	Symbol *s, *ns;

	h = hash(p) % SYMHASHSIZE;
	s = byhash[h];

	for(;s;s=s->next){
		if( !strcmp(p, s->name))
			return MAKSYM(s->id);
	}

	/* insert */
	/* into hash table */
	ns = (Symbol*)my_malloc(sizeof(Symbol));
	ns->next = byhash[h];
	byhash[h] = ns;
	ns->id = id = symbol_id_counter ++;
	if( reallocate ){
		ns->name = (char*)my_malloc(1 + strlen(p));
		strcpy(ns->name, p);
	}else{
		/* for constant compiled in strings */
		ns->name = p;
	}

	/* into array */
	ida = id / SYMARRAYSIZE;
	idb = id % SYMARRAYSIZE;

	if( ida > byid_len ){
		/* grow it */
		Symbol *** na;
		int i;

		na = (Symbol***)my_malloc(byid_len * 2 * sizeof( Symbol** ));
		for(i=0; i<byid_len; i++)
			na[i] = byid[i];
		for(; i<2*byid_len; i++)
			na[i] = 0;
		free( byid );
		byid = na;
		byid_len *= 2;
	}

	if( ! byid[ida] ){
		/* create it */
		int i;
		byid[ida] = (Symbol**)my_malloc(SYMARRAYSIZE * sizeof(Symbol*));
		for(i=0;i<SYMARRAYSIZE;i++)
			byid[ida][i] = 0;
	}
	
	byid[ida][idb] = ns;
	return MAKSYM(id);
}

EXTERN_C
char *symbolname(Obj a){
	int id, ida, idb;
	
	if( ! SYMBOLP(a)){
		JLERROR("symbolname", a, "WTA: symbolp");
		return 0;
	}
	id = CSYMID(a);
	ida = id / SYMARRAYSIZE;
	idb = id % SYMARRAYSIZE;

	return byid[ida][idb]->name;

}


int hash(char *p){
	register int hv = 0;

	while(*p) hv = hv<<1 ^ *p++;
	hv = hv<0 ? -hv : hv;
	return hv;
}

/* for debugging purposes */
DEFUN("symbolstats", Fsymbolstats, Ssymbolstats, 0,1,1,0, "(symbolstats [sym]) return some stats",
      (Obj a))
{

	if( SYMBOLP(a)){
		return MAKINT( CSYMID(a));
	}
	if( INUMP(a)){
		return MAKSYM( CINT(a));
	}

	return Fcons( MAKINT( symbol_id_counter ), MAKINT( byid_len ));
}

