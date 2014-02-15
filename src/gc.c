
/*
    Copyright (c) 1994,1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: gc.c,v 2.3 1998/06/18 20:12:33 jaw Exp $";
#endif

#include <jlisp.h>
#include <stdio.h>

extern Obj boxhead, envcurr, envlist, catchlist;
extern int cells_since_gc;
extern Obj internal_gc_protect;
extern Obj Vdebug_on_next_call;
extern Obj_Vtbl jlisp_vtbl[];
extern Backtrace *backtrace_list;

extern void gc_sweep_weak(void);
extern void freecell(Obj);

#define USE_TABLE
#ifdef USE_TABLE
/* this table speeds up marking the stack
   at a cost of higher memory usage (to hold the table)

   this reduces the total time spent in gc_mark stack
   from 15% to less than 1% of the total execution time
*/   
#define GC_BOX_TABLE_SIZE 1024

struct GcBoxTableEntry {
	Cell *low, *high;
};
Obj gc_box_table_next_box;
int gc_box_table_nentries;
struct GcBoxTableEntry gc_box_table[ GC_BOX_TABLE_SIZE ];
#endif


DEFVAR(".gc_stats", V_gc_stats, ".gc_stats  vector desribing the results of the last gc",
       IC_NIL)
     
void gc_unmark(void){
	Obj box = boxhead, lka;
	int i, sz;
	
	while( BOXCELLP( box )){

		/* travel the whole box */
		sz = CBOXSIZE( box );
		for(i=3; i< sz; i++){
			lka = (Obj)(CBOXPTR( box ) + i);
			if( FREECELLP(lka) ) continue;
			if( CONSP(lka) ) CDR( lka ) &= ~1;
			else CAR(lka) &= ~ GCBIT;
		}

		box = CBOXNEXT( box );
	}
}

int gc_sweep(void){
	Obj box = boxhead, lka;
	register int i, t;
	int sz;
	Obj fv;
	int totals[ TPV_LAST ];

	for(i=0; i<TPV_LAST; i++)
		totals[i] = 0;
	
	while( BOXCELLP( box )){

		/* travel the whole box */
		sz = CBOXSIZE( box );
		for(i=3; i< sz; i++){
			lka = (Obj)( CBOXPTR(box) + i);
			if( FREECELLP( lka ) ) continue;
			
			/* look at GC bit -- if clear, free cell */
			/* if set, clear it and continue */
			if( CONSP( lka ) ){
				if( CDR( lka ) & 1 ){
					CDR( lka ) &= ~1;
					continue;
				}
				totals[0] ++;
			}else{
				if( CAR( lka ) & (SDBIT | GCBIT)){
					CAR( lka ) &= ~GCBIT;
					continue;
				}
				t = TYPEOF( lka );
				totals[t] += jlisp_vtbl[ t ].free( lka );
			}

			freecell( lka );
		}
		box = CBOXNEXT( box );
	}
	fv =  makvect( TPV_LAST);
	for(i=0; i<TPV_LAST; i++)
		CVECTOR(fv)[i] = MAKINT( totals[i] );
	
	return fv;
}


void gc_mark(Obj a){

	if( IMMEDP( a )) return;
	
	if( CONSP( a )){
		if( CDR( a ) & 1) return;	/* already marked */
		CDR( a ) |= 1;			/* mark */
		gc_mark( CAR( a ));		/* recurse */
		gc_mark( CDR( a ) & ~1 );
	}else{
		if( CAR(a) & GCBIT) return;	/* already marked */
		CAR(a) |= GCBIT;
		jlisp_vtbl[ TYPEOF( a ) ].mark( a );
	}
}

void gc_mark_stack(Cell** base){
	int len = (int)(((Cell**)&base) - base);
	/* since stacks grow down, len is <0 */
	register int n;
	Cell *p, *btm, *top, *foo;
	Obj box;
	Obj boxstart = boxhead;
	int found;
	
	for(n=0; n>len; n--){
		/* traipse thru' the stack */
		p = base[n];
		if( CLASS0P((Obj)p) || ((Obj)p & 3) )
			continue;

		if( !gc_box_table[GC_BOX_TABLE_SIZE-1].low
		    || p < gc_box_table[GC_BOX_TABLE_SIZE-1].high ){
			/* if it looks like p is within the table - check
			   otherwise we start the search at the next box after
			   the table (if any)
			*/
			int lb=0, lm, at, k = gc_box_table_nentries;

			/* bsearch the table */
                        for(lm=k;lm;lm/=2){
                                at = lb + lm/2;
				if( p >= gc_box_table[at].low && p < gc_box_table[at].high ){
					/* it is valid */
					gc_mark( (Obj)p );
					break;
				}
				if( p > gc_box_table[at].low ){
					/* move right */
                                        lb = at + 1;
                                        lm--;
                                }
                        }
		}else{
			/* search the rest of the boxes */
			boxstart = gc_box_table_next_box;
			for(box=boxstart; BOXCELLP(box); box=CBOXNEXT(box)){
				/* see if it points to a valid cell */
				foo = CBOXPTR(box);
				btm = foo + 3;
				top = foo + CBOXSIZE(box);
				/* is it within box, and properly aligned within it */
				if( p>=btm && p<top && !(((char*)p - (char*)btm)%sizeof(Cell))){
					gc_mark( (Obj)p );
					break;
				}
			}
		}
	}
}

int comparefnc(struct GcBoxTableEntry *a, struct GcBoxTableEntry *b){
	return a->low - b->low;
}

void gc_mk_table(void){
	Obj box;
	int i = 0;
	Cell *btm, *top, *foo;

	for(i=0; i< GC_BOX_TABLE_SIZE; i++){
		gc_box_table[i].low = gc_box_table[i].high = 0;
	}
	for(i=0,box=boxhead; BOXCELLP(box) && i < GC_BOX_TABLE_SIZE; box=CBOXNEXT(box)){
		foo = CBOXPTR(box);
		btm = foo + 3;
		top = foo + CBOXSIZE(box);

		gc_box_table[i].low  = btm;
		gc_box_table[i].high = top;
		i++;
	}
	/* if not all boxes fit in the table, save the next box
	   when searching, if it isnt in the table, start the search from here
	*/
	gc_box_table_next_box = box;
	gc_box_table_nentries = i;
	qsort( gc_box_table, i, sizeof( struct GcBoxTableEntry ), comparefnc);
}

DEFUN("gc", Fgc, Sgc, 0, 0, 1,0, "(gc) Garbage collect",
      ())
{
	extern Obj *stackbase;
	Obj fv;
	Obj dbger = VALUE( Vdebug_on_next_call );
	int sigs;
	
	/* turn off debugger while gcing */
	VALUE( Vdebug_on_next_call ) = IC_FALSE;
	
	cells_since_gc = 0;
	Frunhooks(maksym_c("before-gc-hooks"));
	
	VALUE(V_gc_stats) = IC_NIL;

	gc_mk_table(); /* to speed up marking stack */
	DISABLE(sigs);
	gc_mark( envlist );
	gc_mark( envcurr );
	gc_mark( catchlist );
	gc_mark( internal_gc_protect );
	gc_mark_stack( (Cell**)stackbase);
#ifdef STACK_IS_UNALIGNED
	gc_mark_stack( (Cell**)((int)stackbase + 1) );
	gc_mark_stack( (Cell**)((int)stackbase + 2) );
	gc_mark_stack( (Cell**)((int)stackbase + 3) ); 
#endif
#ifndef NO_WEAKS	
	gc_sweep_weak();
#endif	
	VALUE(V_gc_stats) = fv = gc_sweep();
	RENABLE(sigs);
	
	Frunhooks(maksym_c("after-gc-hooks"));

	/* restore debugger */
	VALUE( Vdebug_on_next_call ) = dbger;
	return fv;
}





	
	
