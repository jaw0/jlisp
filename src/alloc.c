
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: alloc.c,v 2.2 1997/05/30 00:28:27 jaw Exp $";
#endif

#ifdef __GNUC__
#	define	__USE_FIXED_PROTOTYPES__
#endif

#include <jlisp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define FIRST_BOX_SIZE		1024
#define DEFAULT_BOX_SIZE	(VALUE( V_box_size ))
#define GC_CELL_THRESH		(VALUE( V_gc_thresh))

DEFVAR(".box_size", V_box_size,
       ".box_size  number of cells to allocate at a time from the system",
       MAKINT(1024))

DEFVAR(".gc_thresh", V_gc_thresh,
       ".gc_thresh  threshhold number of cells after which we do a gc",
       MAKINT(10240) )

extern Obj V_gc_stats;

Obj boxhead = IC_NIL, freehint = IC_NIL;
int cells_since_gc = 0;
int cells_being_used = 0;
int boxes_being_used = 0;
int cells_total = 0;

int good_hints = 0;
int bad_hints  = 0;


EXTERN_C
Obj newcell(void){
	/* this ought be sped up */
	Obj x, y;

	cells_being_used ++;
	cells_since_gc ++;
	
	if( BOXCELLP( freehint ) && CBOXNFREE( freehint )!=0 ){
		/* a good hint -- use it */
		x = freehint;
		good_hints ++;
	}else{
		x = boxhead;
		bad_hints ++;
		/* search list of boxes for one with a free cell */
		while( BOXCELLP( x ) && CBOXNFREE( x )==0 ){
			x = CBOXNEXT( x );
		}
	}
	
	if( ! BOXCELLP( x )){
		/* found none! */

		/* try a gc? */
		if( V_gc_thresh ){
			if( CINT(GC_CELL_THRESH) < CINT(DEFAULT_BOX_SIZE) )
				GC_CELL_THRESH = DEFAULT_BOX_SIZE;
	
			if(cells_since_gc > CINT(GC_CELL_THRESH)){
				Fgc();
				return newcell();
			}
		}
		
		/* allocate more */
		if( V_box_size ){
			if( CINT( DEFAULT_BOX_SIZE) < 10)
				DEFAULT_BOX_SIZE = MAKINT(10);
		
			y = newbox( CINT(DEFAULT_BOX_SIZE) );
		}else{
			y = newbox( FIRST_BOX_SIZE );
		}
		/* prepend to list */
		CBOXNEXT( y ) = boxhead;
		boxhead = x = y;
	}
	
	if( CBOXNFREE( x )){
		/* we have a box with some free cells */
		y = CBOXFREECELL( x );
		CBOXFREECELL( x ) = CFREENEXT( y );
		CBOXNFREE( x ) --;

		if( CBOXNFREE( x ) ){
			freehint = x;
		}else{
			freehint = CBOXNEXT( x );
		}
		CAR(y) = IC_UNDEF;
		CDR(y) = IC_UNDEF;
		return y;
	}
	/* a most serious error has occured */
	puts("A most serious error condition has arisen\n"
	     "Moriturus te saluto");	/* I who am about to die salute you */
	return Fquit(MAKINT(2));

}

EXTERN_C
Obj newbox(int size){
	/* there are 3 cells of overhead needed */
	Cell * boxes = (Cell*)my_malloc((size)*sizeof(Cell));
	int i;

	boxes_being_used ++;
	cells_total += size;
	
	boxes[0].car = size;		/* total */
	boxes[0].cdr = IC_NIL;		/* next box */
	
	boxes[1].car = size-3;		/* n free */
	boxes[1].cdr = (Obj)(boxes + 3);/* free list */
	
	/* identify self */
	boxes[2].car = MAKETYPE(TPV_BOX_CELLS);	
	boxes[2].cdr = (Obj)boxes;

	for(i=size-1; i>=3; i--){
		boxes[i].car = MAKETYPE( TPV_FREE_CELL );
		boxes[i].cdr = (Obj)(boxes + i + 1);
	}
	boxes[size-1].cdr = IC_NIL;
	
	return (Obj)(boxes + 2);
}

EXTERN_C
void freecell(Obj cell){
	Obj x;
	if( !CLASS1P( cell )){
		/* error */
	}

	cells_being_used --;
	
	/* pick a nice place to put it */
	/* this will cause the free lists to be tangled */
	/* we probobly want the GC to untangle them */
	
	if( BOXCELLP( freehint )) x = freehint;
	else x = boxhead;
	
	CAR( cell ) = MAKETYPE( TPV_FREE_CELL );
	CDR( cell ) = CBOXFREECELL( x );
	CBOXFREECELL( x ) = cell;
	CBOXNFREE( x ) ++;

	freehint = x;
}

EXTERN_C
void *my_malloc(int size){
	void *x;

	while(1){
		x = (void*)malloc( size );
		if(x) return x;
		Fgc();
		x = (void*)malloc( size );
		if(x) return x;
		/* compact_space();
		
		x = (void*)malloc( size ); */
		if(x) return x;

		sleep(60);	/* and cross fingers */
	}
}

/* some statistics reports */
DEFUN("memory-stats", Fmem_stat, Smem_stat, 0,0,1,0,
      "(memory-stats) return statistics on memory usage\n"
      "returns: #(cells_since_gc cells_being_used boxes_being_used cells_total\n"
      "\tgood_hints bad_hints gc_stats)",
      ())
{
	Obj v = makvect(7);

	CVECTOR(v)[0] = MAKINT(cells_since_gc);
	CVECTOR(v)[1] = MAKINT(cells_being_used);
	CVECTOR(v)[2] = MAKINT(boxes_being_used);
	CVECTOR(v)[3] = MAKINT(cells_total);
	CVECTOR(v)[4] = MAKINT(good_hints);
	CVECTOR(v)[5] = MAKINT(bad_hints);
	CVECTOR(v)[6] = VALUE(V_gc_stats);
	
	return v;
}

