
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: weak.c,v 2.2 1997/05/30 00:28:37 jaw Exp $";
#endif


#include <jlisp.h>

#define WEAKLYP(w)	(TYPEOFX(w)==TPV_WEAK)
#define WEAKOK(w)	( CLENGTH(w) ? IC_TRUE : IC_FALSE )
#define CWEAK(w)	((Weak*)CDR(w))

typedef struct {
	Obj obj;
	Obj next;
} Weak;

#ifndef NO_WEAKS

Obj list_of_weak = 0;

DEFUN("weakly", Fweakly, Sweakly, 1,1,1,0,
      "(weakly obj) creates a weakly held obj\n"
      "which may be GC'ed\n"
      "[see also: ",
      (Obj a))
{
	Obj c = newcell(), bar;
	int sigs;

	bar = (Obj)(Obj*)my_malloc(sizeof(Weak));
	DISABLE( sigs );
	CAR(c) = MAKETYPE( TPV_WEAK ) | ( 1 << 12 );
	CDR(c) = bar;
	CWEAK(c)->obj = a;
	CWEAK(c)->next = 0;
	RENABLE( sigs );
	
	return c;
}

DEFUN("weakp", Fweakp, Sweakp, 1,1,1,0,
      "(weakp obj) is obj a weakly held obj?",
      (Obj a))
{
	return WEAKLYP( a ) ? IC_TRUE : IC_FALSE;
}

DEFUN("weak-set!", Fweak_set, Sweak_set, 2,2,1,0,
      "(weak-set! weak obj) set the weakly held obj of weak to obj\n"
      " returns obj",
      (Obj w, Obj val))
{
	int sigs;
	
	if(! WEAKLYP(w))
		return JLERROR(Sweak_set.name, w, "WTA: weakp");

	DISABLE( sigs );
	CAR(w) |= ( 1 << 12 );
	CWEAK(w)->obj = val;
	RENABLE( sigs );
	return val;
}

DEFUN("weak-ok?", Fweak_ok, Sweak_ok, 1,1,1,0,
      "(weak-ok? weak) has the obj held not been reclaimed?",
      (Obj w))
{

	if( WEAKLYP(w))
		return WEAKOK(w);
	return IC_FALSE;
}


DEFUN("weak-obj", Fweak_obj, Sweak_obj, 1,1,1,0,
      "(weak-obj weak) return the obj held by weak",
      (Obj w))
{
	if(! WEAKLYP(w))
		return JLERROR(Sweak_obj.name, w, "WTA: weakp");

	return CWEAK(w)->obj;
}


void markweak(Obj w){

	CWEAK(w)->next = list_of_weak;
	list_of_weak   = w;
}

Obj eqweak(Obj a, Obj b){

	return (CWEAK(a)->obj == CWEAK(b)->obj) ? IC_TRUE : IC_FALSE;
}

void gc_sweep_weak(void){
	/* done before gc_sweep, to adjust the weak's */

	Obj w = list_of_weak;
	Obj c;
	
	while( w ){
		c = CWEAK(w)->obj;

		/* check the object's gc bit */
		if( IMMEDP(c)){
			/* immediates are not gc'ed */
		}else if( CONSP(c) ?( !(CDR(c) & 1) ):( !(CAR(c) & (SDBIT | GCBIT)) )){

			CWEAK(w)->obj = IC_FALSE;
			CAR(w) &= ~( 1<<12 );
		}

		w = CWEAK(w)->next;
	}
	list_of_weak = 0;
}

#endif /* NO_WEAKS */

			
		

				
			
		
	
