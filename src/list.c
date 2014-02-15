
/*
    Copyright (c) 1994, 1997 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: list.c,v 2.2 1997/05/30 00:28:30 jaw Exp $";
#endif


#include <jlisp.h>

DEFUN("append", Fappend, Sappend, 1,1,1,1,
      "(append list...) concatenate lists",
      (Obj lsts))
{
	Obj cls,cel, head=IC_NIL,tail=IC_NIL, nw;

	for(cls=lsts; CONSP(cls); cls=CDR(cls)){
		if(NULLP(CDR(cls))){
			/* last list -- don't copy */
			if(NNULLP(tail)){
				CDR(tail) = CAR(cls);
			}else{
				tail = head = CAR(cls);
			}
			break;
		}
		for(cel=CAR(cls); CONSP(cel); cel=CDR(cel)){
			nw = Fcons(CAR(cel), IC_NIL);
			if(NULLP(tail)){
				head = tail = nw;
			}else{
				CDR(tail) = nw;
				tail = nw;
			}
		}
	}

	return head;
}

DEFUN("append!", Fappendb,Sappendb, 1,1,1,1,
      "(append! list...) destructively concatenate lists",
      (Obj lsts))
{
	Obj cls, cel,head=IC_NIL,tail=IC_NIL;

	for(cls=lsts; CONSP(cls); cls=CDR(cls)){
		if(NULLP(tail)){
			tail = head = CAR(cls);
		}else{
			CDR(tail) = CAR(cls);
		}
		
		for(cel=CAR(cls); CONSP(cel); cel=CDR(cel)) tail = cel;
	}
	return head;
}

#define xxBODY					\
if( NULLP(tail)){				\
	tail = head = Fcons(result, IC_NIL);	\
}else{						\
	CDR(tail) = Fcons(result, IC_NIL);	\
	tail = CDR(tail);			\
}

DEFUN("mapcar", Fmapcar, Smapcar, 2,2,1,0,
      "(mapcar fnc list) Apply function to all elems of list and\n"
      "return list of results",
      (Obj fnc, Obj lst))
{
	Obj cls, head=IC_NIL, tail=IC_NIL, result;
	int i,l;

	if( CONSP(lst)){
		for(cls=lst; CONSP(cls); cls=CDR(cls)){
			result = funcall_1(Smapcar.name, fnc, CAR(cls));
			xxBODY;
		}
	}else if(VECTORP(lst) || STRUCTP(lst) || ENVECP(lst)){
		l = CLENGTH(lst);
		for(i=0; i<l; i++){
			result = funcall_1(Smapcar.name, fnc, CVECTOR(lst)[i]);
			xxBODY;
		}
	}else if(STRINGP(lst)){
		l = CLENGTH(lst);
		for(i=0; i<l; i++){
			result = funcall_1(Smapcar.name, fnc,
					   MAKCHAR(CCHARS(lst)[i]));
			xxBODY;
		}
	}else if( NULLP(lst)){
		/* nop */
	}else{
		return JLERROR(Smapcar.name, lst, "WTA: sequence p");
	}
	
	return head;
}

DEFUN("map", Fmap, Smap, 1,1,1,1,
      "(map fnc lists...) apply fnc to elements of lists",
      (Obj l))
{
	Obj f, result, head=IC_NIL, tail=IC_NIL, flh, flt;
	Obj lc;
	
	f = CAR(l);
	l = CDR(l);

	if( !CCODEP(f) && !FUNCTIONP(f))
		return JLERROR(Smap.name, f, "WTA: functionp");

	while( NNULLP(l) && NNULLP(CAR(l)) ){

		/* build up list to eval */
		/* func and 1 arg from each list */
		lc = l;
		flh = Fcons(f, IC_NIL);
		flt = flh;
		while( NNULLP(lc) ){
			CDR(flt) = Fcons( CAAR(lc), IC_NIL );
			flt = CDR(flt);
			if( CONSP( CAR(lc) ))
				CAR(lc) = CDAR(lc);
			lc = CDR(lc);
		}
		result = eval_internal(flh, 0, Smap.name);
		xxBODY;
	}
	return head;
}

/* like mapcar - but we don't cons up the results */
DEFUN("for-each", Fforeach, Sforeach, 2,2,1,0,
      "(for-each fnc list) Apply function to all elems of list",
      (Obj fnc, Obj lst))
{
	Obj cls;
	int i,l;
	
	if( CONSP(lst)){
		for(cls=lst; CONSP(cls); cls=CDR(cls)){
			funcall_1(Sforeach.name, fnc, CAR(cls));
		}
	}else if( VECTORP(lst) || STRUCTP(lst) || ENVECP(lst)){
		l = CLENGTH(lst);
		for(i=0; i<l; i++){
			funcall_1(Sforeach.name, fnc, CVECTOR(lst)[i]);
		}
	}else if( STRINGP(lst)){
		l = CLENGTH(lst);
		for(i=0; i<l; i++){
			funcall_1(Sforeach.name, fnc, MAKCHAR(CCHARS(lst)[i]));
		}
	}else if( NULLP(lst)){
		/* nop */
	}else{
		return JLERROR(Sforeach.name, lst, "WTA: sequence p");
	}
		
	return IC_UNSPEC;
}

DEFUN("member", Fmember,Smember, 2,2,1,0,
      "(member itm list) is item a member (equal) of list",
      (Obj elt, Obj list))
{
	Obj itm, res;

	for(itm=list; CONSP(itm); itm=CDR(itm)){
		res = Fequal(CAR(itm), elt);
		if(!FALSEP(res))
			return itm;
	}
	return IC_FALSE;
}

DEFUN("memq", Fmemq, Smemq, 2,2,1,0,
      "(memq item list) is item a member (eq) of list",
      (Obj elt, Obj list))
{
	Obj itm, res;

	for(itm=list; CONSP(itm); itm=CDR(itm)){
		res = Feq(CAR(itm), elt);
		if(!FALSEP(res))
			return itm;
	}
	return IC_FALSE;
}

DEFUN("memv", Fmemv, Smemv, 2,2,1,0,
      "(memv item list) is item a member (eqv) of list",
      (Obj elt, Obj list))
{
	Obj itm, res;

	for(itm=list; CONSP(itm); itm=CDR(itm)){
		res = Feqv(CAR(itm), elt);
		if(!FALSEP(res))
			return itm;
	}
	return IC_FALSE;
}



DEFUN("list->vector",Flist_vect,Slist_vect,1,1,1,0,
      "(list->vector list) create a vector from the elemnts of list",
      (Obj l))
{
	int len, i;
	Obj v;

	if(NULLP(l)) return makvect(0);
	if(NCONSP(l)) return JLERROR(Slist_vect.name ,l, "WTA: listp");

	len = CINT(Flength(l));
	v = makvect(len);

	for(i=0; CONSP(l); l=CDR(l)){
		CVECTOR(v)[i++] = CAR(l);
	}
	return v;
}

DEFUN("vector->list", Fvect_list,Svect_list,1,1,1,0,
      "(vector->list vector) create a list from elements of vector",
      (Obj v))
{
	int len, i;
	Obj head=IC_NIL, tail;

	if(! VECTORP(v)) return JLERROR(Svect_list.name, v, "WTA: vectorp");

	len = CLENGTH(v);
	for(i=0; i<len; i++){
		if( NULLP(head))
			head = tail = Fcons( CVECTOR(v)[i], IC_NIL);
		else{
			CDR(tail) = Fcons( CVECTOR(v)[i], IC_NIL);
			tail= CDR(tail);
		}
	}
	return head;
}

DEFUN("copy-deep", Fcopydeep, Scopydeep, 1,1,1,0,
      "(copy-deep list) return a list that is a copy of the original\n"
      "all sublists are recursively copied",
      (Obj l))
{

	if( NCONSP(l))
		return l;

	return Fcons(
		Fcopydeep( CAR(l)),
		Fcopydeep( CDR(l)));
}

DEFUN("copy-list", Fcopylist, Scopylist, 1,1,1,0,
      "(copy-list list) return a copy of the list",
      (Obj l))
{
	Obj cls, head=IC_NIL, tail=IC_NIL;

	for(cls=l; CONSP(cls); cls=CDR(cls)){
		
		if(NULLP(tail)){
			tail = head = Fcons(CAR(cls), IC_NIL);
		}else{
			CDR(tail) = Fcons(CAR(cls), IC_NIL);
			tail = CDR(tail);
		}
	}
	return head;
}

