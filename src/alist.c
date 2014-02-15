
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: alist.c,v 2.2 1997/05/30 00:28:29 jaw Exp $";
#endif

#include <jlisp.h>


DEFUN("assoc", Fassoc, Sassoc, 2,2,1,0, 
      "(assoc elt alist) is item the car of a member (equal) of alist",
      (Obj elt, Obj alist) )
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Fequal(CAAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

DEFUN("assq", Fassq, Sassq, 2,2,1,0,
      "(assq elt alist) is item the car of a member (eq) of alist",
      (Obj elt, Obj alist))
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Feq(CAAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

DEFUN("assv", Fassv, Sassv, 2,2,1,0,
      "(assv elt alist) is item the car of a member (eqv) of alist",
      (Obj elt, Obj alist))
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Feqv(CAAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

#ifndef MINIMAL
DEFUN("rassoc", Frassoc, Srassoc, 2,2,1,0,
      "(rassoc relt alist) is item the cdr of a member (equal) of alist",
      (Obj elt, Obj alist))
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Fequal(CDAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

DEFUN("rassq", Frassq, Srassq, 2,2,1,0,
      "(rassq relt alist) is item the cdr of a member (eq) of alist",
      (Obj elt, Obj alist))
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Feq(CDAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

DEFUN("rassv", Frassv, Srassv, 2,2,1,0,
      "(rassv relt alist) is item the cdr of a member (eqv) of alist",
      (Obj elt, Obj alist))
{
	Obj itm, res;

	for(itm=alist; CONSP(itm); itm=CDR(itm)) {
		if(NCONSP(CAR(itm))) continue;
		res = Feqv(CDAR(itm), elt);
		if(!FALSEP(res))
			return CAR(itm);
	}
	return IC_FALSE;
}

#endif /* MINIMAL */
