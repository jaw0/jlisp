
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: dl.c,v 2.2 1997/05/30 00:28:46 jaw Exp $";
#endif

#include <jlisp.h>

void dlh = 0;

DEFUN("dlopen", Fdlopen, Sdlopen, 1,1,0,1,
      "(dlopen...) ",
      (Obj l))
{


}

DEFUN("dynlink", Fdynlink, Sdynlink, 4,8, 1,0,
      "(dynlink ...) dynamic linking support",
      (Obj lname, Obj cname, Obj min, Obj max, Obj ep, Obj lp, Obj fl, Obj cv))
{





}

