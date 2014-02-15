
/*
  comment out any that you don't have
*/
 
#ifdef AGEN_JLISP
Title  libc.g
#       define DOC(x)   x
#       define ErrNo    errno0
#else
#       define DOC(x)   /* x */
#       define ErrNo    int
#endif


/* char* crypt(char*, char*); */

