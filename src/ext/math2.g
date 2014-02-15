
/*
  comment out any that you don't have
*/

#ifdef AGEN_JLISP
Title  math2.g
#	define DOC(x)   x
#else
#	define DOC(x)	/* x */
#endif

#ifndef MINIMAL

double  sin(double)      DOC("(sin n) take the sin\\n[see also: sin cos tan asin acos atan atan2]");
double  cos(double)      DOC("(cos n) take the cosine\\n[see also: sin cos tan asin acos atan atan2]");
double  tan(double)      DOC("(tan n) take the tangent\\n[see also: sin cos tan asin acos atan atan2]");
double  asin(double)     DOC("(asin n) take the arcsin\\n[see also: sin cos tan asin acos atan atan2]");
double  acos(double)     DOC("(acos n) take the arccosine\\n[see also: sin cos tan asin acos atan atan2]");
double  atan(double)     DOC("(atan n) take the arctangent\\n[see also: sin cos tan asin acos atan atan2]");
double  sqrt(double)     DOC("(sqrt n) take the square root");
double  cbrt(double)     DOC("(cbrt n) take the cube root");
double  sinh(double)     DOC("(sinh n) take the hyperbolic sin\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  cosh(double)     DOC("(cosh n) take the hyperbolic cosine\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  tanh(double)     DOC("(tanh n) take the hyperbolic tangent\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  asinh(double)    DOC("(asinh n) take the inverse hyperbolic sin\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  acosh(double)    DOC("(acosh n) take the inverse hyperbolic cosine\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  atanh(double)    DOC("(atanh n) take the inverse hyperbolic tangent\\n[see also: sinh cosh tanh asinh acosh atanh]");
double  fabs(double)     DOC("(fabs n) take the absolute value");

double  atan2(double, double)    DOC("(atan2 n1 n2)  the arctan of n2/n1\\n[see also: sin cos tan asin acos atan atan2]");
double  hypot(double, double)    DOC("(hypot n1 n2) Euclidean distance\\n[see also: atan2]");
double  pow(double, double)      DOC("(pow n1 n2) take n1 to the n2 power\\n[see also: log exp]");

#ifndef __NetBSD__
double  sinpi(double)    DOC("(sinpi n) take sin of pi * n\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  cospi(double)    DOC("(cospi n) take cos of pi * n\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  tanpi(double)    DOC("(tanpi n) take tan of pi * n\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  asinpi(double)   DOC("(asinpi n) take asin of (n) / pi\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  acospi(double)   DOC("(acospi n) take acos of (n) / pi\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  atanpi(double)   DOC("(atanpi n) take atan of (n) / pi\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");
double  atan2pi(double, double)  DOC("(atan2pi n1 n2)  arctan of (n2/n1) / pi\\n[see also: sinpi cospi tanpi asinpi acospi atanpi atan2pi]");

double  exp2(double)     DOC("(exp2 n) 2**x");
double  exp10(double)    DOC("(exp10 n) 10**x");
double  log2(double)     DOC("(log2 n) logarithm to  base 2");
double  log10(double)    DOC("(log10 n) logarithm to  base 10");

double  compound(double, double) DOC("(compound n1 n2) computes  (1+r)**n - the compound interest factor\\n[see also: annuity]");
double  annuity(double, double)  DOC("(annuity n1 n2) computes (1 - (1+r)**-n)/r - the present value of annuity factor\\n[see also: compound]");
#endif

double  j0(double)       DOC("(j0 n) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");
double  j1(double)       DOC("(j1 n) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");
double  jn(double, int)  DOC("(jn n1 n2) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");
double  y0(double)       DOC("(y0 n) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");
double  y1(double)       DOC("(y1 n) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");
double  yn(double, int)  DOC("(yn n1 n2) Bessel function\\n[see also: j0 j1 jn y0 y1 yn]");

double  expm1(double)    DOC("(expm1 n) e**x-1 accurately even for tiny x");

double  log1p(double)    DOC("(log1p n) log(1+x) accurately even for tiny x");


double  erf(double)      DOC("(erf n) error function\\n[see also: erfc]");
double  erfc(double)     DOC("(erfc n) error function\\n[see also: erf]");
double  lgamma(double)   DOC("(lgamma n) log gamma function");

#endif /* MINIMAL */

