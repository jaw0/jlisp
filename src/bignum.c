
/*
    Copyright (c) 1994 Jeff Weisberg

    see the file "License"
*/

#ifdef RCSID
static const char *const rcsid
= "@(#)$Id: bignum.c,v 2.2 1997/05/30 00:28:39 jaw Exp $";
#endif


#include <jlisp.h>
#include <math.h>
#include <limits.h>

#define CRLENGTH(x)	(CAR(x) >> 12)
#define BIG_NEGP(x)	(CRLENGTH(x) < 0)

#define big_alloc_size	str_alloc_size
#define BIG_ALLOC( x )	((unsigned short*)my_malloc(sizeof(short)*big_alloc_size(x)))

/*
    XXX XXX XXX
    we assume 2 * sizeof(short) == sizeof(int) == 32 
*/

#define BITS_DIGIT	(sizeof(short) * CHAR_BIT)
#define BIGRADIX	(1UL << BITS_DIGIT)
#define MAXDIGIT	(BIGRADIX - 1UL)

#ifdef HAS_NO_LOG2
#	define log2(x)	(log((double)x) / M_LN2)
#endif

extern void writechar(Obj, int);

#ifndef NO_BIGNUMS


/*
  Note:

       bignums are stored as little indians

       ( [0] = lsw .. [N-1] = msb)
*/       
  
EXTERN_C
Obj makebig(int len){
	Obj foo=newcell(), bar;
	int sigs;
	
	bar = (Obj)BIG_ALLOC(len);
	DISABLE( sigs );
	CAR(foo) = MAKETYPE( TPV_BIGNUM ) | ((len)<<12);
	CDR(foo) = bar;
	RENABLE( sigs );

	return foo;
}

EXTERN_C
void free_big(Obj b){
	int sigs;

	DISABLE(sigs);
	free( (void*)CDR(b));
	freecell(b);
	RENABLE(sigs);
}
	
static void big_set_sign(Obj b, int minusp){
	int l = CLENGTH(b);

	if(minusp) l = -l;
	CAR(b) =  MAKETYPE( TPV_BIGNUM ) | ((l)<<12);
}

static Obj copy_big(Obj b){
	int l = CLENGTH(b);
	Obj c = makebig( l );
	int i;

	for(i=0; i<l; i++){
		CBIGNUM(c)[i] = CBIGNUM(b)[i];
	}
	
	big_set_sign(c, BIG_NEGP(b));

	return c;
}

static Obj big_adjust_len(Obj b, int nl){
	int ol = CLENGTH(b);
	int ml = MIN(ol, nl);
	int i;
	int sigs;
	int sign = BIG_NEGP(b);
	unsigned short* bar;

	if( nl == ol) return b;
	
	if( big_alloc_size(ol) == big_alloc_size(nl)){
		if(sign) nl = -nl;
		CAR(b) =  MAKETYPE( TPV_BIGNUM ) | ((nl)<<12);
		return b;
	}
	if(! nl){
		nl = 1;
		CBIGNUM(b)[0] = 0;
	}

	bar = BIG_ALLOC( nl );
	for(i=0; i<ml; i++){
		bar[i] = CBIGNUM(b)[i];
	}
	for( ; i<nl; i++){
		bar[i] = 0;
	}
	if(sign) nl = -nl;
	DISABLE(sigs);
	CAR(b) =  MAKETYPE( TPV_BIGNUM ) | ((nl)<<12);
	free( (short*) CDR(b));
	CDR(b) = (Obj)bar;
	RENABLE(sigs);

	return b;
}

static Obj big_normal(Obj b){
	int l = CLENGTH(b);
	int s = BIG_NEGP(b);
	int i, nl=l;

	for(i=l-1; i>=0; i--){
		if( CBIGNUM(b)[i] ) break;
		/* else it has leading 0s -- remove */
		nl --;
	}
	return big_adjust_len(b, nl);
}

Obj cint_to_big(IntType l){
	Obj foo;
	int sgn = l<0 ? 1:0;
	int size;
	l = l<0 ? -l : l;

	size = l >= BIGRADIX ? 2 : 1;
	foo = makebig( size );
	big_set_sign(foo, sgn);
	
	CBIGNUM(foo)[0] = l & MAXDIGIT;
	if (size > 1)
		CBIGNUM(foo)[1] = l >> BITS_DIGIT;

	return big_normal( foo);
}

IntType big_to_cint(Obj b){
	int len=CLENGTH(b);
	IntType l = 0;
	
	if( len)
		l = CBIGNUM(b)[0];
	if( len > 1)
		l += (UIntType)CBIGNUM(b)[1] << BITS_DIGIT;

	/* if even bigger, it won't fit in an int
	   ergo, it is truncated */
	
	if( BIG_NEGP(b))
		l = -l;

	return l;
}

double big_to_cdouble(Obj b){
	double d=0.0, sc=1.0;
	int i;
	int l = CLENGTH(b);
	
	for(i=0; i<l; i++){
		d += (UIntType)CBIGNUM(b)[i] * sc;
		sc *= 1 << BITS_DIGIT;
	}
	if( BIG_NEGP(b))
		d = -d;

	return d;
}

Obj cdouble_to_big(double d){
	int i, l;
	int sign = d<0;
	UIntType foo;
	double div;
	Obj b;

	if( d < 1.0 ) return makebig(0);

	if(d<0) d = -d;
	/* get upper bound on required size */
	l = 1.0 + floor( log2(d) / (double)BITS_DIGIT );

	b = makebig( l );
	big_set_sign(b, sign);

	div = pow(BIGRADIX, l-1);
	for(i=l-1; i>=0; i--){
		foo = d / div;
		CBIGNUM(b)[i] = foo;
		d -= foo * div;
		div /= BIGRADIX;
	}
	b = big_normal( b);
	return b;
}

Obj eqbign(Obj a, Obj b){
	int l = CLENGTH(a);
	int i;
	
	if( l != CLENGTH(b)) return IC_FALSE;
	if( BIG_NEGP(a) != BIG_NEGP(b)) return IC_FALSE;

	for(i=0; i<l; i++)
		if( CBIGNUM(a)[i] != CBIGNUM(b)[i]) return IC_FALSE;

	return IC_TRUE;
}

int big_compare(Obj a, Obj b){
	int la = CLENGTH(a),
		lb = CLENGTH(b),
		/*lax = MAX(la, lb),*/
		lmin = MIN(la, lb),
		sa = BIG_NEGP(a),
		sb = BIG_NEGP(b);
	int i;
	IntType foo;

	if(la > lb){
		for(i=la-1; i>=lb; i--)
			if( CBIGNUM(a)[i] )
				return sa ? -1 : 1;
	}else if( lb > la){
		for(i=lb-1; i>=la; i--)
			if( CBIGNUM(b)[i] )
				return sb ? 1 : -1;
	}
	for(i=lmin-1; i>=0; i--){
		foo = (sa ? -1 : 1) * (IntType)CBIGNUM(a)[i] 
			- (sb ? -1 : 1) * (IntType)CBIGNUM(b)[i] ;
		if( foo < 0)
			return -1;
		else if( foo > 0)
			return 1;
	}

	return 0;
}

Obj big_plus_minus(Obj a, Obj b, int minusp){
	int la = CLENGTH(a),
		lb = CLENGTH(b),
		sa = BIG_NEGP(a),
		sb = BIG_NEGP(b) ^ minusp,
		lmin = MIN(la, lb),
		lmax = MAX(la, lb),
		lc=0,
		resign, i, j;
	IntType foo, carry=0;
	Obj c = makebig( lmax ), qux;

	if( la < lb){
		/* swap */
		qux = a; a =  b; b = qux;
		foo = sa; sa = sb; sb = foo;
	}

	resign = sa;
	minusp = sa!=sb;
	
	for(i=0; i<lmin; i++){
		foo = minusp? ((IntType)CBIGNUM(a)[i] - (IntType)CBIGNUM(b)[i] - carry)
			: ((IntType)CBIGNUM(a)[i] + (IntType)CBIGNUM(b)[i] + carry);
		CBIGNUM(c)[i] = foo & MAXDIGIT;
		carry = foo >= BIGRADIX;
		if( foo ) lc = i;
	}
	for( ;i<lmax; i++){
		foo = minusp? ((IntType)CBIGNUM(a)[i] - carry)
			: ((IntType)CBIGNUM(a)[i] + carry);
		CBIGNUM(c)[i] = foo & MAXDIGIT;
		carry = foo >= BIGRADIX;
		if( foo ) lc = i;
	}
	if( carry ){
		if(! minusp){
			CBIGNUM(c)[i] = 1;
			lc = i;
		}else{
			resign ^= 1;
			for(j=0; j<i; j++){
				foo = - (IntType)CBIGNUM(c)[j];
				CBIGNUM(c)[j] = foo & MAXDIGIT;
			}
		}
	}

	/* set sign and adjust length of c to lc+1 */
	big_set_sign(c, resign);
	c = big_normal( big_adjust_len(c, lc+1));
	return c;
}

Obj big_multiply(Obj a, Obj b){
	int la = CLENGTH(a),
		lb = CLENGTH(b),
		lc; 
	int i,j;
	IntType foo, carry=0;
	Obj c;

	lc = la + lb;
	c = makebig(lc);
	big_set_sign(c, BIG_NEGP(a) ^ BIG_NEGP(b));

	for(i=0; i<lc-1; i++){
		foo = carry;
		for(j=MAX(i-la+1, 0); j<MIN(lb, i+1); j++){
			foo += (IntType)CBIGNUM(b)[j] * (IntType)CBIGNUM(a)[i - j];
		}
		carry = foo >> BITS_DIGIT;
		CBIGNUM(c)[i] = foo & MAXDIGIT;
	}
	CBIGNUM(c)[i] = carry;
	if(! carry ){
		lc--;
	}

	c = big_normal( big_adjust_len(c, lc));
	return c;
}

static Obj big_short_multiply(Obj a, unsigned short b){
	int la = CLENGTH(a), lc=la + 1;
	int i;
	IntType foo, carry=0;
	Obj c = makebig( lc );
	
	for(i=0; i< la; i++){
		foo = (IntType)CBIGNUM(a)[i] * b + carry;
		CBIGNUM(c)[i] = foo & MAXDIGIT;
		carry = foo >> BITS_DIGIT;
	}
	CBIGNUM(c)[i] = carry;
	if(! carry ){
		lc--;
	}

	big_set_sign(c, BIG_NEGP(a));
	c = big_normal( big_adjust_len(c, lc));
	return c;
}

static Obj big_short_div(Obj a, unsigned short b, int mode){
	int la = CLENGTH(a);
	int i;
	IntType foo, bar, carry=0;
	Obj c = makebig( la );
	
	for(i=la-1; i>=0; i--){
		carry <<= BITS_DIGIT;
		bar = (IntType)CBIGNUM(a)[i] + carry;
		foo = (bar / (IntType)b) & MAXDIGIT;
		CBIGNUM(c)[i] = foo;
		carry = bar - foo * (IntType)b;
	}
	switch (mode){
	  case '/':
		big_set_sign(c, BIG_NEGP(a));
		c = big_normal(c);
		return c;
	  case '%':
		free_big( c );
		return MAKINT( carry );
	}
}

Obj big_invert(Obj a){
	int la = CLENGTH(a);
	int i;
	Obj c;

	c = makebig(la);

	for(i=0; i<la; i++){
		CBIGNUM(c)[i] = ~ CBIGNUM(a)[i];
	}
	big_set_sign(c, BIG_NEGP(a)?0:1);	/* invert sign */
	c = big_normal(c);
	return c;
}

Obj big_logical(Obj a, Obj b, int op, char *name){
	int la = CLENGTH(a),
		lb = CLENGTH(b),
		lmin = MIN(la, lb),
		lmax = MAX(la, lb);

	Obj foo, c;
	int sa, sb, za=0, zb=0;
	int i;
	IntType qux, quux;
	
	c = makebig(lmax);
	
	if( la < lb){
		/* swap */
		foo = a; a =  b; b = foo;
	}

	sa = BIG_NEGP(a);
	sb = BIG_NEGP(b);
	
	for(i=0; i<lmin; i++){
		
		/* handle negative args correctly */
		/* (for some definition of correctly) */
		qux = sa ? (za ?
			  (~(IntType)CBIGNUM(a)[i])
			  : (-(IntType)CBIGNUM(a)[i]))
			: (IntType)CBIGNUM(a)[i];
		quux = sb ? (zb ?
			  (~(IntType)CBIGNUM(b)[i])
			  : (-(IntType)CBIGNUM(b)[i]))
			: (IntType)CBIGNUM(b)[i];

		if( !za && CBIGNUM(a)[i]) za = 1;
		if( !zb && CBIGNUM(b)[i]) zb = 1;

		switch ( op ){
		  case '&':
			CBIGNUM(c)[i] = qux & quux;
			break;
		  case '|':
			CBIGNUM(c)[i] = qux | quux;
			break;
		  case '^':
			CBIGNUM(c)[i] = qux ^ quux;
			break;
		}
	}
	for( ; i<lmax; i++){
		qux = sa ? (za ?
			(~(IntType)CBIGNUM(a)[i])
			: (-(IntType)CBIGNUM(a)[i]))
			: (IntType)CBIGNUM(a)[i];
		
		if( !za && CBIGNUM(a)[i]) za = 1;
		switch ( op ){
		  case '&':
			CBIGNUM(c)[i] = 0;
			break;
		  case '|':
		  case '^':
			CBIGNUM(c)[i] = qux;
			break;
		}
	}

	c = big_normal( c);
	return c;
}

Obj big_shr(Obj a, Obj b, char *name){
	int shift = big_to_cint(b);	/* shifts larger than this are ridiculous */
	int i, ii;
	int la = CLENGTH(a);
	int lc = (la * BITS_DIGIT - shift + BITS_DIGIT - 1) / BITS_DIGIT;
	Obj c = makebig(lc);
	int shift_w;
	IntType carry=0;
	IntType foo;
	
	/* shift words and bits */
	shift_w = shift / BITS_DIGIT;
	shift %= BITS_DIGIT;

	for(i=lc-1; i>=0; i--){
		ii = i + shift_w;
		if( ii >= 0 && ii < la)
			foo = carry | (IntType)CBIGNUM(a)[ii];
		else
			foo = carry;
		CBIGNUM(c)[i] = (foo >> shift) & MAXDIGIT;
		carry = (foo << BITS_DIGIT) & (MAXDIGIT<<BITS_DIGIT);  /*0xFFFF0000U*/
	}
	
	big_set_sign(c, BIG_NEGP(a));
	c = big_normal( c);
	return c;
}

Obj big_shl(Obj a, Obj b, char *name){
	int shift = big_to_cint(b);	/* shifts larger than this are ridiculous */
	int i, ii;
	int la = CLENGTH(a);
	int lc = (la * BITS_DIGIT + shift + BITS_DIGIT - 1) / BITS_DIGIT;
	Obj c = makebig(lc);
	int shift_w;
	IntType carry=0;
	IntType foo;

	/* shift words and bits */
	shift_w = shift / BITS_DIGIT;
	shift %= BITS_DIGIT;

	for(i=0; i<lc; i++){
		ii = i - shift_w;
		if( ii >= 0 && ii<la)
			foo = CBIGNUM(a)[ii];
		else
			foo = 0;
		foo <<= shift;
		CBIGNUM(c)[i] = foo & MAXDIGIT | carry;
		carry = (foo & (MAXDIGIT<<BITS_DIGIT) /*0xFFFF0000U*/ ) >> BITS_DIGIT;
	}
	
	big_set_sign(c, BIG_NEGP(a));
	c = big_normal( c);
	return c;
}

Obj big_divide(Obj a, Obj b, int mode){
	/* adapted from Knuth AoCPv2 p. 257

	    How come his psuedo-asm code looks
	    so much simpler...?
	*/
	
	int la = CLENGTH(a),
		lb = CLENGTH(b);
	int d, carry, borrow;
	IntType foo, qhat;
	int i, j;
	Obj q, u, v, vv;
	int m;

	m = la - lb;

	if( m < 0 ){
		switch( mode ){
		  case '/':
			return makebig(0);
		  case '%':
			return a;
		}
	}
	
	v  = makebig( lb );
	u  = makebig( la + 1 );
	vv = makebig( lb + 1 );
	q  = makebig( m + 1 );
	
	/* D1 -- normalize */
	foo = (UIntType)CBIGNUM(b)[lb-1];
	d = BIGRADIX / (foo + 1);

	/* v = d * b */
	for(i=carry=0; i< lb; i++){
		foo = (UIntType)CBIGNUM(b)[i] * d + carry;
		CBIGNUM(v)[i] = foo & MAXDIGIT;
		carry = foo >> BITS_DIGIT;
	}
	/* won't be a carry here */

	/* u = d * a */
	for(i=carry=0; i< la; i++){
		foo = (UIntType)CBIGNUM(a)[i] * d + carry;
		CBIGNUM(u)[i] = foo & MAXDIGIT;
		carry = foo >> BITS_DIGIT;
	}
	CBIGNUM(u)[i] = carry;

	/* D2 */
	for(j=0; j<=m; j++){

		/* D3 -- find q^ */
		if( CBIGNUM(u)[la -j] == CBIGNUM(v)[lb-1] ){
			qhat = BIGRADIX - 1;
		}else{
			qhat =  (   ((UIntType)CBIGNUM(u)[la - j]<<BITS_DIGIT)	/* u[j] * b */
				   + ( la-j-1 >= 0 ?	 
				       (UIntType)CBIGNUM(u)[la - j-1]	/* u[j+1] */
				     : 0)
				) / (UIntType)CBIGNUM(v)[lb-1];		/* v[1] */
		}
		for(i=0;i<2;i++){
			foo = ((   ((UIntType)CBIGNUM(u)[la - j]<<BITS_DIGIT)	/* u[j] * b */
			          + ( la-j-1 >= 0 ?
				         (UIntType)CBIGNUM(u)[la -  j-1]	/* u[j+1] */
				     : 0)
			          - qhat * (UIntType)CBIGNUM(v)[lb-1]	/* v[1] */
		       	       )<<BITS_DIGIT
			      ) + ( la-j-2 >= 0 ?
				    (UIntType)CBIGNUM(u)[la - j-2]		/* u[j+2] */
				   : 0);

			if( lb>1 && ((UIntType)CBIGNUM(v)[lb-2] * qhat > foo) )	/* q^ * v[2] */
				qhat --;
			else
				break;
		}

		/* D4 -- multiply and subtract */

		/* vv = v * qhat */
		for(i=carry=0; i<lb; i++){
			foo = (UIntType)CBIGNUM(v)[i] * qhat + carry;
			CBIGNUM(vv)[i] = foo & MAXDIGIT;
			carry = foo >> BITS_DIGIT;
		}
		CBIGNUM(vv)[i] = carry;

		/* uj.. = uj.. - vv  (vv is lb+1) */
		for(i=borrow=0; i<lb+1; i++){
			foo = (UIntType)CBIGNUM(u)[m - j + i] - (UIntType)CBIGNUM(vv)[i] - borrow;
			CBIGNUM(u)[m - j + i] = foo & MAXDIGIT;
			borrow = foo > BIGRADIX;
		}
		/* keep borrow for below */
		
		/* D5 -- test remainder */
		CBIGNUM(q)[m - j] = qhat;

		if( borrow){
			/* D6 -- add back */
			/* the probobility of this step being taken is ~ 2/BIGRADIX */
			CBIGNUM(q)[j] --;
			for(i=carry=0; i<lb; i++){
				foo = (UIntType)CBIGNUM(u)[m - j + i] + (UIntType)CBIGNUM(v)[i] + carry;
				CBIGNUM(u)[m - j + i] = foo & MAXDIGIT;
				carry = foo > BIGRADIX;
			}
			CBIGNUM(u)[m - j + i] = (IntType)CBIGNUM(u)[m - j + i] + carry;
			/* ignore carry */
		}
	} /* back to D3 */

	free_big( vv );
	free_big( v  );
	switch( mode ){

	  case '/':
		free_big( u );
		big_set_sign(q, BIG_NEGP(a) ^ BIG_NEGP(b));
		q =  big_normal(q);
		return q;

	  case '%':
		free_big( q );
		q = big_short_div(u, d, '/');
		free_big( u );
		return q;
	}
}

Obj big_log(Obj a, Obj b){
	/* approx bignum log */
	int len = CLENGTH(a), adj = 1;
	unsigned long mant;
	double foo, bar;
	
	/* grab 2 msd */
	if( len) mant = CBIGNUM(a)[len-1];
	if( len > 1){
		mant <<= BITS_DIGIT;
		mant |=  CBIGNUM(a)[len-2];
		adj = 2;
	}

	foo = log2( (double)mant) / BITS_DIGIT + len - adj;
	
	/* fprintf( stderr, "mant=%d len=%d l2m=%lf res=%lf\n", mant, len, log2( (double)mant), foo ); */
	
	if( BOUNDP(b))
		bar = log2( dblof(b));
	else
		bar = M_LOG2E;
	foo /= bar;
	foo *= 16.0;
	
	return makdbl( foo );
}	
	
int prnbign(Obj a, Obj stream, int how){
	/* this is inefficent ... */
	int c, base, i, len, zc=0;
	Obj radix, vv=cint_to_big(1), val=copy_big(a);
	Obj baz;
	
	extern Obj sym_oradix;
	
	radix = getvalue( sym_oradix);
	if(DEFINEDP(radix)&& INUMP(radix))
		base =CINT(radix);
	else
		base = 10;

	if( BIG_NEGP(val))
		writechar(stream, '-');
	big_set_sign(val, 0);

	len = CLENGTH(val) * BITS_DIGIT / log2( (double) base );	/* slight over-estimate */
	for(i=0; i<len; i++){
		vv  = big_short_multiply(baz=vv, base);
		if( baz!=vv && BIGNUMP(baz)) free_big( baz );
	}

	while( Fgreater(vv, MAKINT(0))==IC_TRUE){
		c = intof( Fdivide(val, vv));

		if( !c && !zc){
		}else{
			
			if(c>=0 && c<=9) c+= '0';
			else c += 'A' - 0xA;			/* we are assuming ascii... */
			writechar(stream, c);
			zc = 1;
		}
		val = Fmod(baz=val, vv);
		if( baz!=val && BIGNUMP(baz)) free_big( baz );
		if( BIGNUMP(vv)){
			/* Fmod may have returned an int (handled below)
			   otherwise a bignum, for which this is quicker
			   than a full Fdivide... */
			vv  = big_short_div(baz=vv, base, '/');
			if( baz!=vv && BIGNUMP(baz)) free_big( baz );
		}else{
			vv = Fdivide(vv, base);
		}
	}
	if(! zc) writechar(stream, '0');
	
	return 1;
}

void big_dbg_prn(Obj a){
	int l = CLENGTH(a);
	int i;
	extern Obj stderr_port;
	
	Fdisplay(a, stderr_port);
	fprintf(stderr, "\n");

	for(i=l-1; i>=0; i--){
		fprintf(stderr, "%.4x ", (UIntType)CBIGNUM(a)[i]);
	}
	fprintf(stderr, "\n");
}

#endif /* NO_BIGNUMS */


