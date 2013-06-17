/*
 * f2clib.c
 *
 * SCCS ID:  @(#)f2clib.c 1.2 96/05/19
 */

#include "f2c.h"

#ifdef KR_headers
integer pow_ii(ap, bp) integer *ap, *bp;
#else
integer pow_ii(integer *ap, integer *bp)
#endif
{
	integer pow, x, n;
	unsigned long u;
	
	x = *ap;
	n = *bp;
	
	// handle special cases where exponent is zero or negative
	if (n <= 0) {
		switch (x) {
		case 0:
			return x;
		case -1:
			return n&1 ? -1 : 1;
		case 1:
			return 1;
		case -2:
			switch (n) {
			case 0:
				return 1;
			case -1:
				return -1;		// round away from zero
			default:
				return 0;
			}
		case 2:
			switch (n) {
			case 0:
				return 1;
			case -1:
				return 1;
			default:
				return 0;
			}
		default:
			return n==0 ? 1 : 0;
		}
	}
	u = n;
	for(pow = 1; ; )
	{
		if(u & 01)
			pow *= x;
		if(u >>= 1)
			x *= x;
		else
			break;
	}
	return(pow);
}



#ifdef KR_headers
double r_sign(a,b) real *a, *b;
#else
double r_sign(real *a, real *b)
#endif
{
	double x;
	x = (*a >= 0 ? *a : - *a);
	return( *b >= 0 ? x : -x);
}



#ifdef KR_headers
double floor();
integer i_nint(x) real *x;
#else
#undef abs
#include "math.h"
integer i_nint(real *x)
#endif
{
	return (integer) ((*x)>=0 ? floor(*x + .5) : -floor(.5 - *x));
}
