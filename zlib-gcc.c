/*
 * Tue Jul 11 17:39:41 CDT 1995     Andy Fingerhut (jaf@arl.wustl.edu)
 *
 * This is intended to be the feeble beginning of a version of zmul,
 * zdiv, and zsqrt that can run on Linux with GCC.
 *
 * SCCS ID: @(#)zlib-gcc.c 1.1 96/02/29
 */

#include "nautilus.h"

/*
 * Define FIXED_POINT_ARITHMETIC and include fixfloat.h, which
 * contains the definition for FRACTIONAL_BITS.
 */
#define FIXED_POINT_ARITHMETIC
#include "fixfloat.h"

/*
 * zmul, zdiv, zsqrt  --  fix point mult, div, sqrt for NAUTILUS
 *
 * This file is a component of the Nautilus digital voice application.
 *
 * Author: Ray Berry, Bellevue WA
 *
 * fixed point math routines - require 386 cpu.  Assume
 * FRACTIONAL_BITS fractional bits- set below at compile
 * time. FRACTIONAL_BITS is assumed to be an EVEN #.
 *
 * don't use for zsqrt for general purpose computing - it is deficient
 * in a manner benign to NAUTILUS.
 * overflow behavior:
 *	zmul:  result is clamped to maximum magnitude representable
 *	zdiv:  div by zero silently returns max legal magnitude
 *	       overflow silently returns max legal magnitude
 *
 * v1.0 12/17/93  rjb	initial version
 * v1.1 01/08/93  rjb	extended zsqrt to return 32 bit result
 *      07/09/95  jaf   Started to convert to GCC inline assembler for Linux
 */

typedef long long int INT64;



INT32
zmul(INT32 a, INT32 b)
{
    INT64 intermediate_result;
    INT32 sign_bits, result;

    /* Multiply a and b */
    intermediate_result = (INT64) a_extended * (INT64) b_extended;

    /*
     * Shift the part of the result that we are interested in
     * back into the least significant 32 bits.
     */
    intermediate_result >>= FRACTIONAL_BITS;
    
    /*
     * If there was no overflow, then the upper 32 bits of
     * intermediate_result are all sign bits, and all of those bits
     * should be identical to the sign bit of the least significant 32
     * bits.
     *
     * If there was overflow, then force the result to the smallest
     * or largest value, as appropriate.
     */
    sign_bits = (INT32) (intermediate_result >> 32);
    result = (INT32) intermediate_result;

    if (intermediate_result < 0) {
      if (sign_bits != (INT32) 0xFFFFFFFFL) {
	result = (INT32) 0x80000000L;
      }
    } else {
      if (sign_bits != (INT32) 0L) {
	result = (INT32) 0x7FFFFFFFL;
      }
    }
    return result;
}



INT32
zdiv(INT32 a, INT32 b)
{
    if (b == 0) {
      return (INT32) 0x???L;
    }

    intermediate_result = (INT64) a / (INT64) b;

    register INT32 __res;
__asm__(
	/* return = a/b (signed)  USES: eax, ebx, ecx, edx */

	/* convert dividend and divisor to absolute values */
"	xor	%%eax, %%eax\n"	/* form abs(divisor) */
"	add	%%eax, %2\n"	/* %2 is b.  swallow div-by-0 exception */
"	jz	oflo\n"
"	cdq\n"
"	xor	%%eax, %%edx\n"
"	sub	%%eax, %%edx\n"
"	mov	%%ebx, %%eax\n"	/* ebx = abs(divisor) */
				/* the TMS9900 had an ABS instruction */
				/* (& look what happened to IT :) */
"	mov	%%eax, %1\n"	/* %1 is a */
"	cdq\n"	
"	xor	%%eax, %%edx\n"
"	sub	%%eax, %%edx\n"
"	xor	%%edx, %%edx\n"	/* abs(dividend) = edx:eax */

	/* do 64/32 >> 64  divide */
"	shld	%%edx, %%eax, FRACTIONAL_BITS+1\n"
"	shl	%%eax, FRACTIONAL_BITS+1\n"
"	xor	%%ecx, %%ecx\n"
"	xchg	%%eax, %%ecx\n"	/* cx = bot, ax = 0, dx=top */
"	xchg	%%eax, %%edx\n"	/* cx = bot, ax =top, dx =0 */
"	div	%%ebx\n"
"	xchg	%%eax, %%ecx\n"	/* dx = top(rem), ax=bot */
"	div	%%ebx\n"	/* quotient now = ecx:eax */

	/* check overflow */
"	jcxz	noflo\n"	/* things are arranged so that */
"oflo:	xor	%%eax, %%eax\n"	/* bit 31 of eax is largest legal bit */
"	not	%%eax\n"	/* 0xffffffff = max mag */

	/* restore sign */
"noflo:	inc	%%eax\n"	/* round result */
"	shr	%%eax, 1\n"
"	mov	dl, byte ptr a+3\n"  /* How should "byte ptr" be converted? */
"	xor	dl, byte ptr b+3\n"	/* quotient neg? */
"	jns	p3\n"
"	neg	%%eax\n"

"p3: 	shld	%%edx, %%eax, $16\n"	/* compiler expects result in DX:AX */

	: /* outputs */
	  /* parameter 0 */ "?" (__res)	/* What should constraint string be? */
	: /* inputs */
	  /* parameter 1 */ "=?" (a),   /* What should constraint string be? */
	  /* parameter 2 */ "=?" (b)	/* What should constraint string be? */
	: /* registers that are clobbered by instructions above */
	  /* Which registers should be listed here? */
	);
    return __res;
}



UINT32
zsqrt(UINT32 arg1)
{
    register INT32 __res;
__asm__(

/*
 * integer square root of 32 bit FixP # by successive approximation
 * warnings:
 * 	1. assumes unsigned input. msb is NOT treated as sign bit
 *	2. FRACTIONAL_BITS must be EVEN!
 *	3. code will FAIL by overflow exception for small negative inputs.
 *	4. resultant root, squared, may exceed input argument.
 * register usage:
 *	ebx:ecx	... stores copy of 64b extended arg throughout ...
 *	edi	... "last" estimate of isqrt(source)...
 *	esi	... current estimate of isqrt(source)...
 *	edx:eax	... used to divide source by edi
 */

	/*
	 * Does this USES line need to have some corresponding
	 * statement in GCC?  Perhaps these registers should go in the
	 * list of clobbered registers below?
	 */
"	USES	esi, edi\n"

"	xor	%%edi, %%edi\n"
"	mov	%%ebx, %1\n"	/* %1 is arg1 */
"	bsr	%%ecx, %%ebx\n"	/* get index of ms bit in arg */
"	jz	done\n"		/* return 0 */
"	inc	%%di\n"		/* build crude 2-bit approx of root */
"	shr	%%cl, $1\n"
"	rcl	%%di, $1\n"
"	add	%%cl, $15\n"
"	rcl	%%edi, %%cl\n"

"refine: mov	%%esi, %%edi\n"

"	mov	%%edx, %%ebx\n"	/* prep for division by estimate */
"	xor	%%eax, %%eax\n"
"	div	%%edi\n"

"	add	%%edi, %%eax\n"	/* average quotient into last estimate */
"	rcr	%%edi, $1\n"

"	sub	%%esi, %%edi\n"	/* diff between old(si) & new estimate */
"	inc	%%esi\n"	/* change (-1 to 1) to (0 to 2) */
"	js	refine\n"	/* if negative, keep iterating */
"	cmp	%%esi, $2\n"	/* almost done if 0 to 2 */
"	ja	refine\n"	/* if > 2, keep iterating */

"done:	mov	%%eax, %%edi\n"	/* results returned in dx:ax */
"	shr	%%eax, $16 - (FRACTIONAL_BITS/2)\n"
"	shld	%%edx, %%eax, $16\n"

	: /* outputs */
	  /* parameter 0 */ "?" (__res)	/* What should constraint string be? */
	: /* inputs */
	  /* parameter 1 */ "=?" (arg1) /* What should constraint string be? */
	: /* registers that are clobbered by instructions above */
	  /* Which registers should be listed here? */
	);
    return __res;
}
