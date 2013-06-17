/*
 * lpc10.c
 *
 * SCCS ID:  @(#)lpc10.c 1.1 96/03/31
 */

/*
 * The author of this software is J. Andrew Fingerhut.
 * Copyright (c) 1996 by J. Andrew Fingerhut.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 *  lpc10.c -- coder based on linear prediction coefficients
 *             that codes 8000 samples/sec speech in 2400 bits/sec.
 */

/*
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 96/03/26  J. A. Fingerhut    Wrote original version
 */

#include <stdio.h>

#include "machine.h"

/*
 * Include fixfloat.h just for the audio sample conversion macros
 * audio_s2f and audio_f2s.
 */
#undef FIXED_POINT_ARITHMETIC
#include "fixfloat.h"



/* Prototype for the C routines that were converted from Fortran. */

/*int lpcenc_(real *speech, integer *bits);*/
/*int lpcdec_(integer *bits, real *speech);*/
/*int lpcini_(void);*/

/* Note that this file depends on the facts that a Fortran real is a
 * float (defined in f2c.h), and a value_t is a float when
 * FIXED_POINT_ARITHMETIC is undefined (defined in fixfloat.h). */

int lpcini_(void);
int lpcenc_(float *speech, INT32 *bits);
int lpcdec_(INT32 *bits, float *speech);

#define FRAME_SIZE 180
#define NBITS 54



void
lpc10_init(void)
{
    lpcini_();
}



void
lpc10_encode(INT16 *x, UINT8 *bits)
{
    int             i;		/* generic loop variable */
    value_t         xr[FRAME_SIZE];	/* The input samples after
					   conversion to type
					   value_t. */
    INT32           coded_frame[NBITS];	/* One bit of coded speech per
					   array entry. */
    UINT8           mask;	/* The next bit position within the
				   variable data to place the next
				   encoded bit. */
    UINT8           data;	/* The contents of the next byte to
				   place in the compressed output. */
    int             next_byte_pos;	/* The next position in the
					   output array bits to
					   fill. */

    for (i = 0; i < FRAME_SIZE; i++) {
	xr[i] = (float) audio_s2f(x[i]);
    }

    /* Call the necessary functions, which were converted from Fortran
     * with f2c, to encode the speech.
     */
    lpcenc_(xr, coded_frame);

    /* Fill in the array bits.
     * The first compressed output bit will be the most significant
     * bit of the byte, so initialize mask to 0x80.  The next byte of
     * compressed data is initially 0, and the desired bits will be
     * turned on below.
     */
    mask = 0x80;
    data = 0;
    next_byte_pos = 0;

    for (i = 0; i < NBITS; i++) {
	/* Turn on the next bit of output data, if necessary. */
	if (coded_frame[i]) {
	    data |= mask;
	}
	/*
	 * If the byte data is full, determined by mask becoming 0,
	 * then add the byte to the output array bits, and
	 * reinitialize data and mask for the next output byte.
	 * Also add the byte if (i == NBITS-1), because if NBITS
	 * is not a multiple of 8, then mask won't yet be 0.
	 */
	mask >>= 1;
	if ((mask == 0) || (i == NBITS-1)) {
	    bits[next_byte_pos] = data;
	    ++next_byte_pos;
	    data = 0;
	    mask = 0x80;
	}
    }
}



void
lpc10_decode(UINT8 * bits, INT16 * x)
{
    int             i;		/* generic loop variable */
    value_t         xr[FRAME_SIZE];	/* The decoded frame of
					   samples before conversion
					   to standard samples */
    INT32           coded_frame[NBITS];	/* One bit of coded speech per
					   array entry. */

    /* Unpack the array bits into coded_frame. */
    for (i = 0; i < NBITS; i++) {
	if (bits[i >> 3] & (0x80 >> (i & 7))) {
	    coded_frame[i] = 1;
	} else {
	    coded_frame[i] = 0;
	}
    }

    /* Call the necessary functions, which were converted from Fortran
     * with f2c, to decode the speech, filling in array xr.
     */
    lpcdec_(coded_frame, xr);

    /* Convert real samples to standard samples. */
    for (i = 0; i < FRAME_SIZE; i++) {
	x[i] = audio_f2s(xr[i]);
    }
}
