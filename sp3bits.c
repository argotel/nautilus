/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 *  sp3bits.c -- coder based on switched prediction that produces 3
 *               bits per audio sample
 */

/* REVISION HISTORY
 *
 * SCCS ID: @(#)sp3bits.c 1.2 96/03/31
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/11/12  B. Dorsey		Wrote original version
 * 94/11/17  B. Dorsey		Adapted for use in Nautilus
 * 94/11/30  B. Dorsey		Modified to use 6KHZ base sampling rate
 * 94/12/11  B. Dorsey		Modified to use fixed point arithmetic
 * 95/06/08  B. Dorsey		Renamed to sp124 from dmsp
 * 95/06/25  J. A. Fingerhut	Merged both fixed point arithmetic and
 * 				floating point arithmetic versions of
 * 				SP124 coders into one file.
 * 95/08/21  B. Dorsey		Created 3-bit coder from 2-bit coder.
 * 96/02/23  J. A. Fingerhut	Added code for clipping of samples.
 */

#include <stdio.h>

#include "machine.h"

/*
 * There will be several places in this file containing constructs
 * like this:
 * 
 * #ifdef FIXED_POINT_ARITHMETIC
 * 
 * ... some code for fixed point version ...
 * 
 * #else /* FIXED_POINT_ARITHMETIC * /
 * 
 * ... some code for floating point version ...
 * 
 * #endif /* FIXED_POINT_ARITHMETIC * /
 *                                  ^^^
 *                          Don't eliminate this blank.  I think you
 *                          can figure out why :-)
 * 
 * will be seen.  This same file is intended to be the source file for
 * both the fixed point version of the SP64 and SP85 speech coders,
 * and the floating point version.  This symbol is also used in the
 * header file fixfloat.h.
 * 
 * The preprocessor symbol AUDIO_DEVICE_8KHZ_ONLY is also used to
 * conditionally compile code, in this case for upsampling and
 * downsampling the signals.  This symbol should be defined when
 * compiling on Sun workstations whose audio devices can only sample
 * and play sound at 8000 samples/sec.
 */


#include "fixfloat.h"


/* Operating Parameters */

/* The number of voice samples to be encoded together. */
#define FRAME_SIZE	120		/* 20 ms */
/* Precomputed value of sqrt(FRAME_SIZE-1). */
#define FSROOT		FLOAT2VALUE(10.9087)

#if defined(AUDIO_DEVICE_8KHZ_ONLY)
/*
 * The number of samples at 8000 samples/sec required to have the same
 * duration as FRAME_SIZE samples at 6000 samples/sec.  This is the
 * same as (FRAME_SIZE*8000/6000).
 */

#define IBLKSIZE	(FRAME_SIZE*4/3)
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */


/*
 * YPSIZE is the number of bits used to encode the step size.
 *
 * YPMIN and YPMAX are minimum and maximum step sizes to use in the
 * encoding.
 *
 * SLOPE_MULT is for ???
 */

#define YPSIZE          6
#define	YPMIN		FLOAT2VALUE(0.001)
#define YPMAX		FLOAT2VALUE(0.250)


/*
 * START_VALUE is the finest quantization we can make (as a fraction
 * of step_size).  D0 is set to be the starting value.  Successive
 * quantization values are set in relation to START_VALUE according
 * to some mathematical relation.  Currently, the following relation
 * is used:
 *
 *     D[0] = START_VALUE
 *     D[x] = (x+1)^2 * D[x-1]
 */

#define START_VALUE	0.130
#define	D0		FLOAT2VALUE(START_VALUE)
#define D1		FLOAT2VALUE(4.0*START_VALUE)
#define D2		FLOAT2VALUE(9.0*START_VALUE)
#define D3		FLOAT2VALUE(16.0*START_VALUE)
#define SPLIT1		((D0+D1)/2)
#define SPLIT2		((D1+D2)/2)
#define SPLIT3		((D2+D3)/2)


/*
 * While compressing a frame, y[0] and y[1] contain the previous two
 * values that were estimated by the linear prediction filter (y[0] is
 * the most recent, y[1] is the one before that).  These values are
 * initialized in sp184_init(), and changed in the sp184_encode() and
 * sp184_decode() functions.  When sending or receiving multiple
 * frames in sequence, their values are maintained from one frame to
 * the next.  This could be a problem if a packet is lost during
 * transmission, because the receiver's state could become out of sync
 * with the sender's state.  Would it be a good idea to reinitialize
 * them at the beginning of each frame in both sp184_encode() and
 * sp184_decode() for this reason?
 *
 * ypdelta is a value computed once in sp184_init().  It is the
 * difference between each consecutive pair of quantized values of the
 * step_size parameter.
 */

static struct coder_state_t {
    value_t           y[2];
    value_t           ypdelta;
} handle;


/*
 * These values are filter coefficients used to predict a "good guess"
 * value for the next sample based on the value of the previous two
 * samples.  There are 4 sets of filter coefficients, one set for each
 * of the 4 states of the coder.  These values can be found on p. 301
 * of [Jayant and Noll, 1984], where they cite [Evci, Xydeas, and
 * Steele, 1981] as the source.
 */

static struct ifilter_t {
    value_t           h1;
    value_t           h2;
} filters[] = {	/* filter coefficients */
    { FLOAT2VALUE( 1.53), FLOAT2VALUE(-0.72) },
    { FLOAT2VALUE( 0.95), FLOAT2VALUE(-0.34) },
    { FLOAT2VALUE( 0.48), FLOAT2VALUE(-0.21) },
    { FLOAT2VALUE(-0.63), FLOAT2VALUE(-0.36) }
};


void PutBits(int bits, int nbits, UINT8 *buf);
int GetBits(int nbits, UINT8 *buf);

#if defined(AUDIO_DEVICE_8KHZ_ONLY)
void downsample(float *x, int nx, float *y);
void upsample(float *x, int nx, float *y);
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */



void
sp184_init(void)
{
    /* initializations */
    handle.y[0] = FLOAT2VALUE(0.0);
    handle.y[1] = FLOAT2VALUE(0.0);

    /*
     * The step size transmitted over the modem is represented in
     * YPSIZE bits, and it represents a value in the range [YPMIN,
     * YPMAX].  A bit pattern of all 0's is used to represent YPMIN,
     * and all 1's represents YPMAX.  The intermediate values
     * represent evenly spaced values in the interval [YPMIN, YPMAX].
     * ypdelta is the difference between two consecutive such values.
     */
    handle.ypdelta = DIV(YPMAX - YPMIN, FLOAT2VALUE((float) ((1L << YPSIZE) - 1)) );
}



void
sp184_encode(INT16 *x, UINT8 *bits)
{
    int             i;		/* generic loop variable */
    int             state;	/* one of 4 states of the switched
				   predictor */
    int             step;	/* the quantized step size to use for
				   the differential coding */
    int             next_bits;	/* the next 2 bits to send to the
				   receiver in the differential coding */
    value_t         phi0;	/* the autocorrelation of the frame of
				   samples at lag 0 */
    value_t         phi1;	/* the autocorrelation of the frame of
				   samples at lag 1 */
    value_t         step_size;	/* First used as the un-quantized step
				   size, and then later the quantized
				   step size represented in type
				   value_t. */
    value_t         y;		/* First used as a generic computation
				   variable, then later as the sample
				   value predicted by the 2nd order
				   linear predictor. */
    value_t         yy;		/* Generic computation variable. */
    value_t         xr[FRAME_SIZE];	/* The input samples after
					   possible resampling, and
					   then conversion to type
					   value_t. */
#if defined(AUDIO_DEVICE_8KHZ_ONLY)
    /*
     * If downsampling is needed, define a buffer to hold the data
     * after conversion from standard form to type value_t, but before
     * downsampling.
     */
    value_t         xf[IBLKSIZE];
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */

#if defined(AUDIO_DEVICE_8KHZ_ONLY)
    /*
     * NOTE: The following code assumes that if AUDIO_DEVICE_8KHZ_ONLY
     * is defined, then we must also be using floating point
     * arithmetic, i.e., FIXED_POINT_ARITHMETIC is not defined.  This
     * is true as of version 0.9.2 of Nautilus, but might not be true
     * in the future.  At such a time, this code would need to be
     * changed.
     * 
     * On a Sun, the audio input device can only sample at 8000
     * samples/sec.  The SP124 speech coder expects speech input
     * sampled at 6000 samples/sec.  Resample the input at the
     * desired rate.
     */

    /* convert input to floating point */
    for (i = 0; i < IBLKSIZE; i++) {
	xf[i] = audio_s2f(x[i]);
    }

    /* resample input */
    downsample(xf, IBLKSIZE, xr);
#else /* defined(AUDIO_DEVICE_8KHZ_ONLY) */
    /*
     * On other platforms (e.g., MS-DOS and Intel Linux), we expect
     * the incoming speech samples to already be at (or very near)
     * 6000 samples/sec for the SP124 coder.  Only conversion from
     * standard samples to the samples needed by the coder is
     * necessary.
     */
    for (i = 0; i < FRAME_SIZE; i++) {
	xr[i] = audio_s2f(x[i]);
    }
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */

    /*
     * Compute normalized autocorrelation at lag 0 & 1.
     * Compute step size based on RMS value
     */
    yy = xr[0];					/* priming the loop */
    phi0 = MUL(yy, yy) + FLOAT2VALUE(1.0e-20);	/* +1.0e-20 to prevent divide by 0 */
    step_size = phi1 = FLOAT2VALUE(0.0);

    for (i = 1; i < FRAME_SIZE; i++) {
	y = xr[i];
	phi0 += MUL(y, y);			/* autocorr at lag 0 */
	phi1 += MUL(yy, y);			/* autocorr at lag 1 */
	step_size += MUL(y - yy, y - yy);	/* rms calc */
	yy = y;
    }

    /*
     * At this time, the following is true:
     *
     * xr[0..(FRAME_SIZE-1)] = FRAME_SIZE samples at 6000 samples/sec,
     *                         where each sample is either a fixed point
     *                         or floating point value in the range
     *                         [-1, +1].
     * phi0 = sum from i=0 to (FRAME_SIZE-1) of xr[i]^2    (plus 1.0e-20)
     * phi1 = sum from i=1 to (FRAME_SIZE-1) of (xr[i]*xr[i-1])
     * step_size = sum from i=1 to (FRAME_SIZE-1) of (xr[i]-xr[i-1])^2
     *
     * This formula for step_size is algebraically equivalent to:
     *
     * = sum from i=1 to (FRAME_SIZE-1) of (xr[i]^2 + xr[i-1]^2
     *                                         - 2 * xr[i] * xr[i-1])
     *
     * = ( sum from i=1 to (FRAME_SIZE-1) of 2 * (xr[i]^2 - xr[i] * xr[i-1]) )
     *       - ( xr[0]^2 + xr[FRAME_SIZE-1]^2 )
     *
     * = 2 * phi0 - 2 * phi1 - ( 3 * xr[0]^2 + xr[FRAME_SIZE-1]^2 )
     *
     * Thus it seems that one could remove the computation for step_size
     * from within the loop above, and simply make the following assignment
     * afterwards:
     *
     * step_size = MUL( INT2VALUE(2), phi0 - phi1 );
     *
     * (This is probably close enough to the value computed above, but
     * if not, it would be easy to add in the
     * -(3*xr[0]^2 + xr[FRAME_SIZE-1]^2) term.)
     *
     * Note that it might still be worth computing step_size as in the
     * code above, because even though it is algebraically equivalent
     * to the formula above, the way it is computed may have better
     * numerical properties (e.g., overflow, underflow, roundoff
     * error).
     */

    phi1 = DIV(phi1, phi0);			/* normalize phi1 */

    /* select predictor state */
    if (phi1 > FLOAT2VALUE(0.7)) {
	state = 0;
    } else if (phi1 > FLOAT2VALUE(0.4)) {
	state = 1;
    } else if (phi1 > FLOAT2VALUE(0.0)) {
	state = 2;
    } else {
	state = 3;
    }

    /* compute step sized based on RMS value of input */
    step_size = SQRT(step_size);
    step_size = DIV(step_size, FSROOT);

    /*
     * Quantize step_size to YPSIZE bits and store in step.  Then
     * convert the quantized value back into step_size so that the
     * coder is using the same value for compression that the decoder
     * will be using for decompression.
     */

    /* check step size for bounds */
    if (step_size < YPMIN) {
	step_size = YPMIN;
    } else if (step_size > YPMAX) {
	step_size = YPMAX;
    }
    step = VALUE2INT( DIV(step_size - YPMIN, handle.ypdelta) );
    step_size = YPMIN + MUL( INT2VALUE(step), handle.ypdelta);

    /* save predictor state and quantized step size in output */
    bits[0] = state + (step << 2);

    /* initialize PutBits() */
    PutBits(0, 0, &bits[1]);

    /* compute output bits */
    for (i = 0; i < FRAME_SIZE; i++) {

	/* apply linear predictive filter */
	y = MUL(filters[state].h1, handle.y[0]) +
	    MUL(filters[state].h2, handle.y[1]);

	handle.y[1] = handle.y[0];

	/* compute error */
	yy = DIV(xr[i] - y, step_size);

	if (yy < FLOAT2VALUE(0.0)) {
	    if (yy < -SPLIT2) {
		if (yy < -SPLIT3) {
		    y -= MUL(D3, step_size);
		    next_bits = 0;
		}
		else {
		    y -= MUL(D2, step_size);
		    next_bits = 1;
		}
	    }
	    else {
		if (yy < -SPLIT1) {
		    y -= MUL(D1, step_size);
		    next_bits = 2;
		}
		else {
		    y -= MUL(D0, step_size);
		    next_bits = 3;
		}
	    }
#ifdef CLIP
	    if (y < FLOAT2VALUE(-1.0)) {
	        y = FLOAT2VALUE(-1.0);
	    }
#endif  /* CLIP */
	} else {
	    if (yy < SPLIT2) {
		if (yy < SPLIT1) {
		    y += MUL(D0, step_size);
		    next_bits = 4;
		}
		else {
		    y += MUL(D1, step_size);
		    next_bits = 5;
		}
	    }
	    else {
		if (yy < SPLIT3) {
		    y += MUL(D2, step_size);
		    next_bits = 6;
		}
		else {
		    y += MUL(D3, step_size);
		    next_bits = 7;
		}
	    }
#ifdef CLIP
	    if (y > FLOAT2VALUE(1.0)) {
	        y = FLOAT2VALUE(1.0);
	    }
#endif  /* CLIP */
	}

	/* output bits for next delta */
	PutBits(next_bits, 3, (char *)0);

	handle.y[0] = y;
    }
}



void
sp184_decode(UINT8 *bits, INT16 *x)
{
    int             i;		/* generic loop variable */
    int             state;	/* one of 4 states of the switched
				   predictor */
    int             step;	/* the quantized step size to use for
				   the differential coding */
    int             next_bits;	/* the next bit from the sender in the
				   differential coding */
    value_t         step_size;	/* the quantized step size represented
				   in type value_t. */
    value_t         y;		/* The decoded sample value */
    value_t         xr[FRAME_SIZE];	/* The decoded frame of
					   samples before possible
					   resampling, and before
					   conversion to standard
					   samples */
#if defined(AUDIO_DEVICE_8KHZ_ONLY)
    /*
     * If upsampling is needed, define a buffer to hold the upsampled
     * data before conversion to standard samples.
     */
    value_t         xf[IBLKSIZE];
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */

    /* get predictor state and step size from input */
    state = bits[0] & 0x3;	/* least significant 2 bits */
    step = bits[0] >> 2;	/*  most significant 6 bits */

    /* decode step_size from quantized step size */
    step_size = YPMIN + MUL(INT2VALUE(step), handle.ypdelta);

    /* initialize GetBits() */
    GetBits(0, &bits[1]);

    /* compute output from input bits */
    for (i = 0; i < FRAME_SIZE; i++) {

	/* apply linear predictive filter */
	y = MUL(filters[state].h1, handle.y[0]) +
	    MUL(filters[state].h2, handle.y[1]);

	handle.y[1] = handle.y[0];

	/* get input */
	next_bits = GetBits(3, (char *) 0);

	/* update output */
	switch (next_bits) {
	case 0:
	    y -= MUL(D3, step_size);
	    break;
	case 1:
	    y -= MUL(D2, step_size);
	    break;
	case 2:
	    y -= MUL(D1, step_size);
	    break;
	case 3:
	    y -= MUL(D0, step_size);
	    break;
	case 4:
	    y += MUL(D0, step_size);
	    break;
	case 5:
	    y += MUL(D1, step_size);
	    break;
	case 6:
	    y += MUL(D2, step_size);
	    break;
	case 7:
	    y += MUL(D3, step_size);
	    break;
	}
#ifdef CLIP
	if (y < FLOAT2VALUE(-1.0)) {
	    y = FLOAT2VALUE(-1.0);
	} else if (y > FLOAT2VALUE(1.0)) {
	    y = FLOAT2VALUE(1.0);
	}
#endif  /* CLIP */

	/* save output */
	handle.y[0] = y;
	xr[i] = y;
    }

#if defined(AUDIO_DEVICE_8KHZ_ONLY)
    /*
     * NOTE: The following code assumes that if AUDIO_DEVICE_8KHZ_ONLY
     * is defined, then we must also be using floating point
     * arithmetic, i.e., FIXED_POINT_ARITHMETIC is not defined.  This
     * is true as version 0.9.2 of Nautilus, but might not be true in
     * the future.  At such a time, this code would need to be
     * changed.
     * 
     * On a Sun, the audio output device can only play sound sampled
     * at 8000 samples/sec.  The SP124 speech decoder produces speech
     * sampled at 6000 samples/sec.  Resample the output at the
     * desired rate.
     */
    upsample(xr, FRAME_SIZE, xf);

    /* convert output to 16-bit signed linear format */
    for (i = 0; i < IBLKSIZE; i++) {
	x[i] = audio_f2s(xf[i]);
    }
#else /* defined(AUDIO_DEVICE_8KHZ_ONLY) */
    /*
     * On other platforms (e.g., MS-DOS and Intel Linux), it is
     * acceptable for the resulting speech samples produced by this
     * function to be at 6000 samples/sec, because the sound card can
     * play at (or very near) this rate.  Only conversion from the
     * samples used by this coder to standard samples is necessary.
     */
    for (i = 0; i < FRAME_SIZE; i++) {
	x[i] = audio_f2s(xr[i]);
    }
#endif /* defined(AUDIO_DEVICE_8KHZ_ONLY) */
}
