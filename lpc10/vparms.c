/*
 * vparms.c
 *
 * SCCS ID:  @(#)vparms.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int vparms_(integer *vwin, real *inbuf, real *lpbuf, integer *buflim, integer *half, real *dither, integer *mintau, integer *zc, integer *lbe, integer *fbe, real *qs, real *rc1, real *ar_b__, real *ar_f__);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b2 = 1.f;

/* ********************************************************************* */

/* 	VPARMS Version 50 */

/* $Log: vparms.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.7  1996/07/13  01:21:16  dm */
/* Changed dabs() to abs(), which is defined as a macro in f2c.h.  The */
/* macro returns the same type as goes in (as opposed to the function abs() */
/* which the macro overrides.  Because this is done by the pre-processor, */
/* the compiler could not optimize this unneccessary typecasting and would
/* be forced to work with doublereals, instead of reals. */

/* Revision 1.7  1996/07/13  01:21:16  dm */
/* Adjusted indexes for buflim and vwin leftover from FORTRAN translation */

/* Revision 1.6  1996/03/29  18:01:16  jaf */
/* Added some more comments about the range of INBUF and LPBUF that can */
/* be read.  Note that it is possible for index VWIN(2)+1 to be read from */
/* INBUF, which might be outside of its defined range, although that will */
/* require more careful checking. */

/* Revision 1.5  1996/03/19  00:02:02  jaf */
/* I just noticed that the argument DITHER is modified inside of this */
/* subroutine.  Comments were added explaining the possible final values. */

/* Revision 1.4  1996/03/18  22:22:59  jaf */
/* Finishing the job I said I did with the last check-in comments. */

/* Revision 1.3  1996/03/18  22:22:17  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  15:02:58  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:50:42  jaf */
/* Initial revision */


/* ********************************************************************* */

/*  Calculate voicing parameters: */

/* Input: */
/*  VWIN   - Voicing window limits */
/*           Indices 1 through 2 read. */
/*  INBUF  - Input speech buffer */
/*           Indices START-1 through STOP read, */
/*          where START and STOP are defined in the code (only written once).
*/
/*           Note that STOP can be as large as VWIN(2)+1 ! */
/*  LPBUF  - Low pass filtered speech */
/*           Indices START-MINTAU through STOP+MINTAU read, */
/*          where START and STOP are defined in the code (only written once).
*/
/*  BUFLIM - Array bounds for INBUF and LPBUF */
/*           Indices 1 through 4 read. */
/*  HALF   - Half frame (1 or 2) */
/*  MINTAU - Lag corresponding to minimum AMDF value (pitch estimate) */
/* Input/Output: */
/*  DITHER - Zero crossing threshold */
/*           The resulting value might be the negation of the input */
/*           value.  It might always be the same as the input value, */
/*           if the DO loop below always executes an even number of times. */
/* Output: (all of them are written on every call) */
/*  ZC     - Zero crossing rate */
/*  LBE    - Low band energy (sum of magnitudes - SM) */
/*  FBE    - Full band energy (SM) */
/*  QS     - Ratio of 6 dB/oct preemphasized energy to full band energy */
/*  RC1    - First reflection coefficient */
/*  AR_B   - Product of the causal forward and reverse pitch */
/*           prediction gains */
/*  AR_F   - Product of the noncausal forward and reverse pitch */
/*           prediction gains */
/* Internal: */
/*  OLDSGN - Previous sign of dithered signal */
/*  VLEN   - Length of voicing window */
/*  START  - Lower address of current half of voicing window */
/*  STOP   - Upper address of current half of voicing window */
/*  E_0    - Energy of LPF speech (sum of squares - SS) */
/*  E_B    - Energy of LPF speech backward one pitch period (SS) */
/*  E_F    - Energy of LPF speech forward one pitch period (SS) */
/*  R_B    - Autocovariance of LPF speech backward one pitch period */
/*  R_F    - Autocovariance of LPF speech forward one pitch period */
/*  LP_RMS - Energy of LPF speech (sum of magnitudes - SM) */
/*  AP_RMS - Energy of all-pass speech (SM) */
/*  E_PRE  - Energy of 6dB preemphasized speech (SM) */
/*  E0AP   - Energy of all-pass speech (SS) */

/* This subroutine has no local state. */

/*< 	S >*/
/* Subroutine */ int vparms_(integer *vwin, real *inbuf, real *lpbuf, integer 
	*buflim, integer *half, real *dither, integer *mintau, integer *zc, 
	integer *lbe, integer *fbe, real *qs, real *rc1, real *ar_b__, real *
	ar_f__)
{
    /* System generated locals */
    integer inbuf_offset, lpbuf_offset, i__1;
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);
    integer i_nint(real *);

    /* Local variables */
    integer vlen, stop, i__;
    real e_pre__;
    integer start;
    real ap_rms__, e_0__, oldsgn, lp_rms__, e_b__, e_f__, r_b__, r_f__, e0ap;

/*       Arguments */
/*< 	INTEGER VWIN(2), BUFLIM(4) >*/
/*< 	REAL INBUF(BUFLIM(1):BUFLIM(2)) >*/
/*< 	REAL LPBUF(BUFLIM(3):BUFLIM(4)) >*/
/*< 	INTEGER HALF >*/
/*< 	REAL DITHER >*/
/*< 	INTEGER MINTAU, ZC, LBE, FBE >*/
/*< 	REAL QS, RC1, AR_B, AR_F >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, VLEN, START, STOP >*/
/*< 	REAL OLDSGN, E_0, E_B, R_B, LP_RMS, AP_RMS, E_PRE, E0AP >*/
/*< 	REAL E_F, R_F >*/
/*   Calculate zero crossings (ZC) and several energy and correlation */
/*   measures on low band and full band speech.  Each measure is taken */
/*   over either the first or the second half of the voicing window, */
/*   depending on the variable HALF. */
/*< 	LP_RMS = 0. >*/
    lpbuf_offset = buflim[2];
    lpbuf -= lpbuf_offset;
    inbuf_offset = buflim[0];
    inbuf -= inbuf_offset;

    /* Function Body */
    lp_rms__ = 0.f;
/*< 	AP_RMS = 0. >*/
    ap_rms__ = 0.f;
/*< 	E_PRE = 0. >*/
    e_pre__ = 0.f;
/*< 	E0AP = 0. >*/
    e0ap = 0.f;
/*< 	RC1 = 0. >*/
    *rc1 = 0.f;
/*< 	E_0 = 0. >*/
    e_0__ = 0.f;
/*< 	E_B = 0. >*/
    e_b__ = 0.f;
/*< 	E_F = 0. >*/
    e_f__ = 0.f;
/*< 	R_F = 0. >*/
    r_f__ = 0.f;
/*< 	R_B = 0. >*/
    r_b__ = 0.f;
/*< 	ZC = 0 >*/
    *zc = 0;
/*< 	VLEN = VWIN(2) - VWIN(1) + 1 >*/
    vlen = vwin[1] - vwin[0] + 1;
/*< 	START = VWIN(1) + (HALF-1)*VLEN/2 + 1 >*/
    start = vwin[0] + (*half - 1) * vlen / 2 + 1;
/*< 	STOP = START + VLEN/2 - 1 >*/
    stop = start + vlen / 2 - 1;

/* I'll use the symbol HVL in the table below to represent the value */
/* VLEN/2.  Note that if VLEN is odd, then HVL should be rounded down, */
/* i.e., HVL = (VLEN-1)/2. */

/* HALF  START          STOP */

/* 1     VWIN(1)+1      VWIN(1)+HVL */
/* 2     VWIN(1)+HVL+1  VWIN(1)+2*HVL */

/* Note that if VLEN is even and HALF is 2, then STOP will be */
/* VWIN(1)+VLEN = VWIN(2)+1.  That could be bad, if that index of INBUF */
/* is undefined. */

/*< 	OLDSGN = SIGN( 1., INBUF(START-1)-DITHER ) >*/
    r__1 = inbuf[start - 1] - *dither;
    oldsgn = (real) r_sign(&c_b2, &r__1);
/*< 	DO I = START, STOP >*/
    for (i__ = start; i__ <= stop; ++i__) {
/*< 	   LP_RMS = LP_RMS + ABS(LPBUF(I)) >*/
	lp_rms__ += abs(lpbuf[i__]);
/*< 	   AP_RMS = AP_RMS + ABS(INBUF(I)) >*/
	ap_rms__ += abs(inbuf[i__]);
/*< 	   E_PRE = E_PRE + ABS(INBUF(I)-INBUF(I-1)) >*/
	e_pre__ += abs(inbuf[i__] - inbuf[i__-1]);
/*< 	   E0AP = E0AP + INBUF(I)**2 >*/
/* Computing 2nd power */
	e0ap += inbuf[i__] * inbuf[i__];
/*< 	   RC1 = RC1 + INBUF(I)*INBUF(I-1) >*/
	*rc1 += inbuf[i__] * inbuf[i__-1];
/*< 	   E_0 = E_0 + LPBUF(I)**2 >*/
/* Computing 2nd power */
	e_0__ += lpbuf[i__] * lpbuf[i__];
/*< 	   E_B = E_B + LPBUF(I-MINTAU)**2 >*/
/* Computing 2nd power */
	r__1 = lpbuf[i__ - *mintau];
	e_b__ += r__1 * r__1;
/*< 	   E_F = E_F + LPBUF(I+MINTAU)**2 >*/
/* Computing 2nd power */
	r__1 = lpbuf[i__ + *mintau];
	e_f__ += r__1 * r__1;
/*< 	   R_F = R_F + LPBUF(I)*LPBUF(I+MINTAU) >*/
	r_f__ += lpbuf[i__] * lpbuf[i__ + *mintau];
/*< 	   R_B = R_B + LPBUF(I)*LPBUF(I-MINTAU) >*/
	r_b__ += lpbuf[i__] * lpbuf[i__ - *mintau];
/*< 	   IF( SIGN(1.,INBUF(I)+DITHER) .NE. OLDSGN ) THEN >*/
	r__1 = inbuf[i__] + *dither;
	if (r_sign(&c_b2, &r__1) != oldsgn) {
/*< 	      ZC = ZC + 1 >*/
	    ++(*zc);
/*< 	      OLDSGN = -OLDSGN >*/
	    oldsgn = -oldsgn;
/*< 	   END IF >*/
	}
/*< 	   DITHER = -DITHER >*/
	*dither = -(*dither);
/*< 	END DO >*/
    }
/*   Normalized short-term autocovariance coefficient at unit sample delay
 */
/*< 	RC1 = RC1 / MAX(E0AP,1.) >*/
    *rc1 /= (real) dmax(e0ap,1.f);
/*  Ratio of the energy of the first difference signal (6 dB/oct preemphas
is)*/
/*   to the energy of the full band signal */
/*< 	QS = E_PRE / MAX(2.*AP_RMS,1.) >*/
/* Computing MAX */
    r__1 = ap_rms__ * 2.f;
    *qs = (real) (e_pre__ / dmax(r__1,1.f));
/*   aR_b is the product of the forward and reverse prediction gains, */
/*   looking backward in time (the causal case). */
/*< 	AR_B = (R_B / MAX(E_B,1.)) * (R_B / MAX(E_0,1.)) >*/
    *ar_b__ = (real) (r_b__ / dmax(e_b__,1.f) * (r_b__ / dmax(e_0__,1.f)));
/*  aR_f is the same as aR_b, but looking forward in time (non causal case
).*/
/*< 	AR_F = (R_F / MAX(E_F,1.)) * (R_F / MAX(E_0,1.)) >*/
    *ar_f__ = (real) (r_f__ / dmax(e_f__,1.f) * (r_f__ / dmax(e_0__,1.f)));
/*   Normalize ZC, LBE, and FBE to old fixed window length of 180. */
/*   (The fraction 90/VLEN has a range of .58 to 1) */
/*< 	ZC =       NINT( ZC*2     * (90./VLEN) ) >*/
    r__2 = (real) (*zc << 1);
    r__1 = r__2 * (90.f / vlen);
    *zc = i_nint(&r__1);
/*< 	LBE = MIN( NINT( LP_RMS/4 * (90./VLEN) ), 32767 ) >*/
/* Computing MIN */
    r__1 = lp_rms__ / 4 * (90.f / vlen);
    i__1 = i_nint(&r__1);
    *lbe = min(i__1,32767);
/*< 	FBE = MIN( NINT( AP_RMS/4 * (90./VLEN) ), 32767 ) >*/
/* Computing MIN */
    r__1 = ap_rms__ / 4 * (90.f / vlen);
    i__1 = i_nint(&r__1);
    *fbe = min(i__1,32767);
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* vparms_ */

