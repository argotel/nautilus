/*
 * onset.c
 *
 * SCCS ID:  @(#)onset.c 1.2 96/05/19
 */

#include "f2c.h"

extern int onset_(real *pebuf, integer *osbuf, integer *osptr, integer *oslen, integer *sbufl, integer *sbufh, integer *lframe);
extern int initonset_(void);

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* Table of constant values */

static real c_b2 = 1.f;

/* ****************************************************************** */

/* 	ONSET Version 49 */

/* $Log: onset.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.7  1996/08/24  16:41:01  dm */
/* Changed dabs() macro reference to abs() macro.  No need to typecast to */
/* doublereal */

/* Revision 1.6  1996/07/12  16:41:01  dm */
/* Removed middle-man variable r__1, and pebuf_offset */

/* Revision 1.5  1996/03/15  16:41:01  jaf */
/* Just rearranged INITONSET assignment orders to be consistent with */
/* order of DATA statements in ONSET. */

/* Revision 1.4  1996/03/15  15:48:27  jaf */
/* Changed some comments, and only reordered the DATA statements (their */
/* meaning wasn't changed). */

/* Revision 1.3  1996/03/14  23:53:06  jaf */
/* Added an entry INITONSET that reinitializes the local state variables */
/* of subroutine ONSET. */

/* Rearranged quite a few comments, adding more explaining which */
/* arguments were inputs, and how the modified ones can be changed. */

/* Revision 1.2  1996/03/12  23:53:00  jaf */
/* Lots of comments added about the local state of this subroutine that */
/* must be saved from one invocation to the next. */

/* One constant 180 replaced with LFRAME, which should be "more general", */
/* even though it would probably require many more changes than this to */
/* get this coder to work for other frame sizes. */

/* Revision 1.1  1996/02/07 14:48:09  jaf */
/* Initial revision */


/* ****************************************************************** */

/* 	Floating point version */


/*   Detection of onsets in (or slightly preceding) the futuremost frame */
/*   of speech. */


/* Input: */
/*  PEBUF(SBUFL:SBUFH)  - Preemphasized speech */
/*                        Indices SBUFH-LFRAME through SBUFH are read. */
/*  OSLEN  - Maximum number of onsets that can be stored in OSBUF. */
/*  SBUFL, SBUFH        - Range of PEBUF */
/*  LFRAME              - length of a frame, in samples */
/* Input/Output: */
/*  OSBUF(OSLEN) - Buffer which holds sorted indexes of onsets */
/*                 Indices A through B are modified, where A */
/*                 is the original value of OSPTR, and B is the final */
/*                 value of OSPTR-1.  B is at most OSLEN. */
/*  OSPTR        - Free pointer into OSBUF */
/*                 Initial value should be .LE. OSLEN+1. */
/*                 If so, final value grows by one for each new onset */
/*                 found, and final value will be .LE. OSLEN+1. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this subroutine, or */
/* reinitialize its state for any other reason, call the ENTRY INITONSET. */

/*< 	S >*/
/* Subroutine */ int onset_0_(int n__, real *pebuf, integer *osbuf, integer *
	osptr, integer *oslen, integer *sbufl, integer *sbufh, integer *
	lframe)
{
    /* Initialized data */

    static real n = 0.f;
    static real d__ = 1.f;
    static real l2buf[16] = { 0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f };
    static real l2sum1 = 0.f;
    static integer l2ptr1 = 1;
    static integer l2ptr2 = 9;
    static logical hyst = FALSE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    integer i__;
    static integer lasti;
    real l2sum2;
    static real fpc;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INTEGER OSLEN, SBUFL, SBUFH, LFRAME >*/
/*       Arguments */
/* $Log: onset.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/29  22:03:47  jaf */
/* Removed definitions for any constants that were no longer used. */

/* Revision 1.2  1996/03/26  19:34:33  jaf */
/* Added comments indicating which constants are not needed in an */
/* application that uses the LPC-10 coder. */

/* Revision 1.1  1996/02/07  14:43:51  jaf */
/* Initial revision */

/*   LPC Configuration parameters: */
/* Frame size, Prediction order, Pitch period */
/*< 	parameter (MAXFRM = 180, MAXORD = 10, MAXPIT = 156) >*/
/*< 	REAL PEBUF(SBUFL:SBUFH) >*/
/*< 	INTEGER OSBUF(OSLEN), OSPTR >*/
/*       Parameters/constants */
/*   Parameters for onset detection algorithm: */
/*    L2		Threshold for filtered slope of FPC (function of L2WID!) */
/*    L2LAG	Lag due to both filters which compute filtered slope of FPC */
/*    L2WID	Width of the filter which computes the slope of FPC */
/*    OSHYST	The number of samples of slope(FPC) which must be below */
/* 	        the threshold before a new onset may be declared. */
/*< 	INTEGER L2LAG, L2WID, OSHYST, TEMP >*/
/*< 	REAL L2 >*/
/*< 	PARAMETER (L2=1.7, L2LAG=9, L2WID=16, OSHYST=10) >*/
/*< 	PARAMETER (TEMP=1+L2WID/2) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I >*/
/*< 	REAL L2SUM2 >*/
/*       Local state */
/*   Variables */
/*    N, D       Numerator and denominator of prediction filters */
/*    FPC        Current prediction coefs */
/*    L2BUF, L2SUM1, L2SUM2    State of slope filter */
/*       The only "significant" change I've made is to change L2SUM2 out 
*/
/*       of the list of local variables that need to be saved, since it */
/*       didn't need to be. */
/*       L2SUM1 need not be, but avoiding saving it would require a small 
*/
/*       change to the body of the code.  See comments below for an */
/*       example of how the code could be changed to avoid saving L2SUM1. 
*/
/*       FPC and LASTI are saved from one invocation to the next, but */
/*       they are not given initial values.  This is acceptable, because 
*/
/*       FPC will be assigned a value the first time that this function */
/*       is called after D is initialized to 1, since the formula to */
/*       change D will not change it to 0 in one step, and the IF (D */
/*       .NE. 0) statement will execute its THEN part, initializing FPC. 
*/

/*       LASTI's value will not be used until HYST is .TRUE., and */
/*       whenever HYST is changed from its initial value of .FALSE., */
/*       LASTI is assigned a value. */
/*       In a C version of this coder, it would be nice if all of these */
/*       saved things, in this and all other subroutines, could be stored 
*/
/*       in a single struct lpc10_coder_state_t, initialized with a call 
*/
/*       to a function like lpc10_init(&lpc10_coder_state).  In this way, 
*/
/*       a program that used these functions could conveniently alternate 
*/
/*       coding more than one distinct audio stream. */
/*< 	REAL N, D, FPC >*/
/*< 	REAL L2BUF(L2WID), L2SUM1 >*/
/*< 	INTEGER L2PTR1, L2PTR2, LASTI >*/
/*< 	LOGICAL HYST >*/
/*< 	DATA N/0./, D/1./ >*/
    /* Parameter adjustments */
    if (osbuf) {
	--osbuf;
	}
    if (pebuf) {
	pebuf -= *sbufl;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initonset;
	}

/*< 	DATA L2BUF/L2WID*0./, L2SUM1/0./ >*/
/*< 	DATA L2PTR1/1/, L2PTR2/TEMP/ >*/
/*< 	DATA HYST/.FALSE./ >*/
/*< 	SAVE N, D, FPC >*/
/*< 	SAVE L2BUF, L2SUM1 >*/
/*< 	SAVE L2PTR1, L2PTR2, LASTI >*/
/*< 	SAVE HYST >*/
/*       The following line subtracted a hard-coded "180" from LASTI, */
/*       instead of using a variable like LFRAME or a constant like */
/*       MAXFRM.  I changed it to LFRAME, for "generality". */
/*< 	IF (HYST) LASTI = LASTI - LFRAME >*/
    if (hyst) {
	lasti -= *lframe;
    }
/*< 	DO I = SBUFH-LFRAME+1, SBUFH >*/
    i__1 = *sbufh;
    for (i__ = *sbufh - *lframe + 1; i__ <= i__1; ++i__) {
/*   Compute FPC; Use old FPC on divide by zero; Clamp FPC to +/- 1. 
*/
/*< 	   N=(PEBUF(I)*PEBUF(I-1)+63.*N) / 64. >*/
	n = (pebuf[i__] * pebuf[i__ - 1] + n * 63.f) / 64.f;
/*< 	   D=(PEBUF(I-1)**2+63.*D) / 64. >*/
/* Computing 2nd power */
	d__ = (pebuf[i__-1] * pebuf[i__-1] + d__ * 63.f) / 64.f;
/*< 	   IF (D .NE. 0.) THEN >*/
	if (d__ != 0.f) {
/*< 	      IF (ABS(N) .GT. D) THEN >*/
	    if (abs(n) > d__) {
/*< 	         FPC = SIGN (1., N) >*/
		fpc = (real) r_sign(&c_b2, &n);
/*< 	      ELSE >*/
	    } else {
/*< 	         FPC=N/D >*/
		fpc = n / d__;
/*< 	      END IF >*/
	    }
/*< 	   END IF >*/
	}
/*   Filter FPC */
/*       In order to allow L2SUM1 not to be saved from one invocation 
of */
/*       this subroutine to the next, one could change the sequence of
 */
/*       assignments below, up to the IF statement, to the following. 
 In */
/*       addition, the initial value of L2PTR2 should be changed to */
/*       L2WID/2 instead of L2WID/2+1. */

/*       L2SUM1 = L2BUF(L2PTR2) */
/*       L2PTR2 = MOD(L2PTR2,L2WID)+1 */
/*       L2SUM1 = L2SUM1 - L2BUF(L2PTR2) + FPC */
/*       L2BUF(L2PTR2) = L2SUM1 */

/* *       The following lines didn't change from the original: */
/*       L2SUM2 = L2BUF(L2PTR1) */
/*       L2BUF(L2PTR1) = FPC */
/*       L2PTR1 = MOD(L2PTR1,L2WID)+1 */

/*< 	   L2SUM2 = L2BUF(L2PTR1) >*/
	l2sum2 = l2buf[l2ptr1 - 1];
/*< 	   L2SUM1 = L2SUM1 - L2BUF(L2PTR2) + FPC >*/
	l2sum1 = l2sum1 - l2buf[l2ptr2 - 1] + fpc;
/*< 	   L2BUF(L2PTR2) = L2SUM1 >*/
	l2buf[l2ptr2 - 1] = l2sum1;
/*< 	   L2BUF(L2PTR1) = FPC >*/
	l2buf[l2ptr1 - 1] = fpc;
/*< 	   L2PTR1 = MOD(L2PTR1,L2WID)+1 >*/
	l2ptr1 = l2ptr1 % 16 + 1;
/*< 	   L2PTR2 = MOD(L2PTR2,L2WID)+1 >*/
	l2ptr2 = l2ptr2 % 16 + 1;
/*< 	   IF (ABS(L2SUM1-L2SUM2) .GT. L2) THEN >*/
	if (abs(l2sum1 - l2sum2) > 1.7f) {
/*< 	      IF (.NOT. HYST) THEN >*/
	    if (! hyst) {
/*   Ignore if buffer full */
/*< 	         IF (OSPTR .LE. OSLEN) THEN >*/
		if (*osptr <= *oslen) {
/*< 	            OSBUF (OSPTR) = I - L2LAG >*/
		    osbuf[*osptr] = i__ - 9;
/*< 	            OSPTR = OSPTR + 1 >*/
		    ++(*osptr);
/*< 	         END IF >*/
		}
/*< 	         HYST = .TRUE. >*/
		hyst = TRUE_;
/*< 	      END IF >*/
	    }
/*< 	      LASTI = I >*/
	    lasti = i__;
/*       After one onset detection, at least OSHYST sample times m
ust go */
/*       by before another is allowed to occur. */
/*< 	   ELSE IF (HYST .AND. I - LASTI .GE. OSHYST) THEN >*/
	} else if (hyst && i__ - lasti >= 10) {
/*< 	      HYST = .FALSE. >*/
	    hyst = FALSE_;
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITONSET () >*/

L_initonset:
/*< 	N = 0. >*/
    n = 0.f;
/*< 	D = 1. >*/
    d__ = 1.f;
/*< 	DO I = 1, L2WID >*/
    for (i__ = 1; i__ <= 16; ++i__) {
/*< 	   L2BUF(I) = 0. >*/
	l2buf[i__ - 1] = 0.f;
/*< 	END DO >*/
    }
/*< 	L2SUM1 = 0. >*/
    l2sum1 = 0.f;
/*< 	L2PTR1 = 1 >*/
    l2ptr1 = 1;
/*< 	L2PTR2 = TEMP >*/
    l2ptr2 = 9;
/*< 	HYST = .FALSE. >*/
    hyst = FALSE_;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* onset_ */

/* Subroutine */ int onset_(real *pebuf, integer *osbuf, integer *osptr, 
	integer *oslen, integer *sbufl, integer *sbufh, integer *lframe)
{
    return onset_0_(0, pebuf, osbuf, osptr, oslen, sbufl, sbufh, lframe);
    }

/* Subroutine */ int initonset_(void)
{
    return onset_0_(1, (real *)0, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0);
    }

