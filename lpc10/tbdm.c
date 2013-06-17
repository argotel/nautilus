/*
 * tbdm.c
 *
 * SCCS ID:  @(#)tbdm.c 1.2 96/05/19
 */

#include "f2c.h"

extern int tbdm_(real *speech, integer *lpita, integer *tau, integer *ltau, real *amdf, integer *minptr, integer *maxptr, integer *mintau);
/*:ref: difmag_ 14 8 6 4 4 4 4 6 4 4 */

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* ********************************************************************** */

/* 	TBDM Version 49 */

/* $Log: tbdm.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/18  22:14:00  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  14:48:37  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:49:54  jaf */
/* Initial revision */


/* ********************************************************************* */

/*TURBO DIFMAG: Compute High Resolution Average Magnitude Difference Function
*/

/* Note: There are several constants in here that appear to depend on a */
/* particular TAU table.  That's not a problem for the LPC10 coder, but */
/* watch out if you change the contents of TAU in the subroutine ANALYS. */

/* Input: */
/*  SPEECH - Low pass filtered speech */
/*           Indices 1 through MAX+LPITA-1 are read, where: */
/*           MAX = (TAU(LTAU)-TAU(1))/2+1 */
/*           (If TAU(1) .LT. 39, then larger indices could be read */
/*           by the last call to DIFMAG below.) */
/*  LPITA  - Length of speech buffer */
/*  TAU    - Table of lags, sorted in increasing order. */
/*           Indices 1 through LTAU read. */
/*  LTAU   - Number of lag values to compute */
/* Output: */
/*  AMDF   - Average Magnitude Difference for each lag in TAU */
/*          Indices 1 through LTAU written, and several might then be read.*/
/*  MINPTR - Index of minimum AMDF value */
/*  MAXPTR - Index of maximum AMDF value within +/- 1/2 octave of min */
/*  MINTAU - Lag corresponding to minimum AMDF value */

/* This subroutine has no local state. */

/*< 	S >*/
/* Subroutine */ int tbdm_(real *speech, integer *lpita, integer *tau, 
	integer *ltau, real *amdf, integer *minptr, integer *maxptr, integer *
	mintau)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Local variables */
    real amdf2[6];
    integer minp2, ltau2, maxp2, i__;
    extern /* Subroutine */ int difmag_(real *, integer *, integer *, integer 
	    *, integer *, real *, integer *, integer *);
    integer minamd, ptr, tau2[6];

/* 	Arguments */
/*< 	INTEGER LPITA, LTAU, MINPTR, MAXPTR, MINTAU >*/
/*< 	INTEGER TAU(LTAU) >*/
/*< 	REAL SPEECH(312), AMDF(LTAU) >*/
/* 	REAL SPEECH(LPITA+TAU(LTAU)), AMDF(LTAU) */
/*   Stupid TOAST doesn't understand expressions */
/*       Local variables that need not be saved */
/*< 	INTEGER I, PTR, MINAMD, TAU2(6), LTAU2, MINP2, MAXP2 >*/
/*< 	REAL AMDF2(6) >*/
/*       Local state */
/*       None */
/*   Compute full AMDF using log spaced lags, find coarse minimum */
/*< 	C >*/
    /* Parameter adjustments */
    --speech;
    --amdf;
    --tau;

    /* Function Body */
    difmag_(&speech[1], lpita, &tau[1], ltau, &tau[*ltau], &amdf[1], minptr, 
	    maxptr);
/*< 	MINTAU = TAU(MINPTR) >*/
    *mintau = tau[*minptr];
/*< 	MINAMD = AMDF(MINPTR) >*/
    minamd = (integer) amdf[*minptr];
/*   Build table containing all lags within +/- 3 of the AMDF minimum */
/*    excluding all that have already been computed */
/*< 	LTAU2 = 0 >*/
    ltau2 = 0;
/*< 	PTR = MINPTR - 2 >*/
    ptr = *minptr - 2;
/*< 	DO I = MAX(MINTAU-3,41), MIN(MINTAU+3,TAU(LTAU)-1) >*/
/* Computing MAX */
    i__1 = *mintau - 3;
/* Computing MIN */
    i__3 = *mintau + 3, i__4 = tau[*ltau] - 1;
    i__2 = min(i__3,i__4);
    for (i__ = max(i__1,41); i__ <= i__2; ++i__) {
/*< 	   DO WHILE( TAU(PTR).LT.I ) >*/
	while(tau[ptr] < i__) {
/*< 	      PTR = PTR + 1 >*/
	    ++ptr;
/*< 	   END DO >*/
	}
/*< 	   IF( TAU(PTR).NE.I) THEN >*/
	if (tau[ptr] != i__) {
/*< 	      LTAU2 = LTAU2 + 1 >*/
	    ++ltau2;
/*< 	      TAU2(LTAU2) = I >*/
	    tau2[ltau2 - 1] = i__;
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*   Compute AMDF of the new lags, if there are any, and choose one */
/*    if it is better than the coarse minimum */
/*< 	IF( LTAU2.GT.0 ) THEN >*/
    if (ltau2 > 0) {
/*< 	  >*/
	difmag_(&speech[1], lpita, tau2, &ltau2, &tau[*ltau], amdf2, &minp2, &
		maxp2);
/*< 	   IF( AMDF2(MINP2).LT.MINAMD ) THEN >*/
	if (amdf2[minp2 - 1] < (real) minamd) {
/*< 	      MINTAU = TAU2(MINP2) >*/
	    *mintau = tau2[minp2 - 1];
/*< 	      MINAMD = AMDF2(MINP2) >*/
	    minamd = (integer) amdf2[minp2 - 1];
/*< 	   END IF >*/
	}
/*< 	END IF >*/
    }
/*   Check one octave up, if there are any lags not yet computed */
/*< 	IF( MINTAU.GE.80 ) THEN >*/
    if (*mintau >= 80) {
/*< 	   I = MINTAU/2 >*/
	i__ = *mintau / 2;
/*< 	   IF( AND(I,1).EQ.0 ) THEN >*/
	if ((i__ & 1) == 0) {
/*< 	      LTAU2 = 2 >*/
	    ltau2 = 2;
/*< 	      TAU2(1) = I-1 >*/
	    tau2[0] = i__ - 1;
/*< 	      TAU2(2) = I+1 >*/
	    tau2[1] = i__ + 1;
/*< 	   ELSE >*/
	} else {
/*< 	      LTAU2 = 1 >*/
	    ltau2 = 1;
/*< 	      TAU2(1) = I >*/
	    tau2[0] = i__;
/*< 	   END IF >*/
	}
/*< 	  >*/
	difmag_(&speech[1], lpita, tau2, &ltau2, &tau[*ltau], amdf2, &minp2, &
		maxp2);
/*< 	   IF( AMDF2(MINP2).LT.MINAMD ) THEN >*/
	if (amdf2[minp2 - 1] < (real) minamd) {
/*< 	      MINTAU = TAU2(MINP2) >*/
	    *mintau = tau2[minp2 - 1];
/*< 	      MINAMD = AMDF2(MINP2) >*/
	    minamd = (integer) amdf2[minp2 - 1];
/*< 	      MINPTR = MINPTR - 20 >*/
	    *minptr += -20;
/*< 	   END IF >*/
	}
/*< 	END IF >*/
    }
/*   Force minimum of the AMDF array to the high resolution minimum */
/*< 	AMDF(MINPTR) = MINAMD >*/
    amdf[*minptr] = (real) minamd;
/*   Find maximum of AMDF within 1/2 octave of minimum */
/*< 	MAXPTR = MAX(MINPTR-5,1) >*/
/* Computing MAX */
    i__2 = *minptr - 5;
    *maxptr = max(i__2,1);
/*< 	DO I = MAXPTR+1, MIN(MINPTR+5,LTAU) >*/
/* Computing MIN */
    i__1 = *minptr + 5;
    i__2 = min(i__1,*ltau);
    for (i__ = *maxptr + 1; i__ <= i__2; ++i__) {
/*< 	   IF( AMDF(I).GT.AMDF(MAXPTR) ) MAXPTR = I >*/
	if (amdf[i__] > amdf[*maxptr]) {
	    *maxptr = i__;
	}
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* tbdm_ */

