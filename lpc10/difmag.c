/*
 * difmag.c
 *
 * SCCS ID:  @(#)difmag.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int difmag_(real *speech, integer *lpita, integer *tau, integer *ltau, integer *maxlag, real *amdf, integer *minptr, integer *maxptr);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************** */

/* 	DIFMAG Version 49 */

/* $Log: difmag.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/15  23:09:39  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  14:41:31  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:45:04  jaf */
/* Initial revision */


/* ********************************************************************* */

/*  Compute Average Magnitude Difference Function */

/* Inputs: */
/*  SPEECH - Low pass filtered speech */
/*           Indices MIN_N1 through MAX_N1+LPITA-1 are read, where */
/*      MIN_N1 = (MAXLAG - MAX_TAU)/2+1  MAX_TAU = max of TAU(I) for I=1,LTAU
*/
/*      MAX_N1 = (MAXLAG - MIN_TAU)/2+1  MIN_TAU = min of TAU(I) for I=1,LTAU
*/
/*  LPITA  - Length of speech buffer */
/*  TAU    - Table of lags */
/*           Indices 1 through LTAU read. */
/*  LTAU   - Number of lag values to compute */
/*  MAXLAG - Maximum possible lag value */
/* Outputs: */
/*  (All of these outputs are also read, but only after being written.) */
/*  AMDF   - Average Magnitude Difference for each lag in TAU */
/*           Indices 1 through LTAU written */
/*  MINPTR - Index of minimum AMDF value */
/*  MAXPTR - Index of maximum AMDF value */

/* This subroutine has no local state. */

/*< 	S >*/
/* Subroutine */ int difmag_(real *speech, integer *lpita, integer *tau, 
	integer *ltau, integer *maxlag, real *amdf, integer *minptr, integer *
	maxptr)
{
    /* System generated locals */
    integer i__1, i__2;
/*    real r__1; */

    /* Local variables */
    integer i__, j, n1;
    real sum;

/*       Arguments */
/*< 	INTEGER LPITA, LTAU, MAXLAG, MINPTR, MAXPTR >*/
/*< 	INTEGER TAU(LTAU) >*/
/*< 	REAL SPEECH(LPITA+MAXLAG), AMDF(LTAU) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, J, N1, N2 >*/
/*< 	REAL SUM >*/
/*       Local state */
/*       None */
/*< 	MINPTR = 1 >*/
    /* Parameter adjustments */
    --amdf;
    --tau;
    --speech;

    /* Function Body */
    *minptr = 1;
/*< 	MAXPTR = 1 >*/
    *maxptr = 1;
/*< 	DO I = 1,LTAU >*/
    i__1 = *ltau;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   N1 = (MAXLAG-TAU(I))/2 + 1 >*/
	n1 = (*maxlag - tau[i__]) / 2 + 1;
/*< 	   N2 = N1 + LPITA - 1 >*/
	i__2 = n1 + *lpita - 1;
/*< 	   SUM = 0. >*/
	sum = 0.f;
/*< 	   DO J = N1,N2,4 >*/
	for (j = n1; j <= i__2; j += 4) {
/*< 	      SUM = SUM + ABS( SPEECH(J) - SPEECH(J+TAU(I)) ) >*/
/* was	    sum += (r__1 = speech[j] - speech[j + tau[i__]], dabs(r__1)); */
	    sum += abs(speech[j] - speech[j + tau[i__]]);
/*< 	   END DO >*/
	}
/*< 	   AMDF(I) = SUM >*/
	amdf[i__] = sum;
/*< 	   IF( AMDF(I).LT.AMDF(MINPTR) ) MINPTR = I >*/
	if (amdf[i__] < amdf[*minptr]) {
	    *minptr = i__;
	}
/*< 	   IF( AMDF(I).GT.AMDF(MAXPTR) ) MAXPTR = I >*/
	else if (amdf[i__] > amdf[*maxptr]) {
	    *maxptr = i__;
	}
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* difmag_ */

