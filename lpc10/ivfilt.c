/*
 * ivfilt.c
 *
 * SCCS ID:  @(#)ivfilt.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int ivfilt_(real *lpbuf, real *ivbuf, integer *len, integer *nsamp, real *ivrc);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************* */

/* 	IVFILT Version 48 */

/* $Log: ivfilt.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/15  21:36:29  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  00:01:00  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:47:34  jaf */
/* Initial revision */


/* ********************************************************************* */

/*   2nd order inverse filter, speech is decimated 4:1 */

/* Input: */
/*  LEN    - Length of speech buffers */
/*  NSAMP  - Number of samples to filter */
/*  LPBUF  - Low pass filtered speech buffer */
/*           Indices LEN-NSAMP-7 through LEN read. */
/* Output: */
/*  IVBUF  - Inverse filtered speech buffer */
/*           Indices LEN-NSAMP+1 through LEN written. */
/*  IVRC   - Inverse filter reflection coefficients (for voicing) */
/*          Indices 1 and 2 both written (also read, but only after writing).
*/

/* This subroutine has no local state. */

/*< 	SUBROUTINE IVFILT( LPBUF, IVBUF, LEN, NSAMP, IVRC ) >*/
/* Subroutine */ int ivfilt_(real *lpbuf, real *ivbuf, integer *len, integer *
	nsamp, real *ivrc)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j, k;
    real r__[3], pc1, pc2;

/* 	Arguments */
/*< 	INTEGER LEN, NSAMP >*/
/*< 	REAL LPBUF(LEN), IVBUF(LEN) >*/
/*< 	REAL IVRC(2) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, J, K >*/
/*< 	REAL R(3), PC1, PC2 >*/
/*       Local state */
/*       None */
/*  Calculate Autocorrelations */
/*< 	DO I = 1,3 >*/
    /* Parameter adjustments */
    --ivbuf;
    --lpbuf;
    --ivrc;

    /* Function Body */
    for (i__ = 1; i__ <= 3; ++i__) {
/*< 	   R(I) = 0. >*/
	r__[i__ - 1] = 0.f;
/*< 	   K = 4*(I-1) >*/
	k = i__ - 1 << 2;
/*< 	   DO J = I*4+LEN-NSAMP,LEN,2 >*/
	i__1 = *len;
	for (j = (i__ << 2) + *len - *nsamp; j <= i__1; j += 2) {
/*< 	      R(I) = R(I) + LPBUF(J)*LPBUF(J-K) >*/
	    r__[i__ - 1] += lpbuf[j] * lpbuf[j - k];
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*  Calculate predictor coefficients */
/*< 	PC1 = 0. >*/
    pc1 = 0.f;
/*< 	PC2 = 0. >*/
    pc2 = 0.f;
/*< 	IVRC(1) = 0. >*/
    ivrc[1] = 0.f;
/*< 	IVRC(2) = 0. >*/
    ivrc[2] = 0.f;
/*< 	IF (R(1) .GT. 1.0E-10) THEN >*/
    if (r__[0] > 1e-10f) {
/*< 	   IVRC(1) = R(2)/R(1) >*/
	ivrc[1] = r__[1] / r__[0];
/*< 	   IVRC(2) = (R(3)-IVRC(1)*R(2)) / (R(1)-IVRC(1)*R(2)) >*/
	ivrc[2] = (r__[2] - ivrc[1] * r__[1]) / (r__[0] - ivrc[1] * r__[1]);
/*< 	   PC1 = IVRC(1) - IVRC(1)*IVRC(2) >*/
	pc1 = ivrc[1] - ivrc[1] * ivrc[2];
/*< 	   PC2 = IVRC(2) >*/
	pc2 = ivrc[2];
/*< 	END IF >*/
    }
/*  Inverse filter LPBUF into IVBUF */
/*< 	DO I = LEN+1-NSAMP,LEN >*/
    i__1 = *len;
    for (i__ = *len + 1 - *nsamp; i__ <= i__1; ++i__) {
/*< 	   IVBUF(I) = LPBUF(I) - PC1*LPBUF(I-4) - PC2*LPBUF(I-8) >*/
	ivbuf[i__] = lpbuf[i__] - pc1 * lpbuf[i__ - 4] - pc2 * lpbuf[i__ - 8];
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* ivfilt_ */

