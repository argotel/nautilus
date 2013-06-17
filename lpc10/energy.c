/*
 * energy.c
 *
 * SCCS ID:  @(#)energy.c 1.2 96/05/19
 */

#include "f2c.h"

extern int energy_(integer *len, real *speech, real *rms);

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* ********************************************************************* */

/* 	ENERGY Version 50 */

/* $Log: energy.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.4  1996/08/01  00:45:00  dm */
/* Normalized indexing of speech[] */

/* Revision 1.3  1996/03/18  21:17:41  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  16:46:02  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:45:40  jaf */
/* Initial revision */


/* ********************************************************************* */

/* Compute RMS energy. */

/* Input: */
/*  LEN    - Length of speech buffer */
/*  SPEECH - Speech buffer */
/*           Indices 1 through LEN read. */
/* Output: */
/*  RMS    - Root Mean Square energy */

/* This subroutine has no local state. */

/*< 	SUBROUTINE ENERGY( LEN, SPEECH, RMS ) >*/
/* Subroutine */ int energy_(integer *len, real *speech, real *rms)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;

/*       Arguments */
/*< 	INTEGER LEN >*/
/*< 	REAL SPEECH(LEN), RMS >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I >*/
/*< 	RMS = 0 >*/

    /* Function Body */
    *rms = 0.f;
/*< 	DO I = 1,LEN >*/
    i__1 = *len;
    for (i__ = 0; i__ < i__1; ++i__) {
/*< 	   RMS = RMS + SPEECH(I)*SPEECH(I) >*/
	*rms += speech[i__] * speech[i__];
/*< 	END DO >*/
    }
/*< 	RMS = SQRT( RMS / LEN ) >*/
    *rms = (real) sqrt(*rms / *len);
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* energy_ */

