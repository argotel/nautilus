/*
 * dcbias.c
 *
 * SCCS ID:  @(#)dcbias.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int dcbias_(integer *len, real *speech, real *sigout);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************* */

/* 	DCBIAS Version 50 */

/* $Log: dcbias.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/18  21:19:22  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  16:44:53  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:44:21  jaf */
/* Initial revision */


/* ********************************************************************* */

/* Calculate and remove DC bias from buffer. */

/* Input: */
/*  LEN    - Length of speech buffers */
/*  SPEECH - Input speech buffer */
/*           Indices 1 through LEN read. */
/* Output: */
/*  SIGOUT - Output speech buffer */
/*           Indices 1 through LEN written */

/* This subroutine has no local state. */

/*< 	SUBROUTINE DCBIAS( LEN, SPEECH, SIGOUT ) >*/
/* Subroutine */ int dcbias_(integer *len, real *speech, real *sigout)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    real bias;
    integer i__;

/* 	Arguments */
/*< 	INTEGER LEN >*/
/*< 	REAL SPEECH(LEN), SIGOUT(LEN) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I >*/
/*< 	REAL BIAS >*/
/*< 	BIAS = 0 >*/
    /* Parameter adjustments */
    --sigout;
    --speech;

    /* Function Body */
    bias = 0.f;
/*< 	DO I = 1,LEN >*/
    i__1 = *len;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   BIAS = BIAS + SPEECH(I) >*/
	bias += speech[i__];
/*< 	END DO >*/
    }
/*< 	BIAS = BIAS/LEN >*/
    bias /= *len;
/*< 	DO I = 1,LEN >*/
    i__1 = *len;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   SIGOUT(I) = SPEECH(I) - BIAS >*/
	sigout[i__] = speech[i__] - bias;
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* dcbias_ */

