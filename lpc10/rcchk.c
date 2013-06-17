/*
 * rcchk.c
 *
 * SCCS ID:  @(#)rcchk.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int rcchk_(integer *order, real *rc1f, real *rc2f);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************* */

/* 	RCCHK Version 45G */

/* $Log: rcchk.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.4  1996/03/27  18:13:47  jaf */
/* Commented out a call to subroutine ERROR. */

/* Revision 1.3  1996/03/18  15:48:53  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  16:55:22  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:49:08  jaf */
/* Initial revision */


/* ********************************************************************* */

/*  Check RC's, repeat previous frame's RC's if unstable */

/* Input: */
/*  ORDER - Number of RC's */
/*  RC1F  - Previous frame's RC's */
/*          Indices 1 through ORDER may be read. */
/* Input/Output: */
/*  RC2F  - Present frame's RC's */
/*          Indices 1 through ORDER may be read, and written. */

/* This subroutine has no local state. */

/*< 	SUBROUTINE RCCHK( ORDER, RC1F, RC2F ) >*/
/* Subroutine */ int rcchk_(integer *order, real *rc1f, real *rc2f)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    integer i__;

/*       Arguments */
/*< 	INTEGER ORDER >*/
/*< 	REAL RC1F(ORDER), RC2F(ORDER) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I >*/
/*< 	DO I = 1,ORDER >*/
    /* Parameter adjustments */
    --rc2f;
    --rc1f;

    /* Function Body */
    i__1 = *order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   IF(ABS(RC2F(I)).GT..99) GOTO 10 >*/
	if ((r__1 = rc2f[i__], dabs(r__1)) > .99f) {
	    goto L10;
	}
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*       Note: In version embedded in other software, all calls to ERROR 
*/
/*       should probably be removed. */
/*< 10	CONTINUE >*/
L10:

/*       This call to ERROR is only needed for debugging purposes. */

/*       CALL ERROR('RCCHK',2,I) */
/*< 	DO I = 1,ORDER >*/
    i__1 = *order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   RC2F(I) = RC1F(I) >*/
	rc2f[i__] = rc1f[i__];
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* rcchk_ */

