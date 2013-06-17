/*
 * irc2pc.c
 *
 * SCCS ID:  @(#)irc2pc.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int irc2pc_(real *rc, real *pc, integer *order, real *gprime, real *g2pass);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ***************************************************************** */

/* 	IRC2PC Version 48 */

/* $Log: irc2pc.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/20  15:47:19  jaf */
/* Added comments about which indices of array arguments are read or */
/* written. */

/* Revision 1.2  1996/03/14  16:59:04  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:47:27  jaf */
/* Initial revision */


/* ***************************************************************** */

/*   Convert Reflection Coefficients to Predictor Coeficients */

/* Inputs: */
/*  RC     - Reflection coefficients */
/*           Indices 1 through ORDER read. */
/*  ORDER  - Number of RC's */
/*  GPRIME - Excitation modification gain */
/* Outputs: */
/*  PC     - Predictor coefficients */
/*           Indices 1 through ORDER written. */
/*           Indices 1 through ORDER-1 are read after being written. */
/*  G2PASS - Excitation modification sharpening factor */

/* This subroutine has no local state. */

/*< 	SUBROUTINE IRC2PC( RC, PC, ORDER, GPRIME, G2PASS ) >*/
/* Subroutine */ int irc2pc_(real *rc, real *pc, integer *order, real *gprime,
	 real *g2pass)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    real temp[10];
    integer i__, j;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INTEGER ORDER >*/
/* 	Arguments */
/* $Log: irc2pc.c,v $
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
/*< 	REAL RC(ORDER), PC(ORDER), GPRIME, G2PASS >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, J >*/
/*< 	REAL TEMP(MAXORD) >*/
/*< 	G2PASS = 1. >*/
    /* Parameter adjustments */
    --pc;
    --rc;

    /* Function Body */
    *g2pass = 1.f;
/*< 	DO I = 1,ORDER >*/
    i__1 = *order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   G2PASS = G2PASS*( 1. - RC(I)*RC(I) ) >*/
	*g2pass *= 1.f - rc[i__] * rc[i__];
/*< 	END DO >*/
    }
/*< 	G2PASS = GPRIME*SQRT(G2PASS) >*/
    *g2pass = (real) (*gprime * sqrt(*g2pass));
/*< 	PC(1) = RC(1) >*/
    pc[1] = rc[1];
/*< 	DO I = 2,ORDER >*/
    i__1 = *order;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*< 	   DO J = 1,I-1 >*/
	i__2 = i__ - 1;
	for (j = 1; j <= i__2; ++j) {
/*< 	      TEMP(J) = PC(J) - RC(I)*PC(I-J) >*/
	    temp[j - 1] = pc[j] - rc[i__] * pc[i__ - j];
/*< 	   END DO >*/
	}
/*< 	   DO J = 1,I-1 >*/
	i__2 = i__ - 1;
	for (j = 1; j <= i__2; ++j) {
/*< 	      PC(J) = TEMP(J) >*/
	    pc[j] = temp[j - 1];
/*< 	   END DO >*/
	}
/*< 	   PC(I) = RC(I) >*/
	pc[i__] = rc[i__];
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* irc2pc_ */

