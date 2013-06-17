/*
 * deemp.c
 *
 * SCCS ID:  @(#)deemp.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int deemp_(real *x, integer *n);
extern int initdeemp_(void);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ***************************************************************** */

/* 	DEEMP Version 48 */

/* $Log: deemp.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/20  15:54:37  jaf */
/* Added comments about which indices of array arguments are read or */
/* written. */

/* Added entry INITDEEMP to reinitialize the local state variables, if */
/* desired. */

/* Revision 1.2  1996/03/14  22:11:13  jaf */
/* Comments added explaining which of the local variables of this */
/* subroutine need to be saved from one invocation to the next, and which */
/* do not. */

/* Revision 1.1  1996/02/07 14:44:53  jaf */
/* Initial revision */


/* ***************************************************************** */

/*  De-Emphasize output speech with   1 / ( 1 - .75z**-1 ) */
/*    cascaded with 200 Hz high pass filter */
/*    ( 1 - 1.9998z**-1 + z**-2 ) / ( 1 - 1.75z**-1 + .78z**-2 ) */

/*  WARNING!  The coefficients above may be out of date with the code */
/*  below.  Either that, or some kind of transformation was performed */
/*  on the coefficients above to create the code below. */

/* Input: */
/*  N  - Number of samples */
/* Input/Output: */
/*  X  - Speech */
/*       Indices 1 through N are read before being written. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITDEEMP. */

/*< 	SUBROUTINE DEEMP( X, N ) >*/
/* Subroutine */ int deemp_0_(int n__, real *x, integer *n)
{
    /* Initialized data */

    static real dei1 = 0.f;
    static real dei2 = 0.f;
    static real deo1 = 0.f;
    static real deo2 = 0.f;
    static real deo3 = 0.f;

    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    integer k;
    real dei0;

/*       Arguments */
/*< 	INTEGER N >*/
/*< 	REAL X(N) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER K >*/
/*< 	REAL DEI0 >*/
/*       Local state */
/*       All of the locals saved below were not given explicit initial */
/*       values in the original code.  I think 0 is a safe choice. */
/*< 	REAL DEI1, DEI2, DEO1, DEO2, DEO3 >*/
/*< 	SAVE DEI1, DEI2, DEO1, DEO2, DEO3 >*/
/*< 	DATA DEI1/0./, DEI2/0./, DEO1/0./, DEO2/0./, DEO3/0./ >*/
    /* Parameter adjustments */
    if (x) {
	--x;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initdeemp;
	}

/*< 	DO K = 1,N >*/
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
/*< 	   DEI0 = X(K) >*/
	dei0 = x[k];
/*< 	  >*/
	r__1 = x[k] - dei1 * 1.9998f + dei2;
	x[k] = r__1 + deo1 * 2.5f - deo2 * 2.0925f + deo3 * .585f;
/*< 	   DEI2 = DEI1 >*/
	dei2 = dei1;
/*< 	   DEI1 = DEI0 >*/
	dei1 = dei0;
/*< 	   DEO3 = DEO2 >*/
	deo3 = deo2;
/*< 	   DEO2 = DEO1 >*/
	deo2 = deo1;
/*< 	   DEO1 = X(K) >*/
	deo1 = x[k];
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITDEEMP () >*/

L_initdeemp:
/*< 	DEI1 = 0. >*/
    dei1 = 0.f;
/*< 	DEI2 = 0. >*/
    dei2 = 0.f;
/*< 	DEO1 = 0. >*/
    deo1 = 0.f;
/*< 	DEO2 = 0. >*/
    deo2 = 0.f;
/*< 	DEO3 = 0. >*/
    deo3 = 0.f;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* deemp_ */

/* Subroutine */ int deemp_(real *x, integer *n)
{
    return deemp_0_(0, x, n);
    }

/* Subroutine */ int initdeemp_(void)
{
    return deemp_0_(1, (real *)0, (integer *)0);
    }

