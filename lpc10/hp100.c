/*
 * hp100.c
 *
 * SCCS ID:  @(#)hp100.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int hp100_(real *speech, integer *start, integer *end);
extern int inithp100_(void);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************* */

/*      HP100 Version 55 */

/* $Log: hp100.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.6  1996/03/15  16:45:25  jaf */
/* Rearranged a few comments. */

/* Revision 1.5  1996/03/14  23:20:54  jaf */
/* Added comments about when INITHP100 should be used. */

/* Revision 1.4  1996/03/14  23:08:08  jaf */
/* Added an entry named INITHP100 that initializes the local state of */
/* subroutine HP100. */

/* Revision 1.3  1996/03/14  22:09:20  jaf */
/* Comments added explaining which of the local variables of this */
/* subroutine need to be saved from one invocation to the next, and which */
/* do not. */

/* Revision 1.2  1996/02/12  15:05:54  jaf */
/* Added lots of comments explaining why I changed one line, which was a */
/* declaration with initializations. */

/* Revision 1.1  1996/02/07 14:47:12  jaf */
/* Initial revision */


/* ********************************************************************* */

/*    100 Hz High Pass Filter */

/* Jan 92 - corrected typo (1.937148 to 1.935715), */
/*          rounded coefficients to 7 places, */
/*          corrected and merged gain (.97466**4), */
/*          merged numerator into first two sections. */

/* Input: */
/*  start, end - Range of samples to filter */
/* Input/Output: */
/*  speech(end) - Speech data. */
/*                Indices start through end are read and modified. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITHP100. */
/*< 	subroutine hp100(speech, start, end) >*/
/* Subroutine */ int hp100_0_(int n__, real *speech, integer *start, integer *
	end)
{
    /* Initialized data */

    static real z11 = 0.f;
    static real z21 = 0.f;
    static real z12 = 0.f;
    static real z22 = 0.f;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    real si, err;

/*       Arguments */
/*< 	integer start, end >*/
/*< 	real speech(end) >*/
/*       Local variables that need not be saved */
/*< 	integer i >*/
/*< 	real si, err >*/
/*       Local state */
/*< 	real z11, z21, z12, z22 >*/
/*< 	data z11/0./, z21/0./, z12/0./, z22/0./ >*/
    /* Parameter adjustments */
    if (speech) {
	--speech;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_inithp100;
	}

/*< 	save z11, z21, z12, z22 >*/
/*< 	do i = start,end >*/
    i__1 = *end;
    for (i__ = *start; i__ <= i__1; ++i__) {
/*< 	    si = speech(i) >*/
	si = speech[i__];
/*< 	    err = si + 1.859076*z11 - .8648249*z21 >*/
	err = si + z11 * 1.859076f - z21 * .8648249f;
/*< 	    si = err - 2.00*z11 + z21 >*/
	si = err - z11 * 2.f + z21;
/*< 	    z21 = z11 >*/
	z21 = z11;
/*< 	    z11 = err >*/
	z11 = err;
/*< 	    err = si + 1.935715*z12 - .9417004*z22 >*/
	err = si + z12 * 1.935715f - z22 * .9417004f;
/*< 	    si = err - 2.00*z12 + z22 >*/
	si = err - z12 * 2.f + z22;
/*< 	    z22 = z12 >*/
	z22 = z12;
/*< 	    z12 = err >*/
	z12 = err;
/*< 	    speech(i) = .902428*si >*/
	speech[i__] = si * .902428f;
/*< 	end do >*/
    }
/*< 	return >*/
    return 0;
/*< 	entry inithp100 () >*/

L_inithp100:
/*< 	z11 = 0. >*/
    z11 = 0.f;
/*< 	z21 = 0. >*/
    z21 = 0.f;
/*< 	z12 = 0. >*/
    z12 = 0.f;
/*< 	z22 = 0. >*/
    z22 = 0.f;
/*< 	return >*/
    return 0;
/*< 	end >*/
} /* hp100_ */

/* Subroutine */ int hp100_(real *speech, integer *start, integer *end)
{
    return hp100_0_(0, speech, start, end);
    }

/* Subroutine */ int inithp100_(void)
{
    return hp100_0_(1, (real *)0, (integer *)0, (integer *)0);
    }

