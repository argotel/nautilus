/*
 * prepro.c
 *
 * SCCS ID:  @(#)prepro.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int prepro_(real *speech, integer *length);
extern int initprepro_(void);
/*:ref: hp100_ 14 3 6 4 4 */
/*:ref: inithp100_ 14 0 */
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* ********************************************************************* */

/* 	PREPRO Version 48 */

/* $Log: prepro.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/14  23:22:56  jaf */
/* Added comments about when INITPREPRO should be used. */

/* Revision 1.2  1996/03/14  23:09:27  jaf */
/* Added an entry named INITPREPRO that initializes the local state of */
/* this subroutine, and those it calls (if any). */

/* Revision 1.1  1996/02/07  14:48:54  jaf */
/* Initial revision */


/* ********************************************************************* */

/*    Pre-process input speech: */

/* Inputs: */
/*  LENGTH - Number of SPEECH samples */
/* Input/Output: */
/*  SPEECH(LENGTH) - Speech data. */
/*                   Indices 1 through LENGTH are read and modified. */

/* This subroutine has no local state maintained from one call to the */
/* next, but HP100 does.  If you want to switch to using a new audio */
/* stream for this filter, or reinitialize its state for any other */
/* reason, call the ENTRY INITPREPRO. */

/*< 	SUBROUTINE PREPRO( SPEECH, LENGTH ) >*/
/* Subroutine */ int prepro_0_(int n__, real *speech, integer *length)
{
    extern /* Subroutine */ int hp100_(real *, integer *, integer *), 
	    inithp100_(void);

/*       Arguments */
/*< 	INTEGER LENGTH >*/
/*< 	REAL SPEECH(LENGTH) >*/
/*   High Pass Filter at 100 Hz */
/*< 	CALL HP100( SPEECH, 1, LENGTH ) >*/
    /* Parameter adjustments */
    if (speech) {
	--speech;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initprepro;
	}

    hp100_(&speech[1], &c__1, length);
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITPREPRO () >*/

L_initprepro:
/*< 	CALL INITHP100() >*/
    inithp100_();
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* prepro_ */

/* Subroutine */ int prepro_(real *speech, integer *length)
{
    return prepro_0_(0, speech, length);
    }

/* Subroutine */ int initprepro_(void)
{
    return prepro_0_(1, (real *)0, (integer *)0);
    }

