/*
 * lpcenc.c
 *
 * SCCS ID:  @(#)lpcenc.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int lpcenc_(real *speech, integer *bits);
extern int initlpcenc_(void);
/*:ref: prepro_ 14 2 6 4 */
/*:ref: analys_ 14 5 6 4 4 6 6 */
/*:ref: encode_ 14 7 4 4 6 6 4 4 4 */
/*:ref: chanwr_ 14 5 4 4 4 4 4 */
/*:ref: initprepro_ 14 0 */
/*:ref: initanalys_ 14 0 */
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__180 = 180;
static integer c__10 = 10;

/* ***************************************************************** */

/* $Log: lpcenc.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.2  1996/03/28  00:01:22  jaf */
/* Commented out some trace statements. */

/* Revision 1.1  1996/03/28  00:00:27  jaf */
/* Initial revision */


/* ***************************************************************** */

/* Encode one frame of 180 speech samples to 54 bits. */

/* Input: */
/*  SPEECH - Speech encoded as real values in the range [-1,+1]. */
/*           Indices 1 through 180 read, and modified (by PREPRO). */
/* Output: */
/*  BITS   - 54 encoded bits, stored 1 per array element. */
/*           Indices 1 through 54 written. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITLPCENC. */

/*< 	SUBROUTINE LPCENC(SPEECH, BITS) >*/
/* Subroutine */ int lpcenc_0_(int n__, real *speech, integer *bits)
{
    integer irms, voice[2], pitch, ipitv;
    real rc[10];
    extern /* Subroutine */ int encode_(integer *, integer *, real *, real *, 
	    integer *, integer *, integer *), chanwr_(integer *, integer *, 
	    integer *, integer *, integer *), analys_(real *, integer *, 
	    integer *, real *, real *), prepro_(real *, integer *), 
	    initanalys_(void), initprepro_(void);
    integer irc[10];
    real rms;

/*< 	INCLUDE 'config.fh' >*/
/*< 	REAL SPEECH(MAXFRM) >*/
/*       Arguments */
/* $Log: lpcenc.c,v $
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
/*< 	INTEGER BITS(54) >*/
/*       Local variables that need not be saved */
/*       Uncoded speech parameters */
/*< 	INTEGER VOICE(2), PITCH >*/
/*< 	REAL RMS, RC(MAXORD) >*/
/*       Coded speech parameters */
/*< 	INTEGER IPITV, IRMS, IRC(MAXORD) >*/
/*       Local state */
/*       None */
/*< 	CALL PREPRO(SPEECH, MAXFRM) >*/
    /* Parameter adjustments */
    if (speech) {
	--speech;
	}
    if (bits) {
	--bits;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initlpcenc;
	}

    prepro_(&speech[1], &c__180);
/*< 	CALL ANALYS(SPEECH, VOICE, PITCH, RMS, RC) >*/
    analys_(&speech[1], voice, &pitch, &rms, rc);
/* 	WRITE (10,999) VOICE, PITCH, RMS, RC */
/* 999	FORMAT(1X,'ANALYS-> VOICE ',2I4,' PITCH ',I4,' RMS ',E10.4, */
/*     1         ' RC ',10E10.4) */
/*< 	CALL ENCODE(VOICE, PITCH, RMS, RC, IPITV, IRMS, IRC) >*/
    encode_(voice, &pitch, &rms, rc, &ipitv, &irms, irc);
/* 	WRITE (10,998) IPITV, IRMS, IRC */
/* 998	FORMAT(1X,'ENCODE-> IPITV ',I4,' RMS ',I8, */
/*     1         ' RC ',10I8) */
/*< 	CALL CHANWR(MAXORD, IPITV, IRMS, IRC, BITS) >*/
    chanwr_(&c__10, &ipitv, &irms, irc, &bits[1]);
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITLPCENC () >*/

L_initlpcenc:
/*       Call initialization entries for any subroutines above that have 
*/
/*       them. */
/*< 	CALL INITPREPRO () >*/
    initprepro_();
/*< 	CALL INITANALYS () >*/
    initanalys_();
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* lpcenc_ */

/* Subroutine */ int lpcenc_(real *speech, integer *bits)
{
    return lpcenc_0_(0, speech, bits);
    }

/* Subroutine */ int initlpcenc_(void)
{
    return lpcenc_0_(1, (real *)0, (integer *)0);
    }

