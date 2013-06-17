/*
 * lpcdec.c
 *
 * SCCS ID:  @(#)lpcdec.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int lpcdec_(integer *bits, real *speech);
extern int initlpcdec_(void);
/* comlen contrl_ 12 */
/*:ref: chanrd_ 14 5 4 4 4 4 4 */
/*:ref: decode_ 14 7 4 4 4 4 4 6 6 */
/*:ref: synths_ 14 6 4 4 6 6 6 4 */
/*:ref: initdecode_ 14 0 */
/*:ref: initsynths_ 14 0 */
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

extern struct {
    integer order, lframe;
    logical corrp;
} contrl_;

#define contrl_1 contrl_

/* Table of constant values */

static integer c__10 = 10;

/* ***************************************************************** */

/* $Log: lpcdec.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.1  1996/03/28  00:03:00  jaf */
/* Initial revision */


/* ***************************************************************** */

/* Decode 54 bits to one frame of 180 speech samples. */

/* Input: */
/*  BITS   - 54 encoded bits, stored 1 per array element. */
/*           Indices 1 through 53 read (SYNC bit ignored). */
/* Output: */
/*  SPEECH - Speech encoded as real values in the range [-1,+1]. */
/*           Indices 1 through 180 written. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITLPCDEC. */

/*< 	SUBROUTINE LPCDEC(BITS, SPEECH) >*/
/* Subroutine */ int lpcdec_0_(int n__, integer *bits, real *speech)
{
    integer irms, voice[2], pitch, ipitv;
    extern /* Subroutine */ int decode_(integer *, integer *, integer *, 
	    integer *, integer *, real *, real *);
    real rc[10];
    extern /* Subroutine */ int chanrd_(integer *, integer *, integer *, 
	    integer *, integer *), initdecode_(void), synths_(integer *, 
	    integer *, real *, real *, real *, integer *), initsynths_(void);
    integer irc[10], len;
    real rms;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INCLUDE 'contrl.fh' >*/
/* $Log: lpcdec.c,v $
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
/*       Arguments */
/* $Log: lpcdec.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/29  22:05:55  jaf */
/* Commented out the common block variables that are not needed by the */
/* embedded version. */

/* Revision 1.2  1996/03/26  19:34:50  jaf */
/* Added comments indicating which constants are not needed in an */
/* application that uses the LPC-10 coder. */

/* Revision 1.1  1996/02/07  14:44:09  jaf */
/* Initial revision */

/*   LPC Processing control variables: */

/* *** Read-only: initialized in setup */

/*  Files for Speech, Parameter, and Bitstream Input & Output, */
/*    and message and debug outputs. */

/* Here are the only files which use these variables: */

/* lpcsim.f setup.f trans.f error.f vqsetup.f */

/* Many files which use fdebug are not listed, since it is only used in */
/* those other files conditionally, to print trace statements. */
/* 	integer fsi, fso, fpi, fpo, fbi, fbo, pbin, fmsg, fdebug */
/*  LPC order, Frame size, Quantization rate, Bits per frame, */
/*    Error correction */
/* Subroutine SETUP is the only place where order is assigned a value, */
/* and that value is 10.  It could increase efficiency 1% or so to */
/* declare order as a constant (i.e., a Fortran PARAMETER) instead of as 
*/
/* a variable in a COMMON block, since it is used in many places in the */
/* core of the coding and decoding routines.  Actually, I take that back. 
*/
/* At least when compiling with f2c, the upper bound of DO loops is */
/* stored in a local variable before the DO loop begins, and then that is 
*/
/* compared against on each iteration. */
/* Similarly for lframe, which is given a value of MAXFRM in SETUP. */
/* Similarly for quant, which is given a value of 2400 in SETUP.  quant */
/* is used in only a few places, and never in the core coding and */
/* decoding routines, so it could be eliminated entirely. */
/* nbits is similar to quant, and is given a value of 54 in SETUP. */
/* corrp is given a value of .TRUE. in SETUP, and is only used in the */
/* subroutines ENCODE and DECODE.  It doesn't affect the speed of the */
/* coder significantly whether it is .TRUE. or .FALSE., or whether it is 
*/
/* a constant or a variable, since it is only examined once per frame. */
/* Leaving it as a variable that is set to .TRUE.  seems like a good */
/* idea, since it does enable some error-correction capability for */
/* unvoiced frames, with no change in the coding rate, and no noticeable 
*/
/* quality difference in the decoded speech. */
/* 	integer quant, nbits */
/*< 	logical corrp >*/
/* *** Read/write: variables for debugging, not needed for LPC algorithm 
*/

/*  Current frame, Unstable frames, Output clip count, Max onset buffer, 
*/
/*    Debug listing detail level, Line count on listing page */

/* nframe is not needed for an embedded LPC10 at all. */
/* nunsfm is initialized to 0 in SETUP, and incremented in subroutine */
/* ERROR, which is only called from RCCHK.  When LPC10 is embedded into */
/* an application, I would recommend removing the call to ERROR in RCCHK, 
*/
/* and remove ERROR and nunsfm completely. */
/* iclip is initialized to 0 in SETUP, and incremented in entry SWRITE in 
*/
/* sread.f.  When LPC10 is embedded into an application, one might want */
/* to cause it to be incremented in a routine that takes the output of */
/* SYNTHS and sends it to an audio device.  It could be optionally */
/* displayed, for those that might want to know what it is. */
/* maxosp is never initialized to 0 in SETUP, although it probably should 
*/
/* be, and it is updated in subroutine ANALYS.  I doubt that its value */
/* would be of much interest to an application in which LPC10 is */
/* embedded. */
/* listl and lincnt are not needed for an embedded LPC10 at all. */
/* 	integer nframe, nunsfm, iclip, maxosp, listl, lincnt */
/* 	common /contrl/ fsi, fso, fpi, fpo, fbi, fbo, pbin, fmsg, fdebug */
/*< 	common /contrl/ order, lframe >*/
/* 	common /contrl/ quant, nbits */
/*< 	common /contrl/ corrp >*/
/* 	common /contrl/ nframe, nunsfm, iclip, maxosp, listl, lincnt */
/*< 	REAL SPEECH(MAXFRM) >*/
/*       Local variables that need not be saved */
/*       Uncoded speech parameters */
/*< 	INTEGER VOICE(2), PITCH >*/
/*< 	REAL RMS, RC(MAXORD) >*/
/*       Coded speech parameters */
/*< 	INTEGER IPITV, IRMS, IRC(MAXORD) >*/
/*       Others */
/*< 	INTEGER LEN >*/
/*       Local state */
/*       None */
/*< 	CALL CHANRD(MAXORD, IPITV, IRMS, IRC, BITS) >*/
    /* Parameter adjustments */
    if (bits) {
	--bits;
	}
    if (speech) {
	--speech;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initlpcdec;
	}

    chanrd_(&c__10, &ipitv, &irms, irc, &bits[1]);
/*< 	CALL DECODE(IPITV, IRMS, IRC, VOICE, PITCH, RMS, RC) >*/
    decode_(&ipitv, &irms, irc, voice, &pitch, &rms, rc);
/*< 	CALL SYNTHS(VOICE, PITCH, RMS, RC, SPEECH, LEN) >*/
    synths_(voice, &pitch, &rms, rc, &speech[1], &len);
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITLPCDEC () >*/

L_initlpcdec:
/*       Call initialization entries for any subroutines above that have 
*/
/*       them. */
/*< 	CALL INITDECODE () >*/
    initdecode_();
/*< 	CALL INITSYNTHS () >*/
    initsynths_();
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* lpcdec_ */

/* Subroutine */ int lpcdec_(integer *bits, real *speech)
{
    return lpcdec_0_(0, bits, speech);
    }

/* Subroutine */ int initlpcdec_(void)
{
    return lpcdec_0_(1, (integer *)0, (real *)0);
    }

