/*
 * analys.c
 *
 * SCCS ID:  @(#)analys.c 1.4 96/05/26
 */

#include "f2c.h"

extern int analys_(real *speech, integer *voice, integer *pitch, real *rms, real *rc);
extern int pitdec_(integer *pitch, integer *ptau);
extern int initanalys_(void);
/* comlen contrl_ 12 */
/*:ref: preemp_ 14 5 6 6 4 6 6 */
/*:ref: onset_ 14 7 6 4 4 4 4 4 4 */
/*:ref: placev_ 14 11 4 4 4 4 4 4 4 4 4 4 4 */
/*:ref: lpfilt_ 14 4 6 6 4 4 */
/*:ref: ivfilt_ 14 5 6 6 4 4 6 */
/*:ref: tbdm_ 14 8 6 4 4 4 6 4 4 4 */
/*:ref: voicin_ 14 12 4 6 6 4 4 6 6 4 6 4 4 4 */
/*:ref: dyptrk_ 14 6 6 4 4 4 4 4 */
/*:ref: placea_ 14 9 4 4 4 4 4 4 4 4 4 */
/*:ref: dcbias_ 14 3 4 6 6 */
/*:ref: energy_ 14 3 4 6 6 */
/*:ref: mload_ 14 6 4 4 4 6 6 6 */
/*:ref: invert_ 14 4 4 6 6 6 */
/*:ref: rcchk_ 14 3 4 6 6 */
/*:ref: initonset_ 14 0 */
/*:ref: initvoicin_ 14 0 */
/*:ref: initdyptrk_ 14 0 */
/* Rerunning f2c -P may change prototypes or declarations. */

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* Common Block Declarations */

struct {
    integer order, lframe;
    logical corrp;
} contrl_;

#define contrl_1 contrl_

/* Table of constant values */

static integer c__10 = 10;
static integer c__181 = 181;
static integer c__720 = 720;
static integer c__3 = 3;
static integer c__90 = 90;
static integer c__156 = 156;
static integer c__307 = 307;
static integer c__462 = 462;
static integer c__312 = 312;
static integer c__60 = 60;
static integer c__1 = 1;

/* ****************************************************************** */

/* 	ANALYS Version 55 */

/* $Log: analys.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.9  1996/05/23  19:41:07  jaf */
/* Commented out some unnecessary lines that were reading uninitialized */
/* values. */

/* Revision 1.8  1996/03/27  23:57:55  jaf */
/* Added some comments about which indices of the local buffers INBUF, */
/* LPBUF, etc., get read or modified by some of the subroutine calls.  I */
/* just did this while trying to figure out the discrepancy between the */
/* embedded code compiled with all local variables implicitly saved, and */
/* without. */

/* I added some debugging write statements in hopes of finding a problem. */
/* None of them ever printed anything while running with the long input */
/* speech file dam9.spd provided in the distribution. */

/* Revision 1.7  1996/03/27  18:06:20  jaf */
/* Commented out access to MAXOSP, which is just a debugging variable */
/* that was defined in the COMMON block CONTRL in contrl.fh. */

/* Revision 1.6  1996/03/26  19:31:33  jaf */
/* Commented out trace statements. */

/* Revision 1.5  1996/03/21  15:19:35  jaf */
/* Added comments for ENTRY PITDEC. */

/* Revision 1.4  1996/03/19  20:54:27  jaf */
/* Added a line to INITANALYS.  See comments there. */

/* Revision 1.3  1996/03/19  20:52:49  jaf */
/* Rearranged the order of the local variables quite a bit, to separate */
/* them into groups of "constants", "locals that don't need to be saved */
/* from one call to the next", and "local that do need to be saved from */
/* one call to the next". */

/* Several locals in the last set should have been given initial values, */
/* but weren't.  I gave them all initial values of 0. */

/* Added a separate ENTRY INITANALYS that initializes all local state */
/* that should be, and also calls the corresponding entries of the */
/* subroutines called by ANALYS that also have local state. */

/* There used to be DATA statements in ANALYS.  I got rid of most of */
/* them, and added a local logical variable FIRST that calls the entry */
/* INITANALYS on the first call to ANALYS.  This is just so that one need */
/* not remember to call INITANALYS first in order for the state to be */
/* initialized. */

/* Revision 1.2  1996/03/11  23:29:32  jaf */
/* Added several comments with my own personal questions about the */
/* Fortran 77 meaning of the parameters passed to the subroutine PREEMP. */

/* Revision 1.1  1996/02/07  14:42:29  jaf */
/* Initial revision */


/* ****************************************************************** */

/* SUBROUTINE ANALYS */

/* Input: */
/*  SPEECH */
/*       Indices 1 through LFRAME read. */
/* Output: */
/*  VOICE */
/*       Indices 1 through 2 written. */
/*  PITCH */
/*       Written in subroutine DYPTRK, and then perhaps read and written */
/*       some more. */
/*  RMS */
/*       Written. */
/*  RC */
/*       Indices 1 through ORDER written (ORDER defined in contrl.fh). */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITANALYS. */


/* ENTRY PITDEC */

/* Input: */
/*  PITCH   - Encoded pitch index */
/* Output: */
/*  PTAU    - Decoded pitch period */

/* This entry has no local state.  It accesses a "constant" array */
/* declared in ANALYS. */

/*< 	SUBROUTINE ANALYS(SPEECH, VOICE, PITCH, RMS, RC) >*/
/* Subroutine */ int analys_0_(int n__, real *speech, integer *voice, integer 
	*pitch, real *rms, real *rc, integer *ptau)
{
    /* Initialized data */

    static integer tau[60] = { 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
	    35,36,37,38,39,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,
	    74,76,78,80,84,88,92,96,100,104,108,112,116,120,124,128,132,136,
	    140,144,148,152,156 };
    static integer buflim[4] = { 181,720,25,720 };
    static real precoef = .9375f;
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    real amdf[60];
    integer half;
    real abuf[156];
    static real bias;
    extern /* Subroutine */ int tbdm_(real *, integer *, integer *, integer *,
	     real *, integer *, integer *, integer *);
    static integer awin[6]	/* was [2][3] */;
    integer midx, ewin[6]	/* was [2][3] */;
    real ivrc[2], temp;
    static real zpre;
    static integer vwin[6]	/* was [2][3] */;
    integer i__, j, lanal;
    extern /* Subroutine */ int rcchk_(integer *, real *, real *), mload_(
	    integer *, integer *, integer *, real *, real *, real *);
    static real pebuf[540], rcbuf[30]	/* was [10][3] */, inbuf[540], lpbuf[
	    696], ivbuf[312];
    static integer osbuf[10];
    extern /* Subroutine */ int onset_(real *, integer *, integer *, integer *
	    , integer *, integer *, integer *);
    static integer osptr;
    extern /* Subroutine */ int initonset_(void), placea_(integer *, integer *
	    , integer *, integer *, integer *, integer *, integer *, integer *
	    , integer *), dcbias_(integer *, real *, real *), placev_(integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *);
    integer ipitch;
    static integer obound[3];
    extern /* Subroutine */ int preemp_(real *, real *, integer *, real *, 
	    real *), voicin_(integer *, real *, real *, integer *, integer *, 
	    real *, real *, integer *, real *, integer *, integer *, integer *
	    );
    static integer voibuf[8]	/* was [2][4] */;
    integer mintau;
    static real rmsbuf[3];
    extern /* Subroutine */ int lpfilt_(real *, real *, integer *, integer *),
	     ivfilt_(real *, real *, integer *, integer *, real *), energy_(
	    integer *, real *, real *), invert_(integer *, real *, real *, 
	    real *);
    integer minptr, maxptr;
    extern /* Subroutine */ int dyptrk_(real *, integer *, integer *, integer 
	    *, integer *, integer *), initvoicin_(void), initdyptrk_(void);
    real phi[100]	/* was [10][10] */, psi[10];

/*< 	INCLUDE 'config.fh' >*/
/*< 	INCLUDE 'contrl.fh' >*/
/* $Log: analys.c,v $
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
/*< 	REAL SPEECH(LFRAME) >*/
/*       Arguments to ANALYS */
/* $Log: analys.c,v $
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
/*< 	INTEGER VOICE(2), PITCH >*/
/*< 	REAL RMS, RC(ORDER) >*/
/*       Arguments to entry PITDEC (below) */
/*< 	INTEGER PTAU >*/
/* 	Parameters/constants */
/*  Constants */
/*    NF =     Number of frames */
/*    AF =     Frame in which analysis is done */
/*    OSLEN =  Length of the onset buffer */
/*    LTAU =   Number of pitch lags */
/*    SBUFL, SBUFH =   Start and end index of speech buffers */
/*    LBUFL, LBUFH =   Start and end index of LPF speech buffer */
/*   MINWIN, MAXWIN = Min and Max length of voicing (and analysis) windows
*/
/*    PWLEN, PWINH, PWINL = Length, upper and lower limits of pitch window
 */
/*    DVWINL, DVWINH = Default lower and upper limits of voicing window */
/*< 	INTEGER NF, AF, OSLEN, LTAU, SBUFL, SBUFH, LBUFL, LBUFH >*/
/*< 	INTEGER MINWIN, MAXWIN, PWLEN, PWINL, PWINH, DVWINL, DVWINH >*/
/*< 	PARAMETER (NF=4, AF=3, OSLEN=10, LTAU=60) >*/
/*< 	PARAMETER (SBUFL=(AF-2)*MAXFRM+1, SBUFH=NF*MAXFRM) >*/
/*< 	PARAMETER (LBUFL=(AF-2)*MAXFRM-MAXPIT+1, LBUFH=NF*MAXFRM) >*/
/*< 	PARAMETER (MINWIN=90, MAXWIN=156) >*/
/*< 	PARAMETER (PWLEN=MAXPIT+MAXWIN) >*/
/*< 	PARAMETER (PWINH=AF*MAXFRM, PWINL=PWINH-PWLEN+1) >*/
/*< 	PARAMETER (DVWINL=PWINH-PWLEN/2-MAXWIN/2+1) >*/
/*< 	PARAMETER (DVWINH=DVWINL+MAXWIN-1) >*/
/*       The tables TAU and BUFLIM, and the variable PRECOEF, are not */
/*       Fortran PARAMETER's, but they are initialized with DATA */
/*       statements, and never modified.  Thus, they need not have SAVE */
/*       statements for them to keep their values from one invocation to 
*/
/*       the next. */
/*< 	INTEGER TAU(LTAU) >*/
/*< 	INTEGER BUFLIM(4) >*/
/*< 	REAL PRECOEF >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, J, LANAL, HALF >*/
/*< 	INTEGER IPITCH, MINPTR, MAXPTR, MINTAU, MIDX >*/
/*< 	REAL IVRC(2), PHI(MAXORD,MAXORD), PSI(MAXORD) >*/
/*< 	REAL AMDF(LTAU), TEMP >*/
/*< 	INTEGER EWIN(2,AF) >*/
/*< 	REAL ABUF(MAXWIN) >*/
/*       Local state */
/*  Data Buffers */
/*    INBUF	Raw speech (with DC bias removed each frame) */
/*    PEBUF	Preemphasized speech */
/*    LPBUF	Low pass speech buffer */
/*    IVBUF	Inverse filtered speech */
/*    OSBUF	Indexes of onsets in speech buffers */
/*    VWIN	Voicing window indices */
/*    AWIN	Analysis window indices */
/*    EWIN	Energy window indices */
/*    VOIBUF	Voicing decisions on windows in VWIN */
/*    RMSBUF	RMS energy */
/*    RCBUF	Reflection Coefficients */

/*  Pitch is handled separately from the above parameters. */
/*  The following variables deal with pitch: */
/*    MIDX	Encoded initial pitch estimate for analysis frame */
/*    IPITCH	Initial pitch computed for frame AF (decoded from MIDX) */
/*    PITCH 	The encoded pitch value (index into TAU) for the present */
/* 		frame (delayed and smoothed by Dyptrack) */
/*< 	LOGICAL FIRST >*/
/*< 	REAL INBUF(SBUFL:SBUFH), PEBUF(SBUFL:SBUFH) >*/
/*< 	REAL LPBUF(LBUFL:LBUFH), IVBUF(PWINL:PWINH) >*/
/*< 	REAL BIAS >*/
/*< 	INTEGER OSBUF(OSLEN), OSPTR, OBOUND(AF) >*/
/*< 	INTEGER VWIN(2,AF), AWIN(2,AF), VOIBUF(2,0:AF) >*/
/*< 	REAL RMSBUF(AF), RCBUF(MAXORD, AF) >*/
/*< 	REAL ZPRE >*/
/*< 	SAVE FIRST >*/
/*< 	SAVE INBUF, PEBUF >*/
/*< 	SAVE LPBUF, IVBUF >*/
/*< 	SAVE BIAS >*/
/*< 	SAVE OSBUF, OSPTR, OBOUND >*/
/*< 	SAVE VWIN, AWIN, VOIBUF >*/
/*< 	SAVE RMSBUF, RCBUF >*/
/*< 	SAVE ZPRE >*/
/*< 	D >*/
    /* Parameter adjustments */
    if (speech) {
	--speech;
	}
    if (voice) {
	--voice;
	}
    if (rc) {
	--rc;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_pitdec;
	case 2: goto L_initanalys;
	}

/*< 	DATA BUFLIM / SBUFL, SBUFH, LBUFL, LBUFH / >*/
/*< 	DATA PRECOEF /.9375/ >*/
/*< 	DATA FIRST /.TRUE./ >*/
/* 	IF(LISTL.GE.3) THEN */
/* 	   WRITE(FDEBUG,900) NFRAME */
/* 900	   FORMAT(1X,//,65(2H- ),//,' ANALYSIS DATA -- FRAME',I6/) */
/* 	END IF */
/*   Calculations are done on future frame due to requirements */
/*   of the pitch tracker.  Delay RMS and RC's 2 frames to give */
/*   current frame parameters on return. */
/*   Update all buffers */
/*< 	IF (FIRST) THEN >*/
    if (first) {
/*< 	   CALL INITANALYS () >*/
	initanalys_();
/*< 	   FIRST = .FALSE. >*/
	first = FALSE_;
/*< 	END IF >*/
    }
/*< 	DO I = SBUFL, SBUFH-LFRAME >*/
    i__1 = 720 - contrl_1.lframe;
    for (i__ = 181; i__ <= i__1; ++i__) {
/*< 	   INBUF(I) = INBUF(LFRAME+I) >*/
	inbuf[i__ - 181] = inbuf[contrl_1.lframe + i__ - 181];
/*< 	   PEBUF(I) = PEBUF(LFRAME+I) >*/
	pebuf[i__ - 181] = pebuf[contrl_1.lframe + i__ - 181];
/*< 	END DO >*/
    }
/*< 	DO I = PWINL,PWINH-LFRAME >*/
    i__1 = 540 - contrl_1.lframe;
    for (i__ = 229; i__ <= i__1; ++i__) {
/*< 	   IVBUF(I) = IVBUF(LFRAME+I) >*/
	ivbuf[i__ - 229] = ivbuf[contrl_1.lframe + i__ - 229];
/*< 	END DO >*/
    }
/*< 	DO I = LBUFL,LBUFH-LFRAME >*/
    i__1 = 720 - contrl_1.lframe;
    for (i__ = 25; i__ <= i__1; ++i__) {
/*< 	   LPBUF(I) = LPBUF(LFRAME+I) >*/
	lpbuf[i__ - 25] = lpbuf[contrl_1.lframe + i__ - 25];
/*< 	END DO >*/
    }
/*< 	J=1 >*/
    j = 1;
/*< 	DO I = 1, OSPTR-1 >*/
    i__1 = osptr - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   IF (OSBUF(I) .GT. LFRAME) THEN >*/
	if (osbuf[i__ - 1] > contrl_1.lframe) {
/*< 	      OSBUF(J)=OSBUF(I)-LFRAME >*/
	    osbuf[j - 1] = osbuf[i__ - 1] - contrl_1.lframe;
/*< 	      J=J+1 >*/
	    ++j;
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*< 	OSPTR=J >*/
    osptr = j;
/*< 	VOIBUF(1,0) = VOIBUF(1,1) >*/
    voibuf[0] = voibuf[2];
/*< 	VOIBUF(2,0) = VOIBUF(2,1) >*/
    voibuf[1] = voibuf[3];
/*< 	DO I = 1, AF-1 >*/
    for (i__ = 1; i__ <= 2; ++i__) {
/*< 	   VWIN(1,I) = VWIN(1,I+1) - LFRAME >*/
	vwin[(i__ << 1) - 2] = vwin[(i__ + 1 << 1) - 2] - contrl_1.lframe;
/*< 	   VWIN(2,I) = VWIN(2,I+1) - LFRAME >*/
	vwin[(i__ << 1) - 1] = vwin[(i__ + 1 << 1) - 1] - contrl_1.lframe;
/*< 	   AWIN(1,I) = AWIN(1,I+1) - LFRAME >*/
	awin[(i__ << 1) - 2] = awin[(i__ + 1 << 1) - 2] - contrl_1.lframe;
/*< 	   AWIN(2,I) = AWIN(2,I+1) - LFRAME >*/
	awin[(i__ << 1) - 1] = awin[(i__ + 1 << 1) - 1] - contrl_1.lframe;
/*       EWIN(*,J) is unused for J .NE. AF, so the following shift is */
/*       unnecessary.  It also causes uninitialized memory to be read. */
/*< 	   EWIN(1,I) = EWIN(1,I+1) - LFRAME >*/
/*< 	   EWIN(2,I) = EWIN(2,I+1) - LFRAME >*/
/*< 	   OBOUND(I) = OBOUND(I+1) >*/
	obound[i__ - 1] = obound[i__];
/*< 	   VOIBUF(1,I) = VOIBUF(1,I+1) >*/
	voibuf[i__ * 2] = voibuf[(i__ + 1) * 2];
/*< 	   VOIBUF(2,I) = VOIBUF(2,I+1) >*/
	voibuf[(i__ << 1) + 1] = voibuf[(i__ + 1 << 1) + 1];
/*< 	   RMSBUF(I) = RMSBUF(I+1) >*/
	rmsbuf[i__ - 1] = rmsbuf[i__];
/*< 	   DO J = 1, ORDER >*/
	i__1 = contrl_1.order;
	for (j = 1; j <= i__1; ++j) {
/*< 	      RCBUF(J,I) = RCBUF(J,I+1) >*/
	    rcbuf[j + i__ * 10 - 11] = rcbuf[j + (i__ + 1) * 10 - 11];
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*   Copy input speech, scale to sign+12 bit integers */
/*   Remove long term DC bias. */
/*       If the average value in the frame was over 1/4096 (after current 
*/
/*       BIAS correction), then subtract that much more from samples in */
/*       next frame.  If the average value in the frame was under */
/*       -1/4096, add 1/4096 more to samples in next frame.  In all other 
*/
/*       cases, keep BIAS the same. */
/*< 	TEMP = 0 >*/
    temp = 0.f;
/*< 	DO I = 1,LFRAME >*/
    i__1 = contrl_1.lframe;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   INBUF(SBUFH-LFRAME+I) = SPEECH(I)*4096. - BIAS >*/
	inbuf[720 - contrl_1.lframe + i__ - 181] = speech[i__] * 4096.f - 
		bias;
/*< 	   TEMP = TEMP + INBUF(SBUFH-LFRAME+I) >*/
	temp += inbuf[720 - contrl_1.lframe + i__ - 181];
/*< 	END DO >*/
    }
/*< 	IF( TEMP.GT. LFRAME ) BIAS = BIAS + 1 >*/
    if (temp > (real) contrl_1.lframe) {
	bias += 1;
    }
/*< 	IF( TEMP.LT.-LFRAME ) BIAS = BIAS - 1 >*/
    if (temp < (real) (-contrl_1.lframe)) {
	bias += -1;
    }
/*   Place Voicing Window */
/*< 	I = SBUFH + 1 - LFRAME >*/
    i__ = 721 - contrl_1.lframe;
/*< 	CALL PREEMP(INBUF(I), PEBUF(I), LFRAME, PRECOEF, ZPRE) >*/
    preemp_(&inbuf[i__ - 181], &pebuf[i__ - 181], &contrl_1.lframe, &precoef, 
	    &zpre);
/*< 	C >*/
    onset_(pebuf, osbuf, &osptr, &c__10, &c__181, &c__720, &contrl_1.lframe);

/*       MAXOSP is just a debugging variable. */

/* 	MAXOSP = MAX( MAXOSP, OSPTR ) */

/*< 	C >*/
    placev_(osbuf, &osptr, &c__10, &obound[2], vwin, &c__3, &contrl_1.lframe, 
	    &c__90, &c__156, &c__307, &c__462);
/*        The Pitch Extraction algorithm estimates the pitch for a frame 
*/
/*   of speech by locating the minimum of the average magnitude difference
 */
/*   function (AMDF).  The AMDF operates on low-pass, inverse filtered */
/*   speech.  (The low-pass filter is an 800 Hz, 19 tap, equiripple, FIR 
*/
/*   filter and the inverse filter is a 2nd-order LPC filter.)  The pitch 
*/
/*   estimate is later refined by dynamic programming (DYPTRK).  However, 
*/
/*   since some of DYPTRK's parameters are a function of the voicing */
/*  decisions, a voicing decision must precede the final pitch estimation.
*/
/*   See subroutines LPFILT, IVFILT, and TBDM. */
/*       LPFILT reads indices LBUFH-LFRAME-29 = 511 through LBUFH = 720 */
/*       of INBUF, and writes indices LBUFH+1-LFRAME = 541 through LBUFH 
*/
/*       = 720 of LPBUF. */
/*< 	C >*/
    lpfilt_(&inbuf[228], &lpbuf[384], &c__312, &contrl_1.lframe);
/*       IVFILT reads indices (PWINH-LFRAME-7) = 353 through PWINH = 540 
*/
/*       of LPBUF, and writes indices (PWINH-LFRAME+1) = 361 through */
/*       PWINH = 540 of IVBUF. */
/*< 	CALL IVFILT( LPBUF(PWINL), IVBUF(PWINL), PWLEN, LFRAME, IVRC ) >*/
    ivfilt_(&lpbuf[204], ivbuf, &c__312, &contrl_1.lframe, ivrc);
/*       TBDM reads indices PWINL = 229 through */
/*       (PWINL-1)+MAXWIN+(TAU(LTAU)-TAU(1))/2 = 452 of IVBUF, and writes 
*/
/*       indices 1 through LTAU = 60 of AMDF. */
/*< 	C >*/
    tbdm_(ivbuf, &c__156, tau, &c__60, amdf, &minptr, &maxptr, &mintau);
/*        Voicing decisions are made for each half frame of input speech. 
*/
/*   An initial voicing classification is made for each half of the */
/*   analysis frame, and the voicing decisions for the present frame */
/*   are finalized.  See subroutine VOICIN. */
/*        The voicing detector (VOICIN) classifies the input signal as */
/*   unvoiced (including silence) or voiced using the AMDF windowed */
/*   maximum-to-minimum ratio, the zero crossing rate, energy measures, */
/*   reflection coefficients, and prediction gains. */
/*        The pitch and voicing rules apply smoothing and isolated */
/*   corrections to the pitch and voicing estimates and, in the process, 
*/
/*   introduce two frames of delay into the corrected pitch estimates and 
*/
/*   voicing decisions. */
/*< 	DO HALF = 1,2 >*/
    for (half = 1; half <= 2; ++half) {
/*< 	  >*/
	voicin_(&vwin[4], inbuf, lpbuf, buflim, &half, &amdf[minptr - 1], &
		amdf[maxptr - 1], &mintau, ivrc, obound, voibuf, &c__3);
/*< 	END DO >*/
    }
/*   Find the minimum cost pitch decision over several frames */
/*   given the current voicing decision and the AMDF array */
/*< 	CALL DYPTRK( AMDF, LTAU, MINPTR, VOIBUF(2,AF), PITCH, MIDX ) >*/
    dyptrk_(amdf, &c__60, &minptr, &voibuf[7], pitch, &midx);
/*< 	IPITCH = TAU(MIDX) >*/
    ipitch = tau[midx - 1];
/*   Place spectrum analysis and energy windows */
/*< 	C >*/
    placea_(&ipitch, voibuf, &obound[2], &c__3, vwin, awin, ewin, &
	    contrl_1.lframe, &c__156);
/*  Remove short term DC bias over the analysis window, Put result in ABUF
*/
/*< 	LANAL = AWIN(2,AF) + 1 - AWIN(1,AF) >*/
    lanal = awin[5] + 1 - awin[4];
/*< 	CALL DCBIAS( LANAL, PEBUF(AWIN(1,AF)), ABUF ) >*/
    dcbias_(&lanal, &pebuf[awin[4] - 181], abuf);
/*       ABUF(1:LANAL) is now defined.  It is equal to */
/*       PEBUF(AWIN(1,AF):AWIN(2,AF)) corrected for short term DC bias. */
/* 	IF ((AWIN(1,AF) .LT. SBUFL) .OR. (SBUFH .LT. AWIN(2,AF))) THEN */
/* 	   WRITE (2,999) AWIN(1,AF), AWIN(2,AF), SBUFL, SBUFH */
/* 999	   FORMAT(1X,'AWIN (',I4,':',I4, */
/*     1            ') goes outside of PEBUFs range (',I4,':',I4,')') */
/* 	   STOP */
/* 	END IF */
/* 	IF (MAXWIN .LT. LANAL) THEN */
/* 	   WRITE (2,998) LANAL, MAXWIN */
/* 998	   FORMAT(1X,'LANAL (',I4, */
/*     1            ') goes outside of ABUFs range (1:',I4,')') */
/* 	   STOP */
/* 	END IF */
/*   Compute RMS over integer number of pitch periods within the */
/*   analysis window. */
/*   Note that in a hardware implementation this computation may be */
/*   simplified by using diagonal elements of PHI computed by MLOAD. */
/* 	IF (     (EWIN(1,AF) .LT. AWIN(1,AF)) */
/*     1      .OR. (AWIN(2,AF) .LT. EWIN(2,AF))) THEN */
/* 	   WRITE (2,997) EWIN(1,AF), EWIN(2,AF), AWIN(1,AF), AWIN(2,AF) */
/* 997	   FORMAT(1X,'EWIN (',I4,':',I4, */
/*     1            ') goes outside of AWINs range (',I4,':',I4,')') */
/* 	   STOP */
/* 	END IF */
/*< 	C >*/
    i__1 = ewin[5] - ewin[4] + 1;
    energy_(&i__1, &abuf[ewin[4] - awin[4]], &rmsbuf[2]);
/*   Matrix load and invert, check RC's for stability */
/* 	IF (LANAL .LT. ORDER) THEN */
/* 	   WRITE (2,996) LANAL, ORDER */
/* 996	   FORMAT(1X,'MLOAD will read outside of ABUFs defined range ', */
/*     1            'of (1:LANAL=',I4,') because LANAL is less than', */
/*     1            ' ORDER=',I4) */
/* 	   STOP */
/* 	END IF */
/*< 	CALL MLOAD( ORDER, 1, LANAL, ABUF, PHI, PSI ) >*/
    mload_(&contrl_1.order, &c__1, &lanal, abuf, phi, psi);
/*< 	CALL INVERT( ORDER, PHI, PSI, RCBUF(1,AF) ) >*/
    invert_(&contrl_1.order, phi, psi, &rcbuf[20]);
/*< 	CALL RCCHK( ORDER, RCBUF(1,AF-1), RCBUF(1,AF) ) >*/
    rcchk_(&contrl_1.order, &rcbuf[10], &rcbuf[20]);
/*   Set return parameters */
/*< 	VOICE(1) = VOIBUF(1,AF-2) >*/
    voice[1] = voibuf[2];
/*< 	VOICE(2) = VOIBUF(2,AF-2) >*/
    voice[2] = voibuf[3];
/*< 	RMS = RMSBUF(AF-2) >*/
    *rms = rmsbuf[0];
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   RC(I) = RCBUF(I,AF-2) >*/
	rc[i__] = rcbuf[i__ - 1];
/*< 	END DO >*/
    }
/*   Print out test data */
/* 	IF(LISTL.GE.3) THEN */
/* 	   IF(LISTL.GE.4) THEN */
/* 	      IF(LISTL.GE.6) THEN */
/* 	         WRITE(FDEBUG,980) 'INBUF:',INBUF */
/* 	         WRITE(FDEBUG,980) 'LPBUF:',LPBUF */
/* 	         WRITE(FDEBUG,980) 'IVBUF:',IVBUF */
/* 	         WRITE(FDEBUG,980) 'PEBUF:',PEBUF */
/* 	      END IF */
/* 	      WRITE(FDEBUG,980) 'AMDF:',AMDF */
/* 	   END IF */
/* 	   IF(OSPTR.GT.1) WRITE(FDEBUG,970) */
/*     1    'OSBUF Onset Locations:', (OSBUF(I),I=1,OSPTR-1) */
/* 	   IF(LISTL.GE.4) THEN */
/* 	      WRITE(FDEBUG,980) 'PHI Matrix Values:', */
/*     1       ((PHI(I,J),J=1,ORDER),I=1,ORDER) */
/* 	      WRITE(FDEBUG,980) 'PSI Vector Values:',PSI */
/* 970	      FORMAT(1X,A,100(/1X,20I6)) */
/* 980	      FORMAT(1X,A,100(/1X,10F12.1)) */
/* 	   END IF */
/* 	   WRITE(FDEBUG,990) */
/* 990	   FORMAT('  FRAME   AWIN     EWIN   BIAS',T34, */
/*     1    'V/UV  Pitch RMS',T54, */
/*     1    'RC1     RC2     RC3     RC4     RC5     ', */
/*     1    'RC6     RC7     RC8     RC9    RC10') */
/* 	   WRITE(FDEBUG,992) NFRAME, AWIN(1,AF), AWIN(2,AF), */
/*     1    EWIN(1,AF), EWIN(2,AF), BIAS, */
/*     1    VOIBUF(2,AF), IPITCH, RMSBUF(AF), (RCBUF(I,AF),I=1,ORDER) */
/* 992	   FORMAT(1X,I6,2I4,1X,2I4,F6.1,T34,I2,I8,F6.0,T50,10F8.3) */
/* 	END IF */
/*< 	RETURN >*/
    return 0;
/* ******************************************************************* */
/*   Decode pitch index (PITCH) to pitch period (PTAU) */
/* ******************************************************************* */
/*< 	ENTRY PITDEC( PITCH, PTAU ) >*/

L_pitdec:
/*< 	IF (PITCH .GE. 1 .AND. PITCH .LE. LTAU) THEN >*/
    if (*pitch >= 1 && *pitch <= 60) {
/*< 	   PTAU = TAU(PITCH) >*/
	*ptau = tau[*pitch - 1];
/*< 	ELSE >*/
    } else {
/*< 	   PTAU = 0 >*/
	*ptau = 0;
/*< 	END IF >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITANALYS () >*/

L_initanalys:
/*       Set FIRST to .FALSE., so that just in case someone calls */
/*       INITANALYS before calling ANALYS for the first time, then this */
/*       entry will not be called by ANALYS itself. */
/*< 	FIRST = .FALSE. >*/
    first = FALSE_;
/*       Initialize local state in all subroutines that have local state. 
*/
/*< 	CALL INITONSET () >*/
    initonset_();
/*< 	CALL INITVOICIN () >*/
    initvoicin_();
/*< 	CALL INITDYPTRK () >*/
    initdyptrk_();
/*       INBUF, PEBUF, LPBUF, and IVBUF were not initialized in the */
/*       original code.  Initial values of 0 appear to be safe. */
/*< 	DO I = SBUFL,SBUFH >*/
    for (i__ = 181; i__ <= 720; ++i__) {
/*< 	   INBUF(I) = 0. >*/
	inbuf[i__ - 181] = 0.f;
/*< 	   PEBUF(I) = 0. >*/
	pebuf[i__ - 181] = 0.f;
/*< 	END DO >*/
    }
/*< 	DO I = LBUFL,LBUFH >*/
    for (i__ = 25; i__ <= 720; ++i__) {
/*< 	   LPBUF(I) = 0. >*/
	lpbuf[i__ - 25] = 0.f;
/*< 	END DO >*/
    }
/*< 	DO I = PWINL,PWINH >*/
    for (i__ = 229; i__ <= 540; ++i__) {
/*< 	   IVBUF(I) = 0. >*/
	ivbuf[i__ - 229] = 0.f;
/*< 	END DO >*/
    }
/*< 	BIAS = 0 >*/
    bias = 0.f;
/*       Although OSBUF is saved from one invocation to the next, it need 
*/
/*       not have an initial defined value, because OSPTR is initialized 
*/
/*       to 1, and only entries 1 through OSPTR-1 may be read without */
/*       writing them first. */
/*< 	OSPTR = 1 >*/
    osptr = 1;
/*< 	DO I = 1,AF >*/
    for (i__ = 1; i__ <= 3; ++i__) {
/*< 	   OBOUND(I) = 0 >*/
	obound[i__ - 1] = 0;
/*< 	END DO >*/
    }
/*       Should other indices of VWIN and AWIN be initialized, or is this 
*/
/*       unnecessary?  If unnecessary, why? */
/*< 	VWIN(1,AF) = DVWINL >*/
    vwin[4] = 307;
/*< 	VWIN(2,AF) = DVWINH >*/
    vwin[5] = 462;
/*< 	AWIN(1,AF) = DVWINL >*/
    awin[4] = 307;
/*< 	AWIN(2,AF) = DVWINH >*/
    awin[5] = 462;
/*       VOIBUF was not initialized in the original code.  I believe */
/*       initializing it to all 0's is a safe decision, given that its */
/*       contents are always 0/1 truth values representing the decision */
/*       of whether a half-frame was voiced or not. */
/*< 	DO I = 1,2 >*/
    for (i__ = 1; i__ <= 2; ++i__) {
/*< 	   DO J = 0,AF >*/
	for (j = 0; j <= 3; ++j) {
/*< 	      VOIBUF(I,J) = 0 >*/
	    voibuf[i__ + (j << 1) - 1] = 0;
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*       RMSBUF and RCBUF were also not initialized in the original code. 
*/
/*       Again, initial values of 0 appear to be safe. */
/*< 	DO I = 1,AF >*/
    for (i__ = 1; i__ <= 3; ++i__) {
/*< 	   RMSBUF(I) = 0. >*/
	rmsbuf[i__ - 1] = 0.f;
/*< 	   DO J = 1,ORDER >*/
	i__1 = contrl_1.order;
	for (j = 1; j <= i__1; ++j) {
/*< 	      RCBUF(J,I) = 0. >*/
	    rcbuf[j + i__ * 10 - 11] = 0.f;
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*< 	ZPRE = 0. >*/
    zpre = 0.f;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* analys_ */

/* Subroutine */ int analys_(real *speech, integer *voice, integer *pitch, 
	real *rms, real *rc)
{
    return analys_0_(0, speech, voice, pitch, rms, rc, (integer *)0);
    }

/* Subroutine */ int pitdec_(integer *pitch, integer *ptau)
{
    return analys_0_(1, (real *)0, (integer *)0, pitch, (real *)0, (real *)0, 
	    ptau);
    }

/* Subroutine */ int initanalys_(void)
{
    return analys_0_(2, (real *)0, (integer *)0, (integer *)0, (real *)0, (
	    real *)0, (integer *)0);
    }

