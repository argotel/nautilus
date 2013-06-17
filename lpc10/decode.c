/*
 * decode.c
 *
 * SCCS ID: @(#)decode.c 1.3 96/05/26
 */

#include "f2c.h"

extern int decode_(integer *ipitv, integer *irms, integer *irc, integer *voice, integer *pitch, real *rms, real *rc);
extern int initdecode_(void);
/* comlen contrl_ 12 */
/*:ref: ham84_ 14 3 4 4 4 */
/*:ref: median_ 4 3 4 4 4 */

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

/* Common Block Declarations */

extern struct {
    integer order, lframe;
    logical corrp;
} contrl_;

#define contrl_1 contrl_

/* Table of constant values */

static integer c__2 = 2;

/* ***************************************************************** */

/* 	DECODE Version 54 */

/* $Log: decode.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.5  1996/05/23  20:06:03  jaf */
/* Assigned PITCH a "default" value on the first call, since otherwise it */
/* would be left uninitialized. */
/* Revision 1.4  1996/03/26  19:35:18  jaf */
/* Commented out trace statements. */

/* Revision 1.3  1996/03/21  21:10:50  jaf */
/* Added entry INITDECODE to reinitialize the local state of subroutine */
/* DECODE. */

/* Revision 1.2  1996/03/21  21:04:50  jaf */
/* Determined which local variables should be saved from one invocation */
/* to the next, and guessed initial values for some that should have been */
/* saved, but weren't given initial values.  Many of the arrays are */
/* "constants", and many local variables are only used if the "global" */
/* variable CORRP is .TRUE. */

/* Added comments explaining which indices of array arguments are read or */
/* written. */

/* Revision 1.1  1996/02/12 03:21:10  jaf */
/* Initial revision */


/* ***************************************************************** */

/*   This subroutine provides error correction and decoding */
/*   for all LPC parameters */

/* Input: */
/*  IPITV  - Index value of pitch */
/*  IRMS   - Coded Energy */
/*  CORRP  - Error correction: */
/*    If FALSE, parameters are decoded directly with no delay.  If TRUE, */
/*    most important parameter bits are protected by Hamming code and */
/*    median smoothed.  This requires an additional frame of delay. */
/* Input/Output: */
/*  IRC    - Coded Reflection Coefficients */
/*           Indices 1 through ORDER always read, then written. */
/* Output: */
/*  VOICE  - Half frame voicing decisions */
/*           Indices 1 through 2 written. */
/*  PITCH  - Decoded pitch */
/*  RMS    - Energy */
/*  RC     - Reflection coefficients */
/*           Indices 1 through ORDER written. */

/*  NOTE: Zero RC's should be done more directly, but this would affect */
/*   coded parameter printout. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITDECODE. */

/*< 	S >*/
/* Subroutine */ int decode_0_(int n__, integer *ipitv, integer *irms, 
	integer *irc, integer *voice, integer *pitch, real *rms, real *rc)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer ethrs = 2048;
    static integer ethrs1 = 128;
    static integer ethrs2 = 1024;
    static integer ethrs3 = 2048;
    static integer ivtab[32] = { 24960,24960,24960,24960,25480,25480,25483,
	    25480,16640,1560,1560,1560,16640,1816,1563,1560,24960,24960,24859,
	    24856,26001,25881,25915,25913,1560,1560,7800,3640,1561,1561,3643,
	    3641 };
    static real corth[32]	/* was [4][8] */ = { 32767.f,10.f,5.f,0.f,
	    32767.f,8.f,4.f,0.f,32.f,6.4f,3.2f,0.f,32.f,6.4f,3.2f,0.f,32.f,
	    11.2f,6.4f,0.f,32.f,11.2f,6.4f,0.f,16.f,5.6f,3.2f,0.f,16.f,5.6f,
	    3.2f,0.f };
    static integer detau[128] = { 0,0,0,3,0,3,3,31,0,3,3,21,3,3,29,30,0,3,3,
	    20,3,25,27,26,3,23,58,22,3,24,28,3,0,3,3,3,3,39,33,32,3,37,35,36,
	    3,38,34,3,3,42,46,44,50,40,48,3,54,3,56,3,52,3,3,1,0,3,3,108,3,78,
	    100,104,3,84,92,88,156,80,96,3,3,74,70,72,66,76,68,3,62,3,60,3,64,
	    3,3,1,3,116,132,112,148,152,3,3,140,3,136,3,144,3,3,1,124,120,128,
	    3,3,3,3,1,3,3,3,1,3,1,1,1 };
    static integer rmst[64] = { 1024,936,856,784,718,656,600,550,502,460,420,
	    384,352,328,294,270,246,226,206,188,172,158,144,132,120,110,102,
	    92,84,78,70,64,60,54,50,46,42,38,34,32,30,26,24,22,20,18,17,16,15,
	    14,13,12,11,10,9,8,7,6,5,4,3,2,1,0 };
    static integer detab7[32] = { 4,11,18,25,32,39,46,53,60,66,72,77,82,87,92,
	    96,101,104,108,111,114,115,117,119,121,122,123,124,125,126,127,
	    127 };
    static real descl[8] = { .6953f,.625f,.5781f,.5469f,.5312f,.5391f,.4688f,
	    .3828f };
    static integer ivp2h = 0;
    static integer deadd[8] = { 1152,-2816,-1536,-3584,-1280,-2432,768,-1920 }
	    ;
    static integer qb[8] = { 511,511,1023,1023,1023,1023,2047,4095 };
    static integer nbit[10] = { 8,8,5,5,4,4,4,4,3,2 };
    static integer zrc[10] = { 0,0,0,0,0,3,0,2,0,0 };
    static integer bit[5] = { 2,4,8,16,32 };
    static integer iovoic = 0;
    static integer iavgp = 60;
    static integer iptold = 60;
    static integer erate = 0;
    static integer drc[30]	/* was [3][10] */ = { 0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
    static integer dpit[3] = { 0,0,0 };
    static integer drms[3] = { 0,0,0 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer pow_ii(integer *, integer *);

    /* Local variables */
    extern /* Subroutine */ int ham84_(integer *, integer *, integer *);
    integer ipit, iout, i__, j, icorf, index, ivoic, ixcor, i1, i2, i4;
    extern integer median_(integer *, integer *, integer *);
    integer ishift, errcnt, lsb;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INCLUDE 'contrl.fh' >*/
/* $Log: decode.c,v $
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
/*< 	INTEGER IPITV, IRMS, IRC(MAXORD) >*/
/*       Arguments */
/* $Log: decode.c,v $
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
/*       Function return value definitions */
/*< 	INTEGER MEDIAN >*/

/*       Parameters/constants */

/*       The variables below that are not Fortran PARAMETER's are */
/*       initialized with DATA statements, and then never modified. */
/*       The following are used regardless of CORRP's value. */

/*       DETAU, NBIT, QB, DEADD, DETAB7, RMST, DESCL */

/*       The following are used only if CORRP is .TRUE. */

/*       ETHRS, ETHRS1, ETHRS2, ETHRS3, IVTAB, BIT, CORTH, ZRC */
/*< 	INTEGER FUT, PRES, PAST >*/
/*< 	PARAMETER( FUT=1, PRES=2, PAST=3 ) >*/
/*< 	INTEGER ETHRS, ETHRS1, ETHRS2, ETHRS3 >*/
/*< 	INTEGER IVTAB(32), DETAU(128), BIT(5), NBIT(10) >*/
/*< 	INTEGER QB(8), DEADD(8), DETAB7(32), RMST(64) >*/
/*< 	REAL DESCL(8), CORTH(4,8) >*/
/*< 	INTEGER ZRC(MAXORD) >*/

/*       Local variables that need not be saved */

/*       The following are used regardless of CORRP's value */
/*< 	INTEGER I, J, I1, I2, I4, ISHIFT >*/
/*       The following are used only if CORRP is .TRUE. */
/*< 	INTEGER IVOIC >*/
/*< 	INTEGER ICORF, INDEX, IOUT >*/
/*< 	INTEGER IPIT, IXCOR, LSB >*/
/*< 	INTEGER ERRCNT >*/

/*       Local state */

/*       The following are used regardless of CORRP's value */
/*< 	INTEGER IPTOLD >*/
/*       The following are used only if CORRP is .TRUE. */
/*< 	LOGICAL FIRST >*/
/*< 	INTEGER IVP2H, IOVOIC >*/
/*< 	INTEGER IAVGP >*/
/*< 	INTEGER ERATE >*/
/*< 	INTEGER DRC(3,MAXORD), DPIT(3), DRMS(3) >*/
/*< 	SAVE IPTOLD >*/
/*< 	SAVE FIRST >*/
/*< 	SAVE IVP2H, IOVOIC >*/
/*< 	SAVE IAVGP >*/
/*< 	SAVE ERATE >*/
/*< 	SAVE DRC, DPIT, DRMS >*/
/*       I am guessing the initial values for IVP2H, IOVOIC, DRC, DPIT, */
/*       and DRMS.  They should be checked to see if they are reasonable. 
*/
/*       I'm also guessing for ERATE, but I think 0 is the right initial 
*/
/*       value. */
/*< 	DATA FIRST /.TRUE./ >*/
    /* Parameter adjustments */
    if (irc) {
	--irc;
	}
    if (voice) {
	--voice;
	}
    if (rc) {
	--rc;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initdecode;
	}

/*< 	DATA IVP2H /0/, IOVOIC /0/ >*/
/*< 	DATA IAVGP /60/, IPTOLD /60/ >*/
/*< 	DATA ERATE /0/ >*/
/*< 	DATA DRC /30*0/, DPIT /3*0/, DRMS /3*0/ >*/
/* DATA statements for "constants" defined above. */
/*< 	DATA ETHRS,ETHRS1,ETHRS2,ETHRS3/O'4000',O'200',O'2000',O'4000'/ >*/
/*< 	D >*/
/*< 	D >*/
/*< 	D >*/
/*< 	D >*/
/*< 	D >*/
/*< 	DATA DESCL /.6953,.6250,.5781,.5469,.5312,.5391,.4688,.3828/ >*/
/*< 	DATA DEADD /1152,-2816,-1536,-3584,-1280,-2432,768,-1920/ >*/
/*< 	DATA QB /511,511,1023,1023,1023,1023,2047,4095/ >*/
/*< 	DATA NBIT /8,8,5,5,4,4,4,4,3,2/ >*/
/*< 	DATA ZRC /4*0,0,3,0,2,0,0/ >*/
/*< 	DATA BIT /2,4,8,16,32/ >*/
/* 	IF (LISTL.GE.3) WRITE(FDEBUG,800) IPITV,IRMS,(IRC(J),J=1,ORDER) */
/* 800	FORMAT(1X,' <<ERRCOR IN>>',T32,6X,I6,I5,T50,10I8) */
/*  If no error correction, do pitch and voicing then jump to decode */
/*< 	I4 = DETAU(IPITV+1) >*/
    i4 = detau[*ipitv];
/*< 	IF (.NOT.CORRP) THEN >*/
    if (! contrl_1.corrp) {
/*< 	   VOICE(1) = 1 >*/
	voice[1] = 1;
/*< 	   VOICE(2) = 1 >*/
	voice[2] = 1;
/*< 	   IF (IPITV.LE.1) VOICE(1) = 0 >*/
	if (*ipitv <= 1) {
	    voice[1] = 0;
	}
/*< 	   IF ((IPITV.EQ.0).OR.(IPITV.EQ.2)) VOICE(2) = 0 >*/
	if (*ipitv == 0 || *ipitv == 2) {
	    voice[2] = 0;
	}
/*< 	   PITCH = I4 >*/
	*pitch = i4;
/*< 	   IF (PITCH.LE.4) PITCH = IPTOLD >*/
	if (*pitch <= 4) {
	    *pitch = iptold;
	}
/*< 	   IF ((VOICE(1).EQ.1).AND.(VOICE(2).EQ.1)) IPTOLD = PITCH >*/
	if (voice[1] == 1 && voice[2] == 1) {
	    iptold = *pitch;
	}
/*< 	   IF (VOICE(1).NE.VOICE(2)) PITCH = IPTOLD >*/
	if (voice[1] != voice[2]) {
	    *pitch = iptold;
	}
/*< 	   GOTO 900 >*/
	goto L900;
/*< 	END IF >*/
    }
/*  Do error correction pitch and voicing */
/*< 	IF (I4.GT.4) THEN >*/
    if (i4 > 4) {
/*< 	   DPIT(FUT) = I4 >*/
	dpit[0] = i4;
/*< 	   IVOIC = 2 >*/
	ivoic = 2;
/*< 	   IAVGP = (15*IAVGP+I4+8)/16 >*/
	iavgp = (iavgp * 15 + i4 + 8) / 16;
/*< 	ELSE >*/
    } else {
/*< 	   IVOIC = I4 >*/
	ivoic = i4;
/*< 	   DPIT(FUT) = IAVGP >*/
	dpit[0] = iavgp;
/*< 	END IF >*/
    }
/*< 	DRMS(FUT) = IRMS >*/
    drms[0] = *irms;
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   DRC(FUT,I) = IRC(I) >*/
	drc[i__ * 3 - 3] = irc[i__];
/*< 	END DO >*/
    }
/*  Determine index to IVTAB from V/UV decision */
/*  If error rate is high then use alternate table */
/*< 	INDEX = 16*IVP2H + 4*IOVOIC + IVOIC + 1 >*/
    index = (ivp2h << 4) + (iovoic << 2) + ivoic + 1;
/*< 	I1 = IVTAB(INDEX) >*/
    i1 = ivtab[index - 1];
/*< 	IPIT = AND(I1,3) >*/
    ipit = i1 & 3;
/*< 	ICORF = I1/8 >*/
    icorf = i1 / 8;
/*< 	IF (ERATE.LT.ETHRS) ICORF = ICORF/64 >*/
    if (erate < ethrs) {
	icorf /= 64;
    }
/*  Determine error rate:  4=high    1=low */
/*< 	IXCOR = 4 >*/
    ixcor = 4;
/*< 	IF (ERATE.LT.ETHRS3) IXCOR = 3 >*/
    if (erate < ethrs3) {
	ixcor = 3;
    }
/*< 	IF (ERATE.LT.ETHRS2) IXCOR = 2 >*/
    if (erate < ethrs2) {
	ixcor = 2;
    }
/*< 	IF (ERATE.LT.ETHRS1) IXCOR = 1 >*/
    if (erate < ethrs1) {
	ixcor = 1;
    }
/*  Voice/unvoice decision determined from bits 0 and 1 of IVTAB */
/*< 	VOICE(1) = AND(ICORF/2,1) >*/
    voice[1] = icorf / 2 & 1;
/*< 	VOICE(2) = AND(ICORF,1) >*/
    voice[2] = icorf & 1;
/*  Skip decoding on first frame because present data not yet available */
/*< 	IF (FIRST) THEN >*/
    if (first) {
/*< 	   FIRST = .FALSE. >*/
	first = FALSE_;
/*            Assign PITCH a "default" value on the first call, since */
/*            otherwise it would be left uninitialized.  The two lines */
/*            below were copied from above, since it seemed like a */
/*            reasonable thing to do for the first call. */
/*<        PITCH = I4 >*/
        *pitch = i4;
/*<        IF (PITCH.LE.4) PITCH = IPTOLD >*/
        if (*pitch <=  4) {
            *pitch = iptold;
	}
/*< 	   GO TO 500 >*/
	goto L500;
/*< 	END IF >*/
    }
/*  If bit 4 of ICORF is set then correct RMS and RC(1) - RC(4). */
/*    Determine error rate and correct errors using a Hamming 8,4 code */
/*    during transition or unvoiced frame.  If IOUT is negative, */
/*    more than 1 error occurred, use previous frame's parameters. */
/*< 	IF (AND(ICORF,BIT(4)).NE.0) THEN >*/
    if ((icorf & bit[3]) != 0) {
/*< 	   ERRCNT = 0 >*/
	errcnt = 0;
/*< 	   LSB = AND(DRMS(PRES),1) >*/
	lsb = drms[1] & 1;
/*< 	   INDEX = DRC(PRES,8)*16 + DRMS(PRES)/2 >*/
	index = (drc[22] << 4) + drms[1] / 2;
/*< 	   CALL HAM84(INDEX,IOUT,ERRCNT) >*/
	ham84_(&index, &iout, &errcnt);
/*< 	   DRMS(PRES) = DRMS(PAST) >*/
	drms[1] = drms[2];
/*< 	   IF (IOUT.GE.0) DRMS(PRES) = IOUT*2 + LSB >*/
	if (iout >= 0) {
	    drms[1] = (iout << 1) + lsb;
	}
/*< 	   DO I = 1,4 >*/
	for (i__ = 1; i__ <= 4; ++i__) {
/*< 	      IF (I.EQ.1) THEN >*/
	    if (i__ == 1) {
/*< 	         I1  = ( AND(DRC(PRES,9),7)*2 + AND(DRC(PRES,10),1) ) >*/
		i1 = ((drc[25] & 7) << 1) + (drc[28] & 1);
/*< 	      ELSE >*/
	    } else {
/*< 	         I1  = AND(DRC(PRES,9-I),15) >*/
		i1 = drc[(9 - i__) * 3 - 2] & 15;
/*< 	      END IF >*/
	    }
/*< 	      I2 = AND(DRC(PRES,5-I),31) >*/
	    i2 = drc[(5 - i__) * 3 - 2] & 31;
/*< 	      LSB = AND(I2,1) >*/
	    lsb = i2 & 1;
/*< 	      INDEX = 16*I1 + I2/2 >*/
	    index = (i1 << 4) + i2 / 2;
/*< 	      CALL HAM84(INDEX,IOUT,ERRCNT) >*/
	    ham84_(&index, &iout, &errcnt);
/*< 	      IF (IOUT.GE.0) THEN >*/
	    if (iout >= 0) {
/*< 	         IOUT = IOUT*2+LSB >*/
		iout = (iout << 1) + lsb;
/*< 	         IF (AND(IOUT,16).EQ.16) IOUT = IOUT-32 >*/
		if ((iout & 16) == 16) {
		    iout += -32;
		}
/*< 	      ELSE >*/
	    } else {
/*< 	         IOUT = DRC(PAST,5-I) >*/
		iout = drc[(5 - i__) * 3 - 1];
/*< 	      END IF >*/
	    }
/*< 	      DRC(PRES,5-I) = IOUT >*/
	    drc[(5 - i__) * 3 - 2] = iout;
/*< 	   END DO >*/
	}
/*  Determine error rate */
/*< 	   ERATE = ERATE*.96875 + ERRCNT*102 >*/
	erate = (int) (erate * .96875f + errcnt * 102);
/* 	   IF (ERATE.NE.0 .AND. LISTL.GE.3) WRITE(FDEBUG,987) ERATE,ERRCNT
 */
/* 987	   FORMAT(' ERATE=',I6,'   ERRCNT=',I6) */
/*< 	END IF >*/
    }
/*  Get unsmoothed RMS, RC's, and PITCH */
/*< 	IRMS = DRMS(PRES) >*/
    *irms = drms[1];
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   IRC(I) = DRC(PRES,I) >*/
	irc[i__] = drc[i__ * 3 - 2];
/*< 	END DO >*/
    }
/*< 	IF (IPIT.EQ.1) DPIT(PRES) = DPIT(PAST) >*/
    if (ipit == 1) {
	dpit[1] = dpit[2];
    }
/*< 	IF (IPIT.EQ.3) DPIT(PRES) = DPIT(FUT) >*/
    if (ipit == 3) {
	dpit[1] = dpit[0];
    }
/*< 	PITCH = DPIT(PRES) >*/
    *pitch = dpit[1];
/*  If bit 2 of ICORF is set then smooth RMS and RC's, */
/*< 	IF (AND(ICORF,BIT(2)).NE.0) THEN >*/
    if ((icorf & bit[1]) != 0) {
/*< 	  >*/
	if ((i__1 = drms[1] - drms[0], (real) abs(i__1)) >= corth[ixcor + 3] 
		&& (i__2 = drms[1] - drms[2], (real) abs(i__2)) >= corth[
		ixcor + 3]) {
	    *irms = median_(&drms[2], &drms[1], drms);
	}
/*< 	   DO I = 1,6 >*/
	for (i__ = 1; i__ <= 6; ++i__) {
/*< 	  >*/
	    if ((i__1 = drc[i__ * 3 - 2] - drc[i__ * 3 - 3], (real) abs(i__1))
		     >= corth[ixcor + (i__ + 2 << 2) - 5] && (i__2 = drc[i__ *
		     3 - 2] - drc[i__ * 3 - 1], (real) abs(i__2)) >= corth[
		    ixcor + (i__ + 2 << 2) - 5]) {
		irc[i__] = median_(&drc[i__ * 3 - 1], &drc[i__ * 3 - 2], &drc[
			i__ * 3 - 3]);
	    }
/*< 	   END DO >*/
	}
/*< 	END IF >*/
    }
/*  If bit 3 of ICORF is set then smooth pitch */
/*< 	IF (AND(ICORF,BIT(3)).NE.0) THEN >*/
    if ((icorf & bit[2]) != 0) {
/*< 	  >*/
	if ((i__1 = dpit[1] - dpit[0], (real) abs(i__1)) >= corth[ixcor - 1] 
		&& (i__2 = dpit[1] - dpit[2], (real) abs(i__2)) >= corth[
		ixcor - 1]) {
	    *pitch = median_(&dpit[2], &dpit[1], dpit);
	}
/*< 	END IF >*/
    }
/*  If bit 5 of ICORF is set then RC(5) - RC(10) are loaded with */
/*  values so that after quantization bias is removed in decode */
/*  the values will be zero. */
/*< 500	IF (AND(ICORF,BIT(5)).NE.0) THEN >*/
L500:
    if ((icorf & bit[4]) != 0) {
/*< 	   DO I = 5,ORDER >*/
	i__1 = contrl_1.order;
	for (i__ = 5; i__ <= i__1; ++i__) {
/*< 	      IRC(I) = ZRC(I) >*/
	    irc[i__] = zrc[i__ - 1];
/*< 	   END DO >*/
	}
/*< 	END IF >*/
    }
/*  House keeping  - one frame delay */
/*< 	IOVOIC = IVOIC >*/
    iovoic = ivoic;
/*< 	IVP2H = VOICE(2) >*/
    ivp2h = voice[2];
/*< 	DPIT(PAST) = DPIT(PRES) >*/
    dpit[2] = dpit[1];
/*< 	DPIT(PRES) = DPIT(FUT) >*/
    dpit[1] = dpit[0];
/*< 	DRMS(PAST) = DRMS(PRES) >*/
    drms[2] = drms[1];
/*< 	DRMS(PRES) = DRMS(FUT) >*/
    drms[1] = drms[0];
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   DRC(PAST,I) = DRC(PRES,I) >*/
	drc[i__ * 3 - 1] = drc[i__ * 3 - 2];
/*< 	   DRC(PRES,I) = DRC(FUT,I) >*/
	drc[i__ * 3 - 2] = drc[i__ * 3 - 3];
/*< 	END DO >*/
    }
/*< 900	CONTINUE >*/
L900:
/* 	IF (LISTL.GE.3)WRITE(FDEBUG,801)VOICE,PITCH,IRMS,(IRC(J),J=1,ORDER) */
/* 801	FORMAT(1X,'<<ERRCOR OUT>>',T32,2I3,I6,I5,T50,10I8) */
/*   Decode RMS */
/*< 	IRMS = RMST((31-IRMS)*2+1) >*/
    *irms = rmst[(31 - *irms) * 2];
/*  Decode RC(1) and RC(2) from log-area-ratios */
/*  Protect from illegal coded value (-16) caused by bit errors */
/*< 	DO I = 1,2 >*/
    for (i__ = 1; i__ <= 2; ++i__) {
/*< 	   I2 = IRC(I) >*/
	i2 = irc[i__];
/*< 	   I1 = 0 >*/
	i1 = 0;
/*< 	   IF (I2.LT.0) THEN >*/
	if (i2 < 0) {
/*< 	      I1 = 1 >*/
	    i1 = 1;
/*< 	      I2 = -I2 >*/
	    i2 = -i2;
/*< 	      IF (I2.GT.15) I2 = 0 >*/
	    if (i2 > 15) {
		i2 = 0;
	    }
/*< 	   END IF >*/
	}
/*< 	   I2 = DETAB7(2*I2+1) >*/
	i2 = detab7[i2 * 2];
/*< 	   IF (I1.EQ.1) I2 = -I2 >*/
	if (i1 == 1) {
	    i2 = -i2;
	}
/*< 	   ISHIFT = 15 - NBIT(I) >*/
	ishift = 15 - nbit[i__ - 1];
/*< 	   IRC(I) = I2*2**ISHIFT >*/
	irc[i__] = i2 * pow_ii(&c__2, &ishift);
/*< 	END DO >*/
    }
/*  Decode RC(3)-RC(10) to sign plus 14 bits */
/*< 	DO I = 3,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 3; i__ <= i__1; ++i__) {
/*< 	   I2 = IRC(I) >*/
	i2 = irc[i__];
/*< 	   ISHIFT = 15 - NBIT(I) >*/
	ishift = 15 - nbit[i__ - 1];
/*< 	   I2 = I2*2**ISHIFT >*/
	i2 *= pow_ii(&c__2, &ishift);
/*< 	   I2 = I2 + QB(I-2) >*/
	i2 += qb[i__ - 3];
/*< 	   IRC(I) = I2*DESCL(I-2) + DEADD(I-2) >*/
	irc[i__] = (int) (i2 * descl[i__ - 3] + deadd[i__ - 3]);
/*< 	END DO >*/
    }
/* 	IF (LISTL.GE.3) WRITE(FDEBUG,811) IRMS, (IRC(I),I=1,ORDER) */
/* 811	FORMAT(1X,'<<DECODE OUT>>',T45,I4,1X,10I8) */
/*  Scale RMS and RC's to reals */
/*< 	RMS = IRMS >*/
    *rms = (real) (*irms);
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   RC(I) = IRC(I) / 2.**14 >*/
	rc[i__] = irc[i__] / 16384.f;
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITDECODE () >*/

L_initdecode:
/*< 	FIRST = .TRUE. >*/
    first = TRUE_;
/*< 	IVP2H = 0 >*/
    ivp2h = 0;
/*< 	IOVOIC = 0 >*/
    iovoic = 0;
/*< 	IAVGP = 60 >*/
    iavgp = 60;
/*< 	IPTOLD = 60 >*/
    iptold = 60;
/*< 	ERATE = 0 >*/
    erate = 0;
/*< 	DO J = 1,3 >*/
    for (j = 1; j <= 3; ++j) {
/*< 	   DO I = 1,MAXORD >*/
	for (i__ = 1; i__ <= 10; ++i__) {
/*< 	      DRC(J,I) = 0 >*/
	    drc[j + i__ * 3 - 4] = 0;
/*< 	   END DO >*/
	}
/*< 	   DPIT(J) = 0 >*/
	dpit[j - 1] = 0;
/*< 	   DRMS(J) = 0 >*/
	drms[j - 1] = 0;
/*< 	END DO >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* decode_ */

/* Subroutine */ int decode_(integer *ipitv, integer *irms, integer *irc, 
	integer *voice, integer *pitch, real *rms, real *rc)
{
    return decode_0_(0, ipitv, irms, irc, voice, pitch, rms, rc);
    }

/* Subroutine */ int initdecode_(void)
{
    return decode_0_(1, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0, (real *)0, (real *)0);
    }
