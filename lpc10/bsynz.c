/*
 * bsynz.c
 *
 * SCCS ID:  @(#)bsynz.c 1.2 96/05/19
 */

#include "f2c.h"

extern int bsynz_(real *coef, integer *ip, integer *iv, real *sout, real *rms, real *ratio, real *g2pass);
extern int initbsynz_(void);
/* comlen contrl_ 12 */
/*:ref: random_ 4 0 */

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

/* ***************************************************************** */

/* 	BSYNZ Version 54 */

/* $Log: bsynz.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.6  1996/07/14  13:47:00  dm */
/* More changes to looping bounds to minimize arithmetic within loops */
/* and more elimination of unnecessary assignments to temporary variables */

/* Revision 1.5  1996/07/10  21:00:00  dm */
/* Changed loop bounds to minimize arithmetic within loops, eliminated */
/* middle-man variables. */

/* Revision 1.4  1996/03/27  18:11:22  jaf */
/* Changed the range of NOISE printed out in the debugging statements, */
/* even though they are commented out.  I didn't discover this until I */
/* tried comparing two different versions of the LPC-10 coder, each with */
/* full tracing enabled. */

/* Revision 1.3  1996/03/26  19:33:23  jaf */
/* Commented out trace statements. */

/* Revision 1.2  1996/03/20  17:12:54  jaf */
/* Added comments about which indices of array arguments are read or */
/* written. */

/* Rearranged local variable declarations to indicate which need to be */
/* saved from one invocation to the next.  Added entry INITBSYNZ to */
/* reinitialize the local state variables, if desired. */

/* Revision 1.1  1996/02/07 14:43:15  jaf */
/* Initial revision */


/* ***************************************************************** */

/*   Synthesize One Pitch Epoch */

/* Input: */
/*  COEF  - Predictor coefficients */
/*          Indices 1 through ORDER read. */
/*  IP    - Pitch period (number of samples to synthesize) */
/*  IV    - Voicing for the current epoch */
/*  RMS   - Energy for the current epoch */
/*  RATIO - Energy slope for plosives */
/*  G2PASS- Sharpening factor for 2 pass synthesis */
/* Output: */
/*  SOUT  - Synthesized speech */
/*          Indices 1 through IP written. */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY INITBSYNZ. */

/*< 	SUBROUTINE BSYNZ(COEF, IP, IV, SOUT, RMS, RATIO, G2PASS) >*/
/* Subroutine */ int bsynz_0_(int n__, real *coef, integer *ip, integer *iv, 
	real *sout, real *rms, real *ratio, real *g2pass)
{
    /* Initialized data */

    static integer ipo = 0;
    static real rmso = 0.f;
    static integer kexc[25] = { 8,-16,26,-48,86,-162,294,-502,718,-728,184,
	    672,-610,-672,184,728,718,502,294,162,86,48,26,16,8 };
    static real exc[166] = { 0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f };
    static real exc2[166] = { 0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f };
    static real lpi0 = 0.f;
    static real lpi2 = 0.f;
    static real lpi3 = 0.f;
    static real hpi0 = 0.f;
    static real hpi2 = 0.f;
    static real hpi3 = 0.f;

    /* System generated locals */
    integer i__1, i__2;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    real gain, xssq;
    integer i__, j;
    real noise[166], pulse;
    integer px;
    real sscale;
    extern integer random_(void);
    real xy, sum, ssq;
    static real hpi1, lpi1;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INCLUDE 'contrl.fh' >*/
/* $Log: bsynz.c,v $
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
/*< 	REAL COEF(ORDER) >*/
/*       Arguments */
/* $Log: bsynz.c,v $
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
/* coder significantly whether it is .TRUE. or .FALSE., or whether it is */
/* a constant or a variable, since it is only examined once per frame. */
/* Leaving it as a variable that is set to .TRUE.  seems like a good */
/* idea, since it does enable some error-correction capability for */
/* unvoiced frames, with no change in the coding rate, and no noticeable */
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
/* an application, I would recommend removing the call to ERROR in RCCHK, */
/* and remove ERROR and nunsfm completely. */
/* iclip is initialized to 0 in SETUP, and incremented in entry SWRITE in */
/* sread.f.  When LPC10 is embedded into an application, one might want */
/* to cause it to be incremented in a routine that takes the output of */
/* SYNTHS and sends it to an audio device.  It could be optionally */
/* displayed, for those that might want to know what it is. */
/* maxosp is never initialized to 0 in SETUP, although it probably should */
/* be, and it is updated in subroutine ANALYS.  I doubt that its value */
/* would be of much interest to an application in which LPC10 is embedded. */
/* listl and lincnt are not needed for an embedded LPC10 at all. */
/* 	integer nframe, nunsfm, iclip, maxosp, listl, lincnt */
/* 	common /contrl/ fsi, fso, fpi, fpo, fbi, fbo, pbin, fmsg, fdebug */
/*< 	common /contrl/ order, lframe >*/
/* 	common /contrl/ quant, nbits */
/*< 	common /contrl/ corrp >*/
/* 	common /contrl/ nframe, nunsfm, iclip, maxosp, listl, lincnt */
/*< 	INTEGER IP, IV >*/
/*< 	REAL SOUT(IP), RMS, RATIO, G2PASS >*/
/*       Function return value definitions */
/*< 	INTEGER RANDOM >*/
/* 	Parameters/constants */
/*       KEXC is not a Fortran PARAMETER, but it is an array initialized 
*/
/*       with a DATA statement that is never modified. */
/*< 	INTEGER KEXC(25) >*/
/*< 	REAL A0, A1, A2, A3, B0, B1, B2, B3 >*/
/*< 	REAL MESCL, PESCL >*/
/*< 	PARAMETER (A0= .125, A1=.75, A2= .125, A3=0) >*/
/*< 	PARAMETER (B0=-.125, B1=.25, B2=-.125, B3=0) >*/
/*< 	PARAMETER (MESCL=1.0, PESCL=1.0) >*/
/*       Local variables that need not be saved */
/*       NOISE is declared with range (1:MAXPIT+MAXORD), but only indices 
*/
/*       ORDER+1 through ORDER+IP are ever used, and I think that IP */
/*       .LE. MAXPIT.  Why not declare it to be in the range (1:MAXPIT) */
/*       and use that range? */
/*< 	INTEGER I, J, K >*/
/*< 	INTEGER PX >*/
/*< 	REAL NOISE(MAXPIT+MAXORD) >*/
/*< 	REAL LPI0, HPI0 >*/
/*< 	REAL PULSE, SSCALE, XSSQ, SUM, SSQ, GAIN >*/
/*< 	REAL XY >*/
/*       Local state */
/*       I believe that only indices 1 through ORDER of EXC need to be */
/*       saved from one invocation to the next, but we may as well save */
/*       the whole array. */
/*       None of these local variables were given initial values in the */
/*       original code.  I'm guessing that 0 is a reasonable initial */
/*       value for all of them. */
/*< 	INTEGER IPO >*/
/*< 	REAL EXC(MAXPIT+MAXORD), EXC2(MAXPIT+MAXORD) >*/
/*< 	REAL LPI1, LPI2, LPI3, HPI1, HPI2, HPI3 >*/
/*< 	REAL RMSO >*/
/*< 	SAVE IPO >*/
/*< 	SAVE EXC, EXC2 >*/
/*< 	SAVE LPI1, LPI2, LPI3, HPI1, HPI2, HPI3 >*/
/*< 	SAVE RMSO >*/
/*< 	DATA IPO /0/ >*/
    /* Parameter adjustments */
    if (coef) {
	--coef;
	}
    if (sout) {
	--sout;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initbsynz;
	}

/*                  MAXPIT+MAXORD=166 */
/*< 	DATA EXC /166*0./, EXC2 /166*0./ >*/
/*< 	DATA LPI0 /0./, LPI2 /0./, LPI3 /0./ >*/
/*< 	DATA HPI0 /0./, HPI2 /0./, HPI3 /0./ >*/
/*< 	DATA RMSO /0./ >*/
/*< 	D >*/
/*  Calculate history scale factor XY and scale filter state */
/*< 	XY = MIN( RMSO/(RMS+1E-6), 8. ) >*/
/* Computing MIN */
    r__1 = rmso / (*rms + 1e-6f);
    xy = (real) dmin(r__1,8.f);
/*< 	RMSO = RMS >*/
    rmso = *rms;
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 0; i__ < i__1; ++i__) {
/*< 	   EXC2(I) = EXC2(IPO+I)*XY >*/
	exc2[i__] = exc2[ipo + i__] * xy;
/*< 	END DO >*/
    }
/*< 	IPO = IP >*/
    ipo = *ip;
/*< 	IF(IV.EQ.0) THEN >*/
    if (*iv == 0) {
/*  Generate white noise for unvoiced */
/*< 	   DO I = 1,IP >*/
	i__1 = contrl_1.order + *ip;
	for (i__ = contrl_1.order; i__ < i__1; ++i__) {
/*< 	      EXC(ORDER+I) = RANDOM() / 2**6 >*/
	    exc[i__] = (real) (random_() / 64);
/*< 	   END DO >*/
	}
/*  Impulse doublet excitation for plosives */
/*       (RANDOM()+32768) is in the range 0 to 2**16-1.  Therefore the */
/*       following expression should be evaluated using integers with at */
/*       least 32 bits (16 isn't enough), and PX should be in the range */
/*       ORDER+1+0 through ORDER+1+(IP-2) .EQ. ORDER+IP-1. */
/*< 	   PX = ((RANDOM()+32768)*(IP-1)/2**16) + ORDER + 1 >*/
	px = (random_() + 32768) * (*ip - 1) / 65536 + contrl_1.order + 1;
/*< 	   PULSE = PESCL*(RATIO/4)*342 >*/
	pulse = (*ratio / 4.f) * 342;
/*< 	   IF(PULSE.GT.2000) PULSE = 2000 >*/
/* Why 2e3f and not 2000 as the above FORTRAN shows? --dm */
	if (pulse > 2e3f) {
	    pulse = 2e3f;
	}
/*< 	   EXC(PX)   = EXC(PX)   + PULSE >*/
	exc[px - 1] += pulse;
/*< 	   EXC(PX+1) = EXC(PX+1) - PULSE >*/
	exc[px] -= pulse;
/*  Load voiced excitation */
/*< 	ELSE >*/
    } else {
/*< 	   SSCALE = SQRT(FLOAT(IP))/6.928 >*/
	sscale = (real) sqrt((real) (*ip)) / 6.928f;
/*< 	   DO I = 1,IP >*/
	i__1 = *ip;
	for (i__ = 0; i__ < i__1; ++i__) {
/*< 	      EXC(ORDER+I) = 0. >*/
/*< 	      IF(I.LE.25) EXC(ORDER+I) = SSCALE*KEXC(I) >*/
	    if (i__ < 25)
               lpi0 = sscale * kexc[i__];
            else
	       lpi0 = 0.f;
/*< 	      LPI0 = EXC(ORDER+I) >*/
/*< 	      EXC(ORDER+I) = A0*EXC(ORDER+I) + A1*LPI1 + A2*LPI2 + A3*LPI3 >*/
	    exc[contrl_1.order+i__] =
                       lpi0*.125f + lpi1*.75f + lpi2*.125f + lpi3*0.f;
/*< 	      LPI3 = LPI2 >*/
	    lpi3 = lpi2;
/*< 	      LPI2 = LPI1 >*/
	    lpi2 = lpi1;
/*< 	      LPI1 = LPI0 >*/
	    lpi1 = lpi0;
/*< 	   END DO >*/
	}
/*< 	   DO I = 1,IP >*/
	i__1 = contrl_1.order + *ip;
	for (i__ = contrl_1.order; i__ < i__1; ++i__) {
/*< 	      NOISE(ORDER+I) = MESCL * RANDOM() / 2**6 >*/
/*< 	      HPI0 = NOISE(ORDER+I) >*/
	    hpi0 = random_() * 1.f / 64;
/*< 	  >*/
	    noise[i__] = hpi0*-.125f + hpi1*.25f + hpi2*-.125f + hpi3*0.f;
/*< 	      HPI3 = HPI2 >*/
	    hpi3 = hpi2;
/*< 	      HPI2 = HPI1 >*/
	    hpi2 = hpi1;
/*< 	      HPI1 = HPI0 >*/
	    hpi1 = hpi0;
/*< 	   END DO >*/
	}
/*< 	   DO I = 1,IP >*/
	i__1 = contrl_1.order + *ip;
	for (i__ = contrl_1.order; i__ < i__1; ++i__) {
/*< 	      EXC(ORDER+I) = EXC(ORDER+I) + NOISE(ORDER+I) >*/
	    exc[i__] += noise[i__];
/*< 	   END DO >*/
	}
/*< 	END IF >*/
    }
/*   Synthesis filters: */
/*    Modify the excitation with all-zero filter  1 + G*SUM */
/*< 	XSSQ = 0 >*/
    xssq = 0.f;
/*< 	DO I = 1,IP >*/
    i__1 = contrl_1.order + *ip;
    for (i__ = contrl_1.order; i__ < i__1; ++i__) {
/*< 	   K = ORDER + I >*/
/*< 	   SUM = 0. >*/
	sum = 0.f;
/*< 	   DO J = 1,ORDER >*/
	i__2 = contrl_1.order;
	for (j = 1; j <= i__2; ++j) {
/*< 	      SUM = SUM + COEF(J)*EXC(K-J) >*/
	    sum += coef[j] * exc[i__ - j];
/*< 	   END DO >*/
	}
/*< 	   SUM = SUM*G2PASS >*/
	sum *= *g2pass;
/*< 	   EXC2(K) = SUM + EXC(K) >*/
	exc2[i__] = sum + exc[i__];
/*< 	END DO >*/
    }
/*   Synthesize using the all pole filter  1 / (1 - SUM) */
/*< 	DO I = 1,IP >*/
    i__1 = contrl_1.order + *ip;
    for (i__ = contrl_1.order; i__ < i__1; ++i__) {
/*< 	   K = ORDER + I >*/
/*< 	   SUM = 0. >*/
	sum = 0.f;
/*< 	   DO J = 1,ORDER >*/
	i__2 = contrl_1.order;
	for (j = 1; j <= i__2; ++j) {
/*< 	      SUM = SUM + COEF(J)*EXC2(K-J) >*/
	    sum += coef[j] * exc2[i__ - j];
/*< 	   END DO >*/
	}
/*< 	   EXC2(K) = SUM + EXC2(K) >*/
	exc2[i__] = sum + exc2[i__];
/*< 	   XSSQ = XSSQ + EXC2(K)*EXC2(K) >*/
	xssq += exc2[i__] * exc2[i__];
/*< 	END DO >*/
    }
/*  Save filter history for next epoch */
/*< 	DO I = 1,ORDER >*/
    i__1 = contrl_1.order;
    for (i__ = 0; i__ < i__1; ++i__) {
/*< 	   EXC(I) = EXC(IP+I) >*/
	exc[i__] = exc[*ip + i__];
/*< 	   EXC2(I) = EXC2(IP+I) >*/
	exc2[i__] = exc2[*ip + i__];
/*< 	END DO >*/
    }
/*  Apply gain to match RMS */
/*< 	SSQ = RMS*RMS*IP >*/
    r__1 = *rms * *rms;
    ssq = r__1 * *ip;
/*< 	GAIN = SQRT(SSQ/XSSQ) >*/
    gain = (real) sqrt(ssq / xssq);
/*< 	DO I = 1,IP >*/
    i__1 = *ip;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   SOUT(I) = GAIN*EXC2(ORDER+I) >*/
	sout[i__] = gain * exc2[contrl_1.order + i__ - 1];
/*< 	END DO >*/
    }
/*   Print test data */
/* 	IF(LISTL.GE.5) THEN */

/*           I changed the range of indices to print within NOISE from */
/*           1,IP+ORDER to 1+ORDER,IP+ORDER since indices 1 through ORDER */
/*           of NOISE are never used.  This avoids printing out their */
/*           "garbage" values. */

/* 	   IF(IV.NE.0) */
/*     1     WRITE(FDEBUG,980) 'NOISE:',(NOISE(I),I=1+ORDER,IP+ORDER) */
/* 	   WRITE(FDEBUG,980) 'EXC:',  (EXC(I),  I=1,IP+ORDER) */
/* 	   WRITE(FDEBUG,980) 'EXC2:', (EXC2(I), I=1,IP+ORDER) */
/* 	   WRITE(FDEBUG,980) 'SOUT:', (SOUT(I), I=1,IP) */
/* 980	   FORMAT(1X,A,100(/1X,10F10.1)) */
/* 	END IF */
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITBSYNZ () >*/

L_initbsynz:
/*< 	IPO = 0 >*/
    ipo = 0;
/*< 	DO I = 1,(MAXPIT+MAXORD) >*/
    for (i__ = 0; i__ < 166; ++i__) {
/*< 	   EXC(I) = 0. >*/
/*< 	   EXC2(I) = 0. >*/
	exc[i__] = exc2[i__] = 0.f;
/*< 	END DO >*/
    }
/*< 	LPI0 = 0. >*/
/*< 	LPI2 = 0. >*/
/*< 	LPI3 = 0. >*/
/*< 	HPI0 = 0. >*/
/*< 	HPI2 = 0. >*/
/*< 	HPI3 = 0. >*/
/*< 	RMSO = 0. >*/
    lpi0 = lpi2 = lpi3 = hpi0 = hpi2 = hpi3 = rmso = 0.f;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* bsynz_ */

/* Subroutine */ int bsynz_(real *coef, integer *ip, integer *iv, real *sout, 
	real *rms, real *ratio, real *g2pass)
{
    return bsynz_0_(0, coef, ip, iv, sout, rms, ratio, g2pass);
    }

/* Subroutine */ int initbsynz_(void)
{
    return bsynz_0_(1, (real *)0, (integer *)0, (integer *)0, (real *)0,
                    (real *)0, (real *)0, (real *)0);
    }

