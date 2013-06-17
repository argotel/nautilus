/*
 * dyptrk.c
 *
 * SCCS ID:  @(#)dyptrk.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int dyptrk_(real *amdf, integer *ltau, integer *minptr, integer *voice, integer *pitch, integer *midx);
extern int initdyptrk_(void);
/* comlen contrl_ 12 */
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

/* ********************************************************************* */

/* 	DYPTRK Version 52 */

/* $Log: ndyptrk.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.5  1996/03/26  19:35:35  jaf */
/* Commented out trace statements. */

/* Revision 1.4  1996/03/19  18:03:22  jaf */
/* Replaced the initialization "DATA P/60*DEPTH*0/" with "DATA P/120*0/", */
/* because apparently Fortran (or at least f2c) can't handle expressions */
/* like that. */

/* Revision 1.3  1996/03/19  17:38:32  jaf */
/* Added comments about the local variables that should be saved from one */
/* invocation to the next.  None of them were given initial values in the */
/* original code, but from my testing, it appears that initializing them */
/* all to 0 works. */

/* Added entry INITDYPTRK to reinitialize these local variables. */

/* Revision 1.2  1996/03/13  16:32:17  jaf */
/* Comments added explaining which of the local variables of this */
/* subroutine need to be saved from one invocation to the next, and which */
/* do not. */

/* WARNING!  Some of them that should are never given initial values in */
/* this code.  Hopefully, Fortran 77 defines initial values for them, but */
/* even so, giving them explicit initial values is preferable. */

/* Revision 1.1  1996/02/07 14:45:14  jaf */
/* Initial revision */


/* ********************************************************************* */

/*   Dynamic Pitch Tracker */

/* Input: */
/*  AMDF   - Average Magnitude Difference Function array */
/*           Indices 1 through LTAU read, and MINPTR */
/*  LTAU   - Number of lags in AMDF */
/*  MINPTR - Location of minimum AMDF value */
/*  VOICE  - Voicing decision */
/* Output: */
/*  PITCH  - Smoothed pitch value, 2 frames delayed */
/*  MIDX   - Initial estimate of current frame pitch */
/* Compile time constant: */
/*  DEPTH  - Number of frames to trace back */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITDYPTRK. */

/*< 	SUBROUTINE DYPTRK( AMDF, LTAU, MINPTR, VOICE, PITCH, MIDX ) >*/
/* Subroutine */ int dyptrk_0_(int n__, real *amdf, integer *ltau, integer *
	minptr, integer *voice, integer *pitch, integer *midx)
{
    /* Initialized data */

    static real s[60] = { 0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f };
    static integer p[120]	/* was [60][2] */ = { 0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0 };
    static integer ipoint = 0;
    static real alphax = 0.f;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer pbar;
    real sbar;
    integer path[2], iptr, i__, j;
    real alpha, minsc, maxsc;

/*< 	INCLUDE 'contrl.fh' >*/
/*< 	INTEGER LTAU, MINPTR, VOICE, PITCH, MIDX >*/
/*       Arguments */
/* $Log: ndyptrk.c,v $
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
/*< 	REAL AMDF(LTAU) >*/
/* 	Parameters/constants */
/*< 	INTEGER DEPTH >*/
/*< 	PARAMETER (DEPTH=2) >*/
/*       Local variables that need not be saved */
/*       Note that PATH is only used for debugging purposes, and can be */
/*       removed. */
/*< 	REAL SBAR, MINSC, MAXSC, ALPHA >*/
/*< 	INTEGER PBAR, I, J, IPTR, PATH(DEPTH) >*/
/*       Local state */
/*       It would be a bit more "general" to define S(LTAU), if Fortran */
/*       allows the argument of a function to be used as the dimension of 
*/
/*       a local array variable. */
/*       IPOINT is always in the range 0 to DEPTH-1. */
/*       WARNING! */

/*       In the original version of this subroutine, IPOINT, ALPHAX, */
/*       every element of S, and potentially any element of P with the */
/*       second index value .NE. IPTR were read without being given */
/*       initial values (all indices of P with second index equal to */
/*       IPTR are all written before being read in this subroutine). */

/*       From examining the code carefully, it appears that all of these 
*/
/*       should be saved from one invocation to the next. */

/*       I've run lpcsim with the "-l 6" option to see all of the */
/*       debugging information that is printed out by this subroutine */
/*       below, and it appears that S, P, IPOINT, and ALPHAX are all */
/*       initialized to 0 (these initial values would likely be different 
*/
/*       on different platforms, compilers, etc.).  Given that the output 
*/
/*       of the coder sounds reasonable, I'm going to initialize these */
/*       variables to 0 explicitly. */
/*< 	REAL S(60) >*/
/*< 	INTEGER P(60,DEPTH), IPOINT >*/
/*< 	REAL ALPHAX >*/
/*< 	SAVE S, P, IPOINT >*/
/*< 	SAVE ALPHAX >*/
/*< 	DATA S/60*0./ >*/
    /* Parameter adjustments */
    if (amdf) {
	--amdf;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initdyptrk;
	}

/*< 	DATA P/120*0/, IPOINT/0/ >*/
/*< 	DATA ALPHAX/0./ >*/
/*   Calculate the confidence factor ALPHA, used as a threshold slope in 
*/
/*   SEESAW.  If unvoiced, set high slope so that every point in P array 
*/
/*  is marked as a potential pitch frequency.  A scaled up version (ALPHAX
)*/
/*   is used to maintain arithmetic precision. */
/*< 	IF( VOICE .EQ. 1 ) THEN >*/
    if (*voice == 1) {
/*< 	   ALPHAX = .75*ALPHAX + AMDF(MINPTR)/2. >*/
	alphax = alphax * .75f + amdf[*minptr] / 2.f;
/*< 	ELSE >*/
    } else {
/*< 	   ALPHAX = (63./64.)*ALPHAX >*/
	alphax *= .984375f;
/*< 	END IF >*/
    }
/*< 	ALPHA = ALPHAX/16 >*/
    alpha = alphax / 16;
/*< 	IF( VOICE .EQ. 0 .AND. ALPHAX .LT. 128 ) ALPHA = 8 >*/
    if (*voice == 0 && alphax < 128.f) {
	alpha = 8.f;
    }
/* SEESAW: Construct a pitch pointer array and intermediate winner functio
n*/
/*   Left to right pass: */
/*< 	IPTR = IPOINT+1 >*/
    iptr = ipoint + 1;
/*< 	P(1,IPTR) = 1 >*/
    p[iptr * 60 - 60] = 1;
/*< 	I = 1 >*/
    i__ = 1;
/*< 	PBAR = 1 >*/
    pbar = 1;
/*< 	SBAR = S(1) >*/
    sbar = s[0];
/*< 	DO I = 1,LTAU  >*/
    i__1 = *ltau;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   SBAR = SBAR + ALPHA >*/
	sbar += alpha;
/*< 	   IF (SBAR .LT. S(I)) THEN >*/
	if (sbar < s[i__ - 1]) {
/*< 	      S(I) = SBAR >*/
	    s[i__ - 1] = sbar;
/*< 	      P(I,IPTR) = PBAR >*/
	    p[i__ + iptr * 60 - 61] = pbar;
/*< 	   ELSE >*/
	} else {
/*< 	      SBAR = S(I) >*/
	    sbar = s[i__ - 1];
/*< 	      P(I,IPTR) = I >*/
	    p[i__ + iptr * 60 - 61] = i__;
/*< 	      PBAR = I >*/
	    pbar = i__;
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*   Right to left pass: */
/*< 	I = PBAR-1 >*/
    i__ = pbar - 1;
/*< 	SBAR = S(I+1) >*/
    sbar = s[i__];
/*< 	DO WHILE (I .GE. 1) >*/
    while(i__ >= 1) {
/*< 	   SBAR = SBAR + ALPHA >*/
	sbar += alpha;
/*< 	   IF (SBAR .LT. S(I)) THEN >*/
	if (sbar < s[i__ - 1]) {
/*< 	      S(I) = SBAR >*/
	    s[i__ - 1] = sbar;
/*< 	      P(I,IPTR) = PBAR >*/
	    p[i__ + iptr * 60 - 61] = pbar;
/*< 	   ELSE >*/
	} else {
/*< 	      PBAR = P(I,IPTR) >*/
	    pbar = p[i__ + iptr * 60 - 61];
/*< 	      I = PBAR >*/
	    i__ = pbar;
/*< 	      SBAR = S(I) >*/
	    sbar = s[i__ - 1];
/*< 	   END IF >*/
	}
/*< 	   I = I-1 >*/
	--i__;
/*< 	END DO >*/
    }
/*   Update S using AMDF */
/*   Find maximum, minimum, and location of minimum */
/*< 	S(1) = S(1) + AMDF(1)/2 >*/
    s[0] += amdf[1] / 2;
/*< 	MINSC = S(1) >*/
    minsc = s[0];
/*< 	MAXSC = MINSC >*/
    maxsc = minsc;
/*< 	MIDX = 1 >*/
    *midx = 1;
/*< 	DO I = 2,LTAU >*/
    i__1 = *ltau;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*< 	   S(I) = S(I) + AMDF(I)/2 >*/
	s[i__ - 1] += amdf[i__] / 2;
/*< 	   IF (S(I) .GT. MAXSC) MAXSC = S(I) >*/
	if (s[i__ - 1] > maxsc) {
	    maxsc = s[i__ - 1];
	}
/*< 	   IF (S(I) .LT. MINSC) THEN >*/
	if (s[i__ - 1] < minsc) {
/*< 	      MIDX = I >*/
	    *midx = i__;
/*< 	      MINSC = S(I) >*/
	    minsc = s[i__ - 1];
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*   Subtract MINSC from S to prevent overflow */
/*< 	DO I = 1,LTAU >*/
    i__1 = *ltau;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	   S(I) = S(I) - MINSC >*/
	s[i__ - 1] -= minsc;
/*< 	END DO >*/
    }
/*< 	MAXSC = MAXSC - MINSC >*/
    maxsc -= minsc;
/*   Use higher octave pitch if significant null there */
/*< 	J = 0 >*/
    j = 0;
/*< 	DO I = 20, 40, 10 >*/
    for (i__ = 20; i__ <= 40; i__ += 10) {
/*< 	   IF (MIDX .GT. I) THEN >*/
	if (*midx > i__) {
/*< 	      IF (S(MIDX-I) .LT. MAXSC/4) J = I >*/
	    if (s[*midx - i__ - 1] < maxsc / 4) {
		j = i__;
	    }
/*< 	   END IF >*/
	}
/*< 	END DO >*/
    }
/*< 	MIDX = MIDX - J >*/
    *midx -= j;
/*   TRACE: look back two frames to find minimum cost pitch estimate */
/*< 	J = IPOINT >*/
    j = ipoint;
/*< 	PITCH = MIDX >*/
    *pitch = *midx;
/*< 	DO I = 1,DEPTH >*/
    for (i__ = 1; i__ <= 2; ++i__) {
/*< 	   J = MOD(J,DEPTH) + 1 >*/
	j = j % 2 + 1;
/*< 	   PITCH = P(PITCH,J) >*/
	*pitch = p[*pitch + j * 60 - 61];
/*< 	   PATH(I) = PITCH >*/
	path[i__ - 1] = *pitch;
/*< 	END DO >*/
    }

/*       The following statement subtracts one from IPOINT, mod DEPTH.  I 
*/
/*       think the author chose to add DEPTH-1, instead of subtracting 1, 
*/
/*       because then it will work even if MOD doesn't work as desired on 
*/
/*       negative arguments. */

/*< 	IPOINT = MOD(IPOINT+DEPTH-1,DEPTH) >*/
    ipoint = (ipoint + 1) % 2;
/*   Print test data */
/* 	IF (LISTL .GE. 3) THEN */
/* 	   IF (LISTL .GE. 6) THEN */
/* 	      WRITE(FDEBUG,970) 'DYPTRACK array (P):',P */
/* 	      WRITE(FDEBUG,980) 'Pitch Winner Function (S):',S */
/* 	   END IF */
/* 	   WRITE(FDEBUG,950) IPOINT, MIDX, ALPHA, PITCH, PATH */
/* 950	   FORMAT(' Pitch: IPOINT  MIDX  ALPHA   PITCH     PATH'/ */
/*     1             5X,2I7,F7.0,I7,5X,10I4/) */
/* 970	   FORMAT(1X,A,100(/1X,20I6)) */
/* 980	   FORMAT(1X,A,100(/1X,10F12.1)) */
/* 	END IF */
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITDYPTRK () >*/

L_initdyptrk:
/*< 	DO I = 1,60 >*/
    for (i__ = 1; i__ <= 60; ++i__) {
/*< 	   S(I) = 0. >*/
	s[i__ - 1] = 0.f;
/*< 	   DO J = 1,DEPTH >*/
	for (j = 1; j <= 2; ++j) {
/*< 	      P(I,J) = 0 >*/
	    p[i__ + j * 60 - 61] = 0;
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*< 	IPOINT = 0 >*/
    ipoint = 0;
/*< 	ALPHAX = 0. >*/
    alphax = 0.f;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* dyptrk_ */

/* Subroutine */ int dyptrk_(real *amdf, integer *ltau, integer *minptr, 
	integer *voice, integer *pitch, integer *midx)
{
    return dyptrk_0_(0, amdf, ltau, minptr, voice, pitch, midx);
}

/* Subroutine */ int initdyptrk_(void)
{
    return dyptrk_0_(1, (real *)0, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0, (integer *)0);
}
