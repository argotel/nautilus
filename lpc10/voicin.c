/*
 * voicin.c
 *
 * SCCS ID:  @(#)voicin.c 1.2 96/05/19
 */

#include "f2c.h"

extern int voicin_(integer *vwin, real *inbuf, real *lpbuf, integer *buflim, integer *half, real *minamd, real *maxamd, integer *mintau, real *ivrc, integer *obound, integer *voibuf, integer *af);
extern int initvoicin_(void);
/* comlen contrl_ 12 */
/*:ref: vparms_ 14 14 4 6 6 4 4 6 4 4 4 4 6 6 6 6 */

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

/****************************************************************************/

/* 	VOICIN Version 52 */

/* $Log: voicin.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.10  1996/03/29  17:59:14  jaf */
/* Avoided using VALUE(9), although it shouldn't affect the function of */
/* the code at all, because it was always multiplied by VDC(9,SNRL), */
/* which is 0 for all values of SNRL.  Still, if VALUE(9) had an initial */
/* value of IEEE NaN, it might cause trouble (I don't know how IEEE */
/* defines Nan * 0.  It should either be NaN or 0.) */

/* Revision 1.9  1996/03/29  17:54:46  jaf */
/* Added a few comments about the accesses made to argument array VOIBUF */
/* and the local saved array VOICE. */

/* Revision 1.8  1996/03/27  18:19:54  jaf */
/* Added an assignment to VSTATE that does not affect the function of the */
/* program at all.  The only reason I put it in was so that the tracing */
/* statements at the end, when enabled, will print a consistent value for */
/* VSTATE when HALF .EQ. 1, rather than a garbage value that could change */
/* from one call to the next. */

/* Revision 1.7  1996/03/26  20:00:06  jaf */
/* Removed the inclusion of the file "vcomm.fh", and put its contents */
/* into this file.  It was included nowhere else but here. */

/* Revision 1.6  1996/03/26  19:38:09  jaf */
/* Commented out trace statements. */

/* Revision 1.5  1996/03/19  20:43:45  jaf */
/* Added comments about which indices of OBOUND and VOIBUF can be */
/* accessed, and whether they are read or written.  VOIBUF is fairly */
/* messy. */

/* Revision 1.4  1996/03/19  15:00:58  jaf */
/* Moved the DATA statements for the *VDC* variables later, as it is */
/* apparently illegal to have DATA statements before local variable */
/* declarations. */

/* Revision 1.3  1996/03/19  00:10:49  jaf */
/* Heavily commented the local variables that are saved from one */
/* invocation to the next, and how the local variable FIRST is used to */
/* avoid the need to assign most of them initial values with DATA */
/* statements. */

/* A few should be initialized, but aren't.  I've guessed initial values */
/* for two of these, SFBUE and SLBUE, and I've convinced myself that for */
/* VOICE, the effects of uninitialized values will die out after 2 or 3 */
/* frame times.  It would still be good to choose initial values for */
/* these, but I don't know what reasonable values would be (0 comes to */
/* mind). */

/* Revision 1.2  1996/03/13  16:09:28  jaf */
/* Comments added explaining which of the local variables of this */
/* subroutine need to be saved from one invocation to the next, and which */
/* do not. */

/* WARNING!  Some of them that should are never given initial values in */
/* this code.  Hopefully, Fortran 77 defines initial values for them, but */
/* even so, giving them explicit initial values is preferable. */

/* WARNING!  VALUE(9) is used, but never assigned a value.  It should */
/* probably be eliminated from the code. */

/* Revision 1.1  1996/02/07 14:50:28  jaf */
/* Initial revision */


/****************************************************************************/

/*        Voicing Detection (VOICIN) makes voicing decisions for each half */
/*  frame of input speech.  Tentative voicing decisions are made two frames*/
/*   in the future (2F) for each half frame.  These decisions are carried */
/*   through one frame in the future (1F) to the present (P) frame where */
/*   they are examined and smoothed, resulting in the final voicing */
/*   decisions for each half frame. */
/*        The voicing parameter (signal measurement) column vector (VALUE) */
/*   is based on a rectangular window of speech samples determined by the */
/*  window placement algorithm.  The voicing parameter vector contains the*/
/*  AMDF windowed maximum-to-minimum ratio, the zero crossing rate, energy*/
/*   measures, reflection coefficients, and prediction gains.  The voicing */
/*  window is placed to avoid contamination of the voicing parameter vector*/
/*   with speech onsets. */
/*        The input signal is then classified as unvoiced (including */
/*   silence) or voiced.  This decision is made by a linear discriminant */
/*   function consisting of a dot product of the voicing decision */
/*   coefficient (VDC) row vector with the measurement column vector */
/*  (VALUE).  The VDC vector is 2-dimensional, each row vector is optimized*/
/*   for a particular signal-to-noise ratio (SNR).  So, before the dot */
/*   product is performed, the SNR is estimated to select the appropriate */
/*   VDC vector. */
/*        The smoothing algorithm is a modified median smoother.  The */
/*  voicing discriminant function is used by the smoother to determine how*/
/*   strongly voiced or unvoiced a signal is.  The smoothing is further */
/*   modified if a speech onset and a voicing decision transition occur */
/*   within one half frame.  In this case, the voicing decision transition */
/*  is extended to the speech onset.  For transmission purposes, there are*/
/*   constraints on the duration and transition of voicing decisions.  The */
/*   smoother takes these constraints into account. */
/*        Finally, the energy estimates are updated along with the dither */
/*   threshold used to calculate the zero crossing rate (ZC). */

/* Inputs: */
/*  VWIN      - Voicing window limits */
/*              The indices read of arrays VWIN, INBUF, LPBUF, and BUFLIM */
/*              are the same as those read by subroutine VPARMS. */
/*  INBUF     - Input speech buffer */
/*  LPBUF     - Low-pass filtered speech buffer */
/*  BUFLIM    - INBUF and LPBUF limits */
/*  HALF      - Present analysis half frame number */
/*  MINAMD    - Minimum value of the AMDF */
/*  MAXAMD    - Maximum value of the AMDF */
/*  MINTAU    - Pointer to the lag of the minimum AMDF value */
/*  IVRC(2)   - Inverse filter's RC's */
/*              Only index 2 of array IVRC read under normal operation. */
/*              (Index 1 is also read when debugging is turned on.) */
/*  OBOUND    - Onset boundary descriptions */
/*             Indices 1 through 3 read if (HALF .NE. 1), otherwise untouched.
*/
/*  AF        - The analysis frame number */
/* Output: */
/*  VOIBUF(2,0:AF) - Buffer of voicing decisions */
/*              Index (HALF,3) written. */
/*              If (HALF .EQ. 1), skip down to "Read (HALF,3)" below. */
/*              Indices (1,2), (2,1), (1,2), and (2,2) read. */
/*              One of the following is then done: */
/*                 read (1,3) and possibly write (1,2) */
/*                 read (1,3) and write (1,2) or (2,2) */
/*                 write (2,1) */
/*                 write (2,1) or (1,2) */
/*                 read (1,0) and (1,3) and then write (2,2) or (1,1) */
/*                 no reads or writes on VOIBUF */
/*              Finally, read (HALF,3) */
/* Internal: */
/*  QS        - Ratio of preemphasized to full-band energies */
/*  RC1       - First reflection coefficient */
/* AR_B      - Product of the causal forward and reverse pitch prediction gain
s*/
/* AR_F      - Product of the noncausal forward and rev. pitch prediction gain
s*/
/*  ZC        - Zero crossing rate */
/*  DITHER    - Zero crossing threshold level */
/*  MAXMIN    - AMDF's 1 octave windowed maximum-to-minimum ratio */
/*  MINPTR    - Location  of minimum AMDF value */
/*  NVDC      - Number of elements in each VDC vector */
/*  NVDCL     - Number of VDC vectors */
/*  VDCL      - SNR values corresponding to the set of VDC's */
/*  VDC       - 2-D voicing decision coefficient vector */
/*  VALUE(9)  - Voicing Parameters */
/*  VOICE(2,3)- History of LDA results */
/*              On every call when (HALF .EQ. 1), VOICE(*,I+1) is */
/*              shifted back to VOICE(*,I), for I=1,2. */
/*              VOICE(HALF,3) is written on every call. */
/*              Depending on several conditions, one or more of */
/*              (1,1), (1,2), (2,1), and (2,2) might then be read. */
/*  LBE       - Ratio of low-band instantaneous to average energies */
/*  FBE       - Ratio of full-band instantaneous to average energies */
/*  LBVE      - Low band voiced energy */
/*  LBUE      - Low band unvoiced energy */
/*  FBVE      - Full band voiced energy */
/*  FBUE      - Full band unvoiced energy */
/*  OFBUE     - Previous full-band unvoiced energy */
/*  OLBUE     - Previous low-band unvoiced energy */
/*  REF       - Reference energy for initialization and DITHER threshold */
/*  SNR       - Estimate of signal-to-noise ratio */
/*  SNR2      - Estimate of low-band signal-to-noise ratio */
/*  SNRL      - SNR level number */
/*  OT        - Onset transition present */
/*  VSTATE    - Decimal interpretation of binary voicing classifications */
/*  FIRST     - First call flag */

/* This subroutine maintains local state from one call to the next.  If */
/* you want to switch to using a new audio stream for this filter, or */
/* reinitialize its state for any other reason, call the ENTRY */
/* INITVOICIN. */

/*< 	S >*/
/* Subroutine */ int voicin_0_(int n__, integer *vwin, real *inbuf, real *
	lpbuf, integer *buflim, integer *half, real *minamd, real *maxamd, 
	integer *mintau, real *ivrc, integer *obound, integer *voibuf, 
	integer *af)
{
    /* Initialized data */

    static logical first = TRUE_;
    static real dither = 20.f;
    static real vdc[100]	/* was [10][10] */ = { 0.f,1714.f,-110.f,
	    334.f,-4096.f,-654.f,3752.f,3769.f,0.f,1181.f,0.f,874.f,-97.f,
	    300.f,-4096.f,-1021.f,2451.f,2527.f,0.f,-500.f,0.f,510.f,-70.f,
	    250.f,-4096.f,-1270.f,2194.f,2491.f,0.f,-1500.f,0.f,500.f,-10.f,
	    200.f,-4096.f,-1300.f,2e3f,2e3f,0.f,-2e3f,0.f,500.f,0.f,0.f,
	    -4096.f,-1300.f,2e3f,2e3f,0.f,-2500.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f };
    static integer nvdcl = 5;
    static real vdcl[10] = { 600.f,450.f,300.f,200.f,0.f,0.f,0.f,0.f,0.f,0.f }
	    ;

    /* System generated locals */
    integer inbuf_offset, lpbuf_offset, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    integer i_nint(real *);
    double sqrt(doublereal);

    /* Local variables */
    real ar_b__, ar_f__;
    static integer fbue, fbve, lbue, lbve;
    integer snrl, i__;
    static integer ofbue, sfbue;
    static real voice[6]	/* was [2][3] */;
    static integer olbue, slbue;
    real value[9];
    integer zc;
    logical ot;
    real qs;
    static real maxmin;
    integer vstate;
    real rc1;
    extern /* Subroutine */ int vparms_(integer *, real *, real *, integer *, 
	    integer *, real *, integer *, integer *, integer *, integer *, 
	    real *, real *, real *, real *);
    integer fbe, lbe;
    static real snr;
    real snr2;

/* 	Global Variables: */
/*< 	INCLUDE 'contrl.fh' >*/
/*< 	INTEGER VWIN(2), BUFLIM(4) >*/
/*       Arguments */
/* $Log: voicin.c,v $
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
/*< 	REAL INBUF(BUFLIM(1):BUFLIM(2)) >*/
/*< 	REAL LPBUF(BUFLIM(3):BUFLIM(4)) >*/
/*< 	INTEGER HALF >*/
/*< 	REAL MINAMD, MAXAMD >*/
/*< 	INTEGER MINTAU >*/
/*< 	REAL IVRC(2) >*/
/*< 	INTEGER AF, OBOUND(AF), VOIBUF(2,0:AF) >*/
/* 	Parameters/constants */
/*< 	INTEGER REF >*/
/*< 	PARAMETER (REF = 3000) >*/
/*       Voicing coefficient and Linear Discriminant Analysis variables: 
*/
/*       Max number of VDC's and VDC levels */
/*< 	INTEGER MAXVDC, MXVDCL >*/
/*< 	PARAMETER (MAXVDC = 10, MXVDCL = 10) >*/
/*       The following are not Fortran PARAMETER's, but they are */
/*       initialized with DATA statements, and never modified. */
/*       Actual number of VDC's and levels */
/*< 	INTEGER NVDC, NVDCL >*/
/*< 	REAL VDC(MAXVDC, MXVDCL), VDCL(MXVDCL) >*/
/*       Local variables that need not be saved */
/*       Note: */

/*       VALUE(1) through VALUE(8) are assigned values, but VALUE(9) */
/*       never is.  Yet VALUE(9) is read in the loop that begins "DO I = 
*/
/*       1, 9" below.  I believe that this doesn't cause any problems in 
*/
/*       this subroutine, because all VDC(9,*) array elements are 0, and 
*/
/*       this is what is multiplied by VALUE(9) in all cases.  Still, it 
*/
/*       would save a multiplication to change the loop to "DO I = 1, 8". 
*/
/*< 	INTEGER ZC, LBE, FBE >*/
/*< 	INTEGER I, SNRL, VSTATE >*/
/*< 	REAL SNR2 >*/
/*< 	REAL QS, RC1, AR_B, AR_F >*/
/*< 	REAL VALUE(9) >*/
/*< 	LOGICAL OT >*/
/*       Local state */
/*       WARNING! */

/*       VOICE, SFBUE, and SLBUE should be saved from one invocation to */
/*       the next, but they are never given an initial value. */

/*       Does Fortran 77 specify some default initial value, like 0, or */
/*       is it undefined?  If it is undefined, then this code should be */
/*       corrected to specify an initial value. */

/*       For VOICE, note that it is "shifted" in the statement that */
/*       begins "IF (HALF .EQ. 1) THEN" below.  Also, uninitialized */
/*       values in the VOICE array can only affect entries in the VOIBUF 
*/
/*       array that are for the same frame, or for an older frame.  Thus 
*/
/*       the effects of uninitialized values in VOICE cannot linger on */
/*       for more than 2 or 3 frame times. */

/*       For SFBUE and SLBUE, the effects of uninitialized values can */
/*       linger on for many frame times, because their previous values */
/*       are exponentially decayed.  Thus it is more important to choose 
*/
/*       initial values for these variables.  I would guess that a */
/*       reasonable initial value for SFBUE is REF/16, the same as used */
/*       for FBUE and OFBUE.  Similarly, SLBUE can be initialized to */
/*       REF/32, the same as for LBUE and OLBUE. */

/*       These guessed initial values should be validated by re-running */
/*       the modified program on some audio samples. */

/*< 	REAL DITHER, SNR >*/
/*< 	REAL MAXMIN >*/
/*< 	REAL VOICE(2,3) >*/
/*   Declare and initialize filters: */
/*< 	INTEGER LBVE, LBUE, FBVE, FBUE, OFBUE, OLBUE, SFBUE, SLBUE >*/
/*< 	LOGICAL FIRST >*/
/*< 	DATA FIRST /.TRUE./, DITHER/20/ >*/
    /* Parameter adjustments */
    if (vwin) {
	--vwin;
	}
    if (buflim) {
	--buflim;
	}
    if (inbuf) {
	inbuf_offset = buflim[1];
	inbuf -= inbuf_offset;
	}
    if (lpbuf) {
	lpbuf_offset = buflim[3];
	lpbuf -= lpbuf_offset;
	}
    if (ivrc) {
	--ivrc;
	}
    if (obound) {
	--obound;
	}
    if (voibuf) {
	--voibuf;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initvoicin;
	}


/*       The following variables are saved from one invocation to the */
/*       next, but are not initialized with DATA statements.  This is */
/*       acceptable, because FIRST is initialized ot .TRUE., and the */
/*       first time that this subroutine is then called, they are all */
/*       given initial values. */

/*       SNR */
/*       LBVE, LBUE, FBVE, FBUE, OFBUE, OLBUE */

/*       MAXMIN is initialized on the first call, assuming that HALF */
/*       .EQ. 1 on first call.  This is how ANALYS calls this subroutine. 
*/

/*< 	SAVE DITHER, SNR >*/
/*< 	SAVE MAXMIN >*/
/*< 	SAVE VOICE >*/
/*< 	SAVE LBVE, LBUE, FBVE, FBUE, OFBUE, OLBUE, SFBUE, SLBUE >*/
/*< 	SAVE FIRST >*/
/*   Voicing Decision Parameter vector (* denotes zero coefficient): */

/* 	* MAXMIN */
/* 	  LBE/LBVE */
/* 	  ZC */
/* 	  RC1 */
/* 	  QS */
/* 	  IVRC2 */
/* 	  aR_B */
/* 	  aR_F */
/* 	* LOG(LBE/LBVE) */
/*  Define 2-D voicing decision coefficient vector according to the voicin
g*/
/*  parameter order above.  Each row (VDC vector) is optimized for a speci
fic*/
/*   SNR.  The last element of the vector is the constant. */
/* 	         E    ZC    RC1    Qs   IVRC2  aRb   aRf        c */
/*< 	D >*/
/*< 	DATA NVDC/10/, NVDCL/5/ >*/
/*< 	DATA VDCL/600,450,300,200,6*0/ >*/
/*< 	IF (FIRST) THEN >*/
    if (first) {
/*< 	   LBVE = REF >*/
	lbve = 3000;
/*< 	   FBVE = REF >*/
	fbve = 3000;
/*< 	   FBUE = REF/16 >*/
	fbue = 187;
/*< 	   OFBUE = REF/16 >*/
	ofbue = 187;
/*< 	   SFBUE = REF/16 >*/
	sfbue = 187;
/*< 	   LBUE = REF/32 >*/
	lbue = 93;
/*< 	   OLBUE = REF/32 >*/
	olbue = 93;
/*< 	   SLBUE = REF/32 >*/
	slbue = 93;
/*< 	   SNR = 64*(FBVE/FBUE) >*/
	snr = (real) (fbve / fbue << 6);
/*< 	   FIRST = .FALSE. >*/
	first = FALSE_;
/*< 	END IF >*/
    }
/*  The VOICE array contains the result of the linear discriminant functio
n*/
/*   (analog values).  The VOIBUF array contains the hard-limited binary 
*/
/*   voicing decisions.  The VOICE and VOIBUF arrays, according to FORTRAN
 */
/*   memory allocation, are addressed as: */

/* 	   (half-frame number, future-frame number) */

/* 	   |   Past    |  Present  |  Future1  |  Future2  | */
/* 	   | 1,0 | 2,0 | 1,1 | 2,1 | 1,2 | 2,2 | 1,3 | 2,3 |  --->  time */

/*   Update linear discriminant function history each frame: */
/*< 	IF (HALF .EQ. 1) THEN >*/
    if (*half == 1) {
/*< 	   VOICE(1,1)=VOICE(1,2) >*/
	voice[0] = voice[2];
/*< 	   VOICE(2,1)=VOICE(2,2) >*/
	voice[1] = voice[3];
/*< 	   VOICE(1,2)=VOICE(1,3) >*/
	voice[2] = voice[4];
/*< 	   VOICE(2,2)=VOICE(2,3) >*/
	voice[3] = voice[5];
/*< 	   MAXMIN = MAXAMD/MAX(MINAMD,1.) >*/
	maxmin = (real) (*maxamd / dmax(*minamd,1.f));
/*< 	END IF >*/
    }
/*   Calculate voicing parameters twice per frame: */
/*< 	C >*/
    vparms_(&vwin[1], &inbuf[inbuf_offset], &lpbuf[lpbuf_offset], &buflim[1], 
	    half, &dither, mintau, &zc, &lbe, &fbe, &qs, &rc1, &ar_b__, &
	    ar_f__);
/*   Estimate signal-to-noise ratio to select the appropriate VDC vector. 
*/
/*   The SNR is estimated as the running average of the ratio of the */
/*   running average full-band voiced energy to the running average */
/*   full-band unvoiced energy. SNR filter has gain of 63. */
/*< 	SNR = NINT( 63*( SNR + FBVE/FLOAT(MAX(FBUE,1)) )/64.) >*/
    r__1 = (snr + fbve / (real) max(fbue,1)) * 63 / 64.f;
    snr = (real) i_nint(&r__1);
/*< 	SNR2 = (SNR*FBUE)/MAX(LBUE,1) >*/
    snr2 = snr * fbue / max(lbue,1);
/*   Quantize SNR to SNRL according to VDCL thresholds. */
/*< 	SNRL = 1 >*/
    snrl = 1;
/*< 	DO SNRL = 1, NVDCL-1 >*/
    i__1 = nvdcl - 1;
    for (snrl = 1; snrl <= i__1; ++snrl) {
/*< 	   IF (SNR2 .GT. VDCL(SNRL)) GOTO 69 >*/
	if (snr2 > vdcl[snrl - 1]) {
	    goto L69;
	}
/*< 	END DO >*/
    }
/*   	(Note:  SNRL = NVDCL here) */
/*< 69	CONTINUE >*/
L69:
/*   Linear discriminant voicing parameters: */
/*< 	VALUE(1) = MAXMIN >*/
    value[0] = maxmin;
/*< 	VALUE(2) = FLOAT(LBE)/MAX(LBVE,1) >*/
    value[1] = (real) lbe / max(lbve,1);
/*< 	VALUE(3) = ZC >*/
    value[2] = (real) zc;
/*< 	VALUE(4) = RC1 >*/
    value[3] = rc1;
/*< 	VALUE(5) = QS >*/
    value[4] = qs;
/*< 	VALUE(6) = IVRC(2) >*/
    value[5] = ivrc[2];
/*< 	VALUE(7) = AR_B >*/
    value[6] = ar_b__;
/*< 	VALUE(8) = AR_F >*/
    value[7] = ar_f__;
/*   Evaluation of linear discriminant function: */
/*< 	VOICE(HALF,3) = VDC(10,SNRL) >*/
    voice[*half + 3] = vdc[snrl * 10 - 1];
/*< 	DO I = 1, 8 >*/
    for (i__ = 1; i__ <= 8; ++i__) {
/*< 	   VOICE(HALF,3) = VOICE(HALF,3) + VDC(I,SNRL)*VALUE(I) >*/
	voice[*half + 3] += vdc[i__ + snrl * 10 - 11] * value[i__ - 1];
/*< 	END DO >*/
    }
/*   Classify as voiced if discriminant > 0, otherwise unvoiced */
/*   Voicing decision for current half-frame:  1 = Voiced; 0 = Unvoiced */
/*< 	IF (VOICE(HALF,3) .GT. 0.0) THEN >*/
    if (voice[*half + 3] > 0.f) {
/*< 	   VOIBUF(HALF,3)=1 >*/
	voibuf[*half + 6] = 1;
/*< 	ELSE >*/
    } else {
/*< 	   VOIBUF(HALF,3)=0 >*/
	voibuf[*half + 6] = 0;
/*< 	END IF >*/
    }
/*   Skip voicing decision smoothing in first half-frame: */
/*     Give a value to VSTATE, so that trace statements below will print 
*/
/*     a consistent value from one call to the next when HALF .EQ. 1. */
/*     The value of VSTATE is not used for any other purpose when this is 
*/
/*     true. */
/*<         VSTATE = -1 >*/
    vstate = -1;
/*< 	IF (HALF .EQ. 1) GOTO 99 >*/
    if (*half == 1) {
	goto L99;
    }
/*   Voicing decision smoothing rules (override of linear combination): */

/* 	Unvoiced half-frames:  At least two in a row. */
/* 	-------------------- */

/* 	Voiced half-frames:    At least two in a row in one frame. */
/* 	-------------------    Otherwise at least three in a row. */
/* 			       (Due to the way transition frames are encoded) */

/* 	In many cases, the discriminant function determines how to smooth. */
/*	In the following chart, the decisions marked with a * may be overridden
.*/

/*   Voicing override of transitions at onsets: */
/* 	If a V/UV or UV/V voicing decision transition occurs within one-half 
*/
/* 	frame of an onset bounding a voicing window, then the transition is */
/* 	moved to occur at the onset. */

/* 	P	1F */
/* 	-----	----- */
/* 	0   0   0   0 */
/* 	0   0   0*  1	(If there is an onset there) */
/* 	0   0   1*  0*	(Based on 2F and discriminant distance) */
/* 	0   0   1   1 */
/* 	0   1*  0   0	(Always) */
/* 	0   1*  0*  1	(Based on discriminant distance) */
/* 	0*  1   1   0*	(Based on past, 2F, and discriminant distance) */
/* 	0   1*  1   1	(If there is an onset there) */
/* 	1   0*  0   0	(If there is an onset there) */
/* 	1   0   0   1 */
/* 	1   0*  1*  0	(Based on discriminant distance) */
/* 	1   0*  1   1	(Always) */
/* 	1   1   0   0 */
/* 	1   1   0*  1*	(Based on 2F and discriminant distance) */
/* 	1   1   1*  0	(If there is an onset there) */
/* 	1   1   1   1 */

/*   Determine if there is an onset transition between P and 1F. */
/*   OT (Onset Transition) is true if there is an onset between */
/*   P and 1F but not after 1F. */
/*< 	O >*/
    ot = ((obound[1] & 2) != 0 || obound[2] == 1) && (obound[3] & 1) == 0;
/*   Multi-way dispatch on voicing decision history: */
/*< 	VSTATE = VOIBUF(1,1)*8 + VOIBUF(2,1)*4 + VOIBUF(1,2)*2 + VOIBUF(2,2) >*/
    vstate = (voibuf[3] << 3) + (voibuf[4] << 2) + (voibuf[5] << 1) + voibuf[
	    6];
/*< 	GOTO (99,1,2,99,4,5,6,7,8,99,10,11,99,13,14,99) VSTATE+1 >*/
    switch (vstate + 1) {
	case 1:  goto L99;
	case 2:  goto L1;
	case 3:  goto L2;
	case 4:  goto L99;
	case 5:  goto L4;
	case 6:  goto L5;
	case 7:  goto L6;
	case 8:  goto L7;
	case 9:  goto L8;
	case 10:  goto L99;
	case 11:  goto L10;
	case 12:  goto L11;
	case 13:  goto L99;
	case 14:  goto L13;
	case 15:  goto L14;
	case 16:  goto L99;
    }
/*< 1	   IF (OT .AND. VOIBUF(1,3) .EQ. 1) VOIBUF(1,2) = 1 >*/
L1:
    if (ot && voibuf[7] == 1) {
	voibuf[5] = 1;
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 2	 >*/
L2:
    if (voibuf[7] == 0 || voice[2] < -voice[3]) {
/*< 	      VOIBUF(1,2) = 0 >*/
	voibuf[5] = 0;
/*< 	   ELSE >*/
    } else {
/*< 	      VOIBUF(2,2) = 1 >*/
	voibuf[6] = 1;
/*< 	   END IF >*/
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 4	   VOIBUF(2,1) = 0 >*/
L4:
    voibuf[4] = 0;
/*< 	GOTO 99 >*/
    goto L99;
/*< 5	   IF (VOICE(2,1) .LT. -VOICE(1,2)) THEN >*/
L5:
    if (voice[1] < -voice[2]) {
/*< 	      VOIBUF(2,1) = 0 >*/
	voibuf[4] = 0;
/*< 	   ELSE >*/
    } else {
/*< 	      VOIBUF(1,2) = 1 >*/
	voibuf[5] = 1;
/*< 	   END IF >*/
    }
/*< 	GOTO 99 >*/
    goto L99;
/*   VOIBUF(2,0) must be 0 */
/*< 6	 >*/
L6:
    if (voibuf[1] == 1 || voibuf[7] == 1 || voice[3] > voice[0]) {
/*< 	      VOIBUF(2,2) = 1 >*/
	voibuf[6] = 1;
/*< 	   ELSE >*/
    } else {
/*< 	      VOIBUF(1,1) = 1 >*/
	voibuf[3] = 1;
/*< 	   END IF >*/
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 7	   IF (OT) VOIBUF(2,1) = 0 >*/
L7:
    if (ot) {
	voibuf[4] = 0;
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 8	   IF (OT) VOIBUF(2,1) = 1 >*/
L8:
    if (ot) {
	voibuf[4] = 1;
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 10	   IF (VOICE(1,2) .LT. -VOICE(2,1)) THEN >*/
L10:
    if (voice[2] < -voice[1]) {
/*< 	      VOIBUF(1,2) = 0 >*/
	voibuf[5] = 0;
/*< 	   ELSE >*/
    } else {
/*< 	      VOIBUF(2,1) = 1 >*/
	voibuf[4] = 1;
/*< 	   END IF >*/
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 11	   VOIBUF(2,1) = 1 >*/
L11:
    voibuf[4] = 1;
/*< 	GOTO 99 >*/
    goto L99;
/*< 13	   IF (VOIBUF(1,3) .EQ. 0 .AND. VOICE(2,2) .LT. -VOICE(1,2)) THEN >*/
L13:
    if (voibuf[7] == 0 && voice[3] < -voice[2]) {
/*< 	      VOIBUF(2,2) = 0 >*/
	voibuf[6] = 0;
/*< 	   ELSE >*/
    } else {
/*< 	      VOIBUF(1,2) = 1 >*/
	voibuf[5] = 1;
/*< 	   END IF >*/
    }
/*< 	GOTO 99 >*/
    goto L99;
/*< 14	   IF (OT .AND. VOIBUF(1,3) .EQ. 0) VOIBUF(1,2) = 0 >*/
L14:
    if (ot && voibuf[7] == 0) {
	voibuf[5] = 0;
    }
/* 	GOTO 99 */
/*< 99	CONTINUE >*/
L99:
/*   Now update parameters: */
/*   ---------------------- */

/*  During unvoiced half-frames, update the low band and full band unvoice
d*/
/*   energy estimates (LBUE and FBUE) and also the zero crossing */
/*   threshold (DITHER).  (The input to the unvoiced energy filters is */
/*   restricted to be less than 10dB above the previous inputs of the */
/*   filters.) */
/*   During voiced half-frames, update the low-pass (LBVE) and all-pass */
/*   (FBVE) voiced energy estimates. */
/*< 	IF (VOIBUF(HALF,3) .EQ. 0) THEN >*/
    if (voibuf[*half + 6] == 0) {
/*< 	   SFBUE = NINT(( 63*SFBUE + 8*MIN(FBE,3*OFBUE) )/64.) >*/
/* Computing MIN */
	i__1 = fbe, i__2 = ofbue * 3;
	r__1 = (sfbue * 63 + (min(i__1,i__2) << 3)) / 64.f;
	sfbue = i_nint(&r__1);
/*< 	   FBUE = SFBUE/8 >*/
	fbue = sfbue / 8;
/*< 	   OFBUE = FBE >*/
	ofbue = fbe;
/*< 	   SLBUE = NINT(( 63*SLBUE + 8*MIN(LBE,3*OLBUE) )/64.) >*/
/* Computing MIN */
	i__1 = lbe, i__2 = olbue * 3;
	r__1 = (slbue * 63 + (min(i__1,i__2) << 3)) / 64.f;
	slbue = i_nint(&r__1);
/*< 	   LBUE = SLBUE/8 >*/
	lbue = slbue / 8;
/*< 	   OLBUE = LBE >*/
	olbue = lbe;
/*< 	ELSE >*/
    } else {
/*< 	   LBVE = NINT(( 63*LBVE + LBE )/64.) >*/
	r__1 = (lbve * 63 + lbe) / 64.f;
	lbve = i_nint(&r__1);
/*< 	   FBVE = NINT(( 63*FBVE + FBE )/64.) >*/
	r__1 = (fbve * 63 + fbe) / 64.f;
	fbve = i_nint(&r__1);
/*< 	END IF >*/
    }
/*   Set dither threshold to yield proper zero crossing rates in the */
/*   presence of low frequency noise and low level signal input. */
/*   NOTE: The divisor is a function of REF, the expected energies. */
/*< 	DITHER = MIN(MAX( 64*SQRT(FLOAT(LBUE*LBVE)) / REF,1.),20.) >*/
/* Computing MIN */
/* Computing MAX */
    r__2 = (real) (sqrt((real) (lbue * lbve)) * 64 / 3000);
    r__1 = (real) dmax(r__2,1.f);
    dither = (real) dmin(r__1,20.f);
/*   Print Test Data */
/* 	IF( LISTL.GE.3 ) THEN */
/* 	   IF(HALF.EQ.1) WRITE(FDEBUG,930) VWIN,MINAMD,MAXAMD,MINTAU,IVRC */
/* 930	   FORMAT(' Voicing:VWIN     MINA     MAXA  MINTAU  IVRC1  IVRC2'/ 
*/
/*     1    5X,2I4,2F9.1,I8,2F7.3/ */
/*     1    ' HALF  DISCR  MAX/MIN  LE/LVE   ZC    RC1     QS   IVRC2' */
/*     1    '   aR_B   aR_F : DITH  LBE   FBE  LBVE  FBVE  LBUE  FBUE', */
/*     1    '     SNR SNRL VS OT') */
/* 	   WRITE(FDEBUG,940) HALF, VOICE(HALF,3), (VALUE(I),I=1,8), DITHER, */
/*     1    LBE, FBE, LBVE, FBVE, LBUE, FBUE, */
/*     1    SNR, SNRL, VSTATE, OT */
/* 940	   FORMAT(1X,I3,':',F8.0,F9.1,F7.3,F7.2,5F7.3,F5.1,6I6,F9.1,2I3,L3)
 */
/* 	END IF */
/*   Voicing decisions are returned in VOIBUF. */
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITVOICIN () >*/

L_initvoicin:
/*< 	FIRST = .TRUE. >*/
    first = TRUE_;
/*< 	DITHER = 20 >*/
    dither = 20.f;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* voicin_ */

/* Subroutine */ int voicin_(integer *vwin, real *inbuf, real *lpbuf, integer 
	*buflim, integer *half, real *minamd, real *maxamd, integer *mintau, 
	real *ivrc, integer *obound, integer *voibuf, integer *af)
{
    return voicin_0_(0, vwin, inbuf, lpbuf, buflim, half, minamd, maxamd, 
	    mintau, ivrc, obound, voibuf, af);
    }

/* Subroutine */ int initvoicin_(void)
{
    return voicin_0_(1, (integer *)0, (real *)0, (real *)0, (integer *)0, (
	    integer *)0, (real *)0, (real *)0, (integer *)0, (real *)0, (
	    integer *)0, (integer *)0, (integer *)0);
    }

