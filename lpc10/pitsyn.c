/*
 * pitsyn.c
 *
 * SCCS ID:  @(#)pitsyn.c 1.2 96/05/19
 */

#include "f2c.h"

extern int pitsyn_(integer *order, integer *voice, integer *pitch, real *rms, real *rc, integer *lframe, integer *ivuv, integer *ipiti, real *rmsi, real *rci, integer *nout, real *ratio);
extern int initpitsyn_(void);

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

/* ***************************************************************** */

/* 	PITSYN Version 53 */

/* $Log: pitsyn.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.2  1996/03/25  18:49:07  jaf */
/* Added commments about which indices of array arguments are read or */
/* written. */

/* Rearranged local variable declarations to indicate which need to be */
/* saved from one invocation to the next.  Added entry INITPITSYN to */
/* reinitialize local state variables, if desired. */

/* Added lots of comments about proving that the maximum number of pitch */
/* periods (NOUT) that can be returned is 16.  The call to STOP that */
/* could happen if NOUT got too large was removed as a result. */

/* Also proved that the total number of samples returned from N calls, */
/* each with identical values of LFRAME, will always be in the range */
/* N*LFRAME-MAXPIT+1 to N*LFRAME. */

/* Revision 1.1  1996/02/07 14:48:18  jaf */
/* Initial revision */


/* ***************************************************************** */

/*   Synthesize a single pitch epoch */

/* Input: */
/*  ORDER  - Synthesis order (number of RC's) */
/*  VOICE  - Half frame voicing decisions */
/*           Indices 1 through 2 read. */
/*  LFRAME - Length of speech buffer */
/* Input/Output: */
/*  PITCH  - Pitch */
/*           This value should be in the range MINPIT (20) to MAXPIT */
/*           (156), inclusive. */
/*           PITCH can be modified under some conditions. */
/*  RMS    - Energy  (can be modified) */
/*           RMS is changed to 1 if the value passed in is less than 1. */
/*  RC     - Reflection coefficients */
/*           Indices 1 through ORDER can be temporarily overwritten with */
/*           RCO, and then replaced with original values, under some */
/*           conditions. */
/* Output: */
/*  IVUV   - Pitch epoch voicing decisions */
/*           Indices (I) of IVUV, IPITI, and RMSI are written, */
/*           and indices (J,I) of RCI are written, */
/*           where I ranges from 1 to NOUT, and J ranges from 1 to ORDER. */
/*  IPITI  - Pitch epoch length */
/*  RMSI   - Pitch epoch energy */
/*  RCI    - Pitch epoch RC's */
/*  NOUT   - Number of pitch periods in this frame */
/*           This is at least 0, at least 1 if MAXPIT .LT. LFRAME (this */
/*           is currently true on every call), and can never be more than */
/*           (LFRAME+MAXPIT-1)/PITCH, which is currently 16 with */
/*           LFRAME=180, MAXPIT=156, and PITCH .GE. 20, as SYNTHS */
/*           guarantees when it calls this subroutine. */
/*  RATIO  - Previous to present energy ratio */
/*           Always assigned a value. */

/*< 	S >*/
/* Subroutine */ int pitsyn_0_(int n__, integer *order, integer *voice, 
	integer *pitch, real *rms, real *rc, integer *lframe, integer *ivuv, 
	integer *ipiti, real *rmsi, real *rci, integer *nout, real *ratio)
{
    /* Initialized data */

    static real rmso = 1.f;
    static logical first = TRUE_;

    /* System generated locals */
    integer rci_dim1, rci_offset, i__1, i__2;
    real r__1;

    /* Builtin functions */
    double log(doublereal), exp(doublereal);

    /* Local variables */
    real alrn, alro, yarc[10], prop;
    integer i__, j, vflag, jused, lsamp;
    static integer jsamp;
    real slope;
    static integer ipito;
    real uvpit;
    integer ip, nl, ivoice;
    static integer ivoico;
    integer istart;
    static real rco[10];
    real xxy;

/*< 	INCLUDE 'config.fh' >*/
/*< 	INTEGER ORDER, VOICE(2), PITCH >*/
/*       Arguments */
/* $Log: pitsyn.c,v $
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
/*< 	REAL RMS, RC(ORDER) >*/
/*< 	INTEGER LFRAME >*/
/*< 	INTEGER IVUV(16), IPITI(16) >*/
/*< 	REAL RMSI(16), RCI(ORDER,16) >*/
/*< 	INTEGER NOUT >*/
/*< 	REAL RATIO >*/
/*       Local variables that need not be saved */
/*       LSAMP is initialized in the IF (FIRST) THEN clause, but it is */
/*       not used the first time through, and it is given a value before 
*/
/*       use whenever FIRST is .FALSE., so it appears unnecessary to */
/*       assign it a value when FIRST is .TRUE. */
/*< 	INTEGER I, J, LSAMP, IP, ISTART, IVOICE >*/
/*< 	INTEGER JUSED, NL >*/
/*< 	REAL ALRN, ALRO, PROP >*/
/*< 	REAL SLOPE, UVPIT, XXY >*/
/*< 	INTEGER VFLAG >*/
/*< 	REAL YARC(MAXORD) >*/
/*       Local state */
/* FIRST  - .TRUE. only on first call to PITSYN. */
/* IVOICO - Previous VOICE(2) value. */
/* IPITO  - Previous PITCH value. */
/* RMSO   - Previous RMS value. */
/* RCO    - Previous RC values. */

/* JSAMP  - If this routine is called N times with identical values of */
/*          LFRAME, then the total length of all pitch periods returned */
/*          is always N*LFRAME-JSAMP, and JSAMP is always in the range 0 
*/
/*          to MAXPIT-1 (see below for why this is so).  Thus JSAMP is */
/*          the number of samples "left over" from the previous call to */
/*          PITSYN, that haven't been "used" in a pitch period returned */
/*          from this subroutine.  Every time this subroutine is called, 
*/
/*          it returns pitch periods with a total length of at most */
/*          LFRAME+JSAMP. */

/* IVOICO, IPITO, RCO, and JSAMP need not be assigned an initial value */
/* with a DATA statement, because they are always initialized on the */
/* first call to PITSYN. */

/* FIRST and RMSO should be initialized with DATA statements, because */
/* even on the first call, they are used before being initialized. */
/*< 	INTEGER IVOICO, IPITO >*/
/*< 	REAL RMSO >*/
/*< 	REAL RCO(MAXORD) >*/
/*< 	INTEGER JSAMP >*/
/*< 	LOGICAL FIRST >*/
/*< 	SAVE IVOICO, IPITO, RMSO, RCO, JSAMP, FIRST >*/
/*< 	DATA RMSO /1./ >*/
    /* Parameter adjustments */
    if (rc) {
	--rc;
	}
    if (rci) {
	rci_dim1 = *order;
	rci_offset = rci_dim1 + 1;
	rci -= rci_offset;
	}
    if (voice) {
	--voice;
	}
    if (ivuv) {
	--ivuv;
	}
    if (ipiti) {
	--ipiti;
	}
    if (rmsi) {
	--rmsi;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_initpitsyn;
	}

/*< 	DATA FIRST /.TRUE./ >*/
/*< 	IF (RMS .LT. 1) RMS = 1 >*/
    if (*rms < 1.f) {
	*rms = 1.f;
    }
/*< 	IF (RMSO .LT. 1) RMSO = 1 >*/
    if (rmso < 1.f) {
	rmso = 1.f;
    }
/*< 	UVPIT = 0.0 >*/
    uvpit = 0.f;
/*< 	RATIO = RMS/(RMSO+8.) >*/
    *ratio = *rms / (rmso + 8.f);
/*< 	IF (FIRST) THEN >*/
    if (first) {
/*< 	   LSAMP = 0 >*/
	lsamp = 0;
/*< 	   IVOICE = VOICE(2) >*/
	ivoice = voice[2];
/*< 	   IF (IVOICE .EQ. 0) PITCH = LFRAME/4 >*/
	if (ivoice == 0) {
	    *pitch = *lframe / 4;
	}
/*< 	   NOUT = LFRAME/PITCH >*/
	*nout = *lframe / *pitch;
/*< 	   JSAMP = LFRAME - NOUT*PITCH >*/
	jsamp = *lframe - *nout * *pitch;

/*          SYNTHS only calls this subroutine with PITCH in the range 
20 */
/*          to 156.  LFRAME = MAXFRM = 180, so NOUT is somewhere in th
e */
/*          range 1 to 9. */

/*          JSAMP is "LFRAME mod PITCH", so it is in the range 0 to */
/*          (PITCH-1), or 0 to MAXPIT-1=155, after the first call. */

/*< 	   DO I = 1,NOUT >*/
	i__1 = *nout;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	      DO J = 1,ORDER >*/
	    i__2 = *order;
	    for (j = 1; j <= i__2; ++j) {
/*< 	         RCI(J,I) = RC(J) >*/
		rci[j + i__ * rci_dim1] = rc[j];
/*< 	      END DO >*/
	    }
/*< 	      IVUV(I) = IVOICE >*/
	    ivuv[i__] = ivoice;
/*< 	      IPITI(I) = PITCH >*/
	    ipiti[i__] = *pitch;
/*< 	      RMSI(I) = RMS >*/
	    rmsi[i__] = *rms;
/*< 	   END DO >*/
	}
/*< 	   FIRST = .FALSE. >*/
	first = FALSE_;
/*< 	ELSE >*/
    } else {
/*< 	   VFLAG = 0 >*/
	vflag = 0;
/*< 	   LSAMP = LFRAME + JSAMP >*/
	lsamp = *lframe + jsamp;
/*< 	   SLOPE = (PITCH-IPITO)/FLOAT(LSAMP) >*/
	slope = (*pitch - ipito) / (real) lsamp;
/*< 	   NOUT = 0 >*/
	*nout = 0;
/*< 	   JUSED = 0 >*/
	jused = 0;
/*< 	   ISTART = 1 >*/
	istart = 1;
/*< 	   IF ((VOICE(1) .EQ. IVOICO) .AND. (VOICE(2) .EQ. VOICE(1))) THEN >*/
	if (voice[1] == ivoico && voice[2] == voice[1]) {
/*< 	      IF (VOICE(2) .EQ. 0) THEN >*/
	    if (voice[2] == 0) {
/* SSUV - -   0  ,  0  ,  0 */
/*< 	         PITCH = LFRAME/4 >*/
		*pitch = *lframe / 4;
/*< 	         IPITO = PITCH >*/
		ipito = *pitch;
/*< 	         IF (RATIO .GT. 8) RMSO = RMS >*/
		if (*ratio > 8.f) {
		    rmso = *rms;
		}
/*< 	      END IF >*/
	    }
/* SSVC - -   1  ,  1  ,  1 */
/*< 	      SLOPE = (PITCH-IPITO)/FLOAT(LSAMP) >*/
	    slope = (*pitch - ipito) / (real) lsamp;
/*< 	      IVOICE = VOICE(2) >*/
	    ivoice = voice[2];
/*< 	   ELSE >*/
	} else {
/*< 	      IF (IVOICO .NE. 1) THEN >*/
	    if (ivoico != 1) {
/*< 	         IF (IVOICO .EQ. VOICE(1)) THEN >*/
		if (ivoico == voice[1]) {
/* UV2VC2 - -  0  ,  0  ,  1 */
/*< 	            NL = LSAMP - LFRAME/4 >*/
		    nl = lsamp - *lframe / 4;
/*< 	         ELSE >*/
		} else {
/* UV2VC1 - -  0  ,  1  ,  1 */
/*< 	            NL = LSAMP - 3*LFRAME/4 >*/
		    nl = lsamp - *lframe * 3 / 4;
/*< 	         ENDIF >*/
		}
/*< 	         IPITI(1) = NL/2 >*/
		ipiti[1] = nl / 2;
/*< 	         IPITI(2) = NL - IPITI(1) >*/
		ipiti[2] = nl - ipiti[1];
/*< 	         IVUV(1) = 0 >*/
		ivuv[1] = 0;
/*< 	         IVUV(2) = 0 >*/
		ivuv[2] = 0;
/*< 	         RMSI(1) = RMSO >*/
		rmsi[1] = rmso;
/*< 	         RMSI(2) = RMSO >*/
		rmsi[2] = rmso;
/*< 	         DO I = 1,ORDER >*/
		i__1 = *order;
		for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	            RCI(I,1) = RCO(I) >*/
		    rci[i__ + rci_dim1] = rco[i__ - 1];
/*< 	            RCI(I,2) = RCO(I) >*/
		    rci[i__ + (rci_dim1 << 1)] = rco[i__ - 1];
/*< 	            RCO(I)   = RC(I) >*/
		    rco[i__ - 1] = rc[i__];
/*< 	         END DO >*/
		}
/*< 	         SLOPE = 0 >*/
		slope = 0.f;
/*< 	         NOUT = 2 >*/
		*nout = 2;
/*< 	         IPITO = PITCH >*/
		ipito = *pitch;
/*< 	         JUSED = NL >*/
		jused = nl;
/*< 	         ISTART = NL + 1 >*/
		istart = nl + 1;
/*< 	         IVOICE = 1 >*/
		ivoice = 1;
/*< 	      ELSE >*/
	    } else {
/*< 	         IF (IVOICO .NE. VOICE(1)) THEN >*/
		if (ivoico != voice[1]) {
/* VC2UV1 - -   1  ,  0  ,  0 */
/*< 	            LSAMP = LFRAME/4 + JSAMP >*/
		    lsamp = *lframe / 4 + jsamp;
/*< 	         ELSE >*/
		} else {
/* VC2UV2 - -   1  ,  1  ,  0 */
/*< 	            LSAMP = 3*LFRAME/4 + JSAMP >*/
		    lsamp = *lframe * 3 / 4 + jsamp;
/*< 	         END IF >*/
		}
/*< 	         DO I = 1,ORDER >*/
		i__1 = *order;
		for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	            YARC(I) = RC(I) >*/
		    yarc[i__ - 1] = rc[i__];
/*< 	            RC(I) = RCO(I) >*/
		    rc[i__] = rco[i__ - 1];
/*< 	         END DO >*/
		}
/*< 	         IVOICE = 1 >*/
		ivoice = 1;
/*< 	         SLOPE = 0. >*/
		slope = 0.f;
/*< 	         VFLAG = 1 >*/
		vflag = 1;
/*< 	      END IF >*/
	    }
/*< 	   END IF >*/
	}
/* Here is the value of most variables that are used below, depending 
on */
/* the values of IVOICO, VOICE(1), and VOICE(2).  VOICE(1) and VOICE(2
) */
/* are input arguments, and IVOICO is the value of VOICE(2) on the */
/* previous call (see notes for the IF (NOUT .NE. 0) statement near th
e */
/* end).  Each of these three values is either 0 or 1.  These three */
/* values below are given as 3-bit long strings, in the order IVOICO, 
*/
/* VOICE(1), and VOICE(2).  It appears that the code above assumes tha
t */
/* the bit sequences 010 and 101 never occur, but I wonder whether a 
*/
/* large enough number of bit errors in the channel could cause such a
 */
/* thing to happen, and if so, could that cause NOUT to ever go over 1
1? */

/* Note that all of the 180 values in the table are really LFRAME, but
 */
/* 180 has fewer characters, and it makes the table a little more */
/* concrete.  If LFRAME is ever changed, keep this in mind.  Similarly
, */
/* 135's are 3*LFRAME/4, and 45's are LFRAME/4.  If LFRAME is not a */
/* multiple of 4, then the 135 for NL-JSAMP is actually LFRAME-LFRAME/
4, */
/* and the 45 for NL-JSAMP is actually LFRAME-3*LFRAME/4. */

/* Note that LSAMP-JSAMP is given as the variable.  This was just for 
*/
/* brevity, to avoid adding "+JSAMP" to all of the column entries. */
/* Similarly for NL-JSAMP. */

/* Variable    | 000  001    011,010  111       110       100,101 */
/* ------------+-------------------------------------------------- */
/* ISTART      | 1    NL+1   NL+1     1         1         1 */
/* LSAMP-JSAMP | 180  180    180      180       135       45 */
/* IPITO       | 45   PITCH  PITCH    oldPITCH  oldPITCH  oldPITCH */
/* SLOPE       | 0    0      0        seebelow  0         0 */
/* JUSED       | 0    NL     NL       0         0         0 */
/* PITCH       | 45   PITCH  PITCH    PITCH     PITCH     PITCH */
/* NL-JSAMP    | --   135    45       --        --        -- */
/* VFLAG       | 0    0      0        0         1         1 */
/* NOUT        | 0    2      2        0         0         0 */
/* IVOICE      | 0    1      1        1         1         1 */

/* while_loop  | once once   once     once      twice     twice */

/* ISTART      | --   --     --       --        JUSED+1   JUSED+1 */
/* LSAMP-JSAMP | --   --     --       --        180       180 */
/* IPITO       | --   --     --       --        oldPITCH  oldPITCH */
/* SLOPE       | --   --     --       --        0         0 */
/* JUSED       | --   --     --       --        ??        ?? */
/* PITCH       | --   --     --       --        PITCH     PITCH */
/* NL-JSAMP    | --   --     --       --        --        -- */
/* VFLAG       | --   --     --       --        0         0 */
/* NOUT        | --   --     --       --        ??        ?? */
/* IVOICE      | --   --     --       --        0         0 */


/* UVPIT is always 0.0 on the first pass through the DO WHILE (.TRUE.)
 */
/* loop below. */

/* The only possible non-0 value of SLOPE (in column 111) is */
/* (PITCH-IPITO)/FLOAT(LSAMP) */

/* Column 101 is identical to 100.  Any good properties we can prove 
*/
/* for 100 will also hold for 101.  Similarly for 010 and 011. */

/* SYNTHS calls this subroutine with PITCH restricted to the range 20 
to */
/* 156.  IPITO is similarly restricted to this range, after the first 
*/
/* call.  IP below is also restricted to this range, given the */
/* definitions of IPITO, SLOPE, UVPIT, and that I is in the range ISTA
RT */
/* to LSAMP. */

/*< 	   DO WHILE (.TRUE.) >*/
	while(TRUE_) {

/*             JUSED is the total length of all pitch periods curr
ently */
/*             in the output arrays, in samples. */

/*             An invariant of the DO I = ISTART,LSAMP loop below,
 under */
/*             the condition that IP is always in the range 1 thro
ugh */
/*             MAXPIT, is: */

/*             (I - MAXPIT) .LE. JUSED .LE. (I-1) */

/*             Note that the final value of I is LSAMP+1, so that 
after */
/*             the DO loop is complete, we know: */

/*             (LSAMP - MAXPIT + 1) .LE. JUSED .LE. LSAMP */

/*< 	      DO I = ISTART,LSAMP >*/
	    i__1 = lsamp;
	    for (i__ = istart; i__ <= i__1; ++i__) {
/*< 	         IP = IPITO + SLOPE*I + .5 >*/
		r__1 = ipito + slope * i__;
		ip = (integer) (r__1 + .5f);
/*< 	         IF (UVPIT .NE. 0.0) IP = UVPIT >*/
		if (uvpit != 0.f) {
		    ip = (integer) uvpit;
		}
/*< 	         IF (IP .LE. I-JUSED) THEN >*/
		if (ip <= i__ - jused) {
/*< 	            NOUT = NOUT + 1 >*/
		    ++(*nout);

/*                   The following check is no longer nece
ssary, now that */
/*                   we can prove that NOUT will never go 
over 16. */

/* 		    IF (NOUT .GT. 16) STOP 'PITSYN: too many epochs' 
*/

/*< 	            IPITI(NOUT) = IP >*/
		    ipiti[*nout] = ip;
/*< 	            PITCH = IP >*/
		    *pitch = ip;
/*< 	            IVUV(NOUT) = IVOICE >*/
		    ivuv[*nout] = ivoice;
/*< 	            JUSED = JUSED + IP >*/
		    jused += ip;
/*< 	            PROP = (JUSED-IP/2)/FLOAT(LSAMP) >*/
		    prop = (jused - ip / 2) / (real) lsamp;
/*< 	            DO J = 1,ORDER >*/
		    i__2 = *order;
		    for (j = 1; j <= i__2; ++j) {
/*< 	               ALRO = ALOG((1+RCO(J))/(1-RCO(J))) >*/
			alro = (real) log((rco[j - 1] + 1) / (1 - rco[j - 1]));
/*< 	               ALRN = ALOG((1+RC(J))/(1-RC(J))) >*/
			alrn = (real) log((rc[j] + 1) / (1 - rc[j]));
/*< 	               XXY = ALRO + PROP*(ALRN-ALRO) >*/
			xxy = alro + prop * (alrn - alro);
/*< 	               XXY = EXP(XXY) >*/
			xxy = (real) exp(xxy);
/*< 	               RCI(J,NOUT) = (XXY-1)/(XXY+1) >*/
			rci[j + *nout * rci_dim1] = (xxy - 1) / (xxy + 1);
/*< 	            END DO >*/
		    }
/*< 	            RMSI(NOUT) = ALOG(RMSO) + PROP*(ALOG(RMS)-ALOG(RMSO)) >*/
		    rmsi[*nout] = (real) (log(rmso) + prop * (log(*rms) - log(rmso)));
/*< 	            RMSI(NOUT) = EXP(RMSI(NOUT)) >*/
		    rmsi[*nout] = (real) exp(rmsi[*nout]);
/*< 	         END IF >*/
		}
/*< 	      END DO >*/
	    }
/*< 	      IF (VFLAG .NE. 1) GOTO 100 >*/
	    if (vflag != 1) {
		goto L100;
	    }

/*             I want to prove what range UVPIT must lie in after 
the */
/*             assignments to it below.  To do this, I must determ
ine */
/*             what range (LSAMP-ISTART) must lie in, after the */
/*             assignments to ISTART and LSAMP below. */

/*             Let oldLSAMP be the value of LSAMP at this point in
 the */
/*             execution.  This is 135+JSAMP in state 110, or 45+J
SAMP in */
/*             states 100 or 101. */

/*             Given the loop invariant on JUSED above, we know th
at: */

/*             (oldLSAMP - MAXPIT + 1) .LE. JUSED .LE. oldLSAMP */

/*             ISTART is one more than this. */

/*             Let newLSAMP be the value assigned to LSAMP below. 
 This */
/*             is 180+JSAMP.  Thus (newLSAMP-oldLSAMP) is either 4
5 or */
/*             135, depending on the state. */

/*             Thus, the range of newLSAMP-ISTART is: */

/*             (newLSAMP-(oldLSAMP+1)) .LE. newLSAMP-ISTART */
/*             .LE. (newLSAMP-(oldLSAMP - MAXPIT + 2)) */

/*             or: */

/*             46 .LE. newLSAMP-ISTART .LE. 133+MAXPIT .EQ. 289 */

/*             Therefore, UVPIT is in the range 23 to 144 after th
e first */
/*             assignment to UVPIT below, and after the conditiona
l */
/*             assignment, it is in the range 23 to 90. */

/*             The important thing is that it is in the range 20 t
o 156, */
/*             so that in the loop above, IP is always in this ran
ge. */

/*< 	      VFLAG = 0 >*/
	    vflag = 0;
/*< 	      ISTART = JUSED + 1 >*/
	    istart = jused + 1;
/*< 	      LSAMP = LFRAME + JSAMP >*/
	    lsamp = *lframe + jsamp;
/*< 	      SLOPE = 0 >*/
	    slope = 0.f;
/*< 	      IVOICE = 0 >*/
	    ivoice = 0;
/*< 	      UVPIT = (LSAMP-ISTART)/2 >*/
	    uvpit = (real) ((lsamp - istart) / 2);
/*< 	      IF (UVPIT .GT. 90) UVPIT = UVPIT/2 >*/
	    if (uvpit > 90.f) {
		uvpit /= 2;
	    }
/*< 	      RMSO = RMS >*/
	    rmso = *rms;
/*< 	      DO I = 1,ORDER >*/
	    i__1 = *order;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	         RC(I) = YARC(I) >*/
		rc[i__] = yarc[i__ - 1];
/*< 	         RCO(I) = YARC(I) >*/
		rco[i__ - 1] = yarc[i__ - 1];
/*< 	      END DO >*/
	    }
/*< 	   END DO >*/
	}
/*< 100	   JSAMP = LSAMP - JUSED >*/
L100:
	jsamp = lsamp - jused;
/*< 	END IF >*/
    }
/*       Given that the maximum pitch period MAXPIT .LT. LFRAME (this is 
*/
/*       currently true on every call, since SYNTHS always sets */
/*       LFRAME=180), NOUT will always be .GE. 1 at this point. */
/*< 	IF (NOUT .NE. 0) THEN >*/
    if (*nout != 0) {
/*< 	   IVOICO = VOICE(2) >*/
	ivoico = voice[2];
/*< 	   IPITO = PITCH >*/
	ipito = *pitch;
/*< 	   RMSO = RMS >*/
	rmso = *rms;
/*< 	   DO I = 1,ORDER >*/
	i__1 = *order;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*< 	      RCO(I) = RC(I) >*/
	    rco[i__ - 1] = rc[i__];
/*< 	   END DO >*/
	}
/*< 	END IF >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	ENTRY INITPITSYN () >*/

L_initpitsyn:
/*< 	RMSO = 1. >*/
    rmso = 1.f;
/*< 	FIRST = .TRUE. >*/
    first = TRUE_;
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* pitsyn_ */

/* Subroutine */ int pitsyn_(integer *order, integer *voice, integer *pitch, 
	real *rms, real *rc, integer *lframe, integer *ivuv, integer *ipiti, 
	real *rmsi, real *rci, integer *nout, real *ratio)
{
    return pitsyn_0_(0, order, voice, pitch, rms, rc, lframe, ivuv, ipiti, 
	    rmsi, rci, nout, ratio);
    }

/* Subroutine */ int initpitsyn_(void)
{
    return pitsyn_0_(1, (integer *)0, (integer *)0, (integer *)0, (real *)0, (
	    real *)0, (integer *)0, (integer *)0, (integer *)0, (real *)0, (
	    real *)0, (integer *)0, (real *)0);
    }

