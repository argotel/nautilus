/*
 * placea.c
 *
 * SCCS ID:  @(#)placea.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int placea_(integer *ipitch, integer *voibuf, integer *obound, integer *af, integer *vwin, integer *awin, integer *ewin, integer *lframe, integer *maxwin);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* *********************************************************************** */

/* 	PLACEA Version 48 */

/* $Log: placea.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.5  1996/03/19  20:41:55  jaf */
/* Added some conditions satisfied by the output values in EWIN. */

/* Revision 1.4  1996/03/19  20:24:17  jaf */
/* Added some conditions satisfied by the output values in AWIN. */

/* Revision 1.3  1996/03/18  21:40:04  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  16:43:09  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:48:31  jaf */
/* Initial revision */


/* *********************************************************************** */
/* Input: */
/*  IPITCH */
/*  VOIBUF */
/*          Indices (2,AF-2), (1,AF-1), (2,AF-1), (1,AF), and (2,AF) read.*/
/*           All other indices untouched. */
/*  OBOUND */
/*  AF */
/*  VWIN */
/*           Indices (1,AF) and (2,AF) read. */
/*           All other indices untouched. */
/*  LFRAME */
/*  MAXWIN */
/* Input/Output: */
/*  AWIN */
/*           Index (1,AF-1) read. */
/*           Indices (1,AF) and (2,AF) written, and then read. */
/*           All other indices untouched. */
/*           In all cases (except possibly one), the final values will */
/*           satisfy the condition: AWIN(2,AF)-AWIN(1,AF)+1 = MAXWIN. */
/*           In that other case, */
/*           AWIN(1,AF)=VWIN(1,AF) and AWIN(2,AF)=VWIN(2,AF). */
/* Output: */
/*  EWIN */
/*           Indices (1,AF) and (2,AF) written. */
/*           All other indices untouched. */
/*           In all cases, the final values will satisfy the condition: */
/*           AWIN(1,AF) .LE. EWIN(1,AF) .LE. EWIN(2,AF) .LE. AWIN(2,AF) */
/*           In other words, the energy window is a sub-window of */
/*           the analysis window. */

/* This subroutine has no local state. */

/*< 	S >*/
/* Subroutine */ int placea_(integer *ipitch, integer *voibuf, integer *
	obound, integer *af, integer *vwin, integer *awin, integer *ewin, 
	integer *lframe, integer *maxwin)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    integer i_nint(real *);

    /* Local variables */
    logical allv, winv;
    integer i__, j, k, l, hrange;
    logical ephase;
    integer lrange;

/*       Arguments */
/*< 	INTEGER IPITCH, OBOUND, AF >*/
/*< 	INTEGER VOIBUF(2,0:AF) >*/
/*< 	INTEGER VWIN(2,AF) >*/
/*< 	INTEGER LFRAME, MAXWIN >*/
/*< 	INTEGER AWIN(2,AF) >*/
/*< 	INTEGER EWIN(2,AF) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER I, J, K, L >*/
/*< 	LOGICAL EPHASE, ALLV, WINV >*/
/*< 	INTEGER LRANGE, HRANGE >*/
/*< 	LRANGE = (AF-2)*LFRAME + 1 >*/
    /* Parameter adjustments */
    ewin -= 3;
    awin -= 3;
    vwin -= 3;
    --voibuf;

    /* Function Body */
    lrange = (*af - 2) * *lframe + 1;
/*< 	HRANGE = AF*LFRAME >*/
    hrange = *af * *lframe;
/*   Place the Analysis window based on the voicing window */
/*   placement, onsets, tentative voicing decision, and pitch. */

/*   Case 1:  Sustained Voiced Speech */
/*   If the five most recent voicing decisions are */
/*   voiced, then the window is placed phase-synchronously with the */
/*   previous window, as close to the present voicing window if possible. 
*/
/*   If onsets bound the voicing window, then preference is given to */
/*   a phase-synchronous placement which does not overlap these onsets. */

/*   Case 2:  Voiced Transition */
/*   If at least one voicing decision in AF is voicied, and there are no 
*/
/*   onsets, then the window is placed as in case 1. */

/*   Case 3:  Unvoiced Speech or Onsets */
/*   If both voicing decisions in AF are unvoiced, or there are onsets, */
/*   then the window is placed coincident with the voicing window. */

/*   Note:  During phase-synchronous placement of windows, the length */
/*   is not altered from MAXWIN, since this would defeat the purpose */
/*   of phase-synchronous placement. */
/* Check for case 1 and case 2 */
/*< 	ALLV =            VOIBUF(2,AF-2) .EQ. 1 >*/
    allv = voibuf[(*af - 2 << 1) + 2] == 1;
/*< 	ALLV = ALLV .AND. VOIBUF(1,AF-1) .EQ. 1 >*/
    allv = allv && voibuf[(*af - 1 << 1) + 1] == 1;
/*< 	ALLV = ALLV .AND. VOIBUF(2,AF-1) .EQ. 1 >*/
    allv = allv && voibuf[(*af - 1 << 1) + 2] == 1;
/*< 	ALLV = ALLV .AND. VOIBUF(1,AF  ) .EQ. 1 >*/
    allv = allv && voibuf[(*af << 1) + 1] == 1;
/*< 	ALLV = ALLV .AND. VOIBUF(2,AF  ) .EQ. 1 >*/
    allv = allv && voibuf[(*af << 1) + 2] == 1;
/*< 	WINV = VOIBUF(1,AF  ) .EQ. 1 .OR.  VOIBUF(2,AF  ) .EQ. 1 >*/
    winv = voibuf[(*af << 1) + 1] == 1 || voibuf[(*af << 1) + 2] == 1;
/*< 	IF (ALLV .OR. WINV .AND. OBOUND .EQ. 0) THEN >*/
    if (allv || winv && *obound == 0) {
/* APHASE:  Phase synchronous window placement. */
/* Get minimum lower index of the window. */
/*< 	   I = (LRANGE + IPITCH - 1 - AWIN(1,AF-1)) / IPITCH >*/
	i__ = (lrange + *ipitch - 1 - awin[(*af - 1 << 1) + 1]) / *ipitch;
/*< 	   I = I * IPITCH >*/
	i__ *= *ipitch;
/*< 	   I = I + AWIN(1,AF-1) >*/
	i__ += awin[(*af - 1 << 1) + 1];
/* L = the actual length of this frame's analysis window. */
/*< 	   L = MAXWIN >*/
	l = *maxwin;
/* Calculate the location where a perfectly centered window would star
t. */
/*< 	   K = (VWIN(1,AF) + VWIN(2,AF) + 1 - L) / 2 >*/
	k = (vwin[(*af << 1) + 1] + vwin[(*af << 1) + 2] + 1 - l) / 2;
/* Choose the actual location to be the pitch multiple closest to this
. */
/*< 	   AWIN(1,AF) = I + NINT (FLOAT (K - I) / IPITCH) * IPITCH >*/
	r__1 = (real) (k - i__) / *ipitch;
	awin[(*af << 1) + 1] = i__ + i_nint(&r__1) * *ipitch;
/*< 	   AWIN(2,AF) = AWIN(1,AF) + L - 1 >*/
	awin[(*af << 1) + 2] = awin[(*af << 1) + 1] + l - 1;
/* If there is an onset bounding the right of the voicing window and t
he */
/* analysis window overlaps that, then move the analysis window backwa
rd */
/* to avoid this onset. */
/*< 	   IF (OBOUND .GE. 2 .AND. AWIN (2,AF) .GT. VWIN (2,AF)) THEN >*/
	if (*obound >= 2 && awin[(*af << 1) + 2] > vwin[(*af << 1) + 2]) {
/*< 	      AWIN(1,AF) = AWIN(1,AF) - IPITCH >*/
	    awin[(*af << 1) + 1] -= *ipitch;
/*< 	      AWIN(2,AF) = AWIN(2,AF) - IPITCH >*/
	    awin[(*af << 1) + 2] -= *ipitch;
/*< 	   END IF >*/
	}
/* Similarly for the left of the voicing window. */
/*< 	  >*/
	if ((*obound == 1 || *obound == 3) && awin[(*af << 1) + 1] < vwin[(*
		af << 1) + 1]) {
/*< 	      AWIN(1,AF) = AWIN(1,AF) + IPITCH >*/
	    awin[(*af << 1) + 1] += *ipitch;
/*< 	      AWIN(2,AF) = AWIN(2,AF) + IPITCH >*/
	    awin[(*af << 1) + 2] += *ipitch;
/*< 	   END IF >*/
	}
/* If this placement puts the analysis window above HRANGE, then */
/* move it backward an integer number of pitch periods. */
/*< 	   DO WHILE (AWIN (2,AF) .GT. HRANGE) >*/
	while(awin[(*af << 1) + 2] > hrange) {
/*< 	      AWIN(1,AF) = AWIN(1,AF) - IPITCH >*/
	    awin[(*af << 1) + 1] -= *ipitch;
/*< 	      AWIN(2,AF) = AWIN(2,AF) - IPITCH >*/
	    awin[(*af << 1) + 2] -= *ipitch;
/*< 	   END DO >*/
	}
/* Similarly if the placement puts the analysis window below LRANGE. 
*/
/*< 	   DO WHILE (AWIN (1,AF) .LT. LRANGE) >*/
	while(awin[(*af << 1) + 1] < lrange) {
/*< 	      AWIN(1,AF) = AWIN(1,AF) + IPITCH >*/
	    awin[(*af << 1) + 1] += *ipitch;
/*< 	      AWIN(2,AF) = AWIN(2,AF) + IPITCH >*/
	    awin[(*af << 1) + 2] += *ipitch;
/*< 	   END DO >*/
	}
/* Make Energy window be phase-synchronous. */
/*< 	   EPHASE = .TRUE. >*/
	ephase = TRUE_;
/* Case 3 */
/*< 	ELSE >*/
    } else {
/*< 	   AWIN(1,AF) = VWIN(1,AF) >*/
	awin[(*af << 1) + 1] = vwin[(*af << 1) + 1];
/*< 	   AWIN(2,AF) = VWIN(2,AF) >*/
	awin[(*af << 1) + 2] = vwin[(*af << 1) + 2];
/*< 	   EPHASE = .FALSE. >*/
	ephase = FALSE_;
/*< 	END IF >*/
    }
/* RMS is computed over an integer number of pitch periods in the analysis
 */
/*window.  When it is not placed phase-synchronously, it is placed as clos
e*/
/* as possible to onsets. */
/*< 	J = ((AWIN(2,AF)-AWIN(1,AF)+1)/IPITCH)*IPITCH >*/
    j = (awin[(*af << 1) + 2] - awin[(*af << 1) + 1] + 1) / *ipitch * *ipitch;
/*< 	IF (J .EQ. 0 .OR. .NOT. WINV) THEN >*/
    if (j == 0 || ! winv) {
/*< 	   EWIN(1,AF) = VWIN(1,AF) >*/
	ewin[(*af << 1) + 1] = vwin[(*af << 1) + 1];
/*< 	   EWIN(2,AF) = VWIN(2,AF) >*/
	ewin[(*af << 1) + 2] = vwin[(*af << 1) + 2];
/*< 	ELSE IF (.NOT. EPHASE .AND. OBOUND .EQ. 2) THEN >*/
    } else if (! ephase && *obound == 2) {
/*< 	   EWIN(1,AF) = AWIN(2,AF) - J + 1 >*/
	ewin[(*af << 1) + 1] = awin[(*af << 1) + 2] - j + 1;
/*< 	   EWIN(2,AF) = AWIN(2,AF) >*/
	ewin[(*af << 1) + 2] = awin[(*af << 1) + 2];
/*< 	ELSE >*/
    } else {
/*< 	   EWIN(1,AF) = AWIN(1,AF) >*/
	ewin[(*af << 1) + 1] = awin[(*af << 1) + 1];
/*< 	   EWIN(2,AF) = AWIN(1,AF) + J - 1 >*/
	ewin[(*af << 1) + 2] = awin[(*af << 1) + 1] + j - 1;
/*< 	END IF >*/
    }
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* placea_ */

