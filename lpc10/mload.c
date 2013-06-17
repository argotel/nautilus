/*
 * mload.c
 *
 * SCCS ID:  @(#)mload.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int mload_(integer *order, integer *awins, integer *awinf, real *speech, real *phi, real *psi);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ***************************************************************** */

/* 	MLOAD Version 48 */

/* $Log: mload.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.6  1996/07/12  23:59:51  dm */
/* Removed crazy use of middle-man variable */

/* Revision 1.5  1996/03/27  23:59:51  jaf */
/* Added some more accurate comments about which indices of the argument */
/* array SPEECH are read.  I thought that this might be the cause of a */
/* problem I've been having, but it isn't. */

/* Revision 1.4  1996/03/26  19:16:53  jaf */
/* Commented out the code at the end that copied the lower triangular */
/* half of PHI into the upper triangular half (making the resulting */
/* matrix symmetric).  The upper triangular half was never used by later */
/* code in subroutine ANALYS. */

/* Revision 1.3  1996/03/18  21:16:00  jaf */
/* Just added a few comments about which array indices of the arguments */
/* are used, and mentioning that this subroutine has no local state. */

/* Revision 1.2  1996/03/13  16:47:41  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:48:01  jaf */
/* Initial revision */


/* ***************************************************************** */

/* Load a covariance matrix. */

/* Input: */
/*  ORDER            - Analysis order */
/*  AWINS            - Analysis window start */
/*  AWINF            - Analysis window finish */
/*  SPEECH(AWINF)    - Speech buffer */
/*                     Indices MIN(AWINS, AWINF-(ORDER-1)) through */
/*                             MAX(AWINF, AWINS+(ORDER-1)) read. */
/*                     As long as (AWINF-AWINS) .GE. (ORDER-1), */
/*                     this is just indices AWINS through AWINF. */
/* Output: */
/*  PHI(ORDER,ORDER) - Covariance matrix */
/*                    Lower triangular half and diagonal written, and read.*/
/*                     Upper triangular half untouched. */
/*  PSI(ORDER)       - Prediction vector */
/*                     Indices 1 through ORDER written, */
/*                     and most are read after that. */

/* This subroutine has no local state. */

/*< 	SUBROUTINE MLOAD( ORDER, AWINS, AWINF, SPEECH, PHI, PSI ) >*/
/* Subroutine */ int mload_(integer *order, integer *awins, integer *awinf, 
	real *speech, real *phi, real *psi)
{
    /* System generated locals */
    integer phi_dim1, phi_offset, i__1, i__2;

    /* Local variables */
    integer c__, i__, r__, start;

/*       Arguments */
/*< 	INTEGER ORDER, AWINS, AWINF >*/
/*< 	REAL SPEECH(AWINF) >*/
/*< 	REAL PHI(ORDER,ORDER), PSI(ORDER) >*/
/*       Local variables that need not be saved */
/*< 	INTEGER R, C, I, START >*/
/*   Load first column of triangular covariance matrix PHI */
/*< 	START = AWINS + ORDER >*/
    /* Parameter adjustments */
    --psi;
    phi_dim1 = *order;
    phi_offset = phi_dim1 + 1;
    phi -= phi_offset;
    --speech;

    /* Function Body */
    start = *awins + *order;
/*< 	DO R = 1,ORDER >*/
    i__1 = *order;
    for (r__ = 1; r__ <= i__1; ++r__) {
/*< 	   PHI(R,1) = 0. >*/
	phi[r__ + phi_dim1] = 0.f;
/*< 	   DO I = START,AWINF >*/
	i__2 = *awinf;
	for (i__ = start; i__ <= i__2; ++i__) {
/*< 	      PHI(R,1) = PHI(R,1) + SPEECH(I-1)*SPEECH(I-R) >*/
	    phi[r__ + phi_dim1] += speech[i__ - 1] * speech[i__ - r__];
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*   Load last element of vector PSI */
/*< 	PSI(ORDER) = 0. >*/
    psi[*order] = 0.f;
/*< 	DO I = START,AWINF >*/
    i__1 = *awinf;
    for (i__ = start; i__ <= i__1; ++i__) {
/*< 	   PSI(ORDER) = PSI(ORDER) + SPEECH(I)*SPEECH(I-ORDER) >*/
	psi[*order] += speech[i__] * speech[i__ - *order];
/*< 	END DO >*/
    }
/*   End correct to get additional columns of PHI */
/*< 	DO R = 2,ORDER >*/
    i__1 = *order;
    for (r__ = 2; r__ <= i__1; ++r__) {
/*< 	   DO C = 2,R >*/
	for (c__ = 2; c__ <= r__; ++c__) {
/*< 	  >*/
	    phi[r__ + c__ * phi_dim1] = phi[r__ - 1 + (c__ - 1) * phi_dim1] - 
		    speech[*awinf + 1 - r__] * speech[*awinf + 1 - c__] + 
		    speech[start - r__] * speech[start - c__];
/*< 	   END DO >*/
	}
/*< 	END DO >*/
    }
/*   End correct to get additional elements of PSI */
/*< 	DO C = 1,ORDER-1 >*/
    i__1 = *order - 1;
    for (c__ = 1; c__ <= i__1; ++c__) {
/*< 	  >*/
	psi[c__] = phi[c__ + 1 + phi_dim1] - speech[start - 1] * speech[start 
		- 1 - c__] + speech[*awinf] * speech[*awinf - c__];
/*< 	END DO >*/
    }
/*   Copy lower triangular section into upper (why bother?) */
/*       I'm commenting this out, since the upper triangular half of PHI 
*/
/*       is never used by later code, unless a sufficiently high level of 
*/
/*       tracing is turned on. */
/* 	DO R = 1,ORDER */
/* 	   DO C = 1,R-1 */
/* 	      PHI(C,R) = PHI(R,C) */
/* 	   END DO */
/* 	END DO */
/*< 	RETURN >*/
    return 0;
/*< 	END >*/
} /* mload_ */

