/*
 * lpfilt.c
 *
 * SCCS ID:  @(#)lpfilt.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern int lpfilt_(real *inbuf, real *lpbuf, integer *len, integer *nsamp);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* *********************************************************************** */

/* 	LPFILT Version 55 */

/* $Log: lpfilt.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/15  16:53:49  jaf */
/* Just put comment header in standard form. */

/* Revision 1.2  1996/03/12  23:58:06  jaf */
/* Comments added explaining that none of the local variables of this */
/* subroutine need to be saved from one invocation to the next. */

/* Revision 1.1  1996/02/07 14:47:44  jaf */
/* Initial revision */


/* *********************************************************************** */

/*   31 Point Equiripple FIR Low-Pass Filter */
/*     Linear phase, delay = 15 samples */

/* 	Passband:  ripple = 0.25 dB, cutoff =  800 Hz */
/* 	Stopband:  atten. =  40. dB, cutoff = 1240 Hz */

/* Inputs: */
/*  LEN    - Length of speech buffers */
/*  NSAMP  - Number of samples to filter */
/*  INBUF  - Input speech buffer */
/*           Indices len-nsamp-29 through len are read. */
/* Output: */
/*  LPBUF  - Low passed speech buffer (must be different array than INBUF) */
/*           Indices len+1-nsamp through len are written. */

/* This subroutine has no local state. */

/*< 	subroutine lpfilt(inbuf, lpbuf, len, nsamp) >*/
/* Subroutine */ int lpfilt_(real *inbuf, real *lpbuf, integer *len, integer *
	nsamp)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer j;

/* 	Arguments */
/*< 	integer len, nsamp >*/
/*< 	real inbuf(len), lpbuf(len) >*/
/* 	Parameters/constants */
/*< 	real h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15 >*/
/*< 	p >*/
/*       Local variables that need not be saved */
/*< 	integer j >*/
/*< 	real t >*/
/*       Local state */
/*       None */
/*< 	do j = len+1-nsamp,len >*/

    /* Function Body */
    i__1 = *len;
    for (j = *len - *nsamp; j < i__1; ++j) {
	lpbuf[j] = ((inbuf[j] + inbuf[j - 30]) * -.0097201988f) +
                   ((inbuf[j - 1] + inbuf[j - 29]) * -.0105179986f) +
                   ((inbuf[j - 2] + inbuf[j - 28]) * -.0083479648f) +
                   ((inbuf[j - 3] + inbuf[j - 27]) * 5.860774e-4f) +
                   ((inbuf[j - 4] + inbuf[j - 26]) * .0130892089f) +
                   ((inbuf[j - 5] + inbuf[j - 25]) * .0217052232f) +
                   ((inbuf[j - 6] + inbuf[j - 24]) * .0184161253f) +
                   ((inbuf[j - 7] + inbuf[j - 23]) * 3.39723e-4f) +
                   ((inbuf[j - 8] + inbuf[j - 22]) * -.0260797087f) +
                   ((inbuf[j - 9] + inbuf[j - 21]) * -.0455563702f) +
                   ((inbuf[j - 10] + inbuf[j - 20]) * -.040306855f) +
                   ((inbuf[j - 11] + inbuf[j - 19]) * 5.029835e-4f) +
                   ((inbuf[j - 12] + inbuf[j - 18]) * .0729262903f) +
                   ((inbuf[j - 13] + inbuf[j - 17]) * .1572008878f) +
                   ((inbuf[j - 14] + inbuf[j - 16]) * .2247288674f) +
                   (inbuf[j - 15] * .250535965f);
    }
    return 0;
} /* lpfilt_ */

