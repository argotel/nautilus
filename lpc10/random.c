/*
 * random.c
 *
 * SCCS ID:  @(#)random.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern integer random_(void);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************** */

/* 	RANDOM Version 49 */

/* $Log: random.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.3  1996/03/20  16:13:54  jaf */
/* Rearranged comments a little bit, and added comments explaining that */
/* even though there is local state here, there is no need to create an */
/* ENTRY for reinitializing it. */

/* Revision 1.2  1996/03/14  22:25:29  jaf */
/* Just rearranged the comments and local variable declarations a bit. */

/* Revision 1.1  1996/02/07 14:49:01  jaf */
/* Initial revision */


/* ********************************************************************* */

/*  Pseudo random number generator based on Knuth, Vol 2, p. 27. */

/* Function Return: */
/*  RANDOM - Integer variable, uniformly distributed over -32768 to 32767 */

/* This subroutine maintains local state from one call to the next. */
/* In the context of the LPC10 coder, there is no reason to reinitialize */
/* this local state when switching between audio streams, because its */
/* results are only used to generate noise for unvoiced frames. */

/*< 	FUNCTION RANDOM () >*/
integer random_(void)
{
    /* Initialized data */

    static integer j = 2;
    static integer k = 5;
    static shortint y[5] = { -21161,-8478,30892,-10216,16950 };

    /* System generated locals */
    integer ret_val;

/*< 	INTEGER RANDOM >*/
/* 	Parameters/constants */
/*< 	INTEGER MIDTAP, MAXTAP >*/
/*< 	PARAMETER (MIDTAP=2, MAXTAP=5) >*/
/*       Local state */
/*< 	INTEGER J, K >*/
/*< 	INTEGER*2 Y(MAXTAP) >*/
/*< 	SAVE J, K, Y >*/
/*< 	DATA J/MIDTAP/, K/MAXTAP/ >*/
/*< 	DATA Y /-21161, -8478, 30892,-10216, 16950/ >*/
/*   The following is a 16 bit 2's complement addition, */
/*   with overflow checking disabled */
/*< 	Y(K) = Y(K) + Y(J) >*/
    y[k - 1] += y[j - 1];
/*< 	RANDOM = Y(K) >*/
    ret_val = y[k - 1];
/*< 	K = K - 1 >*/
    --k;
/*< 	IF (K .LE. 0) K = MAXTAP >*/
    if (k <= 0) {
	k = 5;
    }
/*< 	J = J - 1 >*/
    --j;
/*< 	IF (J .LE. 0) J = MAXTAP >*/
    if (j <= 0) {
	j = 5;
    }
/*< 	RETURN >*/
    return ret_val;
/*< 	END >*/
} /* random_ */

