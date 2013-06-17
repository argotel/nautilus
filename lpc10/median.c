/*
 * median.c
 *
 * SCCS ID:  @(#)median.c 1.2 96/05/19
 */

#ifdef P_R_O_T_O_T_Y_P_E_S
extern integer median_(integer *d1, integer *d2, integer *d3);
#endif

/*  -- translated by f2c (version 19951025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* ********************************************************************* */

/* 	MEDIAN Version 45G */

/* $Log: median.c,v $
/* Revision 1.2  2001-01-25 23:45:49  jpoehlmann
/* Version 1.7c. Identical with files on the ftp Server ftp.franken.de.
/* (+ 1 patch in cli.c, wich is on the server too)
/* Not compiled now
/* */
/* Revision 1.2  1996/03/14  22:30:22  jaf */
/* Just rearranged the comments and local variable declarations a bit. */

/* Revision 1.1  1996/02/07 14:47:53  jaf */
/* Initial revision */


/* ********************************************************************* */

/*  Find median of three values */

/* Input: */
/*  D1,D2,D3 - Three input values */
/* Output: */
/*  MEDIAN - Median value */

/*< 	FUNCTION MEDIAN( D1, D2, D3 ) >*/
integer median_(integer *d1, integer *d2, integer *d3)
{
    /* System generated locals */
    integer ret_val;

/*< 	INTEGER MEDIAN >*/
/*       Arguments */
/*< 	INTEGER D1, D2, D3 >*/
/*< 	MEDIAN = D2 >*/
    ret_val = *d2;
/*< 	IF    ( D2 .GT. D1 .AND. D2 .GT. D3 ) THEN >*/
    if (*d2 > *d1 && *d2 > *d3) {
/*< 	   MEDIAN = D1 >*/
	ret_val = *d1;
/*< 	   IF ( D3 .GT. D1 ) MEDIAN = D3 >*/
	if (*d3 > *d1) {
	    ret_val = *d3;
	}
/*< 	ELSEIF( D2 .LT. D1 .AND. D2 .LT. D3 ) THEN >*/
    } else if (*d2 < *d1 && *d2 < *d3) {
/*< 	   MEDIAN = D1 >*/
	ret_val = *d1;
/*< 	   IF ( D3 .LT. D1 ) MEDIAN = D3 >*/
	if (*d3 < *d1) {
	    ret_val = *d3;
	}
/*< 	END IF >*/
    }
/*< 	RETURN >*/
    return ret_val;
/*< 	END >*/
} /* median_ */

