/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* util.c -- miscellaneous utility functions
 *
 * SCCS ID: @(#)util.c 1.5 96/05/22
 */

#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#include "nautilus.h"

/* external variables */
extern struct coder_t coders[];	/* coder table */
extern struct cipher_t ciphers[];    /* cipher table */



/*
 * return index into coder table corresponding to coder specified by arg
 */
int
FindCoder(char *arg)
{
    int             i;

    /* search list for specified coder */
    for (i = 0; i < NCODERS; i++) {
	if (!Strncasecmp(coders[i].name, arg, strlen(arg))) {
	    return i;
	}
    }

    return -1;
}



/*
 * return enum type of cipher specified by argument
 */
int
FindCipher(char *arg)
{
    int i;

    /* search list for specified coder */
    for (i = 0; i < NCIPHERS; i++) {
	if (!Strncasecmp(ciphers[i].name, arg, strlen(arg))) {
	    return i;
	}
    }

    return -1;
}

/*
 * return enum type of key exchange protocol specified by argument
 */
int
FindKeyExch (char *arg)
{
    /* temporary table of names, maybe pick some better ones @@ */
    struct {
	char *name;
	int type;
    } protocols[] = {
	{"pp",		PASSPHRASE },
	{"passphrase",	PASSPHRASE },
	{"dh",		DIFFIE_HELLMAN_MEDIUM},
	{"dhsmall",	DIFFIE_HELLMAN_SMALL},
	{"dh512",	DIFFIE_HELLMAN_SMALL},
	{"dhmedium",	DIFFIE_HELLMAN_MEDIUM},
	{"dh768",	DIFFIE_HELLMAN_MEDIUM},
	{"dhlarge",	DIFFIE_HELLMAN_LARGE},
	{"dh1024",	DIFFIE_HELLMAN_LARGE},
	{"dh2048",	DIFFIE_HELLMAN_ENORMOUS},
	{"dhhuge",	DIFFIE_HELLMAN_ENORMOUS},
    };
    int i;
    
#define NELTS(vec) ((sizeof vec)/(sizeof vec[0]))

    for (i = 0; i < NELTS(protocols); i++)
      if (!Strcasecmp (protocols[i].name, arg))
 	return protocols[i].type;

    return -1;
}

static signed char sintbl[] = {
  0,   2,   4,   7,   9,  11,  13,  15,  18,  20,
 22,  24,  26,  29,  31,  33,  35,  37,  39,  41,
 43,  46,  48,  50,  52,  54,  56,  58,  60,  62,
 63,  65,  67,  69,  71,  73,  75,  76,  78,  80,
 82,  83,  85,  87,  88,  90,  91,  93,  94,  96,
 97,  99, 100, 101, 103, 104, 105, 107, 108, 109,
109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
119, 119, 120, 121, 121, 122, 123, 123, 124, 124,
125, 125, 125, 126, 126, 126, 127, 127, 127, 127,
127
};



/*
 * Return the sine of the angle "deg", which is represented in
 * degrees.  The return value is an integer in the range [-127,127],
 * and it is equal to 127 times the actual sine of the angle.
 */
int
isin(int deg)
{
    if (deg < 90)
        return sintbl[deg];
    else if (deg < 180)
        return sintbl[180 - deg];
    else if (deg < 270)
        return -sintbl[deg - 180];
    else if (deg < 360)
        return -sintbl[360 - deg];
    else
        return isin(deg % 360);
}

/*
 * Compute the audio entropy of the samples contained in the buffer.
 */
float
ComputeEntropy(UINT8 buf[], int n)
{
    int i;
    INT16 *samples;
    float sum;

    samples = (INT16 *) malloc(n * sizeof(INT16));
    Raw2Std(buf, samples, (INT16) n);

    sum = 0.0;
    for (i=0; i<n; i++)
        sum += (samples[i]/256) * (samples[i]/256);
    sum /= (float) n;

    free(samples);
    return sum;
}
