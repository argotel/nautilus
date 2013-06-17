/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 * nuke.c
 *
 * SCCS ID: @(#)nuke.c 1.2 96/05/19
 */

#include <stdio.h>
#include <math.h>
#ifdef SOLARIS
#include <multimedia/audio_encode.h>
#include <multimedia/audio_errno.h>
#include <multimedia/audio_hdr.h>
#else
#include <multimedia/libaudio.h>
#include <multimedia/audio_filehdr.h>
#include <multimedia/ulaw2linear.h>
#endif

#include "machine.h"

/* if invoked as 'nuke', take the Sun Audio file on standard input, strip
 * the header, compress it with apsd, and write it out MSB first to standard
 * output, bit padding with zeros if necessary.
 * if invoked as anything else, take the apsd-compressed file on standard
 * input, uncompress it with apsd, and write the decoded output along with
 * a prepended Sun Audio file header to standard output.
 */

#ifdef SP64
#define	N	160				/* for SP64 */
#define NOUT	(N*3/32+1)			/* for SP64 */
#define AUDIO_INIT		sp64_init	/* for SP64 */
#define AUDIO_COMPRESS		sp64_encode	/* for SP64 */
#define AUDIO_UNCOMPRESS	sp64_decode	/* for SP64 */
#endif
#ifdef SP85
#define	N	120				/* for SP85 */
#define NOUT	(N/8+1)				/* for SP85 */
#define AUDIO_INIT		sp85_init	/* for SP85 */
#define AUDIO_COMPRESS		sp85_encode	/* for SP85 */
#define AUDIO_UNCOMPRESS	sp85_decode	/* for SP85 */
#endif
#ifdef SP124
#define	N	144				/* for SP124 */
#define NOUT	(N*3/16+1)			/* for SP124 */
#define AUDIO_INIT		sp124_init	/* for SP124 */
#define AUDIO_COMPRESS		sp124_encode	/* for SP124 */
#define AUDIO_UNCOMPRESS	sp124_decode	/* for SP124 */
#endif
#ifdef SP184
#define N	160				/* for SP184 */
#define NOUT	(N*9/32+1)			/* for SP184 */
#define AUDIO_INIT		sp184_init	/* for SP184 */
#define AUDIO_COMPRESS		sp184_encode	/* for SP184 */
#define AUDIO_UNCOMPRESS	sp184_decode	/* for SP184 */
#endif
#ifdef LPC10
#define N	180				/* for LPC10 */
#define NOUT	7				/* for LPC10 */
#define AUDIO_INIT		lpc10_init	/* for LPC10 */
#define AUDIO_COMPRESS		lpc10_encode	/* for LPC10 */
#define AUDIO_UNCOMPRESS	lpc10_decode	/* for LPC10 */
#endif

void            nuke(char *cmd);
void            unnuke(char *cmd);

main(int argc, char *argv[])
{
    if (!strcmp(&argv[0][strlen(argv[0]) - 6], "unnuke"))
	unnuke(argv[0]);
    else
	nuke(argv[0]);

    exit(0);
}

void
nuke(char *cmd)
{
    int             i, j;
    unsigned char   bytes[N];
    INT16           samples[N];
    Audio_hdr       hp;
    char            infop[80];
    static void     output_bits();
    static void     output_packed_bits();

    if (audio_read_filehdr(0, &hp, infop, 80) != AUDIO_SUCCESS) {
	fprintf(stderr, "%s: error reading audio header\n", cmd);
    }

    write(1, "APSD", 4);	/* write magic # */
    write(1, &hp.data_size, sizeof(unsigned));	/* write length */
    AUDIO_INIT();
    for (i = N; i <= hp.data_size; i += N) {
	read(0, bytes, N);
	for (j = 0; j < N; j++)
	    samples[j] = (INT16) audio_u2s(bytes[j]);
	AUDIO_COMPRESS(samples, bytes);
	write(1, bytes, NOUT);
    }
    if (hp.data_size % N != 0) {
	read(0, bytes, hp.data_size % N);
	for (j = 0; j < N; j++) {
	    if (j < hp.data_size % N)
		samples[j] = (INT16) audio_u2s(bytes[j]);
	    else
		samples[j] = 0;
	}
	AUDIO_COMPRESS(samples, bytes);
	write(1, bytes, NOUT);
    }
}

void
unnuke(char *cmd)
{
    int             i, j;
    INT16           samples[N];
    unsigned        length;
    unsigned char   bytes[N];
    char            buf[5];
    Audio_hdr       hp;
    static void     input_bits();
    static unsigned input_packed_bits();

    /* read apsd header information */
    read(0, buf, 4);
    buf[4] = '\0';
    if (strcmp(buf, "APSD")) {
	fprintf(stderr, "%s: error reading apsd header\n", cmd);
	exit(1);
    }
    read(0, &length, sizeof(unsigned));

    /* write au header information */
    hp.sample_rate = 8000;
    hp.samples_per_unit = 1;
    hp.bytes_per_unit = 1;
    hp.channels = 1;
    hp.encoding = AUDIO_ENCODING_ULAW;
    hp.data_size = length;
    audio_write_filehdr(1, hp, 0, 0);

    AUDIO_INIT();
    for (i = N; i <= length; i += N) {
	read(0, bytes, NOUT);
	AUDIO_UNCOMPRESS(bytes, samples);
	for (j = 0; j < N; j++)
	    bytes[j] = audio_s2u(samples[j]);
	write(1, bytes, N);
    }
    if (length % N != 0) {	/* partial block? */
	read(0, bytes, NOUT);
	AUDIO_UNCOMPRESS(bytes, samples);
	for (j = 0; j < length % N; j++)
	    bytes[j] = audio_s2u(samples[j]);
	write(1, bytes, length % N);
    }
}
