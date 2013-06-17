/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* init.c
 *
 * SCCS ID:  @(#)init.c 1.13 96/05/25
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/08  B. Dorsey          Module created by breakup of nautilus.c
 * 93/12/31  B. Dorsey          Added config_tbl[] array
 * 96/08/31  D. Miller          Added decoding time CoderSpeedTest()
 * 00/10/10  J. Poehlmann       Added MODEM_PREFIX config file Parameter
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "nautilus.h"

#ifndef PI
#define	PI		3.141592653589
#endif
#define TWOPI		(2.0*PI)

/* Global variables */
struct param_t  params;                 /* operating parameters */
struct coder_t  coders[NCODERS];        /* coder table */
struct cipher_t ciphers[NCIPHERS];      /* cipher table */
struct negotiate_t negotiate;           /* capability negotiation parameters */
struct config_t config_tbl[] = {
    {"SPEED", CONFIG_TYPE_NUMBER, &params.port.speed},
    {"PORT", CONFIG_TYPE_STRING, &params.port.name},
    {"MODEM_INIT", CONFIG_TYPE_STRING, &params.modem.init},
    {"MODEM_PREFIX", CONFIG_TYPE_STRING, &params.modem.prefix},
    {"MODEM_RESET", CONFIG_TYPE_STRING, &params.modem.reset},
    {"SND_IO", CONFIG_TYPE_UINT16, &params.msdos.snd_iobase},
    {"SND_IRQ", CONFIG_TYPE_UINT16, &params.msdos.snd_irq},
    {"SND_DMA", CONFIG_TYPE_UINT16, &params.msdos.snd_dma},
    {"COM1_IO", CONFIG_TYPE_UINT16, &params.msdos.com_iobase[0]},
    {"COM2_IO", CONFIG_TYPE_UINT16, &params.msdos.com_iobase[1]},
    {"COM3_IO", CONFIG_TYPE_UINT16, &params.msdos.com_iobase[2]},
    {"COM4_IO", CONFIG_TYPE_UINT16, &params.msdos.com_iobase[3]},
    {"COM1_IRQ", CONFIG_TYPE_UINT16, &params.msdos.com_irq[0]},
    {"COM2_IRQ", CONFIG_TYPE_UINT16, &params.msdos.com_irq[1]},
    {"COM3_IRQ", CONFIG_TYPE_UINT16, &params.msdos.com_irq[2]},
    {"COM4_IRQ", CONFIG_TYPE_UINT16, &params.msdos.com_irq[3]},
    {"MIC_SENS", CONFIG_TYPE_STRING, &params.audio.mic_sens},
    {"OUT_VOLUME", CONFIG_TYPE_STRING, &params.audio.out_volume},
    {"UPGRADE_FILE", CONFIG_TYPE_STRING, &params.upgrade_file},
    {"LOGON_FILE", CONFIG_TYPE_STRING, &params.logon_file},
    {"RING_FILE", CONFIG_TYPE_STRING, &params.ring_file},
    {"", CONFIG_TYPE_STRING, (char *) 0}
};
UINT8		beep[BEEP_MS*8];	/* assume max sampling rate of 8KHZ */



void
init(void)
{
    int             i = 0;
	
    /*
	 * CODER TYPE:          ADM with switched prediction
	 * CPU REQUIREMENTS:    386SX-20 or faster
	 * PERFORMANCE:         SNR: >6.5 dB  SNRSEG: >6 dB
	 * BANDWIDTH:           6,400 bits/second
	 */
    coders[i].init = sp64_init;
    coders[i].encode = sp64_encode;
    coders[i].decode = sp64_decode;
#ifdef AUDIO_DEVICE_8KHZ_ONLY
    coders[i].sample_rate = 8000;
    coders[i].frame_size = 160;
    coders[i].output_size = 3 * coders[i].frame_size / 32 + 1;
#else
    coders[i].sample_rate = 6000;
    coders[i].frame_size = 120;
    coders[i].output_size = coders[i].frame_size / 8 + 1;
#endif
    coders[i].frames_per_pkt = 8;	/* XXX - affects latency */
    coders[i].bandwidth = 7200;		/* 7200 bps or faster */
    coders[i].preference = 10;		/* relative preference */
    coders[i].name = "SP64";
    coders[i++].desc = "6400 bps ADPCM coder (requires 7200 bps connect)";
	
    /*
	 * CODER TYPE:          ADM with switched prediction
	 * CPU REQUIREMENTS:    386SX-20 or faster
	 * PERFORMANCE:         SNR: >7 dB   SNRSEG: ~7 dB
	 * BANDWIDTH:           8,533 bits/second
	 */
    coders[i].init = sp85_init;
    coders[i].encode = sp85_encode;
    coders[i].decode = sp85_decode;
    coders[i].sample_rate = 8000;
    coders[i].frame_size = 120;
    coders[i].output_size = coders[i].frame_size / 8 + 1;
    coders[i].frames_per_pkt = 8;	/* XXX - may need tweaking */
    coders[i].bandwidth = 9600;		/* 9600 bps or faster */
    coders[i].preference = 20;		/* relative preference */
    coders[i].name = "SP85";
    coders[i++].desc = "8500 bps ADPCM coder (requires 9600 bps connect)";
	
    /*
	 * CODER TYPE:          ADPCM with switched prediction (2 bits/sample)
	 * CPU REQUIREMENTS:    386DX-33 or faster
	 * PERFORMANCE:         SNR: ~11 dB   SNRSEG: >10 dB
	 * BANDWIDTH:           12,444 bits/second
	 */
    coders[i].init = sp124_init;
    coders[i].encode = sp124_encode;
    coders[i].decode = sp124_decode;
#ifdef AUDIO_DEVICE_8KHZ_ONLY
    coders[i].sample_rate = 8000;
    coders[i].frame_size = 144;
    coders[i].output_size = 3 * coders[i].frame_size / 16 + 1;
#else
    coders[i].sample_rate = 6000;
    coders[i].frame_size = 108;
    coders[i].output_size = coders[i].frame_size / 4 + 1;
#endif
    coders[i].frames_per_pkt = 6;	/* XXX - may need tweaking */
    coders[i].bandwidth = 14400;	/* 14400 bps or faster */
    coders[i].preference = 30;		/* relative preference */
    coders[i].name = "SP124";
    coders[i++].desc = "12400 bps ADPCM coder (requires 14.4 Kbps connect)";
	
    /*
	 * CODER TYPE:          U.S. DoD's Federal-Standard-1015/NATO-STANAG-4198
	 *                      based 2400 bps linear prediction coder (LPC-10)
	 * CPU REQUIREMENTS:    486DX-66 or faster (FPU required)
	 * PERFORMANCE:         synthetic quality
	 * BANDWIDTH:           2490 bits/sec (the extra 90 bits/sec could
	 *                      be removed with a bit of programming)
	 */
    coders[i].init = lpc10_init;
    coders[i].encode = lpc10_encode;
    coders[i].decode = lpc10_decode;
    coders[i].sample_rate = 8000;
    coders[i].frame_size = 180;
    coders[i].output_size = 7;
    coders[i].frames_per_pkt = 8;
    coders[i].bandwidth = 4800;
    coders[i].preference = 40;		/* relative preference */
    coders[i].name = "LPC10";
    coders[i++].desc = "2400 bps LPC-10 coder (requires 4800 bps connect)";
	
    /* Initialize cipher information */
    ciphers[NONE].name = "NONE";
    ciphers[NONE].desc = "Turns encryption off";
	
    ciphers[BLOWFISH].name = "BLOWFISH";
    ciphers[BLOWFISH].desc = "Use blowfish";
	
    ciphers[IDEA].name = "IDEA";
    ciphers[IDEA].desc = "Use IDEA";
	
    ciphers[DES3].name = "3DES";
    ciphers[DES3].desc = "Use triple-DES";
	
	
    /* Setup default params */
    params.mode = -1;
    strcpy(params.upgrade_file, DEFAULT_UPGRADE_FILE);
    strcpy(params.logon_file, DEFAULT_LOGON_FILE);
    strcpy(params.ring_file, DEFAULT_RING_FILE);
    params.verbose = FALSE;
	params.jbufsecs = 2.0;
    params.net_flag = FALSE;
    params.rp_timeout = 3;
    params.sp_timeout = 3;
    params.port.speed = DEFAULT_DTE_SPEED;
    strcpy(params.port.name, DEFAULT_PORT);
    params.modem.speed = 0;
    strcpy(params.modem.init, DEFAULT_MODEM_INIT);
    strcpy(params.modem.reset, DEFAULT_MODEM_RESET);
    strcpy(params.audio.mic_sens, "MEDIUM");
    strcpy(params.audio.out_volume, "MEDIUM");
    params.coder.index = DEFAULT_CODER_INDEX;
    params.msdos.snd_iobase = -1;
    params.msdos.snd_irq = -1;
    params.msdos.snd_dma = -1;
    params.msdos.snd_type = -1;
    for (i=0; i<4; i++) {
		params.msdos.com_iobase[i] = -1;
		params.msdos.com_irq[i] = -1;
    }
    params.crypto.keyexch_type = DIFFIE_HELLMAN_LARGE;
    params.crypto.key1.type = BLOWFISH;
    params.crypto.key2.type = BLOWFISH;
	
    /* Initialize negotiation parameters */
    negotiate.major = VERSION_MAJOR;
    negotiate.minor = VERSION_MINOR;
    negotiate.coder = 255;
    negotiate.encrypt_type = params.crypto.key1.type;
    negotiate.keyexch_type = params.crypto.keyexch_type;
    negotiate.modem_speed[0] = 0;
    negotiate.modem_speed[1] = 0;
}



/*
 * Generate turnaround beep.
 *
 * It is BEEP_MS long, sampled at sample_rate samples/sec, with a
 * frequency of BEEP_FREQ Hertz and an amplitude of BEEP_AMPLITUDE.
 * The maximum sample rate for which this function will work is 8000
 * samples/sec.  The maximum possible amplitude (without distortion)
 * is the maximum value of an INT16.
 */

void
InitBeep(int sample_rate)
{
    int             i, samples, theta;
    INT16           beepbuf[BEEP_MS*8];		/* room for max sample_rate */
	
	/*
	 * Note: Since the following statement is doing integer
	 * arithmetic, the expression (sample_rate / 1000) could lose
	 * accuracy if sample_rate is not a multiple of 1000.  However, if
	 * you try to increase the accuracy by shuffling things around
	 * like so:
	 *
	 * samples = (sample_rate * BEEP_MS) / 1000;
	 *
	 * then this will screw up things under the Microsoft C compiler,
	 * which defaults to 16 bit integers.  The multiplication of
	 * (sample_rate * BEEP_MS) will overflow the 16 bit int, and the
	 * value of samples will be nowhere near what you want, causing
	 * hard-to-diagnose problems in later code.
	 */
    samples = (sample_rate / 1000) * BEEP_MS;
    theta = (int) ((INT32) 360 * (INT32) BEEP_FREQ / (INT32) sample_rate);
    for (i = 0; i < samples; i++) {
        beepbuf[i] = (INT16) ((BEEP_AMPLITUDE>>7) *
			isin((int) ((INT32) i * (INT32) theta % (INT32) 360)));
    }
    Std2Raw(beepbuf, beep, (INT16) samples);
}



/*

	FUNCTION: CoderSpeedTest
 
	PURPOSE:
  
	Measure the speed of all of the installed speech encoders to see
	which ones can be used in real-time without overloading the CPU.
   
	ARGUMENTS:
	
	int min_test_duration_ms
	 
	The minimum duration to test each coder, in milliseconds.  The
	actual time that a coder is tested could be slightly larger than
	this.  In fact, it has been observed that the actual duration can
	be as long as 100 ms longer than the specified duration, at least
	with the Linux version.
	  
	int verbose_coder_speed_testing
	   
	If true, then a table of the test results is printed, with one line
	for each coder.  This can be useful for developers who want to know
	detailed information about the speed of a coder, or for regular
	users who are curious about speech coders, and the load that they
	place on their system.
		
	RETURN VALUE:
		 
	Return SUCCESS or FAIL.  Failure should only occur if there is not
	enough memory available for two small buffers.
		  
	SIDE EFFECTS:
		   
	The 'encode' field of the global variable 'negotiate' is changed
	using SetCoderUsable().
			
*/

int
CoderSpeedTest(int min_test_duration_ms, int verbose_coder_speed_testing)
{
    int             i, j;		/* generic loop variables */
    /* Max unencoded frame size over all coders */
    int             max_frame_size;
    /* Max encoded size of a frame over all coders */
    int             max_output_size;
    /* Buffer to hold random sound data for encoding. */
    INT16           *decoded;
    /* Buffer to hold encoded data. */
    UINT8           *encoded;
    /* Maximum fraction of the CPU that may be used by a coder and
	still be considered usable. */
    float           max_fraction_of_cpu;
    /* The minimum encoding rate needed to support real-time
	operation, in frames per second. */
    float           real_time_frame_rate;
    /* The number of frames encoded by a coder during the speed
	test. */
    int             frames_processed;
    /* The duration of the speed test so far. */
    INT32           duration;
    /* The value of duration before the most recent frame encoding. */
    INT32           last_duration;
	/* The number of ms required for the most recent frame encoding,
	 * and the min, max, and average over all frames encoded for a
	 * single coder.
	 */
    INT32           single_frame_time;
    INT32           min_single_frame_time;
    INT32           max_single_frame_time;
    float           avg_single_frame_time;
    /* The measured speed of the encoder, in frames per second. */
    float           measured_frame_rate;
    /* The fraction of the CPU power needed to encode frames "fed" to
	 * the encoder at the real-time frame rate.
	 */
    float           fraction_of_cpu;
    /* TRUE if the fraction_of_cpu < max_fraction_of_cpu for the
	 * coder.
	 */
    int             coder_usable;
	
    /*
	 * Allocate the temporary input and output buffers to be large
	 * enough to hold the largest frame size and encoded size for any
	 * of the coders.
	 */
    max_frame_size = 0;
    max_output_size = 0;
    for (i = 0; i < NCODERS; i++) {
		if (coders[i].frame_size > max_frame_size) {
			max_frame_size = coders[i].frame_size;
		}
		if (coders[i].output_size > max_output_size) {
			max_output_size = coders[i].output_size;
		}
    }
	
    decoded = (INT16 *) malloc(max_frame_size * sizeof(INT16));
    encoded = (UINT8 *) malloc(max_output_size * sizeof(UINT8));
	
    if (decoded == 0 || encoded == 0) {
		if (decoded != 0) {
			free(decoded);
		}
		if (encoded != 0) {
			free(encoded);
		}
		return FAIL;
    }
	
    /*
	 * The frame rates measured below use the whole CPU (or, for
	 * multitasking operating systems, at least all of the CPU given
	 * to the Nautilus process).  Later when speech is being sampled,
	 * encoded, encrypted, and sent out over the modem, the CPU will
	 * be busy doing things other than encoding.  Assume that 90% of
	 * the CPU can be devoted to encoding.
	 *
	 * Note that on a multitasking operating system, this speed
	 * testing might be run at a time when more of the CPU is
	 * available for the Nautilus process than is given to the process
	 * later.  If not enough of the CPU is devoted to Nautilus later,
	 * an encoder or decoder might no longer be able to keep up with
	 * the real-time rate of data, and could lose data.
	 * Unfortunately, few if any Unix systems have ways to guarantee
	 * that a process gets a certain fraction of the CPU, although
	 * giving the process a high priority certainly couldn't hurt.
	 */
    max_fraction_of_cpu = 0.90f;
	
    /* Generate random input vector */
    for (i = 0; i < max_frame_size; i++)
        decoded[i] = (INT16) rand();
	
    /* Test each coder and decoder */
	if (verbose_coder_speed_testing) {
	/*
	 * Make a table that looks like this:
	
	 Time for  Measured    Minimum  % of CPU
	 Coder     # of   Elapsed  1 frame   encoding  required   needed for
	 (encode/  frames   Time     (ms)      rate      rate     real-time  Coder
	 decode)   coded    (ms)   Avg. Max (frames/s) (frames/s) operation  usable?
	 --------  -----  -------  ---- ---  --------  ---------  ---------  -------
	 xxxxxxxx  xxxxx  xxxxxxx  xx.x xxx  xxxxxx.x  xxxxxx.x   xxx.x      xxx
	 xxxxxxxx  xxxxx  xxxxxxx  xx.x xxx  xxxxxx.x  xxxxxx.x   xxx.x      xxx
	 *
	 */
		printf("                          Time for  Measured    Minimum  %% of CPU\n");
		printf("  Coder   # of   Elapsed  1 frame     coding   required  needed for\n");
		printf("(encode/  frames   Time     (ms)        rate       rate  real-time   Coder\n");
		printf(" decode)  coded    (ms)   Avg. Max (frames/s) (frames/s) operation  usable?\n");
		printf("--------  -----  -------  ---- ---  --------  ---------  ---------  -------\n");
	}
    for (i = 0; i < NCODERS; i++) {
		coder_usable = TRUE;		/* assume coder usable at first */
		/* loop once for encoding random data, then again for decoding */
		for (j = 0; j < 2; j++) {
        /*
		 * Determine the minimum encoding rate for this coder, in
		 * frames per second, necessary to support real-time
		 * operation.
		 */
			real_time_frame_rate =   ((float) coders[i].sample_rate)
				/ ((float) coders[i].frame_size);
			
			/*
			 * Measure coder execution speed by counting the number of
			 * frames that can be coded in (approximately)
			 * min_test_duration_ms ms.
			 */
			coders[i].init();			/* initialize coder */
			frames_processed = 0;
			min_single_frame_time = 1000000;
			max_single_frame_time = -1;
			last_duration = UTimer(1);		/* start timer */
			do {
				if (j == 0) {
					coders[i].encode(decoded, encoded);	/* encode a block */
				}
				else {
					coders[i].decode(encoded, decoded);	/* decode a block */
				}
				frames_processed++;
				
				duration = UTimer(0);
				
				single_frame_time = duration - last_duration;
				if (single_frame_time < min_single_frame_time) {
					min_single_frame_time = single_frame_time;
				}
				else if (single_frame_time > max_single_frame_time) {
					max_single_frame_time = single_frame_time;
				}
				last_duration = duration;
				if ( frames_processed > 10000000  ) break;
				
			} while (duration < min_test_duration_ms);
			
			/* Compute the measured encoding rate, in frames per second. */
			measured_frame_rate = (float) (((float) frames_processed) / ((float) duration / 1000.0));
			
			avg_single_frame_time = (float) duration / (float) frames_processed;
			
			/*
			 * Determine the fraction of the CPU that the coder would use
			 * if it were operating at real-time rate.  Note that if this
			 * fraction is larger than 1, that means that the CPU cannot
			 * keep up with real-time rate.
			 */
			fraction_of_cpu = real_time_frame_rate / measured_frame_rate;
			
			/* encode or decode for this coder is not fast enough */
			if (fraction_of_cpu >= max_fraction_of_cpu) {
				coder_usable = FALSE;
			}
			
			if (verbose_coder_speed_testing) {
#if defined(__MSDOS__) || defined(MSDOS)
				printf("%6s %c %5d   %7ld  %4.1f %3ld  %8.1f   %8.1f      %5.1f    %s\n",
					coders[i].name,
					(j == 0 ? 'e' : 'd'),
					frames_processed,
					duration,
					avg_single_frame_time,
					max_single_frame_time,
					measured_frame_rate,
					real_time_frame_rate,
					100.0 * fraction_of_cpu,
					(j == 1 ? (coder_usable ? "yes" : "no") : "")
					);
#else
				printf("%6s %c %5d   %7d  %4.1f %3d  %8.1f   %8.1f      %5.1f    %s\n",
					coders[i].name,
					(j == 0 ? 'e' : 'd'),
					frames_processed,
					duration,
					avg_single_frame_time,
					max_single_frame_time,
					measured_frame_rate,
					real_time_frame_rate,
					100.0 * fraction_of_cpu,
					(j == 1 ? (coder_usable ? "yes" : "no") : "")
					);
#endif
			}
			
		}
		/* set or clear capability field for this encoder */
		SetCoderUsable(&negotiate, i, coder_usable);
    }
	
    if (verbose_coder_speed_testing)
		printf("\n");
	
    free(decoded);
    free(encoded);
	
    return SUCCESS;
}



/*
 * Return TRUE if the negotiation structure neg specifies that the
 * coder with number coder_id is usable, otherwise return FALSE.
 */

int
CoderUsable(struct negotiate_t *neg, int coder_id)
{
    int byte_number = coder_id / 8;
    int bit_position = coder_id % 8;
	
    if ((coder_id < 0) ||
		(coder_id >= 8 * sizeof(neg->encode)) ||
		(coder_id >= NCODERS)) {
		error(MSG_FATAL, "Attempted to examine negotiation bit for nonexistent speech coder");
		/* NOTREACHED */
    }
	
    if (neg->encode[byte_number] & (1 << bit_position)) {
		return TRUE;
    }
    return FALSE;
}



/*
 * Set or clear the appropriate coder capability bit in the 'encode'
 * field of the negotiate_t structure neg for coder number coder_id.
 * Set the bit if coder_usable is true, otherwise clear it.
 */

void
SetCoderUsable(struct negotiate_t *neg, int coder_id, int coder_usable)
{
    int byte_number = coder_id / 8;
    int bit_position = coder_id % 8;
	
    if ((coder_id < 0) ||
		(coder_id >= 8 * sizeof(neg->encode)) ||
		(coder_id >= NCODERS)) {
		error(MSG_FATAL, "Attempted to set negotiation bit for nonexistent speech coder");
		/* NOTREACHED */
    }
	
    if (coder_usable)
		neg->encode[byte_number] |= (1 << bit_position);
	else
		neg->encode[byte_number] &= ~(1 << bit_position);
}
