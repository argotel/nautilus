/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* talk.c
 *
 * SCCS ID: @(#)talk.c 1.8 96/05/12
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/08  B. Dosrey          Module created by breakup of nautilus.c
 * 94/10/15  P. Mullarky        Allow both sides to change PTT status
 * 95/03/11  B. Dorsey          Add encryption
 */

#include <stdio.h>
#include <malloc.h>
#include <errno.h>
#if defined(unix)
#include <unistd.h>
#endif

#include "nautilus.h"

/* external variables */
extern struct coder_t coders[];	/* coder table */
extern struct param_t params;	/* operating parameters */
extern UINT8    beep[];		/* the turnaround cue tone */

/* local variables */
static int timeout_fail = 6;	/* max wait time before failing */
static int timeout_eot = 3;	/* max wait time between EOTs */

void
Talk(enum flow mode)
{
    int             i;
    int             done;
    int             sample_rate;
    int             frame_size;
    int             output_size;
    int             frames_per_pkt;
    struct packet_t packet;
    UINT8          *ptr;
    UINT8          *audio_data;
    UINT8          *output_data;
    INT16          *samples;

    /* initialize memory */
    sample_rate = coders[params.coder.index].sample_rate;
    frame_size = coders[params.coder.index].frame_size;
    output_size = coders[params.coder.index].output_size;
    frames_per_pkt = coders[params.coder.index].frames_per_pkt;
    audio_data = malloc(frame_size + 1);
    output_data = malloc(output_size * frames_per_pkt);
    samples = malloc(frame_size * sizeof(INT16));

    /* initialize coder */
    coders[params.coder.index].init();

    /*
     * Change timeout values if we're running over a network.  Note
     * that the values chosen below is somewhat arbitrary and may
     * need to be tweaked.
     */
    if (params.net_flag) {
        timeout_fail = 45;
        timeout_eot = 10;
    }

    /* main loop initialization */
    done = 0;
    AudioFlow(mode);
    puts("\n***** BEGIN COMMUNICATING *****\n");
    ShowMode(mode);		/* display operating mode to user */

#if 0
    /*
     * If we're in network mode and we're receiving, we should be
     * receiving a bunch of DATA packets.  In the interest of having
     * packets in the queue so that we can handle changes in network
     * latency without loss of audio, we first load the queue with
     * params.net.q_init packets.
     */
    if (params.net_flag && (mode == RECEIVE)) {
        /*
         * this next call just loads the queue, nothing is actually
         * stored in packet.
         */
        ReadPkt(&packet, params.rp_timeout, -params.net.q_init);
    }
#endif

    /* main loop */
    do {
    	switch (mode) {
    	case TRANSMIT:
    	    /*
    	     * see if there are any incoming packets to be processed
    	     */
            if (ReadPkt(&packet, 0) >= 0) {
                switch (packet.type) {
                case REOT:
                    ReadPkt(&packet, timeout_eot);
                    done = 1;
                    break;
                case RECV:		/* change modes */
                    mode = RECEIVE;
                    AudioFlow(mode);	/* resume audio sampling */
                    ShowMode(mode);	/* display operating mode */
                    coders[params.coder.index].init();
                    ptt();		/* discard keyboard input */
                    break;
                case XMIT:		/* ignore XMIT packets */
                    error(MSG_WARNING, "received unexpected XMIT packet");
                    break;
                case UDATA:		/* ignore UDATA packets */
                    break;
                case RDATA:		/* ignore RDATA packets */
                    error(MSG_WARNING, "received unexpected RDATA packet");
                    break;
                case FILL:		/* ignore FILL packets */
                    error(MSG_WARNING, "received unexpected FILL packet");
                    break;
                case UEOT:		/* ignore UEOT packets */
                    error(MSG_WARNING, "received unexpected UEOT packet");
                    break;
                default:		/* ignore unknown packets */
                    error(MSG_WARNING, "unknown packet type received");
                    break;
                }
                /*
                 * If we are now in RECEIVE mode (because we received
                 * a RECV packet above, we need to break out of the
                 * TRANSMIT side of the outermost switch statement.
                 */
                if (mode == RECEIVE)
                    break;
            }
            else if (errno != EINTR) {
                error(MSG_FATAL, "Fatal error reading packet");
            }

	    /*
    	     * see if the user wants to change modes
    	     */
    	    if (ptt()) {
    	        SendPkt(XMIT, NULL, 0, params.sp_timeout);
    	        mode = RECEIVE;
    	        AudioFlow(mode);
    	        ShowMode(mode);		/* display operating mode */
    	        coders[params.coder.index].init();
    	        ptt();			/* flush keyboard input */
    	    }

    	    /*
    	     * see if the user wants to quit
    	     */
    	    if (quit()) {
    	    	SendPkt(REOT, NULL, 0, params.sp_timeout); /* expect ACK */
    	    	SendPkt(UEOT, NULL, 0, params.sp_timeout); /* expect nada */
    	    	sleep(1);			/* let buffers empty */
    	    	done = 1;
    	    }

    	    /*
    	     * exit TRANSMIT mode if appropriate
    	     */
	    if ((mode == RECEIVE) || (done == 1))
	    	break;

	    /*
	     * process incoming audio data and transmit it
	     */
	    for (i = 0, ptr = output_data; i < frames_per_pkt; i++, ptr = &ptr[output_size]) {
#if DEBUG > 0 && defined(sun)
		/* check for overflow on audio device */
		if (AudioOverflow())
		    error(MSG_WARNING, "audio overflow detected");
#endif

	     	/* read samples from the audio device */
	     	if (ReadAudio(audio_data, 1) < 0)
	     	    error(MSG_WARNING, "ReadAudio() failure!\n");

	     	/* convert samples into standard form */
	     	Raw2Std(audio_data, samples, (INT16) frame_size);

	     	/* Encode audio data */
		coders[params.coder.index].encode(samples, ptr);
	    }

	    if (params.crypto.key1.type != NONE) {
		/*
		 * encrypt compressed audio data
		 */
		cfb_encrypt(&params.crypto.xmit_ctx, output_data,
			    output_size * frames_per_pkt);
	    }

	    SendPkt(UDATA, output_data, output_size * frames_per_pkt,
	            params.sp_timeout);
	    break;

    	case RECEIVE:
	    /*
    	     * see if the user wants to change modes
    	     */
    	    if (ptt()) {
    	        SendPkt(RECV, NULL, 0, params.sp_timeout);
    	        mode = TRANSMIT;
    	        AudioFlow(mode);
    	        ShowMode(mode);		/* display operating mode */
    	        coders[params.coder.index].init();
    	        ptt();			/* flush keyboard input */
    	        break;
    	    }

    	    /*
    	     * see if the user wants to quit
    	     */
    	    if (quit()) {
    	    	SendPkt(REOT, NULL, 0, params.sp_timeout); /* expect ACK */
    	    	SendPkt(UEOT, NULL, 0, params.sp_timeout); /* expect nada */
    	    	sleep(1);			/* let buffers empty */
    	    	done = 1;
    	    	break;
    	    }

    	    /*
    	     * get a packet from transmitter or timeout
    	     */
            if (ReadPkt(&packet, timeout_fail) < 0) {
                if (errno == EINTR) {
                    error(MSG_FATAL, "No response from other side");
                }
                else {
                    error(MSG_FATAL, "Fatal error reading packet");
                }
            }

    	    /*
    	     * process packets from transmitter
    	     */
    	    switch (packet.type) {
            case REOT:
                ReadPkt(&packet, timeout_eot);
                done = 1;
                break;
	    case UDATA:
	    	if (packet.length == output_size * frames_per_pkt) {
		    if (params.crypto.key1.type != NONE) {
			/*
			 * decrypt incoming packet
			 */
			cfb_decrypt(&params.crypto.recv_ctx, packet.data,
			            output_size * frames_per_pkt);
		    }

	    	    /*
	    	     * decode incoming audio frames
	    	     */
	    	    for (i = 0, ptr = packet.data; i < frames_per_pkt; i++, ptr = &ptr[output_size]) {
	    	    	/* decode audio data */
	    	    	coders[params.coder.index].decode(ptr, samples);

	    		/* convert samples to raw form */
	    		Std2Raw(samples, audio_data, (INT16) frame_size);

#if DEBUG > 0 && defined(sun)
			/* check for underflow on audio device */
			if (AudioUnderflow())
			    error(MSG_WARNING, "audio underflow detected");
#endif

	    		/* output samples to audio device */
	    		WriteAudio(audio_data, 1);
		    }
	    	}
	    	else {
	    	    error(MSG_WARNING, "received garbled packet");
	    	}
	    	break;
	    case FILL:
#if DEBUG > 0
	        debug_puts("processing FILL packet");
#endif
		/* generate empty packet for output */
		for (i=0; i<frame_size; i++)
		    samples[i] = 0;
		Std2Raw(samples, audio_data, (INT16) frame_size);
		/* output empty packet frames_per_pkt times */
		for (i=0; i<frames_per_pkt; i++)
		    WriteAudio(audio_data, 1);
	        break;
	    case XMIT:
	    	while (WriteAudio(beep, (sample_rate / 1000) * BEEP_MS / frame_size) == FAIL)
	    	     ;
	    	DrainAudio();
	    	mode = TRANSMIT;
	    	AudioFlow(mode);
	    	ShowMode(mode);
	    	coders[params.coder.index].init();
	    	ptt();
	    	break;
	    case RECV:
	        error(MSG_WARNING, "received unexpected RECV packet");
	    	break;
	    case UEOT:
	        error(MSG_WARNING, "received unexpected UEOT packet");
	    	break;
	    case RDATA:
	        error(MSG_WARNING, "received unexpected RDATA packet");
	    	break;
	    default:
                error(MSG_WARNING, "unknown packet type received");
	    	break;
    	    }
	    break;

    	} /* switch (mode) */
    } while (!done);

    puts("\n\n***** TRANSMISSION COMPLETE *****\n\n");
    free(samples);
    free(output_data);
    free(audio_data);
}
