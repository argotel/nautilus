/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* cli.c
 *
 * SCCS ID:  @(#)cli.c 1.29 96/05/27
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/08  W. Dorsey          Module created by breakup of nautilus.c
 * 95/03/11  W. Dorsey          Added basic crypto stuff
 * 95/09/17  P. Kronenwetter    Added socket support from S. Parekh's 
 *                              patches.
 * 96/08/18  D. Miller          Blast pw1 and pw2 if passphrase key exchange
 *                              fails and between keyin mismatch retrys to
 *                              thwart runtime and post-mortem sniffing
 *                              (phew!)
 * 96/08/18  D. Miller          Added support for ring file and added verbose
 *                              parameter to InitAudio() calls.
 *                              Renamed 'vlogon' to 'vsound', since it now
 *                              also controls the ring sound
 * 97/06/01  D. Miller          Renamed 'VoiceLogon()' to 'PlayVoice()'
 * 97/06/15  D. Miller          Moved exit(1) to end of usage(), since all
 *                              calls did exit(1)'d immediately.
 * 00/11/05  J.Poehlmann        UNIX: search config files at standard place
 *                              LINUX:tweak makefile to get nautilus compiled  
 *                              UNIX: Do not throw away Modem init string 
 *                                    and allow to specify dial prefix  
 *                              UNIX: Allow to "go secure" in a phone call
 *                              replace unsafe gets by fgets to solve a 
 *				academic buffer overflow problen: user types 
 *				more then 128 letters when asked to blow 
 *				into the mike.
 * 04/02/29 S. Wieseckel	fixed compiler warning
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#ifdef linux
#include <sys/stat.h>	/* man 2 stat */
#include <unistd.h>	/* for memory lock */
#include <sys/mman.h>	/* for memory lock */
#include <sys/types.h>
#include <fcntl.h>		
#endif
#ifdef _WIN32
#include <math.h>
#endif

#include "nautilus.h"

/* external variables */
extern struct param_t params;		/* operating parameters */
extern struct coder_t coders[];		/* coder table */
extern struct cipher_t ciphers[];	/* cipher table */
extern struct negotiate_t negotiate;/* capability negotiation parameters */

extern char    *optarg;
extern int      optind;

/* forward declarations */
void net_settings();

#define CONFIGFILENAMLEN 256

int 
main(int argc, char *argv[])
{
    int             PlayVoice();
    int             c;
    char            connected;
    char            vsound = TRUE;
    char           *fname;
    char            fname1[CONFIGFILENAMLEN];
    char           *pw;
    char            pw1[MAX_SKEY_LEN+1];
    char            pw2[MAX_SKEY_LEN+1];
    char            tbuf[128];
    UINT8           audio_buf[2048];
    void            debug_puts(char *);
    int             random_fd, urandom_fd, wanted_random_bytes, random_bytes;
	
#ifdef linux					/* Linux only, for now */
    if (geteuid() == 0) {		/* if we have root privs */
		/* lock all current & future pages in physical memory */
		mlockall(MCL_CURRENT|MCL_FUTURE);/* don't care about failure */
		setuid(getuid());		/* drop root privs */
    }
#endif
	
    /* initialize internal variables */
    init();
	
    /* display title message */
    title();
	
    /* OS specific system initialization (interrupts, etc.) */
    if (SysInit() == FAIL) {
		fprintf(stderr, "System initialization failed.\n");
		exit(1);
    }
	
    /* read configuration file */

    if ((fname = getenv("NAUTILUS_CONFIG_FILE")) != NULL) {

	if (ReadConfigFile(fname) == FAIL) 
		exit(1);

#if defined(unix)       /* try home directory */
    

    } else {

       struct stat BUF;

       strncpy(fname1, "/etc/nautilus.cfg", CONFIGFILENAMLEN);
       if (stat(fname1, &BUF) == 0){  /* stat succes, File exists */
               if (ReadConfigFile(fname1) == FAIL)
                       exit(1);
       }
       /* We hope that it is legal to execute ReadConfigFile two times */
	       
	strncpy(fname1,getenv("HOME"),CONFIGFILENAMLEN);
	strcat (fname1,"/.nautilus.cfg");

        /* printf ("configfile: %s\n", fname1);  go away */
        if (stat(fname1, &BUF) == 0){  /* stat succes, File exists */
		if (ReadConfigFile(fname1) == FAIL)
			exit(1);
	}
	
	
#endif
     }
	
    /* parse arguments */
#if defined(unix)
    while ((c = getopt(argc, argv, "aAdhoOixc:e:k:l:L:n:p:s:v")) != -1)
#elif defined(_WIN32)
	while ((c = getopt(argc, argv, "aAdhoixc:e:j:k:l:L:n:v")) != -1)
#else
	while ((c = getopt(argc, argv, "aAdhoxc:e:k:l:p:s:v")) != -1)
#endif
		switch (c) {
		case 'a':
			params.mode = AUTO_ANSWER;
			break;
		case 'A':
			params.mode = ANSWER;
			break;
		case 'd':
		case 'D':
			params.key_ex_only = 1;
			break;
		case 'h':
			help();
			exit(0);
		case 'o':
			params.mode = ORIGINATE;
			params.orig_submode = DIAL;
			break;
		case 'O':
			params.mode = ORIGINATE;
			params.orig_submode = ATD_ONLY;
			break;
		case 'x':
			vsound = FALSE;
			break;
#if defined(unix) || defined(_WIN32)
		case 'i':
			params.net_flag = TRUE;
			break;
		case 'n':
			params.net_flag = TRUE;
			params.net.portnum = atoi(optarg);
			break;
#endif
		case 'c':
			if ((params.coder.index = FindCoder(optarg)) == -1) {
				fprintf(stderr, "%s: invalid coder\n", optarg);
				ListCoders();
				exit(1);
			}
			negotiate.coder = params.coder.index;
			break;
		case 'e':
			if ((params.crypto.key1.type = FindCipher(optarg)) == -1) {
				fprintf(stderr, "%s: invalid cipher\n", optarg);
				ListCiphers();
				exit(1);
			}
			params.crypto.key2.type = params.crypto.key1.type;
			negotiate.encrypt_type = params.crypto.key1.type;
			break;
#if defined(_WIN32)
		case 'j':
			params.jbufsecs = (float) atof(optarg);
			break;
#endif
		case 'k':
			if ((params.crypto.keyexch_type = FindKeyExch(optarg)) == -1) {
				fprintf(stderr, "%s: invalid key exchange protocol\n", optarg);
				fprintf (stderr,
					"valid protocols are: pp (passphrase), dh (768), dh512, dh768, dh1024, dh2048\n");
				exit(1);
			}
			negotiate.keyexch_type = (UINT8) params.crypto.keyexch_type;
			break;
		case 'l':
			if (!Strncasecmp("coders", optarg, strlen(optarg))) {
				ListCoders();
			}
			else if (!Strncasecmp("ciphers", optarg, strlen(optarg)) ||
				!Strncasecmp("cyphers", optarg, strlen(optarg))) {
				ListCiphers();
			}
			else {
				fprintf(stderr, "'%s' is an invalid argument to -l option\n\n",
					optarg);
				usage();
			}
			exit(0);
		case 'L':
            params.net_flag = TRUE;
			params.net.localport = atoi(optarg);
			break;
		case 'p':
			if (strcpy(params.port.name, GetPort(optarg)) == NULL) {
				fprintf(stderr, "%s: invalid serial port\n", optarg);
				exit(1);
			}
			break;
		case 's':
			params.port.speed = atoi(optarg);
			break;
		case 'v':
			params.verbose = TRUE;
			break;
		case '?':
			fprintf(stderr, "Invalid argument(s).\n\n");
			usage();
			break;
		}
	
	/* Finish processing arguments */
	if ( params.net_flag ) {
		net_settings();
	}

	if (params.mode == ORIGINATE) {
		if (optind + 1 == argc) {
#if defined(unix) || defined(_WIN32)
			if (params.net_flag == TRUE)
				strcpy(params.hostname, argv[optind]);
			else
#endif
				strcpy(params.telno, argv[optind]);
		}
		else if (optind != argc) {
			fprintf(stderr, "Extraneous argument(s).\n\n");
			usage();
		}
		else {
			if ( params.orig_submode == DIAL){
			    fprintf(stderr, "-o option requires argument.\n\n");
			    usage();
			}
		}
	}
	else if ((params.mode == ANSWER) || (params.mode == AUTO_ANSWER)) {
		if (optind != argc) {
			fprintf(stderr, "Extraneous argument(s).\n\n");
			usage();
		}

		if ( params.net.localport != 0 ){
				fprintf(stderr, "-L makes no sense with -a\n\n");
					usage();
		}

	}
	else {
		credits();
		fputs("No mode specified\n", stderr);
		usage();
	}
	
	/*
	 * Test the speed of all the speech coders to determine which are
	 * fast enough to be used.  Test each of them for at least 200 ms.
	 */
	if ( ! params.key_ex_only ) 
		CoderSpeedTest(200, params.verbose);
	
	/*
	 * Initialize communications channel (network or serial)
	 */
	
	/*
	  This was part of a failed experiment to allow the answerer to
	  remain listening after a failed connection.  Someone with two
	  machines in the same room could probably get this going.
	  There is a matching while below:
	
	  do {
	*/
	
	if (params.net_flag) {
		params.session = nsp_create(debug_puts, "udp");
		if (params.session == NULL)
			error(MSG_FATAL, "Could not initialize socket");
		sprintf(tbuf, "%d", params.net.portnum);
		nsp_ioctl(params.session, "udp_port", tbuf);
	}
	else {
		params.session = nsp_create(debug_puts, "modem");
		if (params.session == NULL)
			error(MSG_FATAL, "Could not initialize serial port");
		sprintf(tbuf, "%s,%d", params.port.name, params.port.speed);
		if (nsp_ioctl(params.session, "modem_open", tbuf) == -1)
			error(MSG_FATAL, "Could not open serial port");
	}
	
	    if ( ! params.key_ex_only ){
	    /*
	     * Initialize audio device for possible playing of startup sound,
	     * and for recording of sound samples that will be used to
	     * initialize the pseudo-random number generator.
	     */
	        if (InitAudio(8000, 128, params.verbose, 0.5) == FAIL) {
		    error(MSG_FATAL, "Could not initialize audio device");
		    /* NOTREACHED */
	        }
	}

	/* Print startup information. */
	if (params.net_flag == TRUE) {
		if ( params.net.localport != 0 ){
			printf("Selected Source Port : %5d\t Destination Port : %5d\t",
					params.net.localport, params.net.portnum);
		} else {
				printf("Selected Port : %5d\t\t", params.net.portnum);
		}
		printf("Network Protocol : %s\n", "UDP");
	}
	else {
		printf("Selected Port : %s\t\t", params.port.name);
		printf("DTE Speed : %u\n", params.port.speed);
	}
	
	if (vsound && !  params.key_ex_only ) {
		/* Play logon file unless supressed. */
		#ifdef _WIN32
		PlayLogon();

		#else
		if (PlayVoice(params.logon_file) == FAIL)
			fprintf(stderr, "%s not found", params.logon_file);
		#endif
	}
	
	if ((params.crypto.keyexch_type == PASSPHRASE) &&
		(params.crypto.key1.type != NONE)) {
		/*
		* Get the key either from the configuration file, or from the
		* keyboard.
		*/
		if ((pw = getenv("NAUTILUS_PASSPHRASE")) != NULL)
			strncpy(pw1, pw, MAX_SKEY_LEN);
		else
			for (;;) {
				if (GetPassPhrase(pw1, MAX_SKEY_LEN, "\nEnter passphrase: ") < 0) {
					memset(pw1, '\0', MAX_SKEY_LEN);
					error(MSG_FATAL, "could not get passphrase");
					/* NOTREACHED */
				}
				if (GetPassPhrase(pw2, MAX_SKEY_LEN, "Enter it again  : ") < 0) {
					memset(pw1, '\0', MAX_SKEY_LEN);
					memset(pw2, '\0', MAX_SKEY_LEN);
					error(MSG_FATAL, "could not get passphrase");
					/* NOTREACHED */
				}
				if (strcmp(pw1, pw2) == 0) {
					/* pw2 never needed after the above if */
					memset(pw2, '\0', MAX_SKEY_LEN);
					break;
				}
				memset(pw1, '\0', MAX_SKEY_LEN);
				memset(pw2, '\0', MAX_SKEY_LEN);
				fprintf(stderr, "\nPassphrases did not match, try again.\n");
			}
		
		/*
		 * Initialize key structure with the passphrase.
		 */
		if (strlen(pw1) == 0) {
		/*
		 * This used to set the passphrase to the version string if
		 * the user didn't type in any passphrase at all.  However,
		 * that causes incompatibility between versions, so I now
		 * hardcoded it to a specific string that should remain
		 * fixed between versions that are compatible with each
		 * other.
		 */
			strcpy(pw1, "UNODIR, ESBAM");
		}
		if (keyinit(&params.crypto.key1, pw1, strlen(pw1), params.mode) < 0) {
			memset(pw1, '\0', MAX_SKEY_LEN);
			error(MSG_FATAL, "Key initialization failed.");
			/* NOTREACHED */
		}
		if (keyinit(&params.crypto.key2, pw1, strlen(pw1), !params.mode) < 0) {
			memset(pw1, '\0', MAX_SKEY_LEN);
			error(MSG_FATAL, "Key initialization failed.");
			/* NOTREACHED */
		}
		memset(pw1, '\0', MAX_SKEY_LEN);
	}
	
	if (params.crypto.key1.type != NONE) {
		/*
		 * Sample audio data to use as "random" bits for initializing
		 * the pseudo-random number generator.
		 */
		memset(audio_buf, '\0', 2048);
		for (;;) {
			float entropy;
			
			if (!  params.key_ex_only ){
			   AudioFlow(TRANSMIT);
			   if (ReadAudio(audio_buf, 2048/128) == FAIL)
				error(MSG_FATAL, "ReadAudio() failure.");
			   AudioFlow(RECEIVE);
			} else {
			  /* get some random into audio_buf
			     as we disabled audio . Under Linux
			     we could use /dev/urandom. We must
			     realize that we could and probably 
			     will exhaust the entropy pool.
			  */
			
#ifdef linux


			  /* Linux has a random device which offers data as long as the system
			    has still enough "randomnes" collected. We use it */
			 
				  wanted_random_bytes = 1024 - 2;

				  random_fd = open ("/dev/random",O_RDONLY,O_NONBLOCK);
				  if ( random_fd > 0 ) 
				  {
					random_bytes = read (random_fd, audio_buf, 64);

					if  ( random_bytes  < 64 ) /* Just define a minimum of "real random" */
					{
							error(MSG_WARNING, "few real LINUX random ");
					}

					if (random_bytes <= wanted_random_bytes)  /* should be */
					{
						char *p;

						/* Fill the rest with Linux Pseudorandom */
						urandom_fd = open ("/dev/urandom",O_RDONLY,O_NONBLOCK);

						if ( urandom_fd <= 0 )
						{	
							error(MSG_FATAL,"could not open LINUX random dev" );
							exit(1);
						}

						wanted_random_bytes= wanted_random_bytes - random_bytes;
						p= audio_buf+random_bytes;
						random_bytes = read (urandom_fd, audio_buf, 
							 wanted_random_bytes);
						close(urandom_fd);
					} 

					close(random_fd);

				  } else { 
						error(MSG_FATAL,"could not open LINUX random dev" );
						exit(1);
				  }
#else

								  
				/* windows emergeny very pseudo-random */
				  pid_t 	pid=getpid();
				  time_t	tim=time();
				  memcpy (audio_buf,&pid,sizeof(pid_t));
				  memcpy (audio_buf+10,&tim,sizeof(time_t));
#endif

			}              
			
			entropy = ComputeEntropy(audio_buf, 2048);
			if (entropy > 10.0) {
				printf("\nAudio Entropy is %.1f (satisfactory).\n", entropy);
				break;
			}
			else {
				printf("\nWARNING:  Audio Entropy is %.1f (low).\n", entropy);
				printf("\nPlease make sure the microphone is on.  Blow into the microphone for\n");
				printf("about 1 second, and hit the <Return> key while blowing.  If you want\n");
				printf("to override the entropy check, type 'q' followed by <Return>: ");
				fflush(stdout);
				fgets(tbuf,15,stdin); /* be paranoid */
				if (toupper(tbuf[0]) == 'Q')
					break;
			}
		}
		random_init(audio_buf, 2048);
	}
	
	/* close the audio device */
	if (!  params.key_ex_only )
		CloseAudio();
	
	/* begin communicating */
	if (params.net_flag) {
		switch (params.mode) {
		case ANSWER:
		case AUTO_ANSWER:
			printf("Waiting for incoming Nautilus connection...\n");
			if (nsp_open(params.session, NULL, -1) == -1) {
				error(MSG_FATAL, "Failed to connect to remote socket.");
				exit(1);
			}
			break;
		case ORIGINATE:
			if (nsp_open(params.session, params.hostname, -1) == -1) {
				error(MSG_FATAL, "Failed to connect to remote socket.");
				exit(1);
			}
			break;
		}
	}
	else {
		printf("\n");
		switch (params.mode) {
		case ANSWER:
			if (nsp_open(params.session, NULL, 60) == -1) {
				error(MSG_FATAL, "Failed to connect to remote modem.");
				exit(1);
			}
			break;
		case AUTO_ANSWER:
			if (nsp_open(params.session, NULL, 0) == -1) {
				error(MSG_FATAL, "Failed to connect to remote modem.");
				exit(1);
			}
			break;
		case ORIGINATE:
			if (nsp_open(params.session, params.telno, 60) == -1) {
				error(MSG_FATAL, "Failed to connect to remote modem.");
				exit(1);
			}
			break;
		}
		
		/* get modem connect speed */
		params.modem.speed = nsp_ioctl(params.session, "connect_speed", NULL);
	}
	
	/* exchange operating parameters with other side */
	if ((connected = XChange(params.mode)) == FAIL) {
		fprintf(stderr, "Failed to startup.\n");
		if (params.mode == ORIGINATE)
			exit(2);
	}
	/*
	This is part of the failed answering while loop.
	Maybe we can get this going later.                      --dm 06/15/97
	}
	while (connected == FAIL);
	*/
	
	if (vsound && (!  params.key_ex_only ) &&(params.mode == ANSWER 
                       || params.mode == AUTO_ANSWER)) {
		/* Play ring file unless supressed.
		 * We must Re-open audio after getting a connection request,
		 * because we want the device to remain available until someone calls
		 */
		if (InitAudio(8000, 128, FALSE, 0.5) == FAIL) {
			error(MSG_FATAL, "Could not initialize audio device");
			/* NOTREACHED */
		}
        #ifdef _WIN32
        PlayRing();
        #else
		if (PlayVoice(params.ring_file) == FAIL)
			fprintf(stderr, "%s not found", params.ring_file);
		#endif
		CloseAudio();                   /* close the audio device */
	}
	
	/* Print coder & cipher information. */
	printf("\nSelected Coder: %s\n", coders[params.coder.index].name);
	printf("Encryption : %s\n", ciphers[params.crypto.key1.type].name);
	
	/* Print DCE speed if we're using the modem to connect */
	if (!params.net_flag) {
		if (params.modem.speed)
			printf("Modem Speed : %u\n", params.modem.speed);
		else
			printf("Modem Speed : UNKNOWN\n");
	}
	
	if (!  params.key_ex_only ){
	    /*
	     * Create turnaround beep at the correct sampling rate (i.e., the
	     * sampling rate used by the selected speech coder.
	     */
	    InitBeep(coders[params.coder.index].sample_rate);
	
	    /* Initialize audio device for the selected speech coder. */
	    if (InitAudio(coders[params.coder.index].sample_rate,
		coders[params.coder.index].frame_size, params.verbose,
		params.jbufsecs) == FAIL) {
		fprintf(stderr, "Could not initialize audio device.\n");
		exit(2);
	    }
	
	    /* begin talking */
	    Talk(params.mode == ORIGINATE ? TRANSMIT : RECEIVE);
	
	} 
	if (params.crypto.key1.type != NONE)
		keydestroy(&params.crypto.key1);	/* wipe key schedule */
	
	nsp_close(params.session);		/* close communications channel */
	if (!  params.key_ex_only )
		CloseAudio();			/* close audio device */
	
	exit(0);
}


void
title(void)
{
    fprintf(stderr, "\nNautilus Digital Voice Communicator -- %s\n", VERSION_STRING);
    fprintf(stderr, "Copyright (c) 1993-2000 William W. Dorsey, All Rights Reserved\n\n");
}


void
credits(void)
{
    fprintf(stderr, "Nautilus was originally developed by William Dorsey, Pat Mullarky, and\n");
    fprintf(stderr, "Paul Rubin.  For more information on Nautilus, including a list of the\n");
	fprintf(stderr, "current development team, latest version, frequently asked questions,\n");
	fprintf(stderr, "and more, visit <http://www.franken.de/crpyt/nautilus>\n\n");
}


void
help(void)
{
    char *cipher = ciphers[params.crypto.key1.type].name;
	
    fprintf(stderr, "Here's a quick summary of Nautilus commands...\n\n");
    fprintf(stderr, "-h produces this summary\n");
    fprintf(stderr, "-o selects originate dialup mode\n");
#ifndef _WIN32
    fprintf(stderr, "-O selects originate mode with modem takeover\n");
    fprintf(stderr, "    (go secure while in a phone call)\n");
#endif
    fprintf(stderr, "-a/-A selects auto/manual answer mode\n");
    fprintf(stderr, "-d key exchange only and end\n");
#ifndef _WIN32
    fprintf(stderr, "-p <port> selects serial port modem is connected to\n");
    fprintf(stderr, "-s <speed> selects serial DTE (default=%d)\n", params.port.speed);
#endif
    fprintf(stderr, "-c <coder> overrides automatic coder selection\n");
    fprintf(stderr, "-e <cipher> selects encryption cipher (default=%s)\n", cipher);
#ifdef _WIN32
	fprintf(stderr, "-j <seconds> selects size of jitter buffer (default=%4.1f secs)\n", params.jbufsecs);
#endif
    fprintf(stderr, "-k <protocol> selects key exchange protocol (default=768-bit DH)\n");
    fprintf(stderr, "-l <coders|ciphers> lists available coders or ciphers\n");
	fprintf(stderr, "-L <port number> select local port number while origionating call\n");
    fprintf(stderr, "-x suppresses startup and ring sound\n");
    fprintf(stderr, "-v verbose mode.  Currently only gives detailed coder speed testing data.\n");
#if defined(unix) || defined(_WIN32)
    fprintf(stderr, "-i selects the use of sockets rather than modems\n");
    fprintf(stderr, "-n <port> selects port # to use. (default=)\n");
#endif
#ifndef _WIN32
    fprintf(stderr, "\nTo originate a dialup call, type:\n");
    fprintf(stderr, "\tnautilus -o -p COM1 <phone number>\n");
    fprintf(stderr, "To accept an incoming dialup call, type:\n");
    fprintf(stderr, "\tnautilus -a -p COM1\n");
#endif
#if defined(unix) || defined(_WIN32)
    fprintf(stderr, "\nTo connect to another computer via a network (on port #12345):\n");
    fprintf(stderr, "\tnautilus -o -i -n 12345 <hostname>\n");
    fprintf(stderr, "To accept an incoming call via a network (on port #12345):\n");
    fprintf(stderr, "\tnautilus -a -i -n 12345\n\n");
#endif
    fprintf(stderr, "For more information, refer to the Nautilus web page at\n");
	fprintf(stderr, "<http://www.lila.com/nautilus>\n");
}


void
usage(void)
{
    fprintf(stderr, "For a usage summary, type:  nautilus -h\n");
    exit(1);
}


/*
 * display list of available coders
 */

void
ListCoders(void)
{
    int             i;
	
    printf("List of available coders:\n");
    for (i = 0; i < NCODERS; i++)
		printf("%s\t%s\n", coders[i].name, coders[i].desc);
    printf("\n");
}


/*
 * display list of available ciphers
 */

void
ListCiphers(void)
{
    int             i;
	
    printf("List of available ciphers:\n");
    for (i = 0; i < NCIPHERS; i++)
		printf("%10s  %s\n", ciphers[i].name, ciphers[i].desc);
    printf("\n");
}


/*
 * show user what mode we're in (transmit or receive)
 */

void
ShowMode(enum flow mode)
{
    if (mode == RECEIVE)
		printf("\rListening...     (q=quit, <CR>=Talk)  ");
    else
		printf("\rGo ahead...      (q=quit, <CR>=Listen)");
    fflush(stdout);
}


/*
 * error message handler
 */

void
error(enum err_type type, char *fmt, ...)
{
    va_list args;
	
    va_start (args, fmt);
	
    fprintf (stderr, "\n");
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
    fflush(stderr);
    if (type == MSG_FATAL)
		exit(2);
}


/*
 * print a string of debug information to stderr
 */

void
debug_puts(char *s)
{
    fputs(s, stderr);
    fputc('\n', stderr);
    fflush(stderr);
}


/*
 * print a string of debug information to stderr without a newline
 */

void
debug_puts_nr(char *s)
{
    fputs(s, stderr);
    fflush(stderr);
}


/*
 * print a character of debug information to stderr
 */

void
debug_putc(char c)
{
    putc(c, stderr);
    fflush(stderr);
}

void 
net_settings()
{
	/* Setup/override some defaults */
	if( params.net.portnum == 0)
		params.net.portnum = DEFAULT_PORT_NO;
	params.rp_timeout = 30;
	params.sp_timeout = 30;
}

