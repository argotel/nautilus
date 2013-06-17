/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* unix.c -- Unix OS specific functions
 *
 * SCCS ID:  @(#)unix.c 1.10 96/05/25
 */

/* REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/09/12  B. Dorsey          Added skeleton functions.
 * 93/09/23  B. Dorsey		Added IncomingPort() and UngetPort().
 * 93/10/13  B. Dorsey		Filled in all skeleton functions.
 * 93/10/13  B. Dorsey		Implemented ptt() and quit().
 * 93/10/13  B. Dorsey		Added argument to InitPort to allow
 *				port speed selection.
 * 93/10/20  B. Dorsey		Created AudioFlow().
 * 93/11/01  B. Dorsey		Added OS specific initialization and
 *				cleanup functions SysInit() and SysDone().
 * 94/10/17  B. Dorsey		Added timeout capability to ReadPort() and
 *				fixed a long-standing bug in ReadPort().
 * 95/08/26  B. Dorsey		Added UTimer() function.
 * 95/09/16  P. Kronenwetter    Added socket support from S. Parekh patches
 *                              dated 95/06/03. Modified for compilation of
 *                              single program for modems & sockets.
 * 96/03/14  P. Kronenwetter    'Removed' the SunOS 4.1.3 audio library bug
 *                              fix when compiling for Solaris.
 * 96/12/15  D. Miller          Removed unused variables
 * 97/05/01  D. Miller          Renamed 'VoiceLogon()' to 'PlayVoice()'
 * 97/07/22  D. Miller          PlayVoice() now returns SUCCESS/FAIL
 * 04/02/29  S. Wieseckel       fixed compiler warning
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <time.h>

#if defined(sun)
#define	AUDIO_DEVICE	"/dev/audio"
#include <stropts.h>
#include <sys/termios.h>
#include <sys/filio.h>
#include <sys/stat.h>
#ifdef SOLARIS
#include <multimedia/audio_device.h>
#include <multimedia/audio_encode.h>
#include <multimedia/audio_errno.h>
#include <multimedia/libaudio.h>
#else
#include <multimedia/audio_hdr.h>
#include <multimedia/ulaw2linear.h>
#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>
#include <sun/audioio.h>
#endif
#elif defined(linux)
#define	AUDIO_DEVICE	"/dev/dsp"
#define	MIXER_DEVICE	"/dev/mixer"
#include <termios.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#endif

/* Added for sockets */
/* Since I believe these include files are applicable for any type */
/* of unix system there's no reason to put them above. */
/* PJK - 95-09-17 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "machine.h"
#include "nautilus.h"

extern struct param_t params;	/* operating parameters */
extern struct coder_t coders[NCODERS];

#define	IOBUFSZ		256	/* size of i/o buffer */

int             fdtabsize;	/* size of file descriptor table */

/* return pathname corresponding to port designated by arg */
char           *
GetPort(char *arg)
{
    return arg;
}

/* s_port is the file descriptor of the serial port */
/* Removed the 'static' part of this declaration */
/* otherwise s_port isn't available as an extern. */
int      s_port;

/* iobuf is a buffer, internal to the Nautilus application, of
   characters read from the serial port.  New characters read are
   added to the buffer starting from position tail, and characters
   read by calls to ReadPort() come first from position head, if
   (head<tail).  There are always (tail-head) characters in iobuf that
   have not yet been read by a call to ReadPort().

   It appears that the entire reason for this buffer is so that bytes
   may be read from the serial port with the read(2) system call more
   than one at a time, but the application can use ReadPort() to get
   them 1 or a few at a time, without the overhead of a system call.
*/
static int      head, tail;
static UINT8    iobuf[IOBUFSZ];

/* The table bauds[] is used in InitPort to convert the desired speed
   in bits/second into the bit mask needed to do an ioctl() call on
   the serial port.  It should not be changed, unless the necessary
   bit masks used by the ioctl() call change.  In particular, note
   that you can't make other speeds available simply by changing this
   array.  The serial port device driver in the kernel would have to
   be changed as well. */
static int      bauds[] =
{
    0, 50, 75, 110, 134, 150, 200, 300, 600,
    1200, 1800, 2400, 4800, 9600, 19200, 38400, -1
};



/*

   FUNCTION: InitPort

   PURPOSE:

   Initialize the serial port specified by the file name in arg.

   Open the serial port with file name arg and initialize it to be in
   as "raw" a mode as possible.  That is, no terminal-like processing
   of ERASE and KILL characters will be done.  The goal is to set it
   up simply to be a raw pipe of 8 bit characters.  The argument speed
   specifies the speed in bits per second to set the port to.  The
   port will be opened for both reading (receiving data from the
   modem) and writing (sending data to the modem).
   
   ARGUMENTS:

   char *arg

   A file name for a serial port to open for reading and writing.  In
   Linux, for example, this could be the device file name
   "/dev/modem".

   UINT16 speed

   A speed to set the serial port to, in bits per second.  It should
   be one of the values in the array bauds[] declared and initialized
   above, or this function will not do anything except return the
   error code FAIL.

   RETURN VALUE:
   
   FAIL if there is some form of failure, like a failure to open the
   file specified in arg, or if speed gives a rate not equal to one of
   the rates in the array bauds[].  If everything goes well, SUCCESS
   is returned.

   SIDE EFFECTS:

   The pointers head and tail into iobuf are both set to zero,
   which indicates an empty buffer.  s_port is changed to the file
   descriptor of the newly opened serial port.  The ioctl() settings
   of the serial port are changed as detailed below.  All but the last
   side effect are only "visible" to functions declared in this file,
   because head, tail, and iobuf are declared statically within this
   file.  The changes to the serial port are "permanent" until some
   other Unix process changes them.  The changes last even after this
   process exits.
*/

int
InitPort(char *arg, UINT16 speed)
{
    int             brate;
    struct termios  tbuf;

    /* computer baud rate argument */
    /*
     * The bauds[] array is set up so that if bauds[x] ==
     * desired_baud_rate, then x is the bit mask that should be OR'ed with
     * the c_cflag field of the termios structure for the terminal.
     */
    for (brate = 0; bauds[brate] != -1; brate++)
	if (speed == bauds[brate])
	    break;
    if (bauds[brate] < 0)
	return FAIL;

    /* Set head and tail so that iobuf is considered empty. */
    head = tail = 0;

    /* Open the port for reading and writing.  O_NOCTTY means that the
       serial port should not become the controlling terminal for this
       process. */
    s_port = open(arg, O_RDWR | O_NOCTTY);
    if (s_port < 0) {
	return FAIL;
    }

    /*
     * Get the current settings for the serial port and store them in tbuf.
     */
    ioctl(s_port, TCGETS, (char *) &tbuf);

    /* Change some of the settings in tbuf to desired values.

       The GNU C library info documentation contains a section titled
       Low-Level Terminal Interfaces, with a subsection titled
       Terminal Modes.  All section names given in the comments below
       are subsections of Terminal Modes, unless otherwise stated.
       */

    /*
     * The section Control Modes describes the meanings for the
     * c_cflag modes set below.
     * 
     * The first assignment (with &=) turns off all bits in the c_cflag
     * field except for the CREAD, HUPCL, CLOCAL, and CRTSCTS bits.  Those
     * 4 bits are left with their previous values, whatever they were.
     * 
     * Actually, the CREAD, HUPCL, and CLOCAL bits are described in the GNU
     * info documentation, but CRTSCTS is not even mentioned. It is
     * mentioned in the linux include file
     * /usr/src/linux/include/linux/termios.h, and has the comment "flow
     * control" after it, but that doesn't say much about the kind of flow
     * control.  I would guess from the name that it has something to do
     * with the ready to send (RTS) and clear to send (CTS) lines in the
     * RS232 standard.
     * 
     * Why are CREAD, HUPCL, and CLOCAL left at their previous values, rather
     * than simply set to some desired values?  Is it important to
     * preserve their former values?
     * 
     * From looking at the description of CREAD in the info pages, it
     * looks like Nautilus should have this bit set.  Why would we
     * ever want to leave the bit cleared if it was clear before this
     * function was called? Then Nautilus wouldn't work.  Could it be
     * that the authors intended to set the bit with the statement
     * below, but didn't realize that the statement that was written
     * only turns off all of the bits not explicitly mentioned?

     The second assignment (with |=) sets the desired bit rate
     for the serial port, sets all characters to be a full 8 bits
     long, and sets the CRTSCTS flag, whatever that does.

     */

    tbuf.c_cflag &= (CREAD | HUPCL | CLOCAL | CRTSCTS);

    tbuf.c_cflag |= brate | CS8 | CRTSCTS;	/* CRTSCTS? */

    /*
     * From reading the section Input Modes, c_iflag = 0 appears to
     * instruct the kernel to pass through input characters from the
     * serial port in as "raw" a way as possible, with no parity bits
     * checked, all 8 bits of each character passed through
     * unmodified, carriage returns and line feeds are not processed
     * differently, no start/stop flow control is performed, and no
     * BEL character is sent to the terminal if the terminal input
     * buffer is filled up.
     */
    tbuf.c_iflag = 0;

    /*
     * From the section Output Modes, I believe that setting c_oflag =
     * 0 should cause output processing for the terminal to also be as
     * "raw" as possible.
     */
    tbuf.c_oflag = 0;

    /*
     * The section Local Modes describes the meanings for the c_lflag
     * field.
     * 
     * Setting this field to 0 appears to do as much "raw" processing as
     * possible.  In particular, the terminal is set in non-canonical mode
     * (no line editing functions are performed, and input characters are
     * made available to the application as soon as they are available,
     * rather than waiting for a return key to be pressed), no echoing of
     * characters is done, and ERASE, KILL, INTR, QUIT, and SUSP
     * characters cause no special processing.  A few other things are
     * turned off as well.  See the info pages for further details.
     */
    tbuf.c_lflag = 0;

    /*
     * The section Noncanonical Input describes the meanings of the
     * c_cc[VMIN] and c_cc[VTIME] fields modified below.
     * 
     * In that section, it says that when c_cc[VMIN] > 0 and c_cc[VTIME] ==
     * 0, the behavior of a read system call on the port is as follows:
     * 'read' waits until at least MIN bytes (in the code below, 1 byte)
     * are available in the queue.  At that time, 'read' returns as many
     * characters as are available, up to the number requested.  'read'
     * can return more than MIN characters if more than MIN happen to be
     * in the queue.
     */
    tbuf.c_cc[VMIN] = 1;
    tbuf.c_cc[VTIME] = 0;

    /*
     * Use the updated settings in tbuf to make the desired changes to the
     * serial port.
     */
    ioctl(s_port, TCSETSF, (char *) &tbuf);

    return SUCCESS;
}



/*

   FUNCTION: IncomingPort

   PURPOSE:

   Determine if input data is available from the operating system.

   ARGUMENTS: None

   RETURN VALUE:
   
   Non-zero if data is available.

   SIDE EFFECTS:

   None.

*/

int
IncomingPort(void)
{
    struct timeval	timeout;
    fd_set              r_fdset;

    if (!params.net_flag && tail > head)
        return TRUE;

    FD_ZERO(&r_fdset);
    FD_SET(s_port, &r_fdset);
    timeout.tv_sec = timeout.tv_usec = 0;	/* poll */
    return select(fdtabsize, &r_fdset, (fd_set *) 0, (fd_set *) 0, &timeout) > 0;
}



/*

   FUNCTION: ReadPort

   PURPOSE:

   Read a byte of data from the previously initialized serial port. If
   a byte is not currently available, block until it is, or until
   time_out seconds have elapsed.  If more bytes are available, buffer
   them in an internal i/o buffer until they are needed.  If the time
   elapses before any bytes are available to read, return -1.

   ARGUMENTS:

   int time_out

   The number of seconds to wait for data to become available to
   read(2), if none is currently available in the buffer iobuf.

   RETURN VALUE:
   
   If a byte is available to be read within time_val seconds, it is
   returned.  Otherwise, -1 is returned.

   SIDE EFFECTS:

   iobuf, head, and tail are updated as appropriate.

*/

int
ReadPort(int time_out)
{
    int             c;
    struct timeval  timeout;
    fd_set          r_fdset;

    if (head < tail)
	c = iobuf[head++];
    else {
	FD_ZERO(&r_fdset);
	FD_SET(s_port, &r_fdset);
	timeout.tv_sec = (time_out == -1) ? 604800L : time_out;
	timeout.tv_usec = 0;
	head = 0;
	if (select(fdtabsize, &r_fdset, (fd_set *) 0,
		   (fd_set *) 0, &timeout) <= 0) {
	    tail = 0;
	    c = -1;
	} else {
	    if ((tail = read(s_port, iobuf, IOBUFSZ)) <= 0) {
		tail = 0;
		c = -1;
	    } else {
		c = iobuf[head++];
	    }
	}
    }

    return c;
}



/*

   FUNCTION: WritePort

   PURPOSE:

   Write n bytes of data to the previously initialized serial port.

   ARGUMENTS:

   UNIT8 *buf

   A pointer to the first of n bytes of data to write.

   int n

   The number of bytes to write.

   RETURN VALUE:
   
   Same as the write(2) system call.

   SIDE EFFECTS: None

*/

int
WritePort(UINT8 * buf, int n)
{
    return write(s_port, buf, n);
}



/*

   FUNCTION: ClosePort

   PURPOSE:

   Close previously initialized serial port.

   ARGUMENTS:

   None.

   RETURN VALUE:
   
   None.

   SIDE EFFECTS: None

*/

void
ClosePort()
{
    close(s_port);
}



/* The file descriptor of the opened audio port. */
static int      a_port;

/* The frame size, in bytes, to use for the audio device. */
static int      fr_size;

/*
 * The sampling rate at which the audio device was last opened with
 * InitAudio().
 */
static int      current_sample_rate;



/*

   FUNCTION: SetSamplingParams

   PURPOSE:

   This function is only used for the Linux version of the code.  It
   sets the sampling parameters of the sound card to be 8 bits per
   sample, mono (rather than stereo), and a sampling rate as selected
   by the parameter 'sample_rate'.

   ARGUMENTS:

   int sample_rate

   The sampling rate to use for recording, in samples/second.

   int verbose

   If TRUE, then messages will be printed to the standard output
   showing the parameters that were attempted to be set, and those
   that were actually obtained.  The sampling rate is often slightly
   off of the desired value, because the sound cards cannot support
   every sampling rate exactly.

   RETURN VALUE:
   
   SUCCESS or FAIL.  A failure could occur if the desired sampling
   rate is not available.

   SIDE EFFECTS:

   Only the ones stated in the PURPOSE above.

*/

#ifdef linux
int
SetSamplingParams(int sample_rate, int verbose)
{
    /*
     * These parameters are set in the order specified in the Linux
     * sound driver documentation -- specifically, the document called
     * "Hacker's Guide to VoxWare 2.4 (second draft)".  See the
     * Nautilus bibliography for the source of this document.
     */
    int sample_size = 8;	/* Every sample is 8 bits. */
    int sample_size_parm = sample_size;
    int channels = 1;		/* Set mono rather than stereo operation. */
    int channels_parm = channels;	
    int sample_rate_parm = sample_rate;
    float fraction_of_desired_rate;

    if ((ioctl(a_port, SOUND_PCM_WRITE_BITS, &sample_size_parm) == -1) ||
	(sample_size_parm != sample_size)) {
    	perror ("SetSamplingParams: failed to set sample size");
	return FAIL;
    }
    if ((ioctl(a_port, SOUND_PCM_WRITE_CHANNELS, &channels_parm) == -1) ||
	(channels_parm != channels)) {
    	perror ("SetSamplingParams: failed to set number of channels");
	return FAIL;
    }
    if (ioctl(a_port, SOUND_PCM_WRITE_RATE, &sample_rate_parm) == -1) {
	perror ("SetSamplingParams: failed to set SOUND_PCM_WRITE_RATE");
	return FAIL;
    }
    if (verbose) {
	printf("          Samples/Sec  Bits/Sample  Channels\n");
	printf("          -----------  -----------  --------\n");
	printf(" Desired:    %5d         %2d          %d\n",sample_rate, sample_size, channels);
	printf("Obtained:    %5d         %2d          %d\n\n",sample_rate_parm, sample_size_parm, channels_parm);
    }
    /*
     * Compare the true sample rate that was set to the desired sample
     * rate.  If the actual rate set is more than 5% away from the
     * desired rate, then fail.
     */
    fraction_of_desired_rate = (float) sample_rate_parm / (float) sample_rate;
    if (fraction_of_desired_rate < 0.95 || fraction_of_desired_rate > 1.05) {
	return FAIL;
    }
    return SUCCESS;
}
#endif



/*

   FUNCTION: InitAudio

   PURPOSE:

   Open and initialize the audio device for reading (sampling) and
   writing (playing sound).

   ARGUMENTS:

   int sample_rate

   The sampling rate to use for recording, in samples/second.

   int frame_size

   The frame size to use, in bytes.

   RETURN VALUE:
   
   SUCCESS or FAIL.  A failure could occur if the device cannot be
   opened, or if the sample_rate or frame_size arguments have illegal
   values.

   SIDE EFFECTS:

   a_port and fr_size are written to save the values of the audio
   device file descriptor and frame size, for future use by the
   functions ReadAudio and WriteAudio, and a few others.

   The parameter sample_rate is saved in the variable
   current_sample_rate, local to this file.  In this way, it may be
   later used by the function AudioFlow(), where it is needed to reset
   the sampling parameters when switching audio directions on Linux
   machines.
*/

int
InitAudio(int sample_rate, int frame_size, int verbose, float jbuf)
{
#if defined(sun)
    audio_info_t    info;

    /*
     * The Sun audio device only runs at a 8KHZ sampling rate.
     */
    if (sample_rate != 8000)
        return FAIL;

    if ((a_port = open(AUDIO_DEVICE, O_RDWR)) < 0)
        return FAIL;

    /*
     * because of a bug in the SunOS 4.1.3 audio driver, it is necessary to
     * pause and resume recording before the driver will actually begin
     * recording (when opened in O_RDWR mode).
     *
     * However, because Sun knew there was this bug in it's audio libs
     * they fixed it under Solaris. Hence we have this little (hideous) hack.
     * PJK.
     */
    AUDIO_INITINFO(&info);
#ifndef SOLARIS
    info.record.pause = 1;
    ioctl(a_port, AUDIO_SETINFO, &info);
    ioctl(a_port, I_FLUSH, FLUSHR);
#endif /* #ifndef SOLARIS */
#elif defined(linux)
/*
 * Much of this section unscrupulously stolen from the setmixer program
 * whose copyright notice is shown below. PJK.
 *
 * setmixer  - 
 *    simple program to set various sound mixer parameters from a command
 *    line; can be put into rc.local to have an automatic startup
 *    initialization
 *
 * usage: ./setmixer [-V] cntrl val [cntrl val ....]
 *      where val is in 0-100 range and 'cntrl' is one of the following:
 *      vol bass treble synth pcm speaker line mic cd mix pcm2 rec
 * When controlling   
 * Option -V will display current/set values.
 *
 * Compile with the following command:
 *      gcc -s -O6 -fomit-frame-pointer -Wall -pipe -m486 \
 *      -Xlinker -qmagic -o setmixer setmixer.c
 *
 * Michal Jaegermann, 27 Dec 1994
 * This program is released under terms of GNU General Program Licence.
 *
 */

    int mixer_fd, devmask, stereod, lcval, rcval;

    if ((a_port = open(AUDIO_DEVICE, O_RDWR)) < 0) {
	perror ("open(AUDIO_DEVICE, O_RDWR))");
        return FAIL;
    }
    if (SetSamplingParams(sample_rate, verbose) == FAIL) {
	perror ("SetSamplingParams(sample_rate, verbose)");    
        return FAIL;
    }
    if ((mixer_fd = open(MIXER_DEVICE, O_RDWR)) < 0) {
	 perror ("open(MIXER_DEVICE, O_RDWR))");
        return FAIL;
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_DEVMASK, &devmask) == -1) {
        perror("SOUND_MIXER_READ_DEVMASK");
        exit(-1);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_STEREODEVS, &stereod) == -1) {
        perror("SOUND_MIXER_READ_STEREODEVS");
        exit(-1);
    }

    if (!devmask) {
        fprintf(stderr, "No device found.");
        exit(-1);
    }

    /* set microphone sensitivity */
    if (!strcasecmp(params.audio.mic_sens, "low"))
        lcval = 40;
    else if (!strcasecmp(params.audio.mic_sens, "medium"))
        lcval = 80;
    else
        lcval = 100;	/* defaults to HIGH */
    if (stereod & SOUND_MASK_MIC) {
        rcval = lcval;
        lcval |= (rcval << 8);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_WRITE_MIC, &lcval))
        perror("mixer_write(mic)");

    /* set output volume */
    if (!strcasecmp(params.audio.out_volume, "low"))
        lcval = 40;
    else if (!strcasecmp(params.audio.out_volume, "medium"))
        lcval = 80;
    else
        lcval = 100;	/* defaults to HIGH */
    if (stereod & SOUND_MASK_PCM) {
        rcval = lcval;
        lcval |= (rcval << 8);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_WRITE_PCM, &lcval))
        perror("mixer_write(pcm)");
#endif

    current_sample_rate = sample_rate;
    fr_size = frame_size;	/* save frame size */

    return SUCCESS;
}



/*

   FUNCTION: ReadAudio

   PURPOSE:

   Read n frames of data from the previously initialized audio device
   into a buffer provided by the caller.

   ARGUMENTS:

   UINT8 *buf

   A buffer of bytes that has length at least equal to n frames (n *
   frame_size bytes).

   int n

   The maximum number of frames of data to read.

   RETURN VALUE:
   
   SUCCESS if the read is successful, FAIL otherwise.

   SIDE EFFECTS: None

*/

int
ReadAudio(UINT8 * buf, int n)
{
    int i, x;

    for (i = 0; i < n * fr_size; i += x) {
	if ((x = read(a_port, buf + i, n * fr_size - i)) == -1)
	    return FAIL;
    }
    return SUCCESS;
}


/*

   FUNCTION: WriteAudio

   PURPOSE:

   Write n frames of data from a caller-provided buffer to the
   previously initialized audio device.

   ARGUMENTS:

   UINT8 *buf

   A buffer of audio samples that has at least n frames of samples (n
   * frame_size bytes).

   int n

   The number of frames of data to write.

   RETURN VALUE:
   
   If successful, the number of bytes (not frames) written to the
   audio device.  -1 on failure.

   SIDE EFFECTS: None

*/

int
WriteAudio(UINT8 * buf, int n)
{
    return write(a_port, buf, n * fr_size);
}



/*

   FUNCTION: AudioFlow

   PURPOSE:

   The function argument mode is set to indicate whether we are in
   transmit mode (recording from the sound device) or in receive mode
   (playing to the sound device).  Configure the sound hardware
   appropriately for the mode we're in.

   ARGUMENTS:

   int mode

   This argument should have a value from the enumerated type "flow",
   which are currently one of TRANSMIT and RECEIVE.

   RETURN VALUE:
   
   Returns SUCCESS always.

   SIDE EFFECTS:

   Modifies the state of the audio hardware, if necessary.

*/

int
AudioFlow(enum flow direction)
{
#if defined(sun)
    audio_info_t    info;

    AUDIO_INITINFO(&info);

    if (direction == RECEIVE) {
	info.play.pause = 0;
	info.record.pause = 1;
    }
    else {
	info.play.pause = 1;
	info.record.pause = 0;
    }
    ioctl(a_port, AUDIO_SETINFO, &info);

    /*
     * If we're entering RECEIVE mode, flush any remaining input.  If
     * we're entering TRANSMIT mode, wait for output to finish.
     */
    if (info.record.pause)
	ioctl(a_port, I_FLUSH, FLUSHR);

    /*
     * else ioctl(a_port, AUDIO_DRAIN, (void *) 0);
     */
#elif defined(linux)
    /*
     * Fri Jun 16 04:25:15 CDT 1995  Andy Fingerhut (jaf@arl.wustl.edu)
     * 
     * I believe that the Linux sound driver can support an
     * application that opens the /dev/dsp device for both reading and
     * writing, and controls the sound card appropriately whenever the
     * application switches between reading to writing.  If this is
     * so, this code could be removed completely, as long as the
     * device was originally opened for both reading and writing in
     * InitAudio().
     * 
     * I won't put this change in until I can test it, however.
     *
     * Sat Aug 26 16:50:29 CDT 1995  Andy Fingerhut (jaf@arl.wustl.edu)
     *
     * Until then, it is important to reset the sampling parameters
     * for the sound card every time the device is closed and opened
     * again.  If this isn't done, they will go back to default values
     * compiled into the sound driver.
     */
    close(a_port);
    if (direction == RECEIVE) {
	a_port = open(AUDIO_DEVICE, O_WRONLY);
    }
    else {
	a_port = open(AUDIO_DEVICE, O_RDONLY);
    }
    /* The second parameter turns off verbosity. */
    return SetSamplingParams(current_sample_rate, FALSE);
#endif /* defined(sun) */
    return SUCCESS;
}



/*

   FUNCTION: DrainAudio

   PURPOSE:

   Block until previously queued audio data has been played.

   ARGUMENTS: None

   RETURN VALUE: None

   SIDE EFFECTS: None

   COMMENTS:

   audio_drain is defined for SunOS, but not for Linux.  A Linux
   equivalent should be found.  According to Hannu Savolainen's sound
   driver documentation, there is an ioctl "synchronization" call that
   should block until the output buffer is empty.

*/

void
DrainAudio(void)
{
#if defined(sun)
    audio_drain(a_port, 0);
#endif
}



/*

   FUNCTION: CloseAudio

   PURPOSE:

   Close the previously initialized audio device.

   ARGUMENTS: None

   RETURN VALUE: None
   
   SIDE EFFECTS: The audio device is closed.

*/

void
CloseAudio(void)
{
#ifdef sun
    /*
     * Sometimes SunOS gets hung in the close() system call.  Probably
     * has something to do with the audio device being in a paused
     * state.  At any rate, the below call seems to workaround the
     * problem.
     */
    AudioFlow(RECEIVE);
#endif
    close(a_port);
}



/* The following function is only called from talk.c, and there the
   function call is only compiled in to the executable if DEBUG > 0
   and we are compiling on a Sun.  Several of the functions called
   within AudioUnderflow are not defined on Linux, so the #if below
   ensures that AudioUnderflow function is only compiled if it will be
   called. */

#ifdef sun

/*
 * Return true if underflow has occurred while outputing to audio device
 */
int
AudioUnderflow(void)
{
    unsigned int    flag;

    if (audio_get_play_error(a_port, &flag) != AUDIO_SUCCESS) {
        fprintf(stderr, "\naudio_get_play_error() failed\n");
        exit(1);
    }
    if (flag) {
        flag = 0;
        audio_set_play_error(a_port, &flag);
        flag = 1;
    }

    return flag;
}

#endif



/* See the comments above AudioUnderflow() for why this entire
   function is within #if ... #endif directives. */

#ifdef sun

/*
 * Return true if overflow has occurred while reading from audio device
 */
int
AudioOverflow(void)
{
    unsigned int    flag;

    if (audio_get_record_error(a_port, &flag) != AUDIO_SUCCESS) {
        fprintf(stderr, "\naudio_get_record_error() failed\n");
        exit(1);
    }
    if (flag) {
        flag = 0;
        audio_set_record_error(a_port, &flag);
        flag = 1;
    }

    return flag;
}

#endif



/*

   FUNCTION: PlayVoice

   PURPOSE: Play a voice file (logon.v or ring.v).

   ARGUMENTS:

   char *voice_fname

   The file name of the sound data file.

   RETURN VALUE: SUCCESS / FAIL based upon whether file to play was opened
                 successfully

   SIDE EFFECTS: None

   NOTES:

   audio_c2u is defined in SunOS's file
   /usr/demo/SOUND/include/multimedia/audio_encode.h.  The comments
   there claim that it converts 8 bit linear samples to 8 bit U-law
   samples.

   The logon.v file included with Nautilus appears to contain
   8 bit linear voice samples, represented in "excess 128" form.

   This is exactly the form expected by the Linux sound driver for the
   /dev/dsp device, so no conversion is necessary.

*/

int
PlayVoice(char *voice_fname)
{
    int             fd;
    UINT8           audio_buf[fr_size];
#if defined(sun)
    int             i;
#endif

    if ((fd = open(voice_fname, O_RDONLY)) != -1) {
    	while (read(fd, audio_buf, fr_size) > 0) {
#if defined(sun)
    	    for (i=0; i<fr_size; i++)
    	    	audio_buf[i] = audio_c2u(audio_buf[i] - 128);
#endif
	    WriteAudio(audio_buf, 1);
	}
	close(fd);
    }
    DrainAudio();
    if (fd != -1)
      return SUCCESS;
    else
      return FAIL;
}



/*

   FUNCTION: Raw2Std

   PURPOSE:

   Convert raw audio data samples to standard form (16 bit signed
   linear).

   ARGUMENTS:

   UINT8 * data

   The "raw" audio data straight from the appropriate audio device.
   On Sun's, the /dev/audio device returns 8 bit Sun U-law samples.
   The Sun macro audio_u2s(x) returns a 16 bit signed linear sample
   for the 8 bit Sun U-law sample x.

   On Linux machines, the /dev/dsp device returns 8 bit unsigned
   linear samples.  To convert these to 2's complement signed samples,
   we simply need to subtract 128 from them.  This is equivalent to
   toggling the most significant bit.  Shifting left by 8 bit
   positions results in a 16 bit signed sample, as desired.

   INT16 * samples

   All converted samples will be placed in this buffer.

   INT16 len

   The length, in number of samples, of the data[] and samples[]
   buffers.

   RETURN VALUE: None

   SIDE EFFECTS: None

*/

void
Raw2Std(UINT8 * data, INT16 * samples, INT16 len)
{
    int             i;

    for (i = 0; i < len; i++)
#if defined(sun)
	samples[i] = audio_u2s(data[i]);
#elif defined(linux)
	samples[i] = ((INT16) (data[i] ^ 0x80)) << 8;
#endif
}



/*

   FUNCTION: Std2Raw

   PURPOSE:

   Convert audio data in standard form to raw audio data suitable for
   output to the audio device.

   ARGUMENTS:

   INT16 * samples

   Samples in standard form, i.e., 16 bit signed linear.

   UINT8 * data

   All converted samples will be placed in this buffer.  8 bit Sun
   U-law for Sun's /dev/audio device.  8 bit unsigned linear for
   Linux's /dev/dsp device.

   INT16 len

   The length, in number of samples, of the data[] and samples[]
   buffers.

   RETURN VALUE: None

   SIDE EFFECTS: None

*/

void
Std2Raw(INT16 * samples, UINT8 * data, INT16 len)
{
    int             i;

    for (i = 0; i < len; i++)
#if defined(sun)
	data[i] = audio_s2u(samples[i]);
#elif defined(linux)
	data[i] = ((UINT8) (samples[i] >> 8)) ^ 0x80;
#endif
}



/*

   FUNCTION: ptt

   PURPOSE:

   Detect whether the user has pressed any keys that have not yet been
   read.  If so, read them into a buffer that is temporary, and
   inaccessible to the caller.  If the user presses 'q' or 'Q', change
   a flag qflag, internal to the functions in this file, to 1.  The
   caller can detect this by calling the function quit().

   ARGUMENTS: None

   RETURN VALUE:
   
   1 if the user has pressed one or more keys, and the first one was
   neither 'q' nor 'Q'.  0 is returned in all other cases.  (Strange
   definition, huh?  I think the function should be cleaned up.  It
   works as desired if exactly 0 or 1 unread characters are available
   to be read.)

   SIDE EFFECTS:

   Any characters that were waiting to be read, are read, and "thrown
   away".

*/

static int qflag;

int
ptt(void)
{
	int	rval = 0;
	long n;
    char buf[1];

    /* see if there are any characters waiting to be read */
    while (ioctl(0, FIONREAD, &n), n > 0) {
		read(0, buf, 1);
		if (buf[0] == 'q' || buf[0] == 'Q')
			qflag = 1;
		rval = 1;
	}

    return rval && !qflag;
}



/*

   FUNCTION: quit

   PURPOSE: Indicate whether the user has requested to quit the program.

   ARGUMENTS: None

   RETURN VALUE:
   
   True if user desires to abort, false otherwise.

   SIDE EFFECTS: None

   QUESTIONS:

   Why does the MS-DOS version in pc-audio.c set the variable qflag to
   0 before returning the previous value?  Should this function also
   do so?  My guess is that the MS-DOS version of this function is
   unnecessarily complicated.

*/

int
quit(void)
{
    return qflag;
}



/* O.S. specific system initialization */
int
SysInit(void)
{
    /* store max number of file descriptors for later use */
#ifdef SOLARIS
    struct rlimit rlp;

    getrlimit(RLIMIT_NOFILE, &rlp);
    fdtabsize = rlp.rlim_cur;
#else
    fdtabsize = getdtablesize();
#endif

    return SUCCESS;
}



/* Return time in seconds */
long
Clock(void)
{
    return time((time_t *) 0);
}



/* "real time" millisecond timer */
INT32
Timer(int init)
{
    static long sec, usec;
    struct timeval tp;

    if (init) {
        gettimeofday(&tp, (struct timezone *) 0);
        sec = tp.tv_sec;
        usec = tp.tv_usec;
    }

    gettimeofday(&tp, (struct timezone *) 0);
    return (tp.tv_sec - sec) * 1000 + (tp.tv_usec - usec) / 1000;
}



/* "cpu time" millisecond timer */
INT32
UTimer(int init)
{
    static long sec, usec;
    struct rusage rusage;
    long now, unow;

    if (init) {
	getrusage(RUSAGE_SELF, &rusage);
	sec = rusage.ru_utime.tv_sec + rusage.ru_stime.tv_sec;
	usec = rusage.ru_utime.tv_usec + rusage.ru_stime.tv_usec;
    }

    getrusage(RUSAGE_SELF, &rusage);
    now = rusage.ru_utime.tv_sec + rusage.ru_stime.tv_sec;
    unow = rusage.ru_utime.tv_usec + rusage.ru_stime.tv_usec;
    return (now - sec) * 1000 + (unow - usec) / 1000;
}



/*

   FUNCTION: GetPassPhrase

   PURPOSE:

   Get passphrase from user without echo.

   ARGUMENTS:

   char *pw

   Pointer to the beginning of a buffer containing at least maxlen
   characters, where the password read will be placed.

   int maxlen

   The maximum number of characters desired in the password, plus one
   for a newline, and one for a null character to terminate the
   string.

   char *msg

   A prompt message to print on the screen before reading the
   password.

   RETURN VALUE:
   
   SUCCESS or FAIL.  Failure occurs if the device file /dev/tty could
   not be opened, or if the fgets() function fails to read the
   characters from the keyboard.

   SIDE EFFECTS:

   Echoing is temporarily turned off on the input terminal, but the
   terminal settings are set back to their original values before
   returning.

*/

int
GetPassPhrase(char *pw, int maxlen, char *msg)
{
    int tty_fd;
    FILE *tty;
    struct termios mytty, origtty;
    int return_value;
    
    /* make sure we're talking to a terminal */
    if ((tty = fopen("/dev/tty", "r")) == NULL)
        return FAIL;
    tty_fd = fileno(tty);
    
    /* prompt user */
    fputs(msg, stdout);
    fflush(stdout);
    
    /* turn off echoing */
    tcdrain(tty_fd);
    tcgetattr(tty_fd, &origtty);
    mytty = origtty;
    mytty.c_lflag &= ~ECHO;
    tcsetattr(tty_fd, TCSANOW, &mytty);
    
    return_value = SUCCESS;
    
    /* get passphrase */
    if (fgets(pw, maxlen, tty) == NULL) {
        return_value = FAIL;
	/* Continue with the code for restoring the terminal
	   characteristics below. */
    } else {
	/* Overwrite the newline that should be at the end of the
	   string with a null character. */
	pw[strlen(pw)-1] = '\0';
    }
    
    /* restore terminal characteristics */
    tcdrain(tty_fd);
    tcsetattr(tty_fd, TCSANOW, &origtty);
    fclose(tty);
    fputs("\n", stdout);

    return return_value;
}
