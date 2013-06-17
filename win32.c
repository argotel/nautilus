/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* win32.c -- Win32 OS specific functions
 */

#include <windows.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <time.h>
#include <conio.h>

#include "machine.h"
#include "nautilus.h"
#include "win32a.h"
#include <assert.h>


#include <stdint.h>
#include "winsounds.h"

extern struct param_t params;	/* operating parameters */

/* return pathname corresponding to port designated by arg */
char           *
GetPort(char *arg)
{
    return arg;
}

/* The frame size, in bytes, to use for the audio device. */
static int      fr_size;

/*
 * The sampling rate at which the audio device was last opened with
 * InitAudio().
 */
static int      current_sample_rate;

/*
 * The size of the jitter buffer in seconds.
 */
static float	jbufsecs;

/* The mode the audio device is currently open in (closed,
 * reading, or writing.
 */
static enum mode_t { AM_CLOSED, AM_READING, AM_WRITING } audio_mode;



/*

	FUNCTION: InitAudio

	PURPOSE:

	Record specified sampling rate and frame size for the
	audio device.  Code could (should?) also be added to
	set the record and playback volume here.

	Originally, this function was responsible for opening the
	audio device and setting the appropriate parameters for
	sampling.  However, in the win32 implementation, the audio
	device is opened and closed in the appropriate mode with
	the AudioFlow() function instead.

	ARGUMENTS:

	int sample_rate

	The sampling rate to use, in samples/second.

	int frame_size

	The frame size to use, in bytes.

	RETURN VALUE:
   
	SUCCESS or FAIL.  A failure could occur if the device cannot be
	opened, or if the sample_rate or frame_size arguments have illegal
	values.

	SIDE EFFECTS:

	The sampling rate and frame size are recorded in the static
	variables current_sample_rate and fr_size respectively.  These
	values are then referenced when the audio driver is configured
	for record or playback mode in the AudioFlow() function.

*/

int
InitAudio(int sample_rate, int frame_size, int verbose, float jbuf)
{
	if ((sample_rate < 4000) || (sample_rate > 44100))
		return FAIL;
	else
		current_sample_rate = sample_rate;

	if ((frame_size < 1) || (frame_size > 1024))
		return FAIL;
	else
		fr_size = frame_size;

	if (jbuf < 0.0)
		jbufsecs = 0.0;
	else if (jbuf > 10.0)
		jbufsecs = 10.0;
	else
		jbufsecs = jbuf;

	audio_mode = AM_CLOSED;

#ifdef NOT_IMPLEMENTED_YET
    /* set microphone sensitivity */
    if (!strcasecmp(params.audio.mic_sens, "low"))
        /* set record sensitivity to "low" value */
    else if (!strcasecmp(params.audio.mic_sens, "medium"))
        /* set record sensitivity to "medium" value */
    else
		/* set record sensitivity to "high" value (default) */    

    /* set output volume */
    if (!strcasecmp(params.audio.out_volume, "low"))
        /* set output volume to "low" value */
    else if (!strcasecmp(params.audio.out_volume, "medium"))
        /* set output volume to "medium" value */
    else
        /* set output volume to "high" value (default) */
#endif

    return SUCCESS;
}



/*

	FUNCTION: ReadAudio

	PURPOSE:

	Read n frames of data from the previously initialized audio
	device driver into a buffer provided by the caller.

	ARGUMENTS:

	UINT8* buf

	A buffer of bytes that has length at least equal to n frames (n *
	frame_size bytes).

	int n

	The maximum number of frames of data to read.

	RETURN VALUE:
   
	SUCCESS if the read is successful, FAIL otherwise.

	SIDE EFFECTS: None

*/

int
ReadAudio(UINT8* buf, int n)
{
	/* win32ai_read() always returns the specified number of
	 * bytes or -1 if error.
	 */
	if (win32ai_read(buf, n*fr_size) == n*fr_size)
		return SUCCESS;
    else
    	return FAIL;
}



/*

	FUNCTION: WriteAudio

	PURPOSE:

	Write n frames of data from a caller-provided buffer to the
	previously initialized audio device driver.

	ARGUMENTS:

	UINT8* buf

	A buffer of audio samples that has at least n frames of samples
	(n * frame_size bytes).

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
    return win32ao_write(buf, n*fr_size);
}



/*

	FUNCTION: AudioFlow

	PURPOSE:

	The "mode" argument is set to indicate which mode we are in
	(transmit mode where we record audio from the audio device
	and send it to the other end or receive mode where we play
	back audio received from the other end).

	In transmit mode, we call the function responsible for
	terminating audio playback (if necessary), and then call
	the function responsible for initializing the audio hardware
	in record mode.

	In receive mode, we call the function responsible for
	terminating audio recording (if necessary), and then call
	the function responsible for initializing the audio hardware
	in playback mode.

	ARGUMENTS:

	int mode

	This argument should have a value from the enumerated type "flow",
	which are currently one of TRANSMIT and RECEIVE.

	RETURN VALUE:
   
	If successful, returns SUCCESS.  Otherwise, returns FAIL.

	SIDE EFFECTS:

	Modifies the state of the audio hardware, if necessary.

*/

int
AudioFlow(enum flow direction)
{
	switch (audio_mode) {
	case AM_CLOSED:
		break;
	case AM_READING:
		if (direction == TRANSMIT)
			return SUCCESS;
		win32ai_close();
		break;
	case AM_WRITING:
		if (direction == RECEIVE)
			return SUCCESS;
		win32ao_close();
		break;
	}
	
	audio_mode = AM_CLOSED;

	switch (direction) {
	case TRANSMIT:
		if (win32ai_open(current_sample_rate) == -1)
			return FAIL;
		audio_mode = AM_READING;
		break;
	case RECEIVE:
		if (win32ao_open(current_sample_rate, jbufsecs) == -1)
			return FAIL;
		audio_mode = AM_WRITING;
		break;
	}

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

	When the function responsible for closing the audio device
	driver is called (in playback mode), it waits until all
	pending samples have been played before terminating playback
	and returning control to the caller.

	When the function responsible for closing the audio device
	driver is called in record mode, it immediately terminates
	recording and discards samples that have not yet been returned
	to the application.

	Because of the manner in which CloseAudio() is implemented
	in the win32 implementation, DrainAudio() is not needed.


*/

void
DrainAudio(void)
{
}



/*

	FUNCTION: CloseAudio

	PURPOSE:

	Stop the audio device driver from recording or playing back
	audio (depending on what mode it was in), free any resources
	that were being used by the audio device driver.

	ARGUMENTS: None

	RETURN VALUE: None
   
	SIDE EFFECTS: The audio device is closed.

*/

void
CloseAudio(void)
{
	switch (audio_mode) {
	case AM_CLOSED:
		break;
	case AM_READING:
		win32ai_close();
		break;
	case AM_WRITING:
		win32ao_close();
		break;
	}

	audio_mode = AM_CLOSED;
}



/*

	FUNCTION: PlayVoice

	PURPOSE: Play a voice file (logon.v or ring.v).

	ARGUMENTS:

	char* voice_fname

	The file name of the sound data file.

	RETURN VALUE: SUCCESS / FAIL based upon whether file to play
	              was opened successfully

	SIDE EFFECTS: None

	NOTES:

	The logon.v file included with Nautilus contains 8 bit
	linear voice samples, represented in unsigned form.

	This is the same format expected by the win32 audio device
	drivers when operating in 8-bit PCM mode.

*/

int
PlayVoice(char* voice_fname)
{
    int fd, nb;
    UINT8 audio_buf[1024];

	if (win32ao_open(8000, 0.5) == -1)
		return FAIL;
    if ((fd = open(voice_fname, O_RDONLY | O_BINARY)) != -1) {
    	while ((nb = read(fd, audio_buf, sizeof audio_buf)) > 0) {
			win32ao_write(audio_buf, nb);
		}
		close(fd);
    }
    win32ao_close();

    if (fd != -1)
		return SUCCESS;
    else
		return FAIL;
}

/* play logon sound from internal array */
int
PlayLogon(){
	int nb,index,size;
	UINT8 audio_buf[1024];
	
	index=size=0;

    if (win32ao_open(8000, 0.5) == -1)
        return FAIL;
	size=sizeof(logonSoundData); nb=1;
	assert(size>10);
	while ( nb>0 ){
		nb=MIN((size-index)-1,sizeof audio_buf);
		memcpy(audio_buf,logonSoundData+index,nb);
		win32ao_write(audio_buf, nb);
		index+= nb;
	}
    win32ao_close();
	return SUCCESS;
}	

/* play ring sound from internal array */
int
PlayRing(){
	int nb, index,size=0;
	UINT8 audio_buf[1024];

	index=size=0;
    if (win32ao_open(8000, 0.5) == -1)
        return FAIL;
	size=sizeof(ringSoundData);
	assert(size>10);
	while ( nb > 0 ){
		nb=MIN((size-index)-1,sizeof audio_buf);
		memcpy(audio_buf,ringSoundData+index,nb);
		win32ao_write(audio_buf, nb);
		index+= nb;
	}
    win32ao_close();
	return SUCCESS;
}	

/*

	FUNCTION: Raw2Std

	PURPOSE:

	Convert raw audio data samples to standard form (16 bit signed
	linear).

	ARGUMENTS:

	UINT8* data

	The "raw" audio data straight from the appropriate audio device.
	Under win32, 8 bit PCM samples are represented in unsigned
	format.  To convert these to 2's complement signed samples,
	we simply need to subtract 128 from them.  This is equivalent to
	toggling the most significant bit.  Shifting left by 8 bit
	positions results in a 16 bit signed sample, as desired.

	INT16* samples

	All converted samples will be placed in this buffer.

	INT16 len

	The length, in number of samples, of the data[] and samples[]
	buffers.

	RETURN VALUE: None

	SIDE EFFECTS: None

*/

void
Raw2Std(UINT8* data, INT16* samples, INT16 len)
{
    int i;

    for (i = 0; i < len; i++)
		samples[i] = ((INT16) (data[i] ^ 0x80)) << 8;
}



/*

	FUNCTION: Std2Raw

	PURPOSE:

	Convert audio data in standard form to raw audio data suitable for
	output to the audio device.

	ARGUMENTS:

	INT16* samples

	Samples in standard form, i.e., 16 bit signed linear.

	UINT8* data

	All converted samples will be placed in this buffer.  16 bit
	signed samples are converted into 8 bit unsigned samples for
	use with the win32 driver.

	INT16 len

	The length, in number of samples, of the data[] and samples[]
	buffers.

	RETURN VALUE: None

	SIDE EFFECTS: None

*/

void
Std2Raw(INT16* samples, UINT8* data, INT16 len)
{
    int i;

    for (i = 0; i < len; i++)
		data[i] = ((UINT8) (samples[i] >> 8)) ^ 0x80;
}



/*

	FUNCTION: ptt

	PURPOSE:

	Detect whether the user has pressed any keys that have not yet
	been read.  If so, read them into a buffer that is temporary,
	and	inaccessible to the caller.  If the user presses 'q' or
	'Q', change	a flag qflag, internal to the functions in this
	file, to 1.  The caller can detect this by calling the
	function quit().

	ARGUMENTS: None

	RETURN VALUE:
   
	1 if the user has pressed one or more keys, and the first one
	was	neither 'q' nor 'Q'.  0 is returned in all other cases.
	(Strange definition, huh?  I think the function should be
	cleaned up.  It	works as desired if exactly 0 or 1 unread
	characters are available to be read.)

	SIDE EFFECTS:

	Any characters that were waiting to be read, are read, and
	"thrown	away".

*/

static          qflag;

int
ptt(void)
{
	INT16  c;

	/* see if there are any characters waiting to be read */
	if (_kbhit()) {
		c = _getch();
		if (c == 'q' || c == 'Q')
			qflag = 1;
		else
			return c;      /* return actual character */
	}
	return 0;
}



/*

	FUNCTION: quit

	PURPOSE:
   
	Indicate whether the user has requested to quit the
	program.

	ARGUMENTS: None

	RETURN VALUE:
   
	True if user desires to abort, false otherwise.

	SIDE EFFECTS: None

	QUESTIONS:

	Why does the MS-DOS version in pc-audio.c set the variable
	qflag to 0 before returning the previous value?  Should this
	function also do so?  My guess is that the MS-DOS version of
	this function is unnecessarily complicated.

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
	WORD wVersionRequested;
	WSADATA wsaData;
	int err; 

	wVersionRequested = MAKEWORD(2, 0); 
	err = WSAStartup(wVersionRequested, &wsaData);
	if (err != 0) {
		fprintf(stderr, "SysInit: Couldn't find a usable WinSock DLL\n");
		/* it's not clear that a usable WINSOCK DLL ever existed */
		return FAIL;
	} 

	fprintf(stderr, "WinSock vers. : %d.%d\n",
		LOBYTE(wsaData.wVersion), HIBYTE(wsaData.wVersion));
	
	return SUCCESS;
}


/* Return time in seconds */
long
Clock(void)
{
	return (long) time(NULL);
}



/* "user time" millisecond timer */
INT32
UTimer(int init)
{
    static long  base;

    if (init)
        base = GetTickCount();

    return (GetTickCount() - base) ;
}



/*

	FUNCTION: GetPassPhrase

	PURPOSE:

	Get passphrase from user without echo.

	ARGUMENTS:

	char* pw

	Pointer to the beginning of a buffer containing at least maxlen
	characters, where the password read will be placed.

	int maxlen

	The maximum number of characters desired in the password, plus
	one	for a newline, and one for a null character to terminate the
	string.

	char* msg

	A prompt message to print on the screen before reading the
	password.

	RETURN VALUE:
   
	SUCCESS or FAIL.  Failure occurs if the device file /dev/tty
	could not be opened, or if the fgets() function fails to read
	the	characters from the keyboard.

	SIDE EFFECTS:

	Echoing is temporarily turned off on the input terminal, but the
	terminal settings are set back to their original values before
	returning.

*/

int
GetPassPhrase(char *pw, int maxlen, char *msg)
{
    int i, c, done;

    /* prompt user */
    _cputs(msg);

    /* get passphrase */
    i = done = 0;
    do {
    	c = _getch();
    	switch (c) {
    	case '\r':
    	case '\n':
    	    pw[i] = '\0';
    	    done = 1;
    	    break;
	case '\010':
	    if (i > 0)
	    	i--;
	    break;
	default:
    	    if (i < maxlen)
    	    	pw[i++] = c;
	    break;
    	}
    } while (!done);

    _cputs("\n");

    return 0;
}


/* Obvious */
void
sleep(unsigned time)
{
	Sleep(time*1000);
}
