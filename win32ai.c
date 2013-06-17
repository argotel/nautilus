#include <windows.h>
#include <process.h>
#include <stdio.h>
#include <mmsystem.h>
#include <stdlib.h>
#include <mmreg.h>
#include <assert.h>

#define	SAMPLE_BITS		8
#define	WaveBuf_N		8
#define	WaveBuf_SIZE		1024	/* underruns if too small */

static int checkWaveInResult(HANDLE h, MMRESULT res, char *msg)
{
	char buf[80];

	if (h == NULL) {
		fprintf(stderr, "%s: Handle is NULL\n");
		return -1;
	}
	if (res != MMSYSERR_NOERROR) {
		if (waveInGetErrorText(res, buf, sizeof buf))
			fprintf(stderr, "%s: Unknown WaveIn error\n", msg);
		else
			fprintf(stderr, "%s: %s\n", msg, buf);
		return -1;
	}
	return 0;
}

static HANDLE wavein = (HANDLE) 0;
static HANDLE whinEvent;
static LPWAVEHDR whin[WaveBuf_N];
static DWORD whinBytesRead = 0;
static int whinBufNo;
static int whinBufIndex;


/**
 * win32ai_open(int sample_rate)
 * Setup audio for input at specified rate.
 * returns -1 on error, 0 on happy.
 */
int win32ai_open(int sample_rate) {
	WAVEFORMATEX waveFormat;
	MMRESULT res;
	int i;

	// create an event by which audio driver will notify us
	whinEvent = CreateEvent(NULL, FALSE, FALSE, NULL);

	// populate whinFormat struct
	waveFormat.wFormatTag = WAVE_FORMAT_PCM;
	waveFormat.nChannels = 1;
	waveFormat.nSamplesPerSec = sample_rate;
	waveFormat.nAvgBytesPerSec = sample_rate * SAMPLE_BITS / 8;
	waveFormat.nBlockAlign = SAMPLE_BITS / 8;
	waveFormat.wBitsPerSample = SAMPLE_BITS;
	waveFormat.cbSize = 0;

	whinBufNo = 0;
	whinBufIndex = 0;

	// open audio device
	res = waveInOpen(&wavein, WAVE_MAPPER, &waveFormat,
			 (DWORD) whinEvent, (DWORD) 0, CALLBACK_EVENT);
	if (checkWaveInResult(wavein, res, "waveInOpen"))
		return -1;

	// create buffers
	for (i = 0; i < WaveBuf_N; ++i) {
		// allocate buffer header
		whin[i] = (WAVEHDR*) calloc(1, sizeof(WAVEHDR));
		if (whin[i] == NULL) {
			perror("malloc WAVEHDR");
			return -1;		/* need to cleanup XXX */
		}
		// allocate buffer
		whin[i]->lpData = malloc(WaveBuf_SIZE);
		if (whin[i]->lpData == NULL) {
			perror("new char[WaveBuf_SIZE]");
			return -1;		/* need to cleanup XXX */
		}
		whin[i]->dwBufferLength = WaveBuf_SIZE;
		// prepare buffer
		res = waveInPrepareHeader(wavein, whin[i], sizeof(WAVEHDR));
		if (checkWaveInResult(wavein, res, "waveInPrepareHeader"))
			return -1;
		// give buffer to driver
		res = waveInAddBuffer(wavein, whin[i], sizeof(WAVEHDR));
		if (checkWaveInResult(wavein, res, "waveInAddBuffer"))
			return -1;			/* need to cleanup XXX */
	}

	// start device, yeeeeeee haw!
	res = waveInStart(wavein);
	if (checkWaveInResult(wavein, res, "waveInStart"))
		return -1;				/* need to cleanup XXX */

	return 0;
}


/**
 * win32ai_read(buf, size)
 * Read stuff and return count of what was read.
 * Return -1 on error.
 */
int win32ai_read(unsigned char *buf, unsigned bufsiz) {
	unsigned char *bufptr = buf;
	unsigned char *bufend = buf + bufsiz;
	MMRESULT res;
	int i, n;

	// rumage through buffers looking for data
	for (i = whinBufNo; bufptr < bufend; i = (i + 1) % WaveBuf_N) {

		// wait for buffer if necessary
		while ((whin[i]->dwFlags & WHDR_DONE) == 0) {
			// buffer not ready, wait, try again
			WaitForSingleObject(whinEvent, INFINITE);
		}

		// unprepare buffer (no op if already unprepared)
		if (whin[i]->dwUser == 0) {
			whin[i]->dwUser = 1;
			res = waveInUnprepareHeader(wavein, whin[i], sizeof(WAVEHDR));
			if (checkWaveInResult(wavein, res, "waveInUnprepareHeader"))
				return -1;
		}

		// do something with data, dwBytesRecorded
		n = whin[i]->dwBytesRecorded;
		if (bufptr + n > bufend)
			n = bufend - bufptr;
		memcpy(bufptr, whin[i]->lpData + WaveBuf_SIZE
			- whin[i]->dwBytesRecorded, n);
		bufptr += n;
		whin[i]->dwBytesRecorded -= n;

		if (whin[i]->dwBytesRecorded == 0) {
			// initialization
			whin[i]->dwUser = 0;
			whin[i]->dwFlags = 0;
			whin[i]->dwBufferLength = WaveBuf_SIZE;

			// prepare buffer
			res = waveInPrepareHeader(wavein, whin[i], sizeof(WAVEHDR));
			if (checkWaveInResult(wavein, res, "waveInPrepareHeader"))
				return -1;
			
			// give buffer back to driver
			res = waveInAddBuffer(wavein, whin[i], sizeof(WAVEHDR));
			if (checkWaveInResult(wavein, res, "waveInAddBuffer"))
				return -1;

			// done with this buffer, point to the next one
			whinBufNo = (whinBufNo + 1) % WaveBuf_N;
		}
	}

	return bufptr - buf;
}


/**
 * win32ai_close()
 * Terminate recording.
 * This function discards all data that has been sampled but not read.
 * Return 0 if happy, -1 if error.
 */
int win32ai_close(void) {
	MMRESULT res;
	int i;

	// stop device
	res = waveInReset(wavein);
	if (checkWaveInResult(wavein, res, "waveInReset"))
		return -1;

	// wait for buffers to finish and unprepare them all
	for (i = 0; i < WaveBuf_N; ++i) {
		while ((whin[i]->dwFlags & WHDR_DONE) == 0) {
			// buffer not ready, wait, try again
			WaitForSingleObject(whinEvent, INFINITE);
		}
		// unprepare the buffer
		res = waveInUnprepareHeader(wavein, whin[i], sizeof(WAVEHDR));
		if (checkWaveInResult(wavein, res, "waveInUnprepareHeader"))
			return -1;
		// free memory
		free(whin[i]->lpData);
		free(whin[i]);
	}

	// close device
	res = waveInClose(wavein);
	if (checkWaveInResult(wavein, res, "waveInClose"))
		return -1;
			
	return 0;
}
