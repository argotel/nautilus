#include <windows.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mmsystem.h>
#include <mmreg.h>

#define	SAMPLE_BITS		8
#define	WaveBuf_N_MIN	4
#define	WaveBuf_SIZE	1024		/* underruns if too small */

static int checkWaveOutResult(HANDLE h, MMRESULT res, char *msg)
{
	char buf[80];

	if (h == NULL) {
		fprintf(stderr, "%s: Handle is NULL\n");
		return -1;
	}
	if (res != MMSYSERR_NOERROR) {
		if (waveOutGetErrorText(res, buf, sizeof buf))
			fprintf(stderr, "%s: Unknown WaveOut error\n", msg);
		else
			fprintf(stderr, "%s: %s\n", msg, buf);
		return -1;
	}
	return 0;
}


static HANDLE waveout = (HANDLE) 0;
static HANDLE whoutEvent;
static WAVEHDR** whout;
static DWORD whoutBytesRead = 0;
static int whoutBufNo;
static int whoutBufIndex;
static int whoutBufCnt;


/**
 * win32ao_open(int sample_rate)
 * Setup audio for output at specified rate.
 * returns -1 on error, 0 on happy.
 */
int win32ao_open(int sample_rate, float jitterBufSecs) {
	WAVEFORMATEX waveFormat;
	MMRESULT res;
	int i;

	// create an event by which audio driver will notify us
	whoutEvent = CreateEvent(NULL, FALSE, FALSE, NULL);

	// populate whoutFormat struct
	waveFormat.wFormatTag = WAVE_FORMAT_PCM;
	waveFormat.nChannels = 1;
	waveFormat.nSamplesPerSec = sample_rate;
	waveFormat.nAvgBytesPerSec = sample_rate * SAMPLE_BITS / 8;
	waveFormat.nBlockAlign = SAMPLE_BITS / 8;
	waveFormat.wBitsPerSample = SAMPLE_BITS;
	waveFormat.cbSize = 0;

	whoutBufNo = 0;
	whoutBufIndex = 0;

	// open audio device
	res = waveOutOpen(&waveout, WAVE_MAPPER, &waveFormat,
			  (DWORD) whoutEvent, (DWORD) 0, CALLBACK_EVENT);
	if (checkWaveOutResult(waveout, res, "waveOutOpen"))
		return -1;

	// pause playback (unpause when lo water mark reached)
	res = waveOutPause(waveout);
	if (checkWaveOutResult(waveout, res, "waveOutPause"))
		return -1;

	// compute whoutBufCnt
	whoutBufCnt = (int) ceil(jitterBufSecs * (float) waveFormat.nAvgBytesPerSec
					         / (float) WaveBuf_SIZE);
	if (whoutBufCnt < WaveBuf_N_MIN)
		whoutBufCnt = WaveBuf_N_MIN;

	// create buffers
	whout = (WAVEHDR**) malloc(whoutBufCnt * sizeof(WAVEHDR**));
	for (i = 0; i < whoutBufCnt; ++i) {
		// allocate buffer header
		whout[i] = (WAVEHDR*) calloc(1, sizeof(WAVEHDR));
		if (whout[i] == NULL) {
			perror("calloc WAVEHDR");
			return -1;
		}
		// allocate buffer
		whout[i]->lpData = malloc(WaveBuf_SIZE);
		if (whout[i]->lpData == NULL) {
			perror("malloc lpData");
			return -1;
		}
	}

	return 0;
}

static commit(int i) {
	MMRESULT res;

	if (whout[i]->dwBytesRecorded == 0)
		return 0;

	// commit the buffer (it is insane (aren't we all?))
	whout[i]->dwBufferLength = whout[i]->dwBytesRecorded;
	whout[i]->dwBytesRecorded = 0;
	whout[i]->dwFlags = 0;

	// prepare buffer
	res = waveOutPrepareHeader(waveout, whout[i], sizeof(WAVEHDR));
	if (checkWaveOutResult(waveout, res, "waveOutPrepareHeader"))
		return -1;

	// give buffer to driver
	res = waveOutWrite(waveout, whout[i], sizeof(WAVEHDR));
	if (checkWaveOutResult(waveout, res, "waveOutWrite"))
		return -1;

	return 0;
}

/**
 * win32ao_write(buf, cnt)
 * Write stuff and return count of what was written.
 * Return -1 on error.
 */
int win32ao_write(unsigned char *buf, int cnt) {
	unsigned char *bufptr = buf;
	unsigned char *bufend = buf + cnt;
	MMRESULT res;
	int i, n;
	
	// find buffers to put data into
	for (i = whoutBufNo; bufptr < bufend; i = (i + 1) % whoutBufCnt) {
		// wait for buffer if necessary
		while ((whout[i]->dwFlags & WHDR_INQUEUE) != 0) {
			// unpause (no-op if already unpaused)
			res = waveOutRestart(waveout);
			if (checkWaveOutResult(waveout, res, "waveOutRestart"))
				return -1;
			// buffer not ready, wait, try again
			WaitForSingleObject(whoutEvent, INFINITE);
		}

		// copy as much as will fit
		n = WaveBuf_SIZE - whout[i]->dwBytesRecorded;
		if (bufptr + n > bufend)
			n = bufend - bufptr;
		memcpy(whout[i]->lpData + whout[i]->dwBytesRecorded, bufptr, n);
		bufptr += n;
		whout[i]->dwBytesRecorded += n;
	
		// commit buffer if it is full
		if (whout[i]->dwBytesRecorded == WaveBuf_SIZE) {
			if (commit(i))
				return -1;
			whoutBufNo = (i + 1) % whoutBufCnt;
		}
	}

	return bufptr - buf;
}


/**
 * win32ao_close()
 * Terminate playback.
 * This function blocks until all data that was written has been played.
 * Return 0 if happy, -1 if error.
 */
int win32ao_close(void) {
	MMRESULT res;
	int i;

	// commit possible partially filled buffer
	if (commit(whoutBufNo))
		return -1;

	// unpause (no-op if already unpaused)
	res = waveOutRestart(waveout);
	if (checkWaveOutResult(waveout, res, "waveOutRestart"))
		return -1;

	// wait for buffers to finish and unprepare them all
	for (i = 0; i < whoutBufCnt; ++i) {
		while ((whout[i]->dwFlags & WHDR_INQUEUE) != 0) {
			// buffer not ready, wait, try again
			WaitForSingleObject(whoutEvent, INFINITE);
		}
		// unprepare the buffer
		res = waveOutUnprepareHeader(waveout, whout[i], sizeof(WAVEHDR));
		if (checkWaveOutResult(waveout, res, "waveOutUnprepareHeader"))
			return -1;
		// free memory
		free(whout[i]->lpData);
		free(whout[i]);
	}
	free(whout);

	// close 
	res = waveOutClose(waveout);
	if (checkWaveOutResult(waveout, res, "waveOutClose"))
		return -1;
			
	return 0;
}
