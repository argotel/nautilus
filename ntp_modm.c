/*
 * The author of this software is William Dorsey
 * Copyright (c) 1996 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* ntp_modm.c
 *
 * SCCS ID:  @(#)ntp_modm.c 1.4 96/05/20
 *
 * Changes:
 * 00/10/10 J.Poehlmann      Do not throw away modem init string and dial prefix
 * 04/02/29 S. Wieseckel     fixed compiler warning
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "ntp.h"
#include "nautilus.h"

#undef NTP_DEBUG

#if defined(_WIN32)
#define PAUSE(ms)		Sleep(ms)
#elif defined(unix)
#include <unistd.h>
#include <signal.h>
#define	PAUSE(ms)		usleep(ms*1000)
#endif

/* external variables */
extern struct param_t params;           /* operating parameters */

static char	*default_modem_port = NULL;
static unsigned int default_modem_speed = 19200;
static unsigned int connect_speed = 0;

/*
 * Monitor serial rx stream for 'string'.  Return 0 if found.  If not
 * found after 'wait' seconds, return -1 for failure.  If msg_cb is
 * non-NULL, seconds remaining and each string received are displayed on
 * console.  If 'wait' is zero, wait forever for desired string.
 */

static int
WaitFor(char *string, int wait, void (*msg_cb)(char *msg))
{
#ifdef UNIX
    int         c, i=0, timeout, old=-1;
    char        ibuf[80], tbuf[16];
    UINT32      start = Clock();

    if (msg_cb) {
        sprintf(ibuf, "Waiting for \"%s\"", string);
        msg_cb(ibuf);
    }

    for (;;) {
        /*
         * Construct a line of input from the serial port.  Control
         * characters are ignored with the exception of the linefeed
         * which marks the end of the line.
         */
        if (IncomingPort()) {
            c = ReadPort(1);
            if (i && (c == '\r')) {     /* end of line detected */
                ibuf[i] = '\0';
                if (msg_cb) {
                    msg_cb(ibuf);
                    old = -1;
                }
                /*
                 * Check line against string to be matched and return
                 * a status or continue searching.
                 */
                if (strstr(ibuf, string)) {
                    /*
                     * User's string to be matched has been found.  If
                     * it's a "CONNECT" string, see if there is a connect
                     * speed (hopefully the DCE speed) present and store
                     * any such value.
                     */
                    if (!strcmp(string, "CONNECT") && strlen(ibuf) > 7) {
                        connect_speed = atol(&ibuf[8]);
                    }
                    return 0;
                }
                /*
                 * If any of the following string(s) are detected,
                 * throw them out and continue waiting for the user's
                 * string.
                 */
                else if (strstr(ibuf, "AT") ||
                         strstr(ibuf, "RING"))
                    i = 0;
                /*
                 * We've reached the end of a line without finding the
                 * search string.  Return an error status.
                 */
                else
                    break;
            }
            else if (!iscntrl(c)) {
                ibuf[i++] = c;
                if (i == 80)    /* overflow detection */
                    break;
            }
        }
        /*
         * Check for timeout
         */
        if (wait) {
            timeout = Clock() - start;
            if (timeout != old) {
                if (msg_cb) {
                    sprintf(tbuf, "\r%3d: ", wait-timeout);
                    debug_puts_nr(tbuf);
                }
                if (timeout >= wait)
                    break;
                old = timeout;
            }
        }
        /*
         * keypress will abort if waiting "forever".
         */
        else
            if (ptt())
                break;
    }

    if (msg_cb)
        msg_cb("");

    return -1;
#endif
}

/*
 * Dial phone number specified by argument and wait for connection.
 *
 * This function will behave unpredictably if the modem initialization
 * string modem_init, or the phone number in phone, overflow the fixed
 * size internal string buf.
 * secfone: fixed that by just clipping strings
 */

static int
Connect(char *phone, char *modem_init, char *prefix, void (*msg_cb)(char *msg))
{
#ifdef UNIX
#define  BUFLEN  256
    char buf[BUFLEN];

    WritePort("AT\r", 3);
    if (WaitFor("OK", 2, NULL) < 0) {
        WritePort("AT\r", 3);
        if (WaitFor("OK", 2, NULL) < 0) {
            return -1;
        }
    }
    PAUSE(100);                         /* wait 100ms */
    if (strlen(modem_init) != 0) {      /* config string defined? */
        strcpy(buf, "AT");
        strncat(buf, modem_init, BUFLEN-10);
        strcat(buf, "\r");
        WritePort(buf, strlen(buf));
        if (WaitFor("OK", 2, NULL) < 0) {
            return -1;
        }
    }
    PAUSE(100);                 /* wait 100ms */
    strcpy(buf, "AT");
    /* a phone number of more then 100 digits must be bogus - ignore */ 
    if (strlen(phone) != 0 && strlen(phone) < (BUFLEN-10) ) {
	strcat (buf," D"); 
	strncat (buf,prefix,2); 
        sprintf(buf + strlen(buf), " %s\r", phone);
    } else {
        strcat(buf, "D\r");
    }
    WritePort(buf, strlen(buf));

    return WaitFor("CONNECT", 60, msg_cb);
#endif
}

/* when program is interupted, reset modem and delete auto answer */
void modemreset( int signum){
#ifdef UNIX
    WritePort("ATZ\r", 3);
    if (WaitFor("OK", 2, NULL) < 0) {
        WritePort("ATZ\r", 3);
        if (WaitFor("OK", 2, NULL) < 0) {
            return ; /* Give up */
        }
    }
#endif
}

/*
 * If 'flag' is true, put the modem in answer mode (ATS0=1) and wait
 * indefinitely for an incoming call.  Otherwise, take the modem
 * offhook immediately in answer mode.
 */

static int
AnswerMode(int flag, void (*msg_cb)(char *msg))
{
#ifdef UNIX
    WritePort("AT\r", 3);
    if (WaitFor("OK", 2, NULL) < 0) {
        WritePort("AT\r", 3);
        if (WaitFor("OK", 2, NULL) < 0) {
            return -1;
        }
    }
    if((signal(SIGINT, modemreset))==SIG_ERR) {
	  fputs("ntp_modem: error installing break handler\n", stderr);
          return -1;
    }
    PAUSE(100);                 /* wait 100ms */
    if (flag) {
        WritePort("ATS0=1\r", 7);
        if (WaitFor("OK", 2, NULL) < 0) {
            return -1;
        }
        return WaitFor("CONNECT", 0, msg_cb);
    }
    else {
        WritePort("ATA\r", 4);
        return WaitFor("CONNECT", 45, msg_cb);
    }
#endif
}

static int
xxioctl(NTP_HANDLE *h, char *opt, char *val)
{
#ifdef UNIX
    /* opt="modem_open", arg="%s,%d" (port name, port speed) */
    if ((Strcasecmp(opt, "modem_open") == 0) && val) {
	default_modem_port = strdup(val);
        strtok(default_modem_port, ",");
        default_modem_speed = atol(strtok(NULL, " "));
	if (InitPort(default_modem_port, default_modem_speed) < 0)
	    return -1;
	h->fd = 0;	/* only used as a flag */
	return 0;
    }
    else if (Strcasecmp(opt, "connect_speed") == 0) {
        return connect_speed;
    }
    else
	return -1;
    return 0;
#endif
}

static int
xxopen(NTP_HANDLE *h, char *address, long timeout)
{
#ifdef UNIX
    /* check args
     */
    if (!h || !h->ntp) {
	errno = EINVAL;
	return -1;
    }

    /*
     * Check to make sure the port has already been opened
     * (via modem_open ioctl call
     */
    if (h->fd == -1) {
        errno = EBADF;
        return -1;
    }

    /*
     * connect to other modem
     */
    if ((address == NULL) || (strlen(address) == 0)) {
        if (AnswerMode((timeout == 0), h->msg_cb)) {
            errno = ENODEV;
            return -1;
	}
    }
    else {
        if (Connect(	address,
			params.modem.init, 
			params.modem.prefix,
			h->msg_cb
		    ) == -1) {
            errno = ENODEV;
            return -1;
        }
    }

    /*
     * Make sure buffers are completely empty before continuing
     */
    while (IncomingPort()) {
        ReadPort(1);
    }
	sleep(1);

#endif
    return 0;
}

static int
xxclose(NTP_HANDLE *h)
{
#ifdef UNIX
    if (default_modem_port)
	free(default_modem_port);
    ClosePort();
#endif
    return(0);
}

static int
xxput(NTP_HANDLE *h, void *buf, unsigned count, long timeout)
{
#ifdef UNIX
    int         i, j;
    UINT8       pkt[(NTP_MPDU+2)*2+2];

    /*
     * Assemble the data into a packet, add a trailing 16-bit
     * CRC, and send it on its way.
     */
    i = 0;
    pkt[i++] = FRAME;
    for (j=0; j<count; j++)
        AddByte(pkt, i, ((UINT8 *)buf)[j]);
    j = ComputeCRC(buf, count);
    AddByte(pkt, i, (j>>8)&0xff);	/* msb first */
    AddByte(pkt, i, j&0xff);
    pkt[i++] = FRAME;
    if (WritePort(pkt, i) != i)
        return -1;
    else
        return count;
#endif
}

static int
xxget(NTP_HANDLE *h, void *buf, unsigned size, long timeout)
{
#ifdef UNIX
    int	        i, c, r_crc;

    /* First incoming character should be a FRAME character */
    c = ReadPort((int)timeout);

    /* Return EINTR if we timeout */
    if (c == -1) {
        errno = EINTR;
        return -1;
    }

    /*
     * If the first character wasn't a FRAME character, advance
     * until we get a FRAME character and then return an EIO.
     */
    if (c != FRAME) {
        do {
            c = ReadPort((int)timeout);
        } while ((c != FRAME) && (c != -1));
        if (c == -1)
            errno = EINTR;
	else
	    errno = EIO;
	return -1;
    }

    /*
     * Read in a packet.  When we read in another FRAME character
     * we'll have the entire packet in the buffer so we can stop
     * reading at that time.  Return an error if a timeout or
     * overflow occurs.
     */
    i = 0;
    while (i < size) {
        c = ReadPort((int)timeout);
        if (c == -1) {
            errno = EINTR;
            return -1;
	}
        if (c == FRAME)
            break;
	if (c == ESCAPE) {
	    c = ReadPort((int)timeout);
	    if (c == -1) {
	        errno = EINTR;
	        return -1;
	    }
	    ((UINT8 *)buf)[i++] = c ^ 0x20;
	}
	else {
	    ((UINT8 *)buf)[i++] = c;
	}
    }
    if (i >= size) {
        errno = E2BIG;
        return -1;
    }

    /*
     * The last two characters in the buffer contain a 16-bit CRC.
     * Check it against the data and return an error if it doesn't
     * match.
     */
    r_crc = ((UINT8 *)buf)[--i];
    r_crc += ((UINT8 *)buf)[--i] << 8;
    if (r_crc != ComputeCRC((UINT8 *)buf, i)) {
        errno = EIO;
        return -1;
    }

    return i;
#endif
}

NTP_CLASS ntp_modm = {
    "modem",
    xxioctl,
    xxopen,
    xxclose,
    xxput,
    xxget
};
