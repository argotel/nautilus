/*
* The author of this software is William Dorsey.
* Copyright (c) 1996 by William Dorsey.  All rights reserved.
*
* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
* WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
* ITS FITNESS FOR ANY PARTICULAR PURPOSE.
*/

/* ntp.c
*
* SCCS ID:  @(#)ntp.c 1.1 96/05/12
*/

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "ntp.h"

#ifdef _WIN32
#include <memory.h>
#define strcasecmp stricmp
#endif

static NTP_CLASS *
ntp_find(char *name)
{
    NTP_CLASS **pp;
    extern NTP_CLASS *ntp_tab[];	/* ntp_tab.c */
	
    for (pp = ntp_tab; *pp; ++pp)
		if (strcasecmp(name, (**pp).name) == 0)
			return *pp;
		return (NTP_CLASS *)0;
}

NTP_HANDLE *
ntp_create(void (*msg_cb)(char *msg), char *name)
{
    NTP_HANDLE *h;
	
    h = (NTP_HANDLE *) malloc(sizeof *h);
    if (h) {
		h->fd = -1;
		h->msg_cb = msg_cb;
		h->rem_addr_len = 0;
		h->rem_addr = (void *)0;
		h->ntp = ntp_find(name);
		if (!h->ntp) {
			free((char *) h);
			return (NTP_HANDLE *)0;
		}
    }
    return h;
}

void
ntp_destroy(NTP_HANDLE *h)
{
    if (h) {
		if (h->rem_addr)
			free((char *) h->rem_addr);
		free((char *) h);
    }
}

int
ntp_ioctl(NTP_HANDLE *h, char *opt, char *val)
{
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
    if (strcasecmp(opt, "get_ntp_class") == 0)
		strcpy(val, h->ntp->name);
    else
		return (*h->ntp->ioctl)(h, opt, val);
    return 0;
}

int
ntp_open(NTP_HANDLE *h, char *address, long timeout)
{
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
    return (*h->ntp->open)(h, address, timeout);
}

int
ntp_close(NTP_HANDLE *h)
{
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
    return (*h->ntp->close)(h);
}

int
ntp_put(NTP_HANDLE *h, void *buf, unsigned count, long timeout)
{
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
    return (*h->ntp->put)(h, buf, count, timeout);
}

int
ntp_get(NTP_HANDLE *h, void *buf, unsigned size, long timeout)
{
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
    return (*h->ntp->get)(h, buf, size, timeout);
}
