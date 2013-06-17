/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1996 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* ntp.h
 *
 * SCCS ID:  @(#)ntp.h 1.2 96/05/19
 */

/* NTP - Nautilus Transport Protocol */

/* usage:
 *  {
 *	NTP_HANDLE	*ntp;
 *	ntp = ntp_create(, progmsg_cb, "udp");
 *	if (!ntp) {
 *		perror("ntp_create");
 *		exit(1);
 *	}
 *	while (...) {
 *	    if (ntp_open(ntp, address, timeout_secs)) {
 *		perror(address);
 *		exit(1);
 *	    }
 *  ... multiple calls to ntp_put_rel(), ntp_put_unrel(), and ntp_get() ...
 *	    if (ntp_close(ntp)) {
 *		perror("ntp_close");
 *		exit(1);
 *	    }
 *	}
 *	if (ntp_destroy(ntp)) {
 *		perror("ntp_destroy");
 *		exit(1);
 *	}
 *  }
 */

#define NTP_MPDU	256		/* maximum NTP packet size */

typedef struct ntp_handle_s {
    unsigned    fd;			/* file descriptor (-1 if closed) */
    void      (*msg_cb)(char *msg);	/* message callback */
    int		rem_addr_len;		/* size of rem_addr */
    void       *rem_addr;		/* remote address */
    struct ntp_class_s *ntp;		/* pointer to transport functions */
} NTP_HANDLE;

/* create an NTP_HANDLE based on transport class name (e.g. "udp")
 * returns handle on success
 * returns NULL on error
 */
NTP_HANDLE *ntp_create(void (*msg_cb)(char *msg), char *name);

/* destroy an NTP_HANDLE
 */
void ntp_destroy(NTP_HANDLE *h);

/* set options
 * return 0 on success
 * return -1 on error (illegal option or value)
 */
int ntp_ioctl(NTP_HANDLE *h, char *opt, char *val);

/* open an association
 * interpretation of address is network dependent
 * address == NULL for listen
 * calls back (*progressmsg)(msg) zero or more times for progress messages
 * msg should not contain line ending
 * return 0 success
 * return -1 on error
 * return -1, errno=EINTR for timeout
 */
int ntp_open(NTP_HANDLE *h, char *address, long timeout);

/* close an association
 * return 0 on success
 * return -1 on error
 */
int ntp_close(NTP_HANDLE *h);

/* put message
 * return count on success
 * return -1 on error
 * return -1, with errno==EINTR if timeout
 */
int ntp_put(NTP_HANDLE *h, void *buf, unsigned count, long timeout);

/* get message (may reorder, may drop, may duplicate)
 * return count on success
 * return 0 on EOF
 * return -1 on error
 * return -1, with errno==EINTR if timeout
 */
int ntp_get(NTP_HANDLE *h, void *buf, unsigned size, long timeout);

typedef struct ntp_class_s {
    char *name;
    int (*ioctl)(NTP_HANDLE *, char *, char *);
    int (*open)(NTP_HANDLE *, char *, long);
    int (*close)(NTP_HANDLE *);
    int (*put)(NTP_HANDLE *, void *, unsigned, long);
    int (*get)(NTP_HANDLE *, void *, unsigned, long);
} NTP_CLASS;
