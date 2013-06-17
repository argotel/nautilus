/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1996 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* nsp.h
 *
 * SCCS ID:  @(#)nsp.h 1.1 96/05/12
 */

/* NSP - Nautilus Session Protocol */

/* usage:
 *  {
 *	NSP_HANDLE	*nsp;
 *	nsp = nsp_create();
 *	if (!nsp) {
 *		perror("nsp_create");
 *		exit(1);
 *	}
 *	while (...) {
 *	    if (nsp_open(nsp, address, progmsg_cb, timeout_secs)) {
 *		perror(address);
 *		exit(1);
 *	    }
 *  ... multiple calls to nsp_put_rel(), nsp_put_unrel(), and nsp_get() ...
 *	    if (nsp_close(nsp)) {
 *		perror("nsp_close");
 *		exit(1);
 *	    }
 *	}
 *	if (nsp_destroy(nsp)) {
 *		perror("nsp_destroy");
 *		exit(1);
 *	}
 *  }
 */

#define	NSP_MPDU	2048
#define	NSP_DROP	-2		/* return code for dropped packet */

typedef struct nsp_handle_s {
    struct ntp_handle_s *ntp;
    struct nsp_buf_s   *Q_head;
    struct nsp_buf_s   *Q_tail;
    unsigned		put_seq;	/* last seq number sent */
    unsigned		get_seq;	/* last seq returned by nsp_get */
    int			reorder_window;	/* reorder window size */
} NSP_HANDLE;

/* create an NSP_HANDLE
 * returns handle on success
 * returns NULL on error
 */
NSP_HANDLE *nsp_create(void (*msg_cb)(char *), char *ntp_class_name);

/* destroy an NSP_HANDLE
 */
void nsp_destroy(NSP_HANDLE *h);

/* open a connection
 * interpretation of address is network dependent
 * address == NULL for listen
 * calls back (*progressmsg)(msg) zero or more times for progress messages
 * msg should not contain line ending
 * return 0 on success
 * return -1 on error
 * return -1, errno=EINTR for timeout
 */
int nsp_open(NSP_HANDLE *h, char *address, long timeout);

/* close a connection
 * return 0 on success
 * return -1 on error
 */
int nsp_close(NSP_HANDLE *h);

/* put message, unreliably
 * return 0 on success
 * return -1 on error
 * return -1, with errno==EINTR if timeout
 */
int nsp_put_unrel(NSP_HANDLE *h, void *buf, unsigned count, long timeout);

/* put message, reliably
 * return 0 on success
 * return -1 on error
 * return -1, with errno==EINTR if timeout
 */
int nsp_put_rel(NSP_HANDLE *h, void *buf, unsigned count, long timeout);

/* get message (inorder, nonduplicate, possibly lost)
 * return count on success
 * return -1 on error
 * return -1, with errno==EINTR if timeout
 * return NSP_DROP, if packet missing
 */
int nsp_get(NSP_HANDLE *h, void *buf, unsigned size, long timeout);

/* XXX
 * return -1 on error
 */
int nsp_ioctl(NSP_HANDLE *h, char *opt, char *val);

