/*
 * The author of this software is William Dorsey
 * Copyright (c) 1996 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* nsp.c
 *
 * SCCS ID:  @(#)nsp.c 1.3 96/05/20
 */

#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <memory.h>
#endif
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include <time.h>
#include "ntp.h"
#include "nsp.h"

#undef NSPDEBUG

#define	NSP_TYPE_UNREL	0		/* unreliable */
#define NSP_TYPE_REL	1		/* reliable */
#define	NSP_TYPE_ACK	2		/* acknowledge */
#define NSP_TYPE_NAT	3		/* not a NSP packet, used for NAT traversal,
								   NSP should ignore it. If doing NAT traversal,
								   use packets with bits 6,7 set in first byte */


/* structure of a NSP packet
 *
 *	+-+-+-+-+-+-+-+-+
 *	| T | SEQ[13:8] |
 *	+-+-+-+-+-+-+-+-+
 *	|   SEQ[7:0]    |
 *	+-+-+-+-+-+-+-+-+
 *	|		|
 *	| USER DATA ... |
 *	|		|
 *	+-+-+-+-+-+-+-+-+
 */
typedef struct nsp_pkt_s {
    unsigned char	seqh;		/* high-order sequence */
    unsigned char	seql;		/* low-order sequence */
    char		buf[NSP_MPDU];
} NSP_PKT;

#define getnspseq(p)		((unsigned) (((p)->seqh & 0x3F) << 8 | (p)->seql))
#define	getnsptype(p)		(((p)->seqh >> 6) & 0x3)

static void
setnsphdr(NSP_PKT *p, int type, unsigned seq)
{
    p->seqh = type << 6 | (seq >> 8) & 0x3F;
    p->seql = seq;
}

/* queue element structure
 */
typedef struct nsp_buf_s {
    struct nsp_buf_s   *prev;		/* link to prev buf */
    struct nsp_buf_s   *next;		/* link to next buf */
    int			userlen;	/* length of data in pkt.buf */
    NSP_PKT		pkt;
} NSP_BUF;

#ifdef NSPDEBUG
#include <stdio.h>
static char *
seqs(NSP_HANDLE *h) {
    if (h) {
	static char buf[2048];
	char *b = buf;
	NSP_BUF *p;
	*b++ = '{';
	for (p = h->Q_head; p; p = p->next) {
	    sprintf(b, "%d", getnspseq(&p->pkt));
	    b += strlen(b);
	    if (p->next)
		*b++ = ',';
	}
	*b++ = '}';
	*b++ = (char)0;
	return buf;
    } else
	return "{}";
}
#endif

/* subtract two sequence numbers
 */
static int
subtract_seq(unsigned a,  unsigned b)
{
    int	i = a - b;			/* compute using native word size */
    return i & 0x2000 ? i | -0x2000 : i & 0x1FFF;	/* sign extend */
}

NSP_HANDLE *
nsp_create(void (*msg_cb)(char *), char *ntp_class_name)
{
    NSP_HANDLE *h;

    h = (NSP_HANDLE *) malloc(sizeof *h);
    if (h) {
	h->ntp = ntp_create(msg_cb, ntp_class_name);
	if (!h->ntp) {
	    free((char *) h);
	    return (NSP_HANDLE *)0;
	}
	h->Q_head = (NSP_BUF *) 0;
	h->Q_tail = (NSP_BUF *) 0;
	h->put_seq = 0;
	h->get_seq = 0;
	h->reorder_window = 3;
    }
    return h;
}

void
nsp_destroy(NSP_HANDLE *h)
{
    NSP_BUF	       *p, *pnext;

    if (h) {
	/* delete transport handle */
	if (h->ntp)
	    ntp_destroy(h->ntp);

	/* delete all queued packets */
	for (p = h->Q_head; p; p = pnext) {
	    pnext = p->next;
	    free((char *) p);
	}

	/* delete session handle */
	free((char *) h);
    }
}

int
nsp_ioctl(NSP_HANDLE *h, char *opt, char *val)
{
    if (!h || !h->ntp) {
	errno = EINVAL;
	return -1;
    }
#ifdef _WIN32
    if (stricmp(opt, "reorder_window") == 0)
#else
	if (strcasecmp(opt, "reorder_window") == 0)
#endif
	h->reorder_window = atoi(val);
    else
	return ntp_ioctl(h->ntp, opt, val);
    return 0;
}

int
nsp_open(NSP_HANDLE *h, char *address, long timeout)
{
    if (!h || !h->ntp) {
	errno = EINVAL;
	return -1;
    }
    return ntp_open(h->ntp, address, timeout);
}

int
nsp_close(NSP_HANDLE *h) {
    if (!h || !h->ntp) {
	errno = EINVAL;
	return -1;
    }
    return ntp_close(h->ntp);
}

int
nsp_put_unrel(NSP_HANDLE *h, void *buf, unsigned count, long timeout)
{
    NSP_PKT		pkt;

    /* check args
     */
    if (!h || !h->ntp || !buf || count > NSP_MPDU) {
	errno = EINVAL;
	return -1;
    }

    /* build packet
     */
    setnsphdr(&pkt, NSP_TYPE_UNREL, ++h->put_seq);
    memcpy(pkt.buf, (char *) buf, count);

    /* do transport put
     */
#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_put_unrel: type=%d seq=%d userlen=%d\n",
		    getnsptype(&pkt),
		    getnspseq(&pkt),
		    count);
#endif
    return ntp_put(h->ntp,
		   (void *) &pkt,
		   (unsigned) &((NSP_PKT *)0)->buf[count],
		   timeout);
}

/* compute time left before timeout
 */
static long
timeleft(time_t t0, long timeout)
{
    long et = time((time_t *) 0) - t0;
    return (timeout == -1) ? -1 : (et > timeout) ? 0 : timeout - et;
}

/* receive one packet from remote and queue it
 */
static enum {
    REC_ACK,		/* got ACK */
    REC_DAT,		/* got data and put on queue */
    REC_ERR,		/* got error, code in errno */
    REC_EOF}		/* got EOF */
receive(NSP_HANDLE *h, long timeout)
{
    NSP_BUF    *new;
    NSP_BUF    *p;
    int		n;

    /* allocate buffer
     */
    new = (NSP_BUF *) malloc(sizeof *new);
    if (!new)
	return REC_ERR;

    /* get packet
     */
 reread:
    n = ntp_get(h->ntp, (void *) &new->pkt, sizeof new->pkt, timeout);
    switch (n) {
    case -1:
#ifdef NSPDEBUG
	perror("receive: ntp_get");
#endif
	free((char *) new);
	return REC_ERR;
    case 0:
#ifdef NSPDEBUG
	fprintf(stderr, "receive: ntp_get: EOF\n");
#endif
	free((char *) new);
	return REC_EOF;
    }

    /* compute size
     */
    new->userlen = n - (long) &(*(NSP_PKT *) 0).buf[0];
#ifdef NSPDEBUG
	fprintf(stderr, "receive: type=%d seq=%d userlen=%d\n",
		getnsptype(&new->pkt), getnspseq(&new->pkt), new->userlen);
#endif
    if (new->userlen < 0) {
#ifdef NSPDEBUG
	fprintf(stderr, "receive: discarded: too short\n");
#endif
	goto reread;			/* discard short packets */
    }

    /* check for ack
     */
    if (getnsptype(&new->pkt) == NSP_TYPE_ACK) {
	if (getnspseq(&new->pkt) == h->put_seq) {
#ifdef NSPDEBUG
	    fprintf(stderr, "receive: returning REC_ACK\n");
#endif
	    free((char *) new);
	    return REC_ACK;
	} else {
#ifdef NSPDEBUG
	    fprintf(stderr, "receive: unexpected ack ignored\n");
#endif
	    goto reread;		/* discard ack with seq mismatch */
	}
    }

    /* send ack for reliable packet
     */
    if (getnsptype(&new->pkt) == NSP_TYPE_REL) {
	NSP_PKT		ack;
	setnsphdr(&ack, NSP_TYPE_ACK, getnspseq(&new->pkt));
#ifdef NSPDEBUG
    {
	int retval =
#endif
	ntp_put(h->ntp, (void *) &ack,
		(unsigned) &(*(NSP_PKT *)0).buf[0], timeout);
#ifdef NSPDEBUG
	fprintf(stderr, "receive: sent ACK for seq=%d\n",
		getnspseq(&new->pkt));
	if (retval != (int) &(*(NSP_PKT *)0).buf[0])
	    fprintf(stderr, "ntp_put returned %d (expected %d)\n", retval,
	            (int) &(*(NSP_PKT *)0).buf[0]);
    }
#endif
    }

    /* discard aged packets
     */
    if (subtract_seq(getnspseq(&new->pkt), h->get_seq) <= 0) {
#ifdef NSPDEBUG
	fprintf(stderr, "receive: old packet discarded seq=%d\n",
		getnspseq(&new->pkt));
#endif
	goto reread;
    }

    /* find correct place in queue
     */
    for (p = h->Q_tail; p; p = p->prev) {
	if (subtract_seq(getnspseq(&new->pkt), getnspseq(&p->pkt)) >= 0)
	    break;
    }

    /* discard duplicates
     */
    if (p && getnspseq(&p->pkt) == getnspseq(&new->pkt)) {
#ifdef NSPDEBUG
	fprintf(stderr, "receive: duplicate packet discarded seq=%d\n",
		getnspseq(&new->pkt));
#endif
	goto reread;
    }

    /* insert into queue
     */
    if (p) {
	new->prev = p;
	if ((new->next = p->next))
	    new->next->prev = new;
	else
	    h->Q_tail = new;
	p->next = new;
    } else {
	new->prev = (NSP_BUF *) 0;
	if ((new->next = h->Q_head))
	    new->next->prev = new;
	else
	    h->Q_tail = new;
	h->Q_head = new;
    }

#ifdef NSPDEBUG
    fprintf(stderr, "receive: Q=%s queued seq=%d\n",
	    seqs(h), getnspseq(&h->Q_head->pkt));
#endif
    return REC_DAT;
}

int
nsp_put_rel(NSP_HANDLE *h, void *buf, unsigned count, long timeout)
{
    NSP_PKT		pkt;
    time_t		t0, t1, t_lastxmit;
#ifdef NSPDEBUG
    int			retransmit_counter = 0;
#endif

    /* check args
     */
    if (!h || !h->ntp || !buf || count > NSP_MPDU) {
	errno = EINVAL;
	return -1;
    }

    /* build the transmit packet
     */
    setnsphdr(&pkt, NSP_TYPE_REL, ++h->put_seq);
    memcpy(pkt.buf, (char *) buf, count);

    /* transmit loop
     */
    time(&t0);
    t_lastxmit = 0;
    do {
	time(&t1);

	/* (re)transmit the packet (never twice in the same second)
	 */
	if (t1 != t_lastxmit) {
	    t_lastxmit = t1;
#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_put_rel: type=%d seq=%d userlen=%d %s\n",
		    getnsptype(&pkt),
		    getnspseq(&pkt),
		    count,
		    retransmit_counter++ ? "RETRANSMIT" : "");
#endif
	    switch (ntp_put(h->ntp,
			    (void *) &pkt,
			    (unsigned) &((NSP_PKT *)0)->buf[count],
			    1)) {
	    case -1:
#ifdef NSPDEBUG
		perror("ntp_put");
#endif
		return -1;
	    case 0:
#ifdef NSPDEBUG
		fprintf(stderr, "ntp_put: EOF\n");
#endif
		return 0;
	    }
	}

	/* get packet (expecting ack)
	 */
	switch (receive(h, 1)) {
	case REC_ACK:
	    return count;
	case REC_DAT:
	    /* discard data packets */
	    continue;
	case REC_ERR:
	    if (errno == EINTR)
		continue;
	    return -1;
	case REC_EOF:
	    return 0;
	}
    } while (t1 < t0 + timeout);
    errno = EINTR;
    return -1;
}

int
nsp_get(NSP_HANDLE *h, void *buf, unsigned size, long timeout)
{
    time_t	t0;
    long	t;
    NSP_BUF    *p;

    if (!h || !h->ntp || !buf || size > NSP_MPDU) {
	errno = EINVAL;
	return -1;
    }

    /* receive loop
     */
    time(&t0);
    t = timeleft(t0, timeout);
    do {

	/* check queue for obsolete packets and discard them
	 */
	while (h->Q_head &&
	       subtract_seq(getnspseq(&h->Q_head->pkt), h->get_seq) <= 0) {

#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_get: Q=%s get_seq=%d discarded old seq=%d\n",
		    seqs(h),
		    h->get_seq,
		    getnspseq(&h->Q_head->pkt));
#endif

	    /* dequeue it and discard it
	     */
	    if ((h->Q_head = (p = h->Q_head)->next))
		h->Q_head->prev = (NSP_BUF *) 0;
	    else
		h->Q_tail = (NSP_BUF *) 0;
	    free((char *) p);

	    continue;
	}

	/* check queue for next desired packet
	 */
	if (h->Q_head &&
	    subtract_seq(getnspseq(&h->Q_head->pkt), h->get_seq) == 1) {
	    unsigned		n;
	    
#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_get: Q=%s get_seq=%d returned seq=%d\n",
		    seqs(h),
		    h->get_seq,
		    getnspseq(&h->Q_head->pkt));
#endif

	    /* copy it to caller's buffer
	     */
	    n = h->Q_head->userlen;
	    if (n > size)
		n = size;
	    memcpy((char *) buf, h->Q_head->pkt.buf, n);

	    /* dequeue it
	     */
	    if ((h->Q_head = (p = h->Q_head)->next))
		h->Q_head->prev = (NSP_BUF *) 0;
	    else
		h->Q_tail = (NSP_BUF *) 0;
	    free((char *) p);

	    h->get_seq++;
	    return n;
	}

	/* check queue for gap
	 * (gap ::= TYPE_REL packet with seq > expected
	 *        | TYPE_UNREL packet with seq > expected + reorder_window wize
	 * )
	 */
	for (p = h->Q_head; p; p = p->next) {
	    if (getnsptype(&p->pkt) == NSP_TYPE_REL ||
		subtract_seq(getnspseq(&p->pkt), h->get_seq) >=
		h->reorder_window) {

#ifdef NSPDEBUG
		fprintf(stderr, "nsp_get: Q=%s get_seq=%d gap report seq=%d\n",
			seqs(h),
			h->get_seq,
			h->get_seq + 1);
#endif

		h->get_seq++;
		return NSP_DROP;
	    }
	}

	/* get packet
	 */
	switch (receive(h, t)) {
	case REC_ACK:
	    continue;
	case REC_DAT:
	    continue;
	case REC_ERR:
#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_get: receive() returned REC_ERR\n");
#endif
	    return -1;
	case REC_EOF:
#ifdef NSPDEBUG
	    fprintf(stderr, "nsp_get: receive() returned REC_EOF\n");
#endif
	    return 0;
	}
    } while (t = timeleft(t0, timeout));
#ifdef NSPDEBUG
    fprintf(stderr, "nsp_get: timeout\n");
#endif
    errno = EINTR;
    return -1;
}
