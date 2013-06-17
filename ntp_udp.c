/*
* The author of this software is William Dorsey.
* Copyright (c) 1996 by William Dorsey.  All rights reserved.
*
* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
* WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
* ITS FITNESS FOR ANY PARTICULAR PURPOSE.
*/

/* ntp_udp.c
*
* SCCS ID:  @(#)ntp_udp.c 1.2 96/05/22
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <errno.h>
#if defined(unix)
#include <unistd.h>
#include <memory.h>
#if defined(sun)
#include <sys/filio.h>
#endif /* defined(sun) */
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#define ioctlsocket ioctl
#endif
#ifdef _WIN32
#include <winsock2.h>
#include <io.h>
#define strcasecmp stricmp
#endif
#ifndef EDESTADDRREQ
#define EDESTADDRREQ 1216
#endif

#include "ntp.h"
#include "nautilus.h"

#undef NTP_DEBUG

static char    *default_udp_port = "12370";
extern struct param_t params;             /* operating parameters */


static void
progress(NTP_HANDLE *h, char *prefix, char *message)
{
    char *buf = malloc(strlen(prefix) + 3 + strlen(message));
    if (buf) {
		sprintf(buf, "%s: %s", prefix, message);
		(*h->msg_cb)(buf);
		free(buf);
    }
}

static int
udp_connect(NTP_HANDLE *h, char *addr)
{
    char	       *buf, *host, *port, *s;
    struct hostent     *he;
    struct servent     *se;
    struct sockaddr_in	sin;
    struct sockaddr_in	local_sin;

    
    /*
	* If host contains special punct then consider the remainder of the string
	* to be a port name (number) which overrides the one in the 2nd argument.
	*/
    if (!addr || !*addr)
		return -1;
    buf = strdup(addr);
    if (!buf)
		return -1;
    
    host = strtok(buf, ",:");
    if (!host || !*host) {
		free(buf);
		return -1;
    }
    port = strtok(NULL, ",:");
    if (!port || !*port)
		port = default_udp_port;
    
		/*
		* Lookup the host in /etc/hosts or yellow pages.
	*/
    sin.sin_addr.s_addr = inet_addr(host);
    if (sin.sin_addr.s_addr == (u_long) -1) {
		he = gethostbyname(host);
		if (!he) {
			progress(h, host, "Unable to resolve host name");
			free(buf);
			errno = EDESTADDRREQ;
			return -1;
		}
		sin.sin_addr = *(struct in_addr *) he->h_addr;
    }
    
    /*
	* Lookup the port in /etc/services or yellow pages.
	*/
    sin.sin_port = (u_short) strtol(port, &s, 0);
    sin.sin_port = htons(sin.sin_port);
    if (s == port || *s) {
		se = getservbyname(port, (char *) NULL);
		if (!se) {
			progress(h, port, "Unable to resolve service name");
			free(buf);
			errno = EDESTADDRREQ;
			return -1;
		}
		sin.sin_port = htons(se->s_port);
    }
    
	/* When we want to select the local port, do a bind */
	if (params.net.localport){
			/* do not set sin.sin_addr, let it be 0.0.0.0 as netcat does*/
			memset(&local_sin, 0, sizeof(local_sin));
			local_sin.sin_family = AF_INET;
			local_sin.sin_port = htons(params.net.localport);
			if (bind(h->fd, (struct sockaddr *) &local_sin, sizeof local_sin)) {
				progress(h, "bind local port", strerror(errno));
				return -1;
			}

	/* Add Nat traversal preamble here 
		send traversal packets to address sin and do a rcvfrom on 
		10 times: sendto( h->fd, NAT_TRAVERSE_PING, sizeof(NAT_TRAVERSE_PING), 
				(const struct sockaddr *) &sin, sizeof sin )	
		1 time: sendto( h->fd, NAT_TRAVERSE_ACK, sizeof(NAT_TRAVERSE_ACK), 
				(const struct sockaddr *) &sin, sizeof sin )	
		1 time: wait on NAT_TRAVERSE_ACK  with
			ssize_t recvfrom(int s, void *buf, size_t len, int flags,
                        struct sockaddr *from, socklen_t *fromlen);

	*/
	}

    /*
	* Save the address for working around winsock bug that
	* won't let us call recvfrom() on a connected socket.
	*/
    sin.sin_family = AF_INET;
	h->rem_addr_len = sizeof sin;
	h->rem_addr = (void *) malloc(sizeof sin);
	if (h->rem_addr == NULL) {
		progress(h, addr, "malloc() failed");
		free(buf);
		return -1;
	}
	memcpy(h->rem_addr, &sin, sizeof sin);

    free(buf);
    return 0;
}


static int
udp_bind(NTP_HANDLE *h)
{
    char	       *s;
    struct servent     *se;
    struct sockaddr_in	sin;
    
    /*
	* Lookup the host in /etc/hosts or yellow pages.
	*/
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = (u_short) strtol(default_udp_port, &s, 0);
    sin.sin_port = htons(sin.sin_port);
    if (s == default_udp_port || *s) {
		se = getservbyname(default_udp_port, (char *) NULL);
		if (!se) {
			progress(h, default_udp_port,
				"Unable to resolve service name");
			errno = EDESTADDRREQ;
			return -1;
		}
		sin.sin_port = htons(se->s_port);
    }
    
    /*
	* Attempt to bind.
	*/
    sin.sin_family = AF_INET;
    if (bind(h->fd, (struct sockaddr *) &sin, sizeof sin)) {
		progress(h, "bind", strerror(errno));
		return -1;
    }
	
	/* Add Nat traversal preamble here, so we have to "sendto" some packets
       and we have to wait for an ACK package (on bound socket)  */
    return 0;
}

static int
xxioctl(NTP_HANDLE *h, char *opt, char *val)
{
    if (strcasecmp(opt, "udp_port") == 0)
		default_udp_port = strdup(val);
    else
		return -1;
    return 0;
}

static int
xxopen(NTP_HANDLE *h, char *address, long timeout)
{
    int		flag;
	
    /* check args
	*/
    if (!h || !h->ntp) {
		errno = EINVAL;
		return -1;
    }
	
    /* open socket
	*/
    h->fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (h->fd == -1) {
#ifdef _WIN32
		int err;
		perror("socket");
		err = WSAGetLastError();
		fprintf(stderr, "err=%d\n", err);
#else
		perror("socket");
#endif
		return -1;
	}
	
		/* connect/bind as appropriate
	*/
    if (address && *address) {
		/* originator */
		if (udp_connect(h, address)) {
			close(h->fd);
			h->fd = -1;
			return -1;
		}
    } else {
		/* recipient */
		if (udp_bind(h)) {
			close(h->fd);
			h->fd = -1;
			return -1;
		}
    }
	
    /* set non-blocking IO mode   (why?)
	*/
    flag = 1;
    if (ioctlsocket(h->fd, FIONBIO, (char *) &flag)) {
		progress(h, "FIONBIO", strerror(errno));
		close(h->fd);
		h->fd = -1;
		return -1;
    }
	
    return 0;
}

static int
xxclose(NTP_HANDLE *h) {
    int		i;
	
    i = close(h->fd);
    h->fd = -1;
    return i;
}

static int
xxput(NTP_HANDLE *h, void *buf, unsigned count, long timeout) {
    /* UDP never blocks so timeout can be ignored */
#ifdef NTP_DEBUG
    {
        unsigned seqno;
        static prev_dropped = 0;
		
        if (prev_dropped) {
            if ((random()%100) < 50) {
                seqno = ((((unsigned char *)buf)[0]&0x3f) << 8) + ((unsigned char *)buf)[1];
                fprintf(stderr, "dropping packet #%u\n", seqno);
                return count;
			}
        }
        else {
            if ((random()%100) < 4) {
                seqno = ((((unsigned char *)buf)[0]&0x3f) << 8) + ((unsigned char *)buf)[1];
                fprintf(stderr, "dropping packet #%u\n", seqno);
                prev_dropped = 1;
                return count;
            }
        }
        prev_dropped = 0;
		if ((random()%100) < 2) {
			seqno = ((((unsigned char *)buf)[0]&0x3f) << 8) + ((unsigned char *)buf)[1];
			fprintf(stderr, "duplicating packet #%u\n", seqno);
			sendto(h->fd, buf, count, 0,
				(struct sockaddr *) h->rem_addr, h->rem_addr_len);
		}
    }
#endif /* NTP_DEBUG */
	return sendto(h->fd, buf, count, 0,
		(struct sockaddr *) h->rem_addr, h->rem_addr_len);
}

static int
xxget(NTP_HANDLE *h, void *buf, unsigned size, long timeout)
{
    fd_set		r_fdset;
    struct timeval	tval;
    int                 pktlen;
    struct sockaddr	recvfrom_addr;
    int			recvfrom_addr_len;
	
reread:
    FD_ZERO(&r_fdset);
    FD_SET(h->fd, &r_fdset);
    tval.tv_sec = timeout;
    tval.tv_usec = 0;
    switch (select(FD_SETSIZE, &r_fdset, (fd_set *) 0, (fd_set *) 0,
		timeout >= 0 ? &tval : (struct timeval *) 0)) {
    case 1:	/* data available */
		
		/* get the packet
		 */
		recvfrom_addr_len = sizeof recvfrom_addr;
		memset((char *) &recvfrom_addr, 0, recvfrom_addr_len);
		pktlen = recvfrom(h->fd, (char *) buf, size, 0,
			&recvfrom_addr, &recvfrom_addr_len);
		switch (pktlen) {
		case 0:
			return 0;
		case -1:
			return -1;
		}
		
		/* check recvfrom_addr for change in remote address
		 */
		if (h->rem_addr_len == 0) {
			/* first packet received - do connect and save the address */
			h->rem_addr = (void *) malloc(recvfrom_addr_len);
			if (!h->rem_addr) {
				progress(h, "malloc", strerror(errno));
				return -1;
			}
			memcpy((char *) h->rem_addr, (char *) &recvfrom_addr,
				h->rem_addr_len = recvfrom_addr_len);
		}
		else {
			/* The following value for addrlen avoids a problem in
			 * some socket implementations where the unused bytes
			 * at the end of the sockaddr_in struct contains garbage
			 * which varies from call to call (which would cause the
			 * comapre to fail).
			 */
			static int addrlen = (int) ((struct sockaddr_in *)0)->sin_zero;
			if (h->rem_addr_len != recvfrom_addr_len ||
				memcmp((char *) h->rem_addr, (char *) &recvfrom_addr,
				addrlen)) {
				/* subsequent packets should match the rem_addr of the first */
				progress(h,
					"Packet from unexpected source ignored",
					inet_ntoa(((struct sockaddr_in *)h->rem_addr)->sin_addr));
				goto reread;
			}
		}
			
		return pktlen;
	case 0:	/* timeout */
		errno = EINTR;
		return -1;
	case -1:	/* error */
		return -1;
	}
	/* NOTREACHED */
	return(0);
}
	
NTP_CLASS ntp_udp = {
	"udp",
	xxioctl,
	xxopen,
	xxclose,
	xxput,
	xxget
};
