/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */
 
/* comm.c
 *
 * SCCS ID: @(#)comm.c 1.22 96/05/25
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/08  B. Dorsey       Module created by breakup of nautilus.c
 * 95/09/17  P. Kronenwetter Added socket support from S. Parekh patches.
 * 04/02/29  S. Wieseckel    fixed compiler warning
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
#include <netdb.h>
#endif

#include "nautilus.h"

/* RSAREF include files */
#include "global.h"
#include "rsaref.h"

#define XCHG_TIMEOUT    15

/* external variables */
extern struct param_t params;
extern struct coder_t coders[NCODERS];
extern struct negotiate_t negotiate;
extern int s_port;

/* definitions */
int exchange_crypto_keys(enum modes, enum keyexch_type, enum keyexch_type);
int passphrase_key_exchange(enum modes);
int dh_key_exchange(R_DH_PARAMS *, enum modes);
int exchange_structure (char *, enum modes, UINT8 *, UINT8 *, int, int);
void setup_ivs(enum modes, BLOCK *, BLOCK *);

int
XChange(enum modes mode)
{
    int                i, speed, i_am_newer, compatible, quality;
    struct negotiate_t *orig, *ans, *older_version, *newer_version;
    struct packet_t    packet;

    orig = NULL;
    ans = NULL;

    /* Set modem speed in negotiation parameters (if known) */
    if (params.modem.speed > 0) {
        negotiate.modem_speed[0] = params.modem.speed % 256;	/* LSB */
        negotiate.modem_speed[1] = params.modem.speed / 256;	/* MSB */
    }
	
    /* Exchange negotiation parameters */
    switch (mode) {
    case ORIGINATE:
	/*
	 * The call originator sends its negotiation parameters and
	 * then reads the other sides negotiation parameters, failing
	 * on error or timeout conditions.
	 */
		if (SendPkt(RDATA, (UINT8 *) &negotiate, sizeof(struct negotiate_t),
			params.sp_timeout) == FAIL) {
			error(MSG_ERROR, "Error sending negotiate structure");
			return FAIL;
		}
		if (ReadPkt(&packet, params.rp_timeout) == FAIL) {
			if (errno == EINTR) {
				error(MSG_ERROR, "Timeout waiting for negotiate structure");
				return FAIL;
			}
			else {
				error(MSG_ERROR, "Error reading negotiate structure");
				return FAIL;
			}
			return FAIL;
		}
		if (packet.type != RDATA) {
			error(MSG_ERROR, "Unexpected packet type received");
			return FAIL;
		}
		orig = &negotiate;
		ans = (struct negotiate_t *) packet.data;
		break;
		
    case ANSWER:
    case AUTO_ANSWER:
	/*
	 * The call answerer reads the other sides negotiation
	 * parameters and then sends its negotiation parameters
	 * to the other side, failing on errors or timeouts.
	 */
		if (ReadPkt(&packet, -1) == FAIL) {
			if (errno == EINTR) {
				error(MSG_ERROR, "Timeout waiting for negotiate structure");
				return FAIL;
			}
			else {
				error(MSG_ERROR, "Error reading negotiate structure");
				return FAIL;
			}
			return FAIL;
		}
		if (packet.type != RDATA) {
			error(MSG_ERROR, "Unexpected packet type received");
			return FAIL;
		}
		if (SendPkt(RDATA, (UINT8 *) &negotiate, sizeof(struct negotiate_t),
			params.sp_timeout) == FAIL) {
			error(MSG_ERROR, "Error sending negotiate structure");
			return FAIL;
		}
		orig = (struct negotiate_t *) packet.data;
		ans = &negotiate;
		break;
    }
	
    /* Verify that packet is the right size */
    if (packet.length != sizeof(struct negotiate_t)) {
	error(MSG_ERROR, "Error negotiation packet size mismatch");
        return FAIL;
    }
	
    /* Verify version compatibility */
    if ((orig->major == ans->major) && (orig->minor == ans->minor)) {
	/*
	 * The major and minor version numbers match exactly.  Version
	 * numbers 1.0 and later will be assigned such that if this
	 * condition is true, then the two versions are compatible.
	 * For example, it will never be the case that a version 1.0.1
	 * and 1.0.2 will be incompatible.  Version 1.0.2 might be
	 * compatible with 1.1.0, or it might not.
	 * 
	 * Do nothing here, and continue with the rest of the program.
	 */
    }
    else {
	/*
	 * There is a version mismatch.  The older version has no way
	 * to know whether it is compatible with the newer version.
	 * The newer of the two participants must tell the older one
	 * whether they are compatible.  If not, it also sends clear
	 * text to the older version explaining the problem and how to
	 * upgrade.  The older version displays this information on
	 * the screen and saves it to a file UPGRADE_FILE.
	 */
		if ((orig->major > ans->major) ||
			((orig->major == ans->major) && (orig->minor > ans->minor))) {
			/*
			 * Originator is newer.
			 */
			if (mode == ORIGINATE) {
				i_am_newer = TRUE;
			}
			else {
				i_am_newer = FALSE;
			}
			older_version = ans;
			newer_version = orig;
		}
		else {
		/*
		 * Answerer is newer.
		 */
			if (mode == ORIGINATE) {
				i_am_newer = FALSE;
			}
			else {
				i_am_newer = TRUE;
			}
			older_version = orig;
			newer_version = ans;
		}
		
		if (i_am_newer) {
			if (VersionsCompatible(older_version->major,
				older_version->minor)) {
				/*
				 * Tell the older version that all is well, and go on.
				 *
				 * There is a possibility that this packet will be
				 * garbled and discarded, and the rest of the protocol
				 *  would likely fail.
				 *
				 * A similar problem can occur in other parts of the
				 * protocol as well.  For example, in the negotiation
				 * parameter exchange above, the answerer could
				 * successfully receive the originator's parameters,
				 * and reply with its own, but the answerer's packet
				 * could then be lost.  The answerer goes on with the
				 * protocol, but the originator keeps sending
				 *
				 * I'm not certain, but the protocol used here may
				 * contain pieces which are similar to the well known
				 * Coordinated Attack problem in the distributed
				 * algorithms area of research.  If so, then there is
				 * no solution that is guaranteed to work within any
				 * fixed time limit, but there could easily be
				 * solutions which don't fail due to the loss of a
				 * single packet.  I'll think about it.  It is more
				 * likely similar to problems in committing operations
				 * in a distributed database.  Then again, I could be
				 * imagining the problem to be more difficult than it
				 * really is.
				 *
				 * See what happens to you when you take a course in
				 * distributed algorithms.  You turn weird :-)
				 *
				 * To protect the innocent, the comment above was
				 * written by Andy Fingerhut.
				 */
				if (params.verbose ){
					fprintf(stderr, "Sending compatibility info\n");
				}
				SendCompatible(TRUE);
				/*
				 * Go on with the rest of the program in this case.
				 */
			}
			else {
			/* 
			 * Tell the older version about incompatibility, and
			 * how to upgrade.
			 */
				SendCompatible(FALSE);
				SendUpgradeInfo(older_version->major, older_version->minor);
				return FAIL;
			}
		}
		else {
		/*
		 * This is the code executed by the older version.  Wait
		 * for the packet containing compatibility info, and if
		 * the verdict is 'incompatible', then receive, display,
		 * and save the updgrade info.  Acknowledge the receipt
		 * of this message.
		 */
			if (ReceiveCompatible(&compatible) == FAIL) {
				error(MSG_ERROR, "Never received compatibility information");
				return FAIL;
			}
			if (!compatible) {
				ReceiveUpgradeInfo(newer_version->major, newer_version->minor);
				return FAIL;
			}
			/*
			 * If compatible, go on with the rest of the program.
			 */
		}
    }
	
    /* Get modem speed or assume 14400 bps if unknown */
    speed = orig->modem_speed[0] + (orig->modem_speed[1] << 8);
    if (speed == 0)
        speed = ans->modem_speed[0] + (ans->modem_speed[1] << 8);
    if (speed == 0)
        speed = DEFAULT_DCE_SPEED;
	
    /* Find best coder to use */
    quality = -1;
    for (i=0; i<NCODERS; i++) {
        if ((coders[i].preference > quality) &&
            (coders[i].bandwidth <= speed) &&
            CoderUsable(orig, i) && CoderUsable(ans, i)) {
            quality = coders[i].preference;
            params.coder.index = i;
		}
    }
    if (quality < 0 && ! params.key_ex_only){
	 error(MSG_ERROR, "failed to find best Coder (internal error)");
        return FAIL;
    }
	
    /* Did answerer specify a coder? */
    if (ans->coder < NCODERS) {
        if ((coders[ans->coder].bandwidth <= speed)
			&& CoderUsable(orig, ans->coder) && CoderUsable(ans, ans->coder)) {
			params.coder.index = ans->coder;
        }
    }
	
    /* Did originator specify a coder? */
    if (orig->coder < NCODERS) {
        if ((coders[orig->coder].bandwidth <= speed)
			&& CoderUsable(orig, orig->coder) && CoderUsable(ans, orig->coder)) {
			params.coder.index = orig->coder;
        }
    }
	
    /*
	 * For now, make sure that both ends have specified the same type
	 * of encryption (or none).  If they haven't, treat it as an
	 * error.
	 *
	 * NOTE: If the behavior of quitting like this remains in future
	 * versions, it would be nicer if the program told the user which
	 * coders were selected by the caller and callee, to help them
	 * determine what is going wrong.
	 */
    if (orig->encrypt_type != ans->encrypt_type) {
		error(MSG_ERROR, "Selected encryption must match");
		return FAIL;
    }
	
    if (params.crypto.key1.type != NONE) 
        exchange_crypto_keys (mode, orig->keyexch_type, ans->keyexch_type);
	
    return SUCCESS;
}

/*
 * set up crypto keys.
 * MODE is ORIGINATE or ANSWER.
 * KO is method requested by originator,
 * KA is method requested by answerer.
 *
 * For now, having one person request DH while the other wants
 * to use a passphrase is a fatal error.  Alternative would be
 * to figure out from version # if both sides have DH available,
 * or prompt for a passphrase, but who knows what this would break,
 * so we don't mess with it for now.
 *
 * If both sides want DH but request differing lengths, we go with
 * the shorter length in deference to the user who probably has a
 * slower processor.  When we get faster arithmetic, we may reconsider
 * this approach.
 */
int
exchange_crypto_keys (enum modes mode,
		      enum keyexch_type ko,
		      enum keyexch_type ka)
{
    extern R_DH_PARAMS dh_small_params, dh_medium_params, dh_large_params;
    extern R_DH_PARAMS dh_enormous_params;
	
    if (ko != ka) {
		if (ko == PASSPHRASE || ka == PASSPHRASE)
			error (MSG_FATAL,
			"Inconsistent key exchange methods specified; try again.");
		error (MSG_WARNING,
			"Two sides want differing DH key lengths; choosing smaller length");
		if (ka < ko)
			ko = ka;
    }
	
    switch (ko) {
    case PASSPHRASE:
		passphrase_key_exchange (mode);
		break;
		
    case DIFFIE_HELLMAN_SMALL:
		dh_key_exchange (&dh_small_params, mode);
		break;
		
    case DIFFIE_HELLMAN_MEDIUM:
		dh_key_exchange (&dh_medium_params, mode);
		break;
		
    case DIFFIE_HELLMAN_LARGE:
		dh_key_exchange (&dh_large_params, mode);
		break;
		
    case DIFFIE_HELLMAN_ENORMOUS:
		dh_key_exchange (&dh_enormous_params, mode);
		break;
		
    default:
		error (MSG_FATAL, "invalid key exchange type");
		break;
    }
	
    return SUCCESS;
}

int
dh_key_exchange (R_DH_PARAMS *dh_params, enum modes mode)
{
    void print_dh_verification();
    R_RANDOM_STRUCT random_struct;
    UINT8 junk[64];
	
    UINT8 dh_public[MAX_DH_MESSAGE_LENGTH];
    UINT8 dh_private[MAX_DH_MESSAGE_LENGTH];
    UINT8 remote_dh_public[MAX_DH_MESSAGE_LENGTH];
    UINT8 agreed_key[MAX_DH_MESSAGE_LENGTH];
    BLOCK local_iv, remote_iv;
    /* dh_bytes is length in bytes of agreed DH session key. */
    int dh_bytes = dh_params->primeLen;
    int i;
	
    /*
	 * steps:
	 * 1. initialize random structure
	 * 2. set up secret parameters with R_SetupDHAgreement
	 * 3. exchange public info with remote host
	 * 4. compute agreed-upon key with R_ComputeDHAgreedkey
	 * 5. load the computed key into the appropriate Nautilus structure.
	 */
	
    /*
	 * RSAREF's DH setup routine uses RSAREF's internal random
	 * number generator, so we initialize that RNG with bytes from
	 * our own RNG.  We call R_RandomUpdate 16 times just
	 * for good measure.
	 */
    R_RandomInit (&random_struct);
	
    for (i = 0; i < 16; i++) {
		random_bytes (junk, sizeof junk);
		R_RandomUpdate (&random_struct, (unsigned char *) junk,
			sizeof junk);
    }
	
    /*
	 * burn sensitive data as soon as it's no longer needed.
	 * This reduces chance of a copy ending up lying around
	 * in swap space.
	 */
    memset (junk, 0, sizeof junk); 
	
    /*
	 * set up the local side of the DH key agreement
	 * Use a very long timeout to exchange the structures,
	 * since it can take a long time to compute if the remote
	 * computer is slow.  With more efficient DH arithemetic (TBD),
	 * the timeout can be made more reasonable.
	 */
    fprintf (stderr,
		"\nStarting DH key exchange (%d bits), please wait... \n",
		dh_params->primeLen * 8);
    R_SetupDHAgreement (dh_public, dh_private, DH_PRIVATE_LENGTH,
		dh_params, &random_struct);
    if (params.verbose ) fprintf (stderr,"\nburn sensitive data \n");
    memset (&random_struct, 0, sizeof random_struct); /* burn sensitive data */
    if (params.verbose ) fprintf (stderr,"\nexchange structure\n");
    exchange_structure ("Diffie-Hellman public key values",
		mode, dh_public, remote_dh_public, dh_bytes, 60);
	
	/*
	 * do the DH computation to get the agreed-upon key
	 */
    R_ComputeDHAgreedKey (agreed_key, remote_dh_public,
		dh_private, DH_PRIVATE_LENGTH,
		dh_params);
	
    memset (dh_private, 0, sizeof dh_private); /* burn sensitive data */
	
    print_dh_verification (agreed_key, dh_bytes);
	
    /* copy agreed to key to params.session key and run keyinit. */
	/* XXX -- MAX_SKEY_LEN is 80 bytes (640 bits).  If the DH
	 * key exchange has more than 640 bits in it, we throw some
	 * of them away and lose some of the benefit of a larger
	 * key.  This should be addressed.  One suggestion is to
	 * always hash the agreed-upon key into 160 bits (SHA hash
	 * size) and use this to initialize the symmetric ciphers.
	 * Another option is to increase the size of MAX_SKEY_LEN
	 * to be as large as the largest DH key that is handled
	 * (currently 2048 bits or 256 bytes).  Unfortunately, neither
	 * of these approaches will provide backwards compatibility
	 * with older versions of Nautilus.  Recommend the latter
	 * approach given the fact that keyinit() calls keycrunch()
	 * which hashes the data in the session key into a buffer
	 * of the correct length for initializing the selected symmetric
	 * cipher.
	 */
    memcpy (params.crypto.skey, agreed_key, 
		dh_bytes < MAX_SKEY_LEN ? dh_bytes : MAX_SKEY_LEN);
	
    memset (agreed_key, 0, sizeof agreed_key); /* burn sensitive data */
	
    params.crypto.skey_len = dh_bytes;
	
    /*
	 * Call keyinit with the session key.
	 */
    if (keyinit(&params.crypto.key1, params.crypto.skey,
		params.crypto.skey_len, 0) < 0) {
		error(MSG_ERROR, "Key initialization failed");
		return FAIL;
    }
    memset(params.crypto.skey, '\0', MAX_SKEY_LEN); /* burn sensitive data */
	
	/*
	 * Set up initialization vectors (these are not secret)
	 */
    random_bytes ((UINT8 *) &local_iv, sizeof local_iv);
    exchange_structure ("Crypto initialization vectors",
		mode, (UINT8 *) &local_iv, (UINT8 *) &remote_iv, sizeof local_iv, 60);
    setup_ivs (mode, &local_iv, &remote_iv);
	
    return 0;
}

/*
 * Print verification code by hashing the DH agreed key.
 * Users should read these digits (by voice) to each other to stop
 * man-in-the-middle attacks.
 * Issues:
 * 1. We may wish to use SHA instead of MD5 in the real release.
 *    This gets rid of the need for MD5 code in a future non-RSAREF release.
 * 2. Revealing this hash leaks a little bit of information about
 *    the agreed key.  An alternative would be to hash the public values
 *    instead of the private one, but that's vulnerable to brute force
 *    attacks unless the users read an awful lot of digits to each other.
 * 3. Printing out the hash before burning the key increases chances
 *    of the key getting copied to swap.  (Printing increases likelihood
 *    of swapping because it causes i/o waits, context switches etc.).
 *    To fix this we'd have to uglify the code by splitting this routine
 *    into two pieces.  We can't really win because the block cipher
 *    has to keep its key schedule around no matter what, so we don't
 *    go crazy doing this.  For maximum security, don't use Nautilus
 *    with virutal memory.
 *
 * Update for 1.6a:
 *
 * We now use another implementation of SHA-1 to hash values that is
 * endian-independent.  This fixes the bug when Intel (little endian)
 * and Sun (big endian) machines print different hash verification values
 * for identical keys.
 *
 * Expect verification codes to differ when connecting 1.6a to previous
 * versions on Sun machines.
 *
 * This function now is of type void, since it doesn't return anything.
 *
 * The calculated message digest is now burned, instead of left on the
 * stack.
 *
 * Update for 1.7a:
 *
 * New implementation of SHA-1 wasn't properly handling endianness
 * so verification codes would not match when a big-endian machine
 * connects to a little-endian machine or vice versa.  This is
 * now fixed.
 *
 * No need to burn the message digest -- it doesn't reveal anything
 * useful to a potential attacker.
 */

void
print_dh_verification (char *agreed_key, int length)
{
    void sha_memory();
    unsigned long digest[5];
    int loop; 
	

#ifdef USE_OLD_SHS
    shs_init();
    while (length-- > 0)
	shs_process(*agreed_key++);
    shs_hash(digest);
#else
    sha_memory(agreed_key, length, digest);
#endif
    	
    fprintf (stderr, "*** DH verification code: %02x%02x %02x%02x \n",
                (unsigned int)digest[1] & 0xFF,		/* was [7] */
               ((unsigned int)digest[3] >> 8) & 0xFF,	/* was [14] */
               ((unsigned int)digest[1] >> 16) & 0xFF,	/* was [5] */
               ((unsigned int)digest[3] >> 24) & 0xFF );/* was [12] */
    fprintf (stderr, "\n *** please read this code to the the other party ! ***\n"
		     "If your code differs form the other party, your\n"
		     "conversation is likely to be listened in via a\n"
		     "'man in the middle attack'\n");

    if ( params.key_ex_only )
    {
	printf("Agreed on a key of %d bytes\n",length);
	puts("- --BEGIN HEXDUMP OF AGREED KEY---");
	for(loop=0; loop < length; loop++){
		printf ("%02x", (unsigned char) agreed_key[loop])	;
	}
	puts("\n- --END BEGIN HEXDUMP OF AGREED KEY---");
    } 
}

void
setup_ivs (enum modes mode, BLOCK *local_iv, BLOCK *remote_iv)
{
    if (mode == ORIGINATE) {
		cfb_init(&params.crypto.xmit_ctx, &params.crypto.key1, local_iv);
		cfb_init(&params.crypto.recv_ctx, &params.crypto.key1, remote_iv);
    }
    else {
		cfb_init(&params.crypto.xmit_ctx, &params.crypto.key1, remote_iv);
		cfb_init(&params.crypto.recv_ctx, &params.crypto.key1, local_iv);
    }
}

/*
 * Exchange a fixed length data structure with the other computer.
 * Exit with fatal error if the exchange fails.
 *
 * NAME is a description of the data structure, used in the error
 *   message in case of failure.
 * MODE is ORIGINATE or ANSWER.
 * LOCAL_BUF is the structure to be sent to the remote computer
 * REMOTE_BUF is the structure to be received from the remote computer
 * LEN is the length of the structure (identical for local and remote)
 * TIMEOUT is the maximum time in seconds to wait before giving up.
 */
int
exchange_structure (char *name, enum modes mode, UINT8 *local_buf,
					UINT8 *remote_buf, int len, int timeout)
{
    struct packet_t packet;
	
    /*
	 * Exchange keyexch structures.  Originator goes first.
	 */
    if (mode == ORIGINATE) {
	    	if (params.verbose ) fprintf(stderr,"exchange_structure ORIGINATE\n");
		if (SendPkt(RDATA, local_buf, len, params.sp_timeout) == FAIL) {
			error(MSG_FATAL, "Error sending keyexch structure");
			return FAIL;
		}
		if (ReadPkt(&packet, timeout) == FAIL) {
			if (errno == EINTR) {
				error(MSG_FATAL, "Timeout while exchanging %s", name);
				return FAIL;
			}
			else {
				error(MSG_FATAL, "Fatal error reading keyexch structure");
				return FAIL;
			}
		}
		if ((packet.type != RDATA) || (packet.length != len)) {
			error(MSG_FATAL, "Packet exchange failure for %s", name);
			return FAIL;
		}
    }
    else {
	    	if (params.verbose ) fprintf(stderr, "exchange_structure ANSWER\n");
		if (ReadPkt(&packet, timeout) == FAIL) {
			if (errno == EINTR) {
				error(MSG_FATAL, "Timeout while exchanging %s", name);
				return FAIL;
			}
			else {
				error(MSG_FATAL, "Fatal error reading keyexch structure");
				return FAIL;
			}
		}
		if ((packet.type != RDATA) || (packet.length != len)) {
			error(MSG_FATAL, "Packet exchange failure for %s", name);
			return FAIL;
		}
		if (SendPkt(RDATA, (UINT8 *) local_buf, len, params.sp_timeout) == FAIL) {
			error(MSG_FATAL, "Error sending keyexch structure");
			return FAIL;
		}
    }
    memcpy ((char *) remote_buf, (char *) packet.data, len);
	return SUCCESS;
}

int
passphrase_key_exchange (enum modes mode)
{
    struct keyexch_t   keyexch1, keyexch2;
    struct packet_t    packet;
    UINT8              *p1, *p2;
    int i;
	
    /*
	 * Generate keyexch structure.
	 */
    params.crypto.skey_len = MAX_SKEY_LEN;
    random_bytes((UINT8 *) &keyexch1.sess_iv, 8);
    random_bytes((UINT8 *) &keyexch1.xmit_iv, 8);
    random_bytes((UINT8 *) &keyexch1.recv_iv, 8);
    random_bytes((UINT8 *) &keyexch1.skey, params.crypto.skey_len);
	
    /*
	 * Copy the keyexch structure just generated and encrypt it
	 * in CFB mode using the sess_iv element (first 8 bytes) as
	 * the IV.  Obviously, the first 8 bytes are not encrypted.
	 */
    keyexch2 = keyexch1;
    cfb_init(&params.crypto.sess_ctx, &params.crypto.key1, &keyexch2.sess_iv);
    cfb_encrypt(&params.crypto.sess_ctx, (UINT8 *) &keyexch2.xmit_iv,
		sizeof(struct keyexch_t) - sizeof(BLOCK));
    keydestroy(&params.crypto.key1);
	
    /*
	 * Exchange keyexch structures.  Originator goes first.
	 */
    if (mode == ORIGINATE) {
		if (SendPkt(RDATA, (UINT8 *) &keyexch2, sizeof(struct keyexch_t),
			params.sp_timeout) == FAIL) {
			error(MSG_FATAL, "Error sending keyexch structure");
			return FAIL;
		}
		if (ReadPkt(&packet, params.rp_timeout) == FAIL) {
			if (errno == EINTR) {
				error(MSG_FATAL, "Timeout during crypto parameter negotiation");
				return FAIL;
			}
			else {
				error(MSG_FATAL, "Fatal error reading keyexch structure");
				return FAIL;
			}
		}
		if ((packet.type != RDATA) || (packet.length != sizeof(struct keyexch_t))) {
			error(MSG_FATAL, "Negotiation of crypto parameters failed");
			return FAIL;
		}
    }
    else {
		if (ReadPkt(&packet, params.rp_timeout) == FAIL) {
			if (errno == EINTR) {
				error(MSG_FATAL, "Timeout during crypto parameter negotiation");
				return FAIL;
			}
			else {
				error(MSG_FATAL, "Fatal error reading keyexch structure");
				return FAIL;
			}
		}
		if ((packet.type != RDATA) || (packet.length != sizeof(struct keyexch_t))) {
			error(MSG_FATAL, "Negotiation of crypto parameters failed");
			return FAIL;
		}
		if (SendPkt(RDATA, (UINT8 *) &keyexch2, sizeof(struct keyexch_t),
			params.sp_timeout) == FAIL) {
			error(MSG_FATAL, "Error sending keyexch structure");
			return FAIL;
		}
    }
	
    /*
	 * Copy the received packet data into the second keyexch structure.
	 * Decrypt the second keyexch structure using the sess_iv element
	 * (first 8 bytes) as the iv.  Since we're done using the key
	 * schedule generated from the pass phrase, burn it.
	 */
    memcpy((char *) &keyexch2, (char *) packet.data, sizeof(struct keyexch_t));
    cfb_init(&params.crypto.sess_ctx, &params.crypto.key2, &keyexch2.sess_iv);
    cfb_decrypt(&params.crypto.sess_ctx, (UINT8 *) &keyexch2.xmit_iv,
		sizeof(struct keyexch_t) - sizeof(BLOCK));
    keydestroy(&params.crypto.key2);
	
    /*
	 * XOR the two decrypted keyexch structures together and copy
	 * the result to the params structure.
	 */
    p1 = (UINT8 *) &keyexch1.xmit_iv;
    p2 = (UINT8 *) &keyexch2.xmit_iv;
    for (i=0; i<sizeof(struct keyexch_t) - sizeof(BLOCK); i++) {
		*p1++ ^= *p2++;
    }
    memcpy((char *) params.crypto.skey, (char *) keyexch1.skey, params.crypto.skey_len);
	
    /*
	 * Call keyinit with the session key.
	 */
    if (keyinit(&params.crypto.key1, params.crypto.skey, params.crypto.skey_len, 0) < 0) {
		error(MSG_ERROR, "Key initialization failed");
		return FAIL;
    }
    memset(params.crypto.skey, '\0', MAX_SKEY_LEN);
	
    /*
	 * Initialize crypto contexts for transmitting/receiving.
	 */
    if (mode == ORIGINATE) {
		cfb_init(&params.crypto.xmit_ctx, &params.crypto.key1, &keyexch1.xmit_iv);
		cfb_init(&params.crypto.recv_ctx, &params.crypto.key1, &keyexch1.recv_iv);
    }
    else {
		cfb_init(&params.crypto.xmit_ctx, &params.crypto.key1, &keyexch1.recv_iv);
		cfb_init(&params.crypto.recv_ctx, &params.crypto.key1, &keyexch1.xmit_iv);
    }
	
    /*
	 * Burn sensitive data
	 */
    memset((char *) &keyexch1, '\0', sizeof(struct keyexch_t));
    memset((char *) &keyexch2, '\0', sizeof(struct keyexch_t));
	
    return SUCCESS;
}

int
ReadPkt(struct packet_t *pkt, int timeout)
{
    int retval;
	
    retval = nsp_get(params.session, pkt, sizeof(struct packet_t), timeout);
    switch (retval) {
    case -1:
        return -1;
    case -2:
        pkt->type = FILL;
        pkt->length = 0;
        return 0;
    default:
        pkt->length = retval-1;
        return retval;
    }
}

int
SendPkt(enum pkt_type type, UINT8 *data, int len, int timeout)
{
    UINT8 buf[MAX_PKT_DATA+1];
	
    buf[0] = type;
    memcpy((char *)buf+1, (char *)data, len);
	
    switch (type) {
    case UDATA:
    case FILL:
    case UEOT:
        return nsp_put_unrel(params.session, buf, len+1, timeout);
    case RDATA:
    case XMIT:
    case RECV:
    case REOT:
        return nsp_put_rel(params.session, buf, len+1, timeout);
    default:
        error(MSG_FATAL, "Internal Error #150");
        return FAIL;
    }
}
