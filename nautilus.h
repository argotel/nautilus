/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* nautilus.h
 *
 * SCCS ID:  @(#)nautilus.h 1.16 96/05/22
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/12/31  B. Dorsey          Module created
 * 00/10/10  J.Poehlmann        added modem dial prefix string
 */

#include "nsp.h"
#include "machine.h"

/* Debug level */
#ifndef DEBUG
#define DEBUG       0
#endif

/* Program definitions */
#define NCODERS				4
#define NCIPHERS			4
#define VERSION_MAJOR		1
#define VERSION_MINOR		8
#define VERSION_STRING		"Version 1.8rc1"

/* Miscellaneous parameters */
#define MAX_SKEY_LEN		80

/* MS-DOS stuff */
enum soundcards { SOUNDBLASTER };

/* Flags */
#define SUCCESS				(0)
#define FAIL				(-1)
#ifndef FALSE
#define FALSE				(0)
#endif
#ifndef TRUE
#define TRUE				(1)
#endif

/* Default operating parameters */
#define DEFAULT_DTE_SPEED       19200
#define DEFAULT_DCE_SPEED       14400
#if defined(_WIN32)
#define DEFAULT_PORT            "COM1"
#elif defined(unix)
#define DEFAULT_PORT            "/dev/modem"
#endif
#define DEFAULT_MODEM_INIT      "V1"
#define DEFAULT_MODEM_RESET		"Z"
#define DEFAULT_CODER_INDEX     0
#define DEFAULT_UPGRADE_FILE    "upgrade"
#define DEFAULT_LOGON_FILE		"logon_new.v"
#define DEFAULT_RING_FILE		"ring_new.v"
#define DEFAULT_PORT_NO			12370

/*NAT traversal constants*/
#define NAT_TRAVERSE_PING		"NAT traverse ping"
#define NAT_TRAVERSE_ACK		"NAT traverse ACK"

/* Turnaround beep parameters */
#define BEEP_MS				80
#define BEEP_AMPLITUDE		25000
#define BEEP_FREQ			750.0

/* Configuration types */
enum config_types { CONFIG_TYPE_STRING, CONFIG_TYPE_NUMBER, CONFIG_TYPE_UINT16 };

/* Operating modes */
enum modes { ORIGINATE, ANSWER, AUTO_ANSWER }; /* ORIGINATE _must_ be first! */
enum flow { RECEIVE, TRANSMIT };
enum o_submodes { DIAL, ATD_ONLY } ;

/* Packet types */
enum pkt_type { UDATA, RDATA, FILL, XMIT, RECV, UEOT, REOT, MAX_PACKET_TYPE };

/* Error types */
enum err_type { MSG_WARNING, MSG_ERROR, MSG_FATAL };

/* Special packet characters */
#define FRAME   0x7E
#define ESCAPE  0x7D

/* Key Exchange types */
enum keyexch_type { PASSPHRASE,
 					DIFFIE_HELLMAN_SMALL,
 					DIFFIE_HELLMAN_MEDIUM,
 					DIFFIE_HELLMAN_LARGE,
 					DIFFIE_HELLMAN_ENORMOUS,
};

/* Buffer size for DH messages.  It's large enough to hold
 * the longest DH prime.
 */
#define MAX_DH_MESSAGE_LENGTH 256 /* 2048 bits */

/* DH_PRIVATE_LENGTH is the length in bits of the private DH exponent.
 * Increasing it slows down the DH computation.
 * This value should be reasonable, though it might be better
 * to choose it dynamically according to the block cipher in
 * use, and maybe according to the DH prime length.
 */
#define DH_PRIVATE_LENGTH (384/8)

/* Encryption types */
enum crypto_type { NONE, BLOWFISH, IDEA, DES3 };

/* Function macros */
#define AddByte(buf, idx, byte) {       \
    switch (byte) {                     \
    case FRAME:                         \
    case ESCAPE:                        \
	buf[idx++] = ESCAPE;			    \
	buf[idx++] = (byte) ^ 0x20;			\
	break;								\
    default:                            \
	buf[idx++] = (byte);				\
	break;								\
    }                                   \
}

#define MAX_PKT_DATA    256             /* max size of data field */
struct packet_t {
    UINT8       type;                   /* packet type */
    UINT8       data[MAX_PKT_DATA];     /* packet data */
    UINT16      length;                 /* length of data in bytes */
};

struct coder_t {
    void        (*init) (void);
    void        (*encode) (INT16 *samples, UINT8 *bits);
    void        (*decode) (UINT8 *bits, INT16 *samples);
    int         sample_rate;	/* sample rate of coder */
    int         frame_size;     /* bytes per frame of input */
    int         output_size;    /* bytes per frame of output */
    int         frames_per_pkt; /* # frames per packet */
    int         bandwidth;		/* coder bandwidth (incl. overhead) */
    int         preference;		/* relative preference of coder */
    char        *name;          /* name of coder */
    char        *desc;          /* description of coder */
};

struct cipher_t {
    char        *name;          /* name of cipher */
    char        *desc;          /* description of cipher */
};

typedef struct {
    UINT32	left;
    UINT32	right;
} BLOCK;

typedef struct cfbctx_t {
    int         bufleft;	/* chars remaining in shift register */
    BLOCK       iv;			/* cfb initial vector */
    struct key_t {
        int     type;		/* type of encryption */
        int     key_len;	/* length of key schedule in bytes */
        void    *key;		/* key schedule */
    } *key;
} CFBCTX;

struct param_t {
    enum modes  mode;       /* startup mode */
    enum o_submodes orig_submode; /* submode for originate */
    char key_ex_only;       /* Stop after key exchange, do not touch sound*/
    char    telno[64];      /* phone # to dial */
    char	hostname[64];	/* hostname of remote host */
    char	upgrade_file[64];/* name of upgrade info file to write to */
    char	logon_file[64]; /* name of startup sound file to play */
    char	ring_file[64];	/* name of startup sound file to play */
    int     verbose;        /* verbose flag */
	float	jbufsecs;		/* size of jitter buffer (in seconds) */
    int		net_flag;		/* net mode flag */
    int		rp_timeout;		/* incoming data timeout in seconds */
    int		sp_timeout;		/* outgoing data timeout in seconds */
    NSP_HANDLE  *session;	/* NSP layer handle for comm channel */
    struct net_t {
        int     portnum;	/* which port to use */
		int		localport;	/* which local port top use - 0 = dont care */
    } net;
    struct port_t {
	int     speed;          /* DTE speed */
	char    name[64];       /* name of port */
    } port;
    struct modem_t {
	unsigned speed;         /* modem connect speed */
	char     init[128];     /* modem initialization string */
	char     prefix[128];   /* modem dial prefix string */
	char	 reset[128];	/* modem reset string */
    } modem;
    struct audio_t {
    	char	mic_sens[64];/* microphone sensitivity */
    	char	out_volume[64];/* audio output level */
    } audio;
    struct coder_p {
	int     index;          /* index of selected coder */
    } coder;
    struct msdos_t {
	INT16   snd_iobase;     /* i/o base address of soundcard */
	INT16   snd_irq;        /* irq # of soundcard */
	INT16   snd_dma;        /* dma channel of soundcard */
	INT16   snd_type;       /* type of soundcard */
	INT16	com_iobase[4];	/* i/o base addresses for com1 thru com4 */
	INT16   com_irq[4];		/* irq #s for com1 thru com4 */
    } msdos;
    struct crypto_t {
 	int keyexch_type;		/* key exchange protocol */
        struct key_t key1;	/* cipher info (for encoding) */
        struct key_t key2;	/* cipher info (for decoding) */
       	CFBCTX  sess_ctx;	/* crypt context for session key exchange */
       	CFBCTX  xmit_ctx;	/* crypt context for transmitting data */
       	CFBCTX  recv_ctx;	/* crypt context for receiving data */
       	int     skey_len;	/* length in bytes of session key */
       	UINT8   skey[MAX_SKEY_LEN];/* session key */
    } crypto;
};

struct config_t {
    char        name[64];	/* config parameter name */
    enum config_types  type;/* config parameter type */
    void        *value;		/* config parameter value */
};

struct negotiate_t {
    UINT8	major;			/* major version number */
    UINT8	minor;			/* minor version number */
    UINT8	encode[2];		/* "encode" capability bitfield */
    UINT8	decode[2];		/* "decode" capability bitfield */
    UINT8	coder;			/* specified coder (255=unspecified) */
    UINT8	encrypt_type;	/* type of encryption */
    UINT8	keyexch_type;	/* type of key exchange */
    UINT8	modem_speed[2];	/* modem speed (0=unknown, LSB first) */
};

/*
 * key exchange structure.  Note that the BLOCK type variables must
 * come first in this structure to force word alignment at the
 * beginning of the structure rather than in the middle where it
 * would introduce alignment problems on different platforms.
 */
struct keyexch_t {
    BLOCK	sess_iv;		/* iv for session key */
    BLOCK	xmit_iv;		/* xmit iv for receiver */
    BLOCK	recv_iv;		/* recv iv for receiver */
    UINT8	skey[MAX_SKEY_LEN];/* encrypted session key */
};

/* nautilus function prototypes */
void    init(void);
void	InitBeep(int sample_rate);
int     CoderSpeedTest(int min_test_duration_ms, int verbose_coder_speed_testing);
int     CoderUsable(struct negotiate_t *neg, int coder_id);
void    SetCoderUsable(struct negotiate_t *neg, int coder_id, int coder_usable);
void    title(void);
void	credits(void);
void    help(void);
void    usage(void);
int     FindCoder(char *arg);
int     FindCipher(char *arg);
void    ListCoders(void);
void	ListCiphers(void);
void    Talk(enum flow mode);
int     ReadPkt(struct packet_t * pkt, int timeout);
int     SendPkt(enum pkt_type type, UINT8 * data, int len, int timeout);
void    SyncPkt(void);
int     GetByte(int timeout);
int     XChange(enum modes mode);
int		isin(int deg);
int     VersionsCompatible(UINT8 old_major, UINT8 old_minor);
void    SendCompatible(int compatible);
int     ReceiveCompatible(int *compatible);
int     SendUpgradeInfo(UINT8 old_major, UINT8 old_minor);
int     ReceiveUpgradeInfo(UINT8 new_major, UINT8 new_minor);
int     CantNotifyBetaVersion();
int     WaitForAck();

/* Serial port function declarations */
char    *GetPort(char *arg);
#ifndef _WIN32
int     InitPort(char *arg, UINT16 speed);
int     IncomingPort(void);
int     ReadPort(int timeout);
int     WritePort(UINT8 * buf, int n);
void	ClosePort(void);
#endif

/* Audio function declarations */
int     InitAudio(int sample_rate, int frame_size, int verbose, float jbufsecs);
int     ReadAudio(UINT8 * buf, int n);
int     WriteAudio(UINT8 * buf, int n);
int     AudioFlow(enum flow direction);
void	DrainAudio(void);
void    CloseAudio(void);
void    Raw2Std(UINT8 * data, INT16 *samples, INT16 len);
void    Std2Raw(INT16 *samples, UINT8 * data, INT16 len);

int     ptt(void);
int     quit(void);
UINT16  ComputeCRC(UINT8 *data, int len);
int     SysInit(void);
long    Clock(void);
#if defined(_WIN32)
void    sleep(unsigned);
#endif
void    ShowMode(enum flow mode);
void    error(enum err_type type, char *s, ...);
void    debug_puts(char *s);
void    debug_puts_nr(char *s);
void    debug_putc(char c);
int     ReadConfigFile(char *fname);
void	VoiceLogon(char *voice_fname);
INT32	UTimer(int init);
int		AudioOverflow(void);
int		AudioUnderflow(void);
int		GetPassPhrase(char *pw, int maxlen, char *msg);
float	ComputeEntropy(UINT8 buf[], int n);

#if defined(_WIN32)
int     getopt(int, char *[], char*);
#endif

/* coder function declarations */
void	lpc10_init(void);
void	sp64_init(void);
void    sp85_init(void);
void	sp124_init(void);
void	lpc10_encode(INT16 *samples, UINT8 *bits);
void	sp64_encode(INT16 *samples, UINT8 *bits);
void    sp85_encode(INT16 *samples, UINT8 *bits);
void	sp124_encode(INT16 *samples, UINT8 *bits);
void	lpc10_decode(UINT8 *bits, INT16 *samples);
void	sp64_decode(UINT8 *bits, INT16 *samples);
void    sp85_decode(UINT8 *bits, INT16 *samples);
void	sp124_decode(UINT8 *bits, INT16 *samples);

/* encryption function declarations */
void	cfb_init(CFBCTX *context, struct key_t *key, BLOCK *iv);
void	cfb_encrypt(CFBCTX *context, UINT8 *data, int len);
void	cfb_decrypt(CFBCTX *context, UINT8 *data, int len);
int		keyinit(struct key_t *k, char *passphrase, int len, int mode);
void	keydestroy(struct key_t *k);
void	ecb_encrypt(struct key_t *k, BLOCK *data);
void	ecb_decrypt(struct key_t *k, BLOCK *data);
void	random_init(UINT8 *data, int len);
void	random_bytes(UINT8 *data, int len);
int		FindKeyExch(char *);


#define MIN(A,B) ( ( A > B ) ? ( B ) : ( A ) )
