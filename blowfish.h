/*********************blowfish.h********************/

/* SCCS ID:  @(#)blowfish.h 1.1 96/02/29 */
/* $Id: blowfish.h,v 1.2 2001-01-25 23:45:49 jpoehlmann Exp $*/

#define MAXKEYBYTES 56		/* 448 bits */
#define bf_N             16
#define noErr            0
#define DATAERROR         -1
#define KEYBYTES         8
#define subkeyfilename   "Blowfish.dat"

/* choose a byte order for your hardware */
/* ABCD - big endian - motorola, sparc */
#ifdef ORDER_ABCD
union aword {
  UINT32 word;
  UINT8 byte [4];
  struct {
    unsigned int byte0:8;
    unsigned int byte1:8;
    unsigned int byte2:8;
    unsigned int byte3:8;
  } w;
};
#endif	/* ORDER_ABCD */

/* DCBA - little endian - intel */
#ifdef ORDER_DCBA
union aword {
  UINT32 word;
  UINT8 byte [4];
  struct {
    unsigned int byte3:8;
    unsigned int byte2:8;
    unsigned int byte1:8;
    unsigned int byte0:8;
  } w;
};
#endif	/* ORDER_DCBA */

/* BADC - vax */
#ifdef ORDER_BADC
union aword {
  UINT32 word;
  UINT8 byte [4];
  struct {
    unsigned int byte1:8;
    unsigned int byte0:8;
    unsigned int byte3:8;
    unsigned int byte2:8;
  } w;
};
#endif	/* ORDER_BADC */


short opensubkeyfile(void);
unsigned long F(unsigned long x);

typedef struct {
  UINT32 P[bf_N + 2];
  UINT32 S[4][256];
} BLOWFISH_KEYSCHED;

void Blowfish_encipher(UINT32 *xl, UINT32 *xr,
		       BLOWFISH_KEYSCHED *keytab);
void Blowfish_decipher(UINT32 *xl, UINT32 *xr,
		       BLOWFISH_KEYSCHED *keytab);
short InitializeBlowfish(unsigned char key[], short keybytes,
		       BLOWFISH_KEYSCHED *keytab);
