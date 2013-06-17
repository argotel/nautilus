/* idea.c - C source code for IDEA block cipher.
 *
 * SCCS ID: @(#)idea.c 1.1 96/02/29
 *
 *      IDEA (International Data Encryption Algorithm), formerly known as 
 *      IPES (Improved Proposed Encryption Standard).
 *      Algorithm developed by Xuejia Lai and James L. Massey, of ETH Zurich.
 *      This implementation modified and derived from original C code 
 *      developed by Xuejia Lai.  
 *      Zero-based indexing added, names changed from IPES to IDEA.
 *      CFB functions added.  Random number routines added.
 *
 *      Extensively optimized and restructured by Colin Plumb.
 *
 *      Hacked and gutted to work in Nautilus by Bill Dorsey.
 *
 *      There are two adjustments that can be made to this code to
 *      speed it up.  Defaults may be used for PCs.  Only the -DIDEA32
 *      pays off significantly if selectively set or not set.
 *      Experiment to see what works best for your machine.
 *
 *      Multiplication: default is inline, -DAVOID_JUMPS uses a
 *              different version that does not do any conditional
 *              jumps (a few percent worse on a SPARC), while
 *              -DSMALL_CACHE takes it out of line to stay
 *              within a small on-chip code cache.
 *      Variables: normally, 16-bit variables are used, but some
 *              machines (notably RISCs) do not have 16-bit registers,
 *              so they do a great deal of masking.  -DIDEA32 uses "int"
 *              register variables and masks explicitly only where
 *              necessary.  On a SPARC, for example, this boosts
 *              performace by 30%.
 *
 *      The IDEA(tm) block cipher is covered by patents held by ETH and a
 *      Swiss company called Ascom-Tech AG.  The Swiss patent number is
 *      PCT/CH91/00117, the European patent number is EP 0 482 154 B1, and
 *      the U.S. patent number is US005214703.  IDEA(tm) is a trademark of
 *      Ascom-Tech AG.  There is no license fee required for noncommercial
 *      use.  Commercial users may obtain licensing details from Dieter
 *      Profos, Ascom Tech AG, Solothurn Lab, Postfach 151, 4502 Solothurn,
 *      Switzerland, Tel +41 65 242885, Fax +41 65 235761.
 *
 *      The IDEA block cipher uses a 64-bit block size, and a 128-bit key 
 *      size.  It breaks the 64-bit cipher block into four 16-bit words
 *      because all of the primitive inner operations are done with 16-bit 
 *      arithmetic.  It likewise breaks the 128-bit cipher key into eight 
 *      16-bit words.
 *
 *      For further information on the IDEA cipher, see the book:
 *        Xuejia Lai, "On the Design and Security of Block Ciphers",
 *        ETH Series on Information Processing (ed. J.L. Massey) Vol 1,
 *        Hartung-Gorre Verlag, Konstanz, Switzerland, 1992.  ISBN
 *        3-89191-573-X.
 *
 *      This code runs on arrays of bytes by taking pairs in big-endian
 *      order to make the 16-bit words that IDEA uses internally.  This
 *      produces the same result regardless of the byte order of the
 *      native CPU.
 */

typedef unsigned char byte;
typedef unsigned short word16;
typedef unsigned long word32;

#define IDEAKEYSIZE 16
#define IDEABLOCKSIZE 8

#define IDEAROUNDS 8
#define IDEAKEYLEN (6*IDEAROUNDS+4)

#ifdef IDEA32			/* Use >16-bit temporaries */
#define low16(x) ((x) & 0xFFFF)
typedef unsigned int uint16;	/* at LEAST 16 bits, maybe more */
#else
#define low16(x) (x)		/* this is only ever applied to uint16's */
typedef word16 uint16;
#endif

#ifdef _GNUC_
/* __const__ simply means there are no side effects for this function,
 * which is useful info for the gcc optimizer
 */
#define CONST __const__
#else
#define CONST
#endif

#ifdef _WIN32
#pragma warning(disable:4244)
#endif

/*
 * Multiplication, modulo (2**16)+1
 * Note that this code is structured on the assumption that
 * untaken branches are cheaper than taken branches, and the
 * compiler doesn't schedule branches.
 */
#ifdef SMALL_CACHE
CONST static uint16
 mul(register uint16 a, register uint16 b)
{
    register word32 p;

    p = (word32) a *b;
    if (p) {
	b = low16(p);
	a = p >> 16;
	return (b - a) + (b < a);
    } else if (a) {
	return 1 - b;
    } else {
	return 1 - a;
    }
}				/* mul */
#endif				/* SMALL_CACHE */

/*
 * MUL(x,y) computes x = x*y, modulo 0x10001.  Requires two temps, 
 * t16 and t32.  x is modified, and must me a side-effect-free lvalue.
 * y may be anything, but unlike x, must be strictly 16 bits even if
 * low16() is #defined.
 * All of these are equivalent - see which is faster on your machine
 */
#ifdef SMALL_CACHE
#define MUL(x,y) (x = mul(low16(x),y))
#else				/* !SMALL_CACHE */
#ifdef AVOID_JUMPS
#define MUL(x,y) (x = low16(x-1), t16 = low16((y)-1), \
		t32 = (word32)x*t16 + x + t16 + 1, x = low16(t32), \
		t16 = t32>>16, x = (x-t16) + (x<t16) )
#else				/* !AVOID_JUMPS (default) */
#define MUL(x,y) \
	((t16 = (y)) ? \
		(x=low16(x)) ? \
			t32 = (word32)x*t16, \
			x = low16(t32), \
			t16 = t32>>16, \
			x = (x-t16)+(x<t16) \
		: \
			(x = 1-t16) \
	: \
		(x = 1-x))
#endif
#endif

/*
 * Expand a 128-bit user key to a working encryption key EK
 */
static void
ideaExpandKey(byte const *userkey, word16 * EK)
{
    int i, j;

    for (j = 0; j < 8; j++) {
	EK[j] = (userkey[0] << 8) + userkey[1];
	userkey += 2;
    }
    for (i = 0; j < IDEAKEYLEN; j++) {
	i++;
	EK[i + 7] = EK[i & 7] << 9 | EK[(i + 1) & 7] >> 7;
	EK += i & 8;
	i &= 7;
    }
}				/* ideaExpandKey */

/*
 * Expand an arbitrary amount of key data to a working
 * encryption key.
 */
void
Idea_Key_Setup(byte keyin[], int keylen, word16 *key)
{
    ideaExpandKey(keyin, key);
}

/*
 * IDEA encryption/decryption algorithm
 * Note that in and out can be the same buffer
 */
void
Idea_encipher(byte const inbuf[8], byte outbuf[8],
		       word16 const *key)
{
    register uint16 x1, x2, x3, x4, s2, s3;
    word16 *in, *out;
#ifndef SMALL_CACHE
    register uint16 t16;	/* Temporaries needed by MUL macro */
    register word32 t32;
#endif
    int r = IDEAROUNDS;

    in = (word16 *) inbuf;
    x1 = *in++;
    x2 = *in++;
    x3 = *in++;
    x4 = *in;
#ifndef ORDER_ABCD
    x1 = (x1 >> 8) | (x1 << 8);
    x2 = (x2 >> 8) | (x2 << 8);
    x3 = (x3 >> 8) | (x3 << 8);
    x4 = (x4 >> 8) | (x4 << 8);
#endif
    do {
	MUL(x1, *key++);
	x2 += *key++;
	x3 += *key++;
	MUL(x4, *key++);

	s3 = x3;
	x3 ^= x1;
	MUL(x3, *key++);
	s2 = x2;
	x2 ^= x4;
	x2 += x3;
	MUL(x2, *key++);
	x3 += x2;

	x1 ^= x2;
	x4 ^= x3;

	x2 ^= s3;
	x3 ^= s2;
    } while (--r);
    MUL(x1, *key++);
    x3 += *key++;
    x2 += *key++;
    MUL(x4, *key);

    out = (word16 *) outbuf;
#ifdef ORDER_ABCD
    *out++ = x1;
    *out++ = x3;
    *out++ = x2;
    *out = x4;
#else				/* !ORDER_ABCD */
    x1 = low16(x1);
    x2 = low16(x2);
    x3 = low16(x3);
    x4 = low16(x4);
    *out++ = (x1 >> 8) | (x1 << 8);
    *out++ = (x3 >> 8) | (x3 << 8);
    *out++ = (x2 >> 8) | (x2 << 8);
    *out = (x4 >> 8) | (x4 << 8);
#endif
}				/* ideaCipher */
