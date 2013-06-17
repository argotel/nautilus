/* d3des.h -
 *
 * SCCS ID:  @(#)des3.h 1.1 96/02/29
 *
 * This is the header file for the triple-DES functions used in Nautilus.
 * It is based on Richard Outerbridge's public domain implementation.
 *
 * Copyright (c) 1988,1989,1990,1991,1992 by Richard Outerbridge
 *	(GEnie : OUTER; CIS : [71755,204])
 */

#define EN0	0	/* MODE == encrypt */
#define DE1	1	/* MODE == decrypt */

typedef struct {
	unsigned long	KnL[32];
	unsigned long	KnR[32];
	unsigned long	Kn3[32];
} DES3_KEYSCHED;

void deskey(unsigned char *key, unsigned long *ks, short edf);
/*
 * Sets up the key schedule pointed to by ks according to the
 * key contained in the 8 bytes of key.  The key schedule is
 * configured for encryption/decryption according to the value
 * of the flag, edf.
 */

void Ddes(unsigned char *from, unsigned char *into, DES3_KEYSCHED *ks);
/*
 * Performs a triple-DES encryption/decryption (depends on key
 * schedule) on one block of eight bytes at address 'from' and
 * stores the result to a block of eight bytes at address 'to'.
 * 'from' and 'to' can be the same address.
 */

void DES3_Key_Setup(unsigned char keyin[], int keybytes, DES3_KEYSCHED *ks);
/*
 * Hashes data with length 'keybytes' stored at address 'keyin' into a
 * triple-DES keyschedule and stores the result at the address 'ks'.
 */

void DES3_encipher(unsigned char in[8], unsigned char out[8], DES3_KEYSCHED *ks);
/*
 * Alias for Ddes.
 */
