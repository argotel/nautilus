/*
 * Low level Blowfish encryption code.  Adapted and optimized
 * from Bruce Schneier's version by Paul Rubin, April 1995.
 */

/*********************blowfish.c*********************/

/* SCCS ID: @(#)naut_bf.c 1.1 96/02/29 */
/* TODO: test with zero length key */
/* TODO: test with a through z as key and plain text */
/* TODO: make this byte order independent */

#include <stdio.h>			/* used for debugging */
#ifdef _WIN32
#include <memory.h>
#endif
#ifdef UNIX	
#include <string.h>
#endif
#include "nautilus.h"		/* get nautilus datatypes (UINT32, etc.) */

#include "blowfish.h"
#include "naut_bf.h"		/* P-box P-array, S-box  */

#define bf_P (key->P)
#define bf_S (key->S)

#define S(x,i) (bf_S[i][x.w.byte##i])
#define bf_F(x) (((S(x,0) + S(x,1)) ^ S(x,2)) + S(x,3))
#define ROUND(a,b,n) (a.word ^= bf_F(b) ^ bf_P[n])

void
Blowfish_encipher(UINT32 *xl, UINT32 *xr, BLOWFISH_KEYSCHED *key)
{
  union aword  Xl;
  union aword  Xr;

  Xl.word = *xl;
  Xr.word = *xr;

  Xl.word ^= bf_P[0];
  ROUND (Xr, Xl, 1);  ROUND (Xl, Xr, 2);
  ROUND (Xr, Xl, 3);  ROUND (Xl, Xr, 4);
  ROUND (Xr, Xl, 5);  ROUND (Xl, Xr, 6);
  ROUND (Xr, Xl, 7);  ROUND (Xl, Xr, 8);
  ROUND (Xr, Xl, 9);  ROUND (Xl, Xr, 10);
  ROUND (Xr, Xl, 11); ROUND (Xl, Xr, 12);
  ROUND (Xr, Xl, 13); ROUND (Xl, Xr, 14);
  ROUND (Xr, Xl, 15); ROUND (Xl, Xr, 16);
  Xr.word ^= bf_P[17];

  *xr = Xl.word;
  *xl = Xr.word;
}
/* nautilus uses CFB mode so save some code space by omitting
   the decrypt routine */
#if 0
void
Blowfish_decipher(UINT32 *xl, UINT32 *xr, BLOWFISH_KEYSCHED *key)
{
   union aword  Xl;
   union aword  Xr;

   Xl.word = *xl;
   Xr.word = *xr;

   Xl.word ^= bf_P[17];
   ROUND (Xr, Xl, 16);  ROUND (Xl, Xr, 15);
   ROUND (Xr, Xl, 14);  ROUND (Xl, Xr, 13);
   ROUND (Xr, Xl, 12);  ROUND (Xl, Xr, 11);
   ROUND (Xr, Xl, 10);  ROUND (Xl, Xr, 9);
   ROUND (Xr, Xl, 8);   ROUND (Xl, Xr, 7);
   ROUND (Xr, Xl, 6);   ROUND (Xl, Xr, 5);
   ROUND (Xr, Xl, 4);   ROUND (Xl, Xr, 3);
   ROUND (Xr, Xl, 2);   ROUND (Xl, Xr, 1);
   Xr.word ^= bf_P[0];

   *xl = Xr.word;
   *xr = Xl.word;
}
#endif

void
Blowfish_Key_Setup (UINT8 key[], short keybytes, BLOWFISH_KEYSCHED *keytab)
{
  extern BLOWFISH_KEYSCHED Blowfish_Init_Key;

  memcpy (keytab, &Blowfish_Init_Key, sizeof (Blowfish_Init_Key));
  /* Call InitializeBlowfish to stir up the P and S tables */
  InitializeBlowfish (key, keybytes, keytab);
  /* now call it again to stir them up some more */
  InitializeBlowfish (key, keybytes, keytab);
}

short
InitializeBlowfish(UINT8 key[], short keybytes, BLOWFISH_KEYSCHED *keytab)
{
  short          i;		/* FIXME: unsigned int, char? */
  short          j;		/* FIXME: unsigned int, char? */
  UINT32  data;
  UINT32  datal;
  UINT32  datar;
  union aword temp;

#ifdef DEBUG_BLOWFISH
  fprintf (stderr, "0x%x 0x%x ", bf_P[0], bf_P[1]);
  fprintf (stderr, "%d %d\n", bf_P[0], bf_P[1]);
#endif

  j = 0;
  for (i = 0; i < bf_N + 2; ++i) {
    temp.word = 0;
    temp.w.byte0 = key[j];
    temp.w.byte1 = key[(j+1)%keybytes];
    temp.w.byte2 = key[(j+2)%keybytes];
    temp.w.byte3 = key[(j+3)%keybytes];
    data = temp.word;
    keytab->P[i] = keytab->P[i] ^ data;
    j = (j + 4) % keybytes;
  }

  datal = 0x00000000;
  datar = 0x00000000;

  for (i = 0; i < bf_N + 2; i += 2) {
    Blowfish_encipher(&datal, &datar, keytab);

    keytab->P[i] = datal;
    keytab->P[i + 1] = datar;
  }

  for (i = 0; i < 4; ++i) {
    for (j = 0; j < 256; j += 2) {

      Blowfish_encipher(&datal, &datar, keytab);
   
      keytab->S[i][j] = datal;
      keytab->S[i][j + 1] = datar;
    }
  }
  return 0;
}
