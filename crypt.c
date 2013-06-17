/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/*
 * crypt.c, written by P. Rubin and W. Dorsey
 *
 * SCCS ID: @(#)crypt.c 1.1 96/02/29
 */

#include <memory.h>
#include <malloc.h>

#include "nautilus.h"
#include "blowfish.h"
#include "des3.h"

void Blowfish_Key_Setup(UINT8 keyin[], short keylen, BLOWFISH_KEYSCHED *key);
void Blowfish_encipher(UINT32 *xl, UINT32 *xr, BLOWFISH_KEYSCHED *key);
void Idea_Key_Setup(UINT8 keyin[], int keybytes, UINT16 *key);
void Idea_encipher(UINT8 in[8], UINT8 out[8], UINT16 *key);

/*
 * Encrypt a buffer of data in CFB using cipher specified in
 * the context structure.  Thanks go to Colin Plumb for his Idea
 * CFB function which this and cfb_decrypt() are based on.
 */
void
cfb_encrypt(CFBCTX *context, UINT8 *data, int count)
{
    int bufleft = context->bufleft;
    UINT8 *bufptr = ((UINT8 *) &context->iv) + 8 - bufleft;

    /*
     * If there are no more bytes to encrypt than there are bytes
     * in the buffer, XOR them in and return.
     */
    if (count <= bufleft) {
    	context->bufleft = bufleft - count;
    	while (count--) {
    	    *bufptr ^= *data;
    	    *data++ = *bufptr++;
	}
	return;
    }

    /*
     * Encrypt the first bufleft (0 to 7) bytes of the input by
     * XORing with the last bufleft bytes in the iv buffer.
     */
    count -= bufleft;
    while (bufleft--) {
    	*bufptr ^= *data;
    	*data++ = *bufptr++;
    }

    /*
     * Encrypt middle blocks of the input by cranking the cipher,
     * XORing 8-byte blocks, and repeating until the count is
     * 8 or less.
     */
    while (count > 8) {
    	ecb_encrypt(context->key, &context->iv);
    	bufptr = (UINT8 *) &context->iv;
    	bufleft = 8;
    	count -= 8;
    	do {
    	    *bufptr ^= *data;
    	    *data++ = *bufptr++;
    	} while (--bufleft);
    }

    /*
     * Do the last 1 to 8 bytes.
     */
    ecb_encrypt(context->key, &context->iv);
    bufptr = (UINT8 *) &context->iv;
    context->bufleft = 8 - count;
    do {
    	*bufptr ^= *data;
    	*data++ = *bufptr++;
    } while (--count);
}

/*
 * Decrypt a buffer of data in CFB mode using cipher specified
 * in the context structure.
 */
void
cfb_decrypt(CFBCTX *context, UINT8 *data, int count)
{
    int bufleft = context->bufleft;
    UINT8 *bufptr = ((UINT8 *) &context->iv) + 8 - bufleft;
    UINT8 t;

    if (count <= bufleft) {
        context->bufleft = bufleft - count;
        while (count--) {
            t = *bufptr;
            *bufptr = *data;
            *data++ = t ^ *bufptr++;
        }
        return;
    }

    count -= bufleft;
    while (bufleft--) {
    	t = *bufptr;
    	*bufptr = *data;
    	*data++ = t ^ *bufptr++;
    }

    while (count > 8) {
    	ecb_encrypt(context->key, &context->iv);
    	bufptr = (UINT8 *) &context->iv;
    	bufleft = 8;
    	count -= 8;
    	do {
    	    t = *bufptr;
    	    *bufptr = *data;
    	    *data++ = t ^ *bufptr++;
    	} while (--bufleft);
    }

    ecb_encrypt(context->key, &context->iv);
    bufptr = (UINT8 *) &context->iv;
    context->bufleft = 8 - count;
    do {
    	t = *bufptr;
    	*bufptr = *data;
    	*data++ = t ^ *bufptr++;
    } while (--count);
}

static void *
xmalloc (int nbytes)
{
  void *p = (void *) malloc (nbytes);
  if (p)
    return p;
  error (MSG_FATAL, "out of memory");
  /* NOTREACHED */
  return(NULL);
}

void
cfb_init(CFBCTX *context, struct key_t *key, BLOCK *iv)
{
    context->bufleft = 0;
    if (iv)
    	context->iv = *iv;
    else
        memset(&context->iv, 0, sizeof(BLOCK));
    context->key = key;
}

/*
 * Return size in bytes of user key used by K's cipher
 */
int
user_key_size (struct key_t *k)
{
  switch (k->type) {
  case BLOWFISH:
    return 18 * 4;
  case IDEA:
    return 16;
  case DES3:
    return 24;
  default:
    error (MSG_FATAL, "illegal cipher");
  }
  /* NOTREACHED */
  return(0);
}

/*
 * allocate an expanded key structure based
 * on the key type.  Assume previous contents
 * of key structure were garbage.
 */
void
keycons (struct key_t *k, enum crypto_type type)
{
  switch (type) {
  case BLOWFISH:
    k->key_len = sizeof (BLOWFISH_KEYSCHED);
    break;
  case IDEA:
    k->key_len = 104;
    break;
  case DES3:
    k->key_len = 384;
    break;
  default:
    error (MSG_FATAL, "keycons: unknown cipher type");
  }
  k->type = type;
  k->key = xmalloc (k->key_len);
}

/*
 * hash a passphrase into an encryption key of length OUTPUTLEN.
 * PASSPHRASE buffer is not modified.  Output
 * buffer OUTPUT is overwritten and must be at least
 * OUTPUTLEN bytes long. 
 */
void
keycrunch (UINT8 *passphrase, int passlen, UINT8 *output, int outputlen, int mode)
{
  BLOCK iv;
  int i, a;
  struct key_t temp_key;
  CFBCTX cfb_context;

  if (passlen == 0)
    error (MSG_FATAL, "empty pass phrase");

  keycons (&temp_key, BLOWFISH);
  Blowfish_Key_Setup (passphrase, (short) passlen, temp_key.key);

  /*
   * Make the contents of the output buffer depend on the mode and
   * the passphrase length (so that "x" and "xx" don't hash to the
   * same key).
   */
  a = mode ? 181 : 69;
  output[0] = passlen;
  for (i=1; i<outputlen; i++)
    output[i] = (a * output[i-1] + 1) & 0xff;

  /* Make the IV depend on the passphrase too. */
  memcpy (&iv, "Nautilus", 8);
  ecb_encrypt (&temp_key, &iv);

  cfb_init (&cfb_context, &temp_key, &iv);
  cfb_encrypt (&cfb_context, output, outputlen);
 
  keydestroy (&temp_key);
}

/*
 * Set up a key schedule from given passphrase.
 * Doesn't change contents of passphrase.
 */
int
keyinit(struct key_t *k, char *passphrase, int len, int mode)
{
  int usize;
  UINT8 *keybuf;
  int status = 0;

  keycons (k, k->type);
  usize = user_key_size (k);
  keybuf = (UINT8 *) xmalloc (usize);
  keycrunch (passphrase, len, keybuf, usize, mode);

  switch(k->type) {
  case BLOWFISH:
    Blowfish_Key_Setup(keybuf, (short) usize, k->key);
    break;
  case IDEA:
    Idea_Key_Setup(keybuf, usize, k->key);
    break;
  case DES3:
    DES3_Key_Setup(keybuf, usize, k->key);
    break;
  default:
    status = -1;
    break;
  }
  memset (keybuf, 0, usize);	/* burn and free keybuf */
  free (keybuf);

  return status;
}

void
keydestroy(struct key_t *k)
{
  if (k->key_len) {
    /* burn old key schedule before freeing the storage */
    memset (k->key, 0, k->key_len);
    free (k->key);
    k->key_len = 0;
  }
}

static struct {
  UINT32 counter0, counter1;
  struct key_t key;
} random_state;

/* fill DATA with LEN random bytes */
void
random_bytes(UINT8 *data, int len)
{
  BLOCK b;

  while (len > 0) {
    if (++random_state.counter0 == 0)
      ++random_state.counter1;
    memcpy (&b, &random_state.counter0, 8);
    ecb_encrypt (&random_state.key, &b);
    memcpy (data, &b, len < 8 ? len : 8);
    data += 8;
    len -= 8;
  }
}

/* initialize random number generator from LEN
   bytes of semi-random data in DATA */
void
random_init (UINT8 *data, int len)
{
  keydestroy (&random_state.key);
  random_state.key.type = BLOWFISH;
  keyinit(&random_state.key, data, len, 0);
  /* don't leave random initialization state around */
  memset (data, '\0', len);
  /* select a random starting point in the count sequence
     by encrypting the old count.  This is probably 0 or
     something predictable, but if it happens to be
     something different, we may as well use the extra
     entropy so we don't bother initializing it. */
  ecb_encrypt (&random_state.key, (BLOCK *) &random_state.counter0);
}

#define byte_swap_block(data) 					 \
{								 \
  union aword temp;						 \
  char *p = (char *) &data->left;				 \
								 \
  temp.w.byte0 = *p++;						 \
  temp.w.byte1 = *p++;						 \
  temp.w.byte2 = *p++;						 \
  temp.w.byte3 = *p++;						 \
  data->left = temp.word;					 \
								 \
  p = (char *) &data->right;					 \
  temp.w.byte0 = *p++;						 \
  temp.w.byte1 = *p++;						 \
  temp.w.byte2 = *p++;						 \
  temp.w.byte3 = *p++;						 \
  data->right = temp.word;					 \
}

void
ecb_encrypt (struct key_t *k, BLOCK *data)
{
  switch (k->type) {

  case BLOWFISH:
    if (k->key_len != sizeof (BLOWFISH_KEYSCHED))
      error(MSG_FATAL, "bad blowfish key schedule");
#ifdef ORDER_DCBA
    byte_swap_block (data);
#endif
    Blowfish_encipher (&data->left, &data->right, k->key);
#ifdef ORDER_DCBA
    byte_swap_block (data);
#endif
    break;

  case IDEA:
    if (k->key_len != 104)
      error(MSG_FATAL, "bad IDEA key schedule");
    Idea_encipher((UINT8 *) data, (UINT8 *) data, k->key);
    break;

  case DES3:
    if (k->key_len != 384)
      error(MSG_FATAL, "bad 3DES key schedule");
    DES3_encipher((UINT8 *) data, (UINT8 *) data, k->key);
    break;

  default:
    error(MSG_FATAL, "unknown encryption type");
    /* NOTREACHED */
  }
}
