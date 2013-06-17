/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* bitstrm.c
 *
 * SCCS ID: @(#)bitstrm.c 1.1 96/02/29
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * ??/??/??  B. Dorsey          Functions written to use for one of the
 *                              switched prediction speech coders that
 *                              used 2 bits per audio sample.
 * 95/06/??  J. A. Fingerhut    Split apart from the speech coder source
 *                              file into a separate file.  A few comments
 *                              were added, but the functions haven't
 *                              changed.
 */

#include "machine.h"

/*

   FUNCTION: PutBits

   PURPOSE:

   Allow an array of bytes to be written into, several bits at a time,
   instead of whole bytes at a time.

   If buf != NULL, PutBits records this pointer in a static local
   variable, and initializes its other local state as well.

   If buf == NULL, then the nbits least significant bits of the first
   argument are put into the buffer.  The bytes of the buffer are
   filled from most significant bit to least significant.

   ARGUMENTS:

   int bits

   The bits to put are the nbits least significant bits of this
   argument.

   int nbits

   The number of bits to put from the argument bits into buf.
   Obviously, this should be at most the number of bits in an int.

   UINT8 *buf

   A pointer to the byte array to put bits into, when initializing
   this function, or NULL when actually placing bits into it.

   RETURN VALUE: None

   SIDE EFFECTS:

   The whole point of calling this function is to cause the side
   effect of modifying the byte array pointed to by buf.  This
   modification happens whenever the total number of bits that have
   been put reaches or exceeds the next multiple of 8.

   Note: This side effect will NOT occur until a multiple of 8 bits
   has been put.  Any remainder bits are stored in the static local
   variables of this function, but they are not written anywhere else.
   There is currently no "flush" function to force them to be written
   out.  The only way to do so is to make calls to PutBits with a
   total number of bits that is a multiple of 8.

*/

void
PutBits(int bits, int nbits, UINT8 *buf)
{
	int	i;
	UINT8			imask;
	static UINT8		data;
	static UINT8		mask;
	static UINT8		*ptr;

	if (buf) {
		data = 0;
		mask = 0x80;
		ptr = buf;
	}
	else {
		imask = 1<<(nbits-1);
		for (i=0; i<nbits; i++) {
			if (bits & imask) {
				data |= mask;
			}
			imask >>= 1;
			mask >>= 1;
			if (mask == 0) {
				*(ptr++) = data;
				data = 0;
				mask = 0x80;
			}
		}
	}
}



/*

   FUNCTION: GetBits

   PURPOSE:

   Take an array of bytes and allow it to be read from as a "bit
   stream", similar to the way that the normal file functions allow a
   file to be read as a byte stream.

   If buf != NULL, then simply initialize the internal state of this
   function that is stored in static local variables.  If buf == NULL,
   then return the next nbits bits from the sequence of bytes.  The
   most significant bits of the next byte are returned first.

   ARGUMENTS:

   int nbits

   The number of bits to return.  This should be at least 0, but no
   more than the number of bits in the int return value (thus the
   maximum is platform dependent).

   UINT8 *buf
   
   A pointer to the first byte to read from in future calls to this
   function.

   RETURN VALUE:
   
   The return value is undefined if buf != NULL.  If buf == NULL, then
   the least significant nbits bits of the return value are equal to
   the next nbits bits from the bit stream.

   SIDE EFFECTS:

   The only side effects are that every call causes an update of the
   static local variables.  The contents of the array pointed to by
   buf are never changed.

   HISTORY:

   07/26/97    David Miller    Fixed uninitialized variable warning (not bug)

*/

int
GetBits(int nbits, UINT8 *buf)
{
	static UINT8		mask;
	static UINT8		*ptr;

	if (buf) {
		mask = 0x80;
		ptr = buf;
		return 0;
	}
	else {
		int i, bits = 0;

		for (i=0; i<nbits; i++) {
			bits <<= 1;
			bits |= (*ptr & mask) ? 1 : 0;
			mask >>= 1;
			if (mask == 0) {
				mask = 0x80;
				ptr++;
			}
		}
		return bits;
	}
}
