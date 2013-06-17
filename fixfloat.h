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
 *  fixfloat.h -- header file that makes it easier to have a fixed point
 *                arithmetic and floating point arithmetic version of
 *                a function in the same file.
 *
 * SCCS ID:  @(#)fixfloat.h 1.1 96/02/29
 */

/* REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 95/06/25  J. A. Fingerhut    Created original version of this file
 *                              by leeching from sp64.c and sun-sp64.c.
 */


/*
 * This file creates definitions for several macros that may be used
 * in a function, and then a fixed point version of that function can
 * be made by compiling with FIXED_POINT_ARITHMETIC defined, or a
 * floating point version can be made by compiling with that symbol
 * undefined.  No further #ifdef's should be necessary in the code to
 * support this, as long as the supported macros are used where
 * necessary.
 *
 * The type value_t is typedef'd appropritely, so that any variables
 * that are fixed point or floating point should be declared with type
 * value_t.
 *
 * The supported macros are:
 *
 * Name and arguments        Description
 * ----------------------------------------------------------------------
 * audio_s2f(X)              Convert standard sample X (type INT16)
 *                           to type value_t in range [-1,+1].
 * audio_s2f(X)              Convert value_t X to standard sample.
 *                           X will be treated as if in the range
 *                           [-1,+1], with clipping if not.
 * FLOAT2VALUE(X)            Convert floating point value X to value_t.
 *                           Note: If X is not a constant floating
 *                           point value, then the definition of this
 *                           macro could cause a floating point multiplication
 *                           to be compiled into the code.  If one wants
 *                           to avoid doing any floating point operations
 *                           anywhere in the program, keep this in mind.
 * INT2VALUE(X)              Convert integer X to value_t.
 * VALUE2INT(X)              Convert value_t X to integer, with rounding.
 * MUL(X,Y)                  Multiply value_t's X and Y, producing value_t.
 * DIV(X,Y)                  Divide value_t's X and Y, producing value_t.
 * SQRT(X)                   Take square root of value_t X, producing value_t.
 *
 * Note that two value_t's may be added or subtracted with the normal
 * C operators + and -, producing a value_t result.
 */


#ifdef FIXED_POINT_ARITHMETIC

/***********************************************************************
 *                                                                     *
 *           Definitions for fixed point arithmetic                    *
 *                                                                     *
 ***********************************************************************/

/*
 * Fixed point values will be represented by 32 bit integers.  The
 * sign bit and integer part will be located in the most significant
 * 12 bits of the integer, and the fractional part will be located in
 * the least significant 20 bits.  This is controlled by the setting
 * of the macro FRACTIONAL_BITS.  Note that if one should want to
 * change this value, there are restrictions on the values it may
 * change to, and still allow this code to work.  In particular, the
 * audio_s2f and audio_f2s macros below shift values left and right by
 * (FRACTIONAL_BITS-15), so it probably wouldn't work to set it below
 * 15.  Additional restrictions are explained in the comments of
 * zmul.asm, where the relevant symbol is abbreviated to FR_BITS.
 * 
 * ZNORM represents a 1 in this fixed point representation.  Floating
 * point values may be converted to fixed point notation by
 * multiplying them by ZNORM, and truncating the result to a 32 bit
 * integer, as is done by the macro FLOAT2VALUE(X) below.  This could
 * also be done to convert integers to fixed point values, but for
 * integers it is probably more efficient to perform a left shift by
 * FRACTIONAL_BITS.
 */

typedef INT32 value_t;

#define FRACTIONAL_BITS	20
#define	ZNORM		(1L << FRACTIONAL_BITS)

INT32           zmul(INT32, INT32);
INT32           zdiv(INT32, INT32);
UINT32          zsqrt(UINT32 v);

/*
 * Convert standard samples to (s2f) and from (f2s) fixed point
 * representation.
 * 
 * To convert an integer value to a fixed point value, shift it left
 * by FRACTIONAL_BITS bits.
 * 
 * Here, though, we wish to treat the 16 bit 2's complement samples as
 * values in the range [-1,+1].  Therefore, logically we wish to
 * divide the integer by 2^15, and then shift it left by
 * FRACTIONAL_BITS bits.  This is the same as shifting left by
 * (FRACTIONAL_BITS-15) bits.
 */
#define audio_s2f(X)	((INT32) (X)) << (FRACTIONAL_BITS - 15)
#define audio_f2s(X)	(INT16) ((X) >> (FRACTIONAL_BITS - 15))

/*
 * Macro to convert a floating point value X to a fixed point
 * value.
 */
#define FLOAT2VALUE(X)	((INT32) ((X) * ZNORM))

/*
 * Macros to convert an integer to (INT2VALUE) and from (VALUE2INT) a
 * fixed point value.
 *
 * Sun Aug 27 09:42:10 CDT 1995   Andy Fingerhut (jaf@arl.wustl.edu)
 *
 * Changed the declaration of INT2VALUE from:
 * #define INT2VALUE(X)	((X) << FRACTIONAL_BITS)
 * to:
 * #define INT2VALUE(X)	(((INT32) X) << FRACTIONAL_BITS)
 *
 * This should make it safer on compilers that default to 16 bit
 * int's, where the first definition seems like it might shift all of
 * the 16 bits in X to the left by FRACTIONAL_BITS (currently 20) and
 * leave you with 0.
 */
#define INT2VALUE(X)	(((INT32) X) << FRACTIONAL_BITS)
#define VALUE2INT(X)	((X) >> FRACTIONAL_BITS)

/*
 * Macros for multiplying and dividing a pair of fixed point values,
 * and taking the square root of a fixed point value.  Note that
 * addition and subtraction of fixed point values may be done by the
 * normal addition and subtraction on 32 bit integers.
 */
#define MUL(X,Y)	zmul(X,Y)
#define DIV(X,Y)	zdiv(X,Y)
#define SQRT(X)		zsqrt(X)

#else /* FIXED_POINT_ARITHMETIC */

/***********************************************************************
 *                                                                     *
 *           Definitions for floating point arithmetic                 *
 *                                                                     *
 ***********************************************************************/

/*
 * Floating point values are represented by C floats.  The most
 * complicated macros here are the ones for converting between
 * standard samples and floats.
 */
#include <math.h>

typedef float value_t;

/*
 * Convert standard samples to (s2f) and from (f2s) floating point
 * linear representation.
 */

/*
 * Round the floating point value X to the nearest integer.
 * 
 * The default way of converting a float to an int with the C type
 * cast (int) is to truncate towards 0 (at least for the gcc compiler
 * on Linux and SunOS machines, which I tested).  The following
 * expression ensures that whether the value is positive or negative,
 * it gets rounded to the nearest integer.
 */
#define Irint(X)	((int)((X) < 0. ? (X) - 0.5 : (X) + 0.5))
/*
 * Convert a 16 bit signed value in the range [-32768, +32767] to a
 * floating point value in the range -1.0 to 1.0.
 * 
 * The weirdest thing here is the comparison ((UINT16)(X)) == 0x8000.
 * 0x8000 is the bit pattern that represents -32768 in a 16 bit 2's
 * complement number.  If X is -32768, the result is exactly -1.0,
 * otherwise the result is (X / 32767.0).  Is it not as portable to
 * compare X directly to -32768?
 */
#define audio_s2f(X)	(((UINT16)(X)) == 0x8000 ? -1. :	\
			((float)((INT16)(X))) / 32767.)
/*
 * Convert a floating point value in the range [-1.0, +1.0] to a
 * signed 16 bit integer in the range [-32767, 32767].  If the
 * floating point value is outside of [-1.0, +1.0], then force the
 * output value to be at the corresponding end of its range.
 */
#define audio_f2s(X)	((X) >= 1. ? 32767 : (X) <= -1. ? -32767 :	\
			(INT16)(Irint((X) * 32767.)))

/*
 * Converting between floating point values and floats/integers is
 * conceptually a little bit simpler than the corresponding conversion
 * for fixed point values.  Multiplication, division, and sqrt also
 * come for free with the compiler.
 */
#define FLOAT2VALUE(X)	((float) (X))

#define INT2VALUE(X)	((float) (X))
#define VALUE2INT(X)	Irint(X)

#define MUL(X,Y)	((X)*(Y))
#define DIV(X,Y)	((X)/(Y))
#define SQRT(X)		sqrt(X)

#endif  /* FIXED_POINT_ARITHMETIC */
