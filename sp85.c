/*
 * Definitions needed to turn sp1bit.c into the SP85 coder.
 *
 * SCCS ID: @(#)sp85.c 1.1 96/02/29
 */

#undef SP64
#ifndef SP85
#define SP85
#endif
#define INIT_FUNC_NAME sp85_init
#define ENCODE_FUNC_NAME sp85_encode
#define DECODE_FUNC_NAME sp85_decode
#define LOWPASS_FILTER

#include "sp1bit.c"
