/*
 * Definitions needed to turn sp1bit.c into the SP64 coder.
 *
 * SCCS ID: @(#)sp64.c 1.1 96/02/29
 */

#ifndef SP64
#define SP64
#endif
#undef SP85
#define INIT_FUNC_NAME sp64_init
#define ENCODE_FUNC_NAME sp64_encode
#define DECODE_FUNC_NAME sp64_decode

#include "sp1bit.c"
