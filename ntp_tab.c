/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1996 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* ntp_tab.c
 *
 * SCCS ID:  @(#)ntp_tab.c 1.1 96/05/12
 */

#include "ntp.h"

#ifndef NTP_CLASSES
#error -DNTP_CLASSES=X(ntp_a)X(ntp_b)... expected in compile command
#endif

#define X(x)	extern NTP_CLASS x;
NTP_CLASSES
#undef X

#define X(x)	&x,
NTP_CLASS *ntp_tab[] = {NTP_CLASSES (NTP_CLASS *)0};
#undef X
