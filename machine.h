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
 * machine.h
 *
 * SCCS ID:  @(#)machine.h 1.1 96/02/29
 */

#ifndef __MACHINE_H
#define __MACHINE_H

/* Machine/compiler definitions */
#include <stdint.h>

typedef int8_t		INT8;
typedef uint8_t		UINT8;
typedef int16_t		INT16;
typedef uint16_t	UINT16;
typedef int32_t		INT32;
typedef uint32_t	UINT32;

#if defined(unix)
#define	Strcasecmp(x, y)	    strcasecmp(x, y)
#define	Strncasecmp(x, y, z)	strncasecmp(x, y, z)
#endif

#if defined(_WIN32)
#define	Strcasecmp(x, y)	    stricmp(x, y)
#define	Strncasecmp(x, y, z)	strnicmp(x, y, z)
#endif

#endif /* __MACHINE_H */
