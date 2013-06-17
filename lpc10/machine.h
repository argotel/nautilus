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

typedef char		    INT8;
typedef unsigned char	UINT8;
typedef short		    INT16;
typedef unsigned short	UINT16;
typedef int		        INT32;
typedef unsigned	    UINT32;

#if defined(unix)
#define	Strcasecmp(x, y)	    strcasecmp(x, y)
#define	Strncasecmp(x, y, z)	strncasecmp(x, y, z)
#endif

#if defined(_WIN32)
#define	Strcasecmp(x, y)	    stricmp(x, y)
#define	Strncasecmp(x, y, z)	strnicmp(x, y, z)
#endif

#endif /* __MACHINE_H */
