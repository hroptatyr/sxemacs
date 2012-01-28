/* Debugging aids -- togglable assertions.
   Copyright (C) 1994 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not in FSF. */

/* Written by Chuck Thompson */

#ifndef INCLUDED_debug_h_
#define INCLUDED_debug_h_

#define DEBUG_STDERR	1
#define DEBUG_ABORT	2

#ifdef DEBUG_SXEMACS

#include <stdio.h>

struct debug_classes {
	unsigned int redisplay:1;
	unsigned int buffers:1;
	unsigned int extents:1;
	unsigned int faces:1;
	unsigned int windows:1;
	unsigned int frames:1;
	unsigned int devices:1;
	unsigned int byte_code:1;

	unsigned int types_of_redisplay;
	unsigned int types_of_buffers;
	unsigned int types_of_extents;
	unsigned int types_of_faces;
	unsigned int types_of_windows;
	unsigned int types_of_frames;
	unsigned int types_of_devices;
	unsigned int types_of_byte_code;
};

extern struct debug_classes active_debug_classes;

#define DASSERT(class, desired_type, action, assertion) do		\
{									\
  if (active_debug_classes.##class					\
      && (active_debug_classes.types_of_##class & desired_type))	\
    {									\
      if (! (assertion))						\
	{								\
	  if (action == DEBUG_STDERR)					\
	    stderr_out ("Assertion failed in %s at line %d\n",		\
			__FILE__, __LINE__);				\
	  else								\
	    abort ();							\
	}								\
    }									\
} while (0)
#else				/* !DEBUG_SXEMACS */

#define DASSERT(class, desired_type, action, assertion)	((void) 0)

#endif				/* !DEBUG_SXEMACS */

#endif				/* INCLUDED_debug_h_ */
