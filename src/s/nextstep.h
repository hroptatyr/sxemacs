/* Configuration file for the NeXTstep system.
   Copyright (C) 1990, 1995 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.31. */

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  We'll need to undo the bsd one. */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "next-mach"

#ifndef NeXT
#define NeXT
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Defining KERNEL_FILE causes lossage because sys/file.h
   stupidly gets confused by it.  */
#undef KERNEL_FILE

#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif

#define SYSTEM_MALLOC

#define environ _environ

#if 0				/* I think these are never used--let's see.  -- rms.  */
/* Mask for address bits within a memory segment */

#define SEGSIZ 0x20000
#define SEGMENT_MASK (SEGSIZ - 1)

#define HAVE_UNIX_DOMAIN

/* Conflicts in process.c between ioctl.h & tty.h use of t_foo fields */

#define NO_T_CHARS_DEFINES

/* This avoids a problem in Xos.h when using co-Xist 3.01.  */
#define X_NOT_POSIX
#endif				/* 0 */

/* Definitions for how to link.  */

/* Link this program just by running cc.  */
#define ORDINARY_LINK

#define LD_SWITCH_SYSTEM "-X -noseglinkedit"

/* Don't use -lc on the NeXT.  */
#define LIB_STANDARD "-lsys_s"

#define START_FILES "pre-crt0.o"

#define LIB_X11_LIB "-L/usr/lib/X11 -lX11"

/* We don't have a g library either, so override the -lg LIBS_DEBUG switch */

#define LIBS_DEBUG

/* We don't have a libgcc.a, so we can't let LIB_GCC default to -lgcc */

#define LIB_GCC

/* Definitions for how to dump.  */
/*
 * everythign is pdump now. --SY
 * #define UNEXEC "unexnext.o"
 */
#undef UNEXEC

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START 0
/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext ()
/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata ()
/* XEmacs change from Barry Warsaw. */
#ifndef NOT_C_CODE
/* this is only typedef'd in types.h if _POSIX_SOURCE is defined
 * but the problem with that is that compiling with -posix links
 * in -lposix instead of -lsys_s, and the latter defines some
 * important NeXT AppKit symbols.
 */
typedef unsigned short mode_t;
#endif				/* ! NOT_C_CODE */
#ifdef hppa
/* The following are glommed from the hp9000s800.h file */
#define STACK_DIRECTION 1
#endif

#undef SYSV_SYSTEM_DIR
#undef NONSYSTEM_DIR_LIBRARY
#define signal_handler_t int
#define pid_t int
#undef BSD_TERMIOS
#undef HAVE_TERMIOS
#undef HAVE_TERMIO
#define TAB3 XTABS
#define C_OPTIMIZE_SWITCH "-pipe"
#undef HAVE_SETITIMER

/* XEmacs addition from Axel Seibert */
#ifndef NOT_C_CODE
#include <sys/types.h>
#endif
