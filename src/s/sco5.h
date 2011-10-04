/* System description file for SCO OpenServer Release 5
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

/* Changes for SCO OpenServer 5 by Robert Lipe, robertl@dgii.com
 * Additions, enhancements by J. Kean Johnston, hug@netcom.com
 */

#define DONT_DEFINE_NO_REMAP	/* `static' hack not needed */

/* JKJ: Yes its a lot like SCO 4, but different enough that its easier to
 * base it on stock SVR3 and add our stuff
 */
#include "usg5-3.h"
#define SCO_R4
#define SCO_R5

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "SCO 3.2v5"

/* SCO has ptys, but with weird names */
#define HAVE_PTYS
#define PTY_ITERATION \
	for (i = 0; ; i++)
#define PTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/ptyp%d", i);		\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

#define PTY_TTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/ttyp%d", i);		\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

#define FORCE_ALLOCATE_PTY_THE_OLD_FASHIONED_WAY

/* We have sockets. Always. */
#ifndef HAVE_SOCKETS
#define HAVE_SOCKETS
#endif

#ifndef __GNUC__
#define LINKER "cc -Xc"
#else
#define LINKER "gcc"
#endif

#define LIBS_SYSTEM "-lsocket -lPW -lgen -lcrypt -lmalloc"

#ifndef MAXPATHLEN
# define MAXPATHLEN PATHSIZE
#endif

/* This is necessary to circumvent stupidity in <X11/Xosdefs.h>.  That
file checks a manifest that is only defined by xmkmf.  Alternately, we
could #define sco and I think everything would work. rjl */
#define ANSICPP 1

#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif

#undef ADDR_CORRECT
#define ADDR_CORRECT(x) (int)((char *)(x) - (char*)0)

#define C_SWITCH_SYSTEM "-D_NO_STATIC"

#ifndef __GNUC__
#define C_OPTIMIZE_SWITCH "-O3 -Xc"
#define C_DEBUG_SWITCH "-g -Xc"
#else
#define C_OPTIMIZE_SWITCH "-O99 -m486 -fomit-frame-pointer"
#define C_DEBUG_SWITCH "-g"
#endif

/* configure can't get this right linking fails unless -lsocket is used.  */
#undef HAVE_XSCREENNUMBEROFSCREEN
#define HAVE_XSCREENNUMBEROFSCREEN

/* We don't have -loldX, and we don't need it.  */
#define LIB_XMENU_LIB

/* SCO does have TIOCGWINSZ.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_PTEM_H

#ifndef __GNUC__
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/values-Xc.o"
#else
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o"
#endif
#define LIB_STANDARD "-lc"

/* Specify program for etc/fakemail to run.  Define SMAIL if you are
   using smail, don't for MMDF.  */

#ifdef SMAIL
#define MAIL_PROGRAM_NAME "/usr/bin/smail -q0"
#else
#define MAIL_PROGRAM_NAME "/usr/lib/mail/execmail"
#endif

/* Tell process_send_signal to use VSUSP instead of VSWTCH.  */
#define PREFER_VSUSP

#define POSIX_SIGNALS

#undef PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT(f) ((f)->__ptr - (f)->__base)

#ifndef HAVE_VFORK
#define HAVE_VFORK
#endif

#ifdef _SCO_ELF
#undef COFF			/* coz we're NOT */
/*
 * everythign is pdump now. --SY
 * #define UNEXEC "unexelf.o"
 */
#undef UNEXEC
#endif

/* For GCC 2.7.2.3 we require the "JKJ" version of gcc.
   Works fine with egcs and gcc 2.8.x. */
#define LIB_GCC "`$(LD) $(LDFLAGS) -print-libgcc-file-name`"
