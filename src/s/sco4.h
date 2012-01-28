/* System description file for SCO 3.2v4.
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


/* Synched up with: FSF 19.31. */

/* Contributed by Ian Lance Taylor, ian@cygnus.com.  */

/* SCO is sort of like SVR3.  */
#include "usg5-3.h"
#define SC0_R4

#if 0				/* Turned off rather than make the Lisp code check for this. -- rms.
				   I am assuming that (at least most of) the tests for usg-unix-v
				   do the right thing for sco3.2v4 also.  Things that *might* be wrong
				   as a result of turning off these lines include the values of
				   ange-ftp-remote-shell-file-name (now remsh)
				   dired-chown-program (now just chown)
				   lpr-command (now lp)
				   nntp-buggy-select (now t)
				   rmail-spool-directory (now /usr/mail?)
				   and the actions of the function print-region-1.  */

/* SYSTEM_TYPE should indicate the kind of system you are using.  */
#undef SYSTEM_TYPE
#define SYSTEM_TYPE "SCO 3.2v4"
#endif

/* SCO has ptys with unusual names.  */
#define HAVE_PTYS

#define PTY_ITERATION \
   for (i = 0; ; i++)
#define PTY_NAME_SPRINTF				       \
	do {						       \
		int sz = snprintf (pty_name, sizeof(pty_name), \
				   "/dev/ptyp%d", i);	       \
		assert(sz>=0 && sz<sizeof(pty_name));	       \
	} while(0)

#define PTY_TTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/ttyp%d", i);		\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

/* Must use 'cc' to link when build with motif toolkit. */
#ifndef __GNUC__
#define LINKER "cc"
#endif

#ifdef HAVE_SOCKETS
#define LIBS_SYSTEM "-lsocket -lPW"

/* SCO has gettimeofday in socket library */
/* Autoconf should determine this, but for now,
   play safe to avoid error rather than deleting this
   and risking the wrong result.  */
#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif
#endif

/* This enables configure to tell that we have alloca.  */
#ifndef LIBS_SYSTEM
#define LIBS_SYSTEM "-lPW"
#endif

#ifdef HAVE_X11R5
/* configure can't get this right linking fails unless -lsocket is used.  */
#undef HAVE_XSCREENNUMBEROFSCREEN
#define HAVE_XSCREENNUMBEROFSCREEN
#endif

/* We don't have -loldX, and we don't need it.  */
#define LIB_XMENU_LIB

/* SCO does have TIOCGWINSZ.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_PTEM_H

/* We need to link with crt1.o and crtn.o.  */
#define START_FILES "pre-crt0.o /lib/crt1.o"
#define LIB_STANDARD "-lc /lib/crtn.o"

/* Specify program for etc/fakemail to run.  Define SMAIL if you are
   using smail, don't for MMDF.  */

#ifdef SMAIL
#define MAIL_PROGRAM_NAME "/bin/smail -q0"
#else
#define MAIL_PROGRAM_NAME "/usr/lib/mail/execmail"
#endif

/* Tell process_send_signal to use VSUSP instead of VSWTCH.  */
#define PREFER_VSUSP

/* wjs@wiis.wang.com says SCO 3.2 v4.2 "has sockets",
   but only for network connections.
   It doesn't have the kind of sockets that emacsclient.c
   and emacsserver.c would use.  */
#define NO_SOCKETS_IN_FILE_SYSTEM
