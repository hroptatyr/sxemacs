/* Definitions file for GNU Emacs running on AT&T's System V Release 4
   Copyright (C) 1987, 1990 Free Software Foundation, Inc.

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

/* This file written by James Van Artsdalen of Dell Computer Corporation.
 * james@bigtex.cactus.org.  Subsequently improved for Dell 2.2 by Eric
 * S. Raymond <esr@snark.thyrsus.com>.
 */

/* Use the SysVr3 file for at least base configuration. */

#define DONT_DEFINE_NO_REMAP	/* `static' hack not needed */
#include "usg5-3.h"

#define USG5_4

#define LIBS_SYSTEM "-lsocket -lnsl -lelf"
#define ORDINARY_LINK

#if 0
#ifdef ORDINARY_LINK
#define LIB_STANDARD "-lc /usr/ucblib/libucb.a"
#else
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o"
#define LIB_STANDARD "-lc /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o"
#endif
#else

#ifdef ORDINARY_LINK
#define LIB_STANDARD
#else
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o"
#define LIB_STANDARD "-lc /usr/ccs/lib/crtn.o"
#endif
#endif

/* there are no -lg libraries on this system, and no libPW */

#define LIBS_DEBUG

/* No <sioctl.h> */

#define NO_SIOCTL_H

/* Undump with ELF */

#undef COFF

/*
 * everything is pdump now. --SY
 * #define UNEXEC "unexelf.o"
 */
#undef UNEXEC

/* Get <sys/ttold.h> to get struct
 * tchars. But get <termio.h> first to make sure ttold.h doesn't
 * interfere.
 */

#ifndef NOT_C_CODE
#include <sys/wait.h>
#endif

#ifdef emacs
#include <sys/filio.h>
#include <termio.h>
#include <sys/ttold.h>
#include <signal.h>
#include <sys/stream.h>
#include <sys/termios.h>
#endif

#undef BROKEN_SIGIO

/* Some SVr4s don't define NSIG in sys/signal.h for ANSI environments;
 * instead, there's a system variable _sys_nsig.  Unfortunately, we need the
 * constant to dimension an array.  So wire in the appropriate value here.
 */

#if defined(emacs) && !defined(NSIG)
#define NSIG	32
#endif				/* defined(emacs) && !defined(NSIG) */

/* We need bss_end from emacs.c for undumping */

#ifndef USG_SHARED_LIBRARIES
#define USG_SHARED_LIBRARIES
#endif

#define HAVE_PTYS
#undef BROKEN_TIOCGWINSZ
#undef BROKEN_TIOCGETC

/* This change means that we don't loop through allocate_pty too many
   times in the (rare) event of a failure. */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'z'

/* This sets the name of the master side of the PTY. */

#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");

/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after EMACS_UNBLOCK_SIGNAL. */

/* XEmacs change */
#ifndef NOT_C_CODE
# if !__STDC__ && !defined(STDC_HEADERS)
char *ptsname();
# endif
#endif

#define PTY_TTY_NAME_SPRINTF				\
  {							\
    char *ptyname;					\
							\
    EMACS_BLOCK_SIGCHLD;				\
    if (grantpt (fd) == -1)				\
      { close (fd); return -1; }			\
    EMACS_UNBLOCK_SIGCHLD;				\
    if (unlockpt (fd) == -1)				\
      { close (fd); return -1; }			\
    if (!(ptyname = ptsname (fd)))			\
      { close (fd); return -1; }			\
    strncpy (pty_name, ptyname, sizeof (pty_name));	\
    pty_name[sizeof (pty_name) - 1] = 0;		\
  }

/* Push various streams modules onto a PTY channel. */

#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1)		\
    fatal ("ioctl I_PUSH ptem: errno %d\n", errno);	\
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)		\
    fatal ("ioctl I_PUSH ldterm: errno %d\n", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1)	\
    fatal ("ioctl I_PUSH ttcompat: errno %d\n", errno);

/* Tell x11term.c and keyboard.c we have the system V streams feature.  */
#define SYSV_STREAMS
/* On Some SysV System , w3 freeze. If freeze your xemacs , Add below definition */
/* This definition added by Shogo Fujii(shogo@bsd1.kbnes.nec.co.jp) */
#define PROCESS_IO_BLOCKING
