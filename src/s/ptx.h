/* Definitions file for GNU Emacs running on Sequent DYNIX/ptx 1.x/2.x
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

/* This file was written by Bill Burton <billb@progress.com>.  Parts were
   adapted from m-ptx1-2.h and process.c as distributed with the Emacs 18.57
   on the Sequent Public software tape. Other parts were adapted from
   usg5-4.h. */

/* Use the SysVr3 file for base configuration even though much is changed.  */
#define DONT_DEFINE_NO_REMAP	/* `static' hack not needed */
#include "usg5-3.h"

/* Undo these defines because they are incorrect or need to be changed.  */
#undef USG_SHARED_LIBRARIES

/* PTX has System V streams.  */
#define SYSV_STREAMS

/* Leave out -lPW since it conflicts with term.o and because we're not sure
   if the alloca found there by autoconf should be trusted on PTX.  */
#define LIB_STANDARD "-lc"

#ifndef HAVE_SOCKETS		/* determined by configure */
#define NO_SUBPROCESSES
#endif

#ifdef HAVE_X_WINDOWS

/* This is also defined so that lib-src/profile can link.  */
#define LIBS_SYSTEM "-lseq"

#else				/* ! HAVE_X_WINDOWS */

#ifdef HAVE_SOCKETS
#define LIBS_SYSTEM "-lsocket -linet -lnsl -lseq"
#else
#define LIBS_SYSTEM "-lseq"
#endif

#endif				/* ! HAVE_X_WINDOWS */

/* No <sioctl.h> */
#define NO_SIOCTL_H

/* If we have X windows, configure should find gettimeofday in -lX11.
   Since we emulate gettimeofday below, we really have it anyway.  */
#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif

#ifdef emacs
#include <sys/conf.h>

				/*#define BROKEN_SIGIO*//* SIGIO is already undef'd elsewhere. PTX
				   has SIGIO, but it's just an alias for
				   SIGPOLL.  */

/* Emulate gettimeofday() except for the time zone information which Emacs
   doesn't use anyway.  Get_process_stats() is in -lseq.  */
#include <sys/procstats.h>
#define gettimeofday(tp, tzp) get_process_stats (tp, PS_SELF, 0, 0)

/* Define timezone since it's not in sys/time.h.  Unfortunately, this causes
   trouble when building with X since this struct is defined in
   <X11/Xos.h>.  */
struct timezone {
	int tz_minuteswest;
	int tz_dsttime;
};

/* Unfortunately, this define is not checked in all files including
   <X11/Xos.h> so we can't use it.  */
/* #define XOS_NEEDS_TIME_H */

/* In ptx/WINDOWS, this prevents problems with the timezone struct being
   redefined in <X11/Xos.h>.  It seems the necessary include files are
   included via systime.h so leaving them out here is not a problem.  This
   may not work in X11R5 or X11R6.  */
#define __TIMEVAL__

#endif				/* emacs */

/* PTX has termios */
#undef BROKEN_TIOCGWINSZ
#undef BROKEN_TIOCGETC

/* PTX has pty's but not like System V */
#define HAVE_PTYS
#undef SYSV_PTYS

/* Provide pty support which is defined into process.c:allocate_pty.
   Basic ideas for handling getpseudotty were lifted from process.c in
   Emacs 18.57 included on the Sequent Public Software tape.  However, this
   implementation bears almost no resemblance to the original and does not
   require that process.c be patched.  */
#define PTY_ITERATION						\
  char *mastername, *slavename;					\
  while (1)

#define PTY_OPEN						\
  if (failed_count++ >= 5) break;				\
  if ((fd = getpseudotty (&slavename, &mastername)) < 0) {	\
    error("Out of ptys.");					\
    continue;							\
  }								\
  strcpy (pty_name, slavename);

/* Define these to prevent the default logic in process.c:allocate_pty
   from being used.  */
#define PTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF

/* PTX doesn't seem to have memmove.  */
#define MEMMOVE_MISSING
