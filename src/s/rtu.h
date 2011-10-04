/* Definitions file for GNU Emacs running on RTU 3.0, ucb universe.
   Copyright (C) 1986 Free Software Foundation, Inc.

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

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define BSD4_2
#define BSD
#define RTU

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "rtu"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'z'	/* i.e. no PTY_LETTERs */

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#undef COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#undef MAIL_USE_FLOCK

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/* On RTU systems (like USG) the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* The "fsync" call on RTU versions 3.0 and 3.1 is badly broken!
   This hack below isn't the best solution, but without it this
   program will cause the whole system to hang!  !@#$#%$ Masscomp!  */

#define fsync(x) 0		/* "Comment out" fsync calls */

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/ttyp%x", i);		\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/pty%x", i);		\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

/* Formerly "BSD_PGRPS" */

#define SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
