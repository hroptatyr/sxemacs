/* Definitions file for GNU Emacs running on HPUX release 7.0.
   Based on AT&T System V.2.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

#define USG			/* System III, System V, etc */

#define USG5

#define HPUX

/* XEmacs change: HPUX_PRE_8_0 needs to be defined for HP-UX 7.X and
   earlier.  DO NOT UNCOMMENT THE FOLLOWING IF HP-UX 8.0 OR LATER IS
   BEING USED; HPUX_PRE_8_0 will be automatically #undef'd later, if
   necessary. */
#define HPUX_PRE_8_0

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "hpux"

/* `nomultiplejobs' should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).

 On hpux this depends on the precise kind of machine in use,
 so the m- file defines this symbol if appropriate.  */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* The file containing the kernel's symbol table is called /hp-ux.  */

#define KERNEL_FILE "/hp-ux"

/* The symbol in the kernel where the load average is found
   depends on the cpu type, so we let the m- files define LDAV_SYMBOL.  */

/* Special hacks needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO
/* XEmacs change */
#define INTERRUPTIBLE_CLOSE

/* Use the system provided termcap(3) library */
#define TERMINFO

/* The 48-bit versions are more winning for Emacs;
   the ordinary ones don't give even 32 bits.  */
#define random lrand48
#define srandom srand48

/* In hpux, the symbol SIGIO is defined, but the feature
   doesn't work in the way Emacs needs it to.

   XEmacs change: XEmacs has patches Darryl Okhahata (?) submitted to
   the FSF which allow interrupt input in emacs. */

/* #define BROKEN_SIGIO */

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this by defining NO_REMAP, which makes the purespace not pure.
   (Formerly this was avoided by doing '#define static' but this just
   fails with inline functions.) */

#ifndef DONT_DEFINE_NO_REMAP
#define NO_REMAP
#endif

/* Define extra libraries to load.
   This should have -lBSD, but that library is said to make
   `signal' fail to work.  */

#ifdef HPUX_NET
#define LIBS_SYSTEM "-ln"
#else
#define LIBS_SYSTEM
#endif

/* Some additional system facilities exist.  */

/* XEmacs change */
			  /* #define HAVE_PERROR *//* Delete this line for version 6.  */

/* The following maps shared exec file to demand loaded exec.
   Don't do this as demand loaded exec is broken in hpux.  */

#if 0

/* Adjust a header field for the executable file about to be dumped.  */

#define ADJUST_EXEC_HEADER   \
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?  \
		 NEWMAGIC : ohdr.a_magic);

#endif

/* Baud-rate values in tty status have nonstandard meanings.  */

#define BAUD_CONVERT  \
{ 0, 50, 75, 110, 135, 150, 200, 300, 600, 900, 1200,  \
  1800, 2400, 3600, 4800, 7200, 9600, 19200, 38400 }

/* This is needed for HPUX version 6.2; it may not be needed for 6.2.1.  */
#define SHORT_CAST_BUG

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF						\
	do {								\
		int sz = snprintf (pty_name, sizeof(pty_name),		\
				   "/dev/pty/tty%c%x", c, i);		\
		assert(sz>=0 && sz<sizeof(pty_name));			\
	} while(0)



/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF					\
	do {							\
		int sz = snprintf (pty_name, sizeof(pty_name),	\
				   "/dev/ptym/pty%c%x", c, i);	\
		assert(sz>=0 && sz<sizeof(pty_name));		\
	} while(0)

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H
