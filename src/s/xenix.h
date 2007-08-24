/* Definitions file for GNU Emacs running SCO Xenix 386 Release 2.2
   Copyright (C) 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* Synched up with: FSF 19.31. */

/* #define UNIPLUS */
#define XENIX
#define USG5
#define USG
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
/* #define BSD4_2 */
/* #define BSD4_3 */
/* #define BSD */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "xenix"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptyp0  */

/* #define FIRST_PTY_LETTER 'p' */

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* Xenix requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC "unexenix.o"

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Compensate for one incompatibility between Xenix and V.0.  */
#define n_zeroes n_name[0]

/* The file containing the kernel's symbol table is called /xenix.  */

#define KERNEL_FILE "/xenix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Prevent -lg from being used for debugging.  Not implemented?  */

#define LIBS_DEBUG

/* Switches for linking temacs.  */

#define LD_SWITCH_SYSTEM "-i"

/* Use terminfo instead of termcap.  */

/* Tell Emacs to use Terminfo.  */

#define TERMINFO

/* Tell Xenix curses to BE Terminfo.  */
#define M_TERMINFO

/* Control program name for etc/fakemail to run.  */

#ifdef SMAIL
#define MAIL_PROGRAM_NAME "/usr/bin/smail -q0"
#else
#define MAIL_PROGRAM_NAME "/usr/lib/mail/execmail"
#endif

/* Some variants have TIOCGETC, but the structures to go with it
   are not declared.  */

#define BROKEN_TIOCGETC
