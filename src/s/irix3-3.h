/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 3.3.
   Copyright (C) 1987,1990 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.31. */

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG
#define USG5
#define IRIS
#ifndef IRIX
#define IRIX
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "irix"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

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

#define MAIL_USE_FLOCK

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "avenrun"


/* On USG systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* XEmacs change -- SGI apparently has a bogus version of memmove,
   which causes ralloc.c great pain.  However, bcopy works, so use that.
   From Paul Flinders <ptf@delcam.co.uk>
   (Resist the temptation to put parens around the args in the expansion.
   Existence is suffering.)
 */
#define memmove(to, from, len) bcopy (from, to, len)

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
 	    sprintf (ptyname, "/dev/ttyq%d", minor (stb.st_rdev));


/* sioctl.h should be included where appropriate.  */

#define NEED_SIOCTL


/* This was formerly in LIBS_MACHINE in iris4d.h,
   but it is not needed for newer system versions.  */
#define LIBS_SYSTEM "-lsun"
