/* Definitions file for GNU Emacs running on Data General's DG/UX
   version 4.32 upto and including 5.4.1.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* #define UNIPLUS */
/* #define USG5 */
/* #define USG */
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
#define BSD4_2
#define BSD4_3
#define BSD4_4
#define BSD

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "dgux"

/* XEmacs: There is apparently a bug in DGUX involving a SIGTTOU getting
   sent to the process if a background write is attempted when we're
   started under a job-control shell, if we turn on FASYNC (which we
   do when SIGIO is working).  So don't use SIGIO. */
#define BROKEN_SIGIO

/*
 *	Define HAVE_UNIX_DOMAIN if the system supports Unix
 *      domain sockets.
 */

#define HAVE_UNIX_DOMAIN

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF".

   DGUX can use either COFF or ELF; the default is ELF.
   To compile for COFF (or BCS) use the TARGET_BINARY_INTERFACE
   environment variable.   */

#if defined(_DGUXCOFF_TARGET) || defined(_DGUXBCS_TARGET)
#undef ELF
#ifndef COFF
#define COFF
#endif  /* COFF */
#else   /* defined(_DGUXCOFF_TARGET) || defined(_DGUXBCS_TARGET) */
#undef COFF
#ifndef ELF
#define ELF
#endif  /* ELF */
#endif  /* defined(_DGUXCOFF_TARGET) || defined(_DGUXBCS_TARGET) */

#ifndef COFF /* People will probably find this apparently unreliable
		till the NFS dumping bug is fixed.  */

/* It is possible to undump to ELF with DG/UX 5.4, but for revisions below
   5.4.1 the undump MUST be done on a local file system, or the kernel will
   panic.  ELF executables have the advantage of using shared libraries,
   while COFF executables will still work on 4.2x systems. */

#define UNEXEC "unexelf.o"

/* This makes sure that all segments in the executable are undumped,
   not just text, data, and bss.  In the case of Mxdb and shared
   libraries, additional information is stored in other sections.
   It does not hurt to have this defined if you don't use Mxdb or
   shared libraries.  In fact, it makes no difference. */

/* Necessary for shared libraries and Mxdb debugging information. */
#define USG_SHARED_LIBRARIES
#endif

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define a replacement for the baud rate switch, since DG/UX uses a different
   from BSD.  */

#define	BAUD_CONVERT    { 0, 110, 134, 150, 300, 600, 1200, 1800, 2400, \
			  4800, 9600, 19200, 38400 }

/*
 *      Make WM Interface Compliant.
 */

#define XICCC

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* DG/UX SPECIFIC ADDITIONS TO TEMPLATE FOLLOW: */

/* Use the Berkeley flavors of the library routines, instead of System V.  */

#define setpgrp(pid, pgrp) setpgrp2 (pid, pgrp)
#define getpgrp(pid) getpgrp2 (pid)

/* Use TERMINFO instead of termcap */

#define	TERMINFO

/*
 *	Use a Berkeley style sys/wait.h.
 *      This makes WIF* macros operate on structures instead of ints.
 */

#define _BSD_WAIT_FLAVOR

/* #define SYSTEM_MALLOC */

/* MAKING_MAKEFILE must be defined in "ymakefile" before including config.h */
#ifndef NOT_C_CODE

/* Define this if you use System 5 Release 4 Streams */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
/* can't hurt to define these, even though read/write should auto restart */
#define INTERRUPTIBLE_IO

#endif /* not NOT_C_CODE */

#ifndef __GNUC__
#error You must use GCC to compile Emacs on DGUX
#endif

#define ORDINARY_LINK
#define START_FILES "pre-crt0.o"
#define LIB_GCC "/usr/lib/gcc/libgcc.a"

#ifdef _M88KBCS_TARGET
/* Karl Berry says: the environment
   recommended by gcc (88/open, a.k.a. m88kbcs) doesn't support some system
   functions, and gcc doesn't make it easy to switch environments.  */
#define NO_GET_LOAD_AVG
#endif

/* definitions for xmakefile production */
#ifdef COFF

/* Define the following to use all of the available pty's. */

#define PTY_ITERATION 						\
  for (c = 'p'; c < 't'; c++)					\
    for (i = 0; (((c == 'p') && (i < 64)) || ((c != 'p') && (i < 16))); i++)

#define PTY_NAME_SPRINTF					\
      if (c == 'p')						\
        sprintf (pty_name, "/dev/pty%c%d", c, i);		\
      else							\
        sprintf (pty_name, "/dev/pty%c%x", c, i);

#define PTY_TTY_NAME_SPRINTF					\
      if (c == 'p')						\
        sprintf (pty_name, "/dev/tty%c%d", c, i);		\
      else							\
        sprintf (pty_name, "/dev/tty%c%x", c, i);

#define C_DEBUG_SWITCH "-g"

#else /* not COFF */

/* We are generating ELF object format.  This makes the system more
   SVR4 like. */

#define SVR4

/* Pseudo-terminal support under SVR4 only loops to deal with errors. */

#define PTY_ITERATION for (i = 0; i < 1; i++)

/* This sets the name of the master side of the PTY. */

#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");

/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after EMACS_UNBLOCK_SIGNAL. */

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
  if (ioctl (xforkin, I_PUSH, "ptem") == -1)	\
    fatal ("ioctl I_PUSH ptem", errno);		\
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) \
    fatal ("ioctl I_PUSH ttcompat", errno);

#ifdef __GNUC__
#define C_DEBUG_SWITCH "-g -V2 -mversion-03.00 -mstandard"
#endif

#endif /* ELF */

/* Formerly "BSD_PGRPS" */

#define SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
