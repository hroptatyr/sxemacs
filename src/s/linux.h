/* This file is the configuration file for the GNU/Linux operating system,
   prior to version 1.1.56.
   Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.31 (called, ahem ... lignux.h in FSF). */

/* This file was put together by Michael K. Johnson and Rik Faith.  */

#define USG
#define LINUX

/* powerpc gcc 2.8.0 doesn't define __ELF__, but it is */

#if defined(__ELF__) || defined(powerpc)
#define LINUX_ELF
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "linux"	/* All the best software is free. */

#define FIRST_PTY_LETTER 'p'
#define HAVE_PTYS

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* Both are used in Linux by different mail programs.  I assume that most
   people are using newer mailers that have heard of flock.  Change this
   if you need to. */

/*#define MAIL_USE_FLOCK*/

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* On POSIX systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

/* This is needed for dispnew.c:update_frame */

#ifndef NOT_C_CODE
#include <stdio.h>		/* Get the definition of _IO_STDIO_H.  */
#if defined(_IO_STDIO_H) || defined(_STDIO_USES_IOSTREAM)
/* new C libio names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#else				/* !_IO_STDIO_H */
/* old C++ iostream names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_pptr - (FILE)->_pbase)
#endif				/* !_IO_STDIO_H */
#endif				/* C_CODE */

/* Ask GCC where to find libgcc.a.  */
#define LIB_GCC "`$(CC) $(C_SWITCH_X_SITE) -print-libgcc-file-name`"

#ifndef LINUX_ELF
/* Linux has crt0.o in a non-standard place */
#define START_FILES "pre-crt0.o /usr/lib/crt0.o"
#else
 /**/
#if	defined(__linux__) && defined(powerpc)	/*Added by Fukui */
#define START_FILES		/*Added by Fukui */
#else				/*Added by Fukui */
#if defined __s390x__ || defined __x86_64__
#define START_FILES "pre-crt0.o /usr/lib64/crt1.o /usr/lib64/crti.o"
#else
#define START_FILES "pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o"
#endif
#endif				/*Added by Fukui */
#endif
/* This is needed for sysdep.c */
#define NO_SIOCTL_H		/* don't have sioctl.h */
#define HAVE_WAIT_HEADER
#define POSIX			/* affects getpagesize.h and systty.h */
/* Best not to include -lg, unless it is last on the command line */
#define LIBS_DEBUG
#define LIBS_TERMCAP "-ltermcap -lcurses"	/* save some space with shared libs */
#ifndef LINUX_ELF
#define LIB_STANDARD "-lc"	/* avoid -lPW */
#else
/*#undef LIB_GCC
  #define LIB_GCC*/
#if defined __s390x__  || defined __x86_64__
#define LIB_STANDARD "-lgcc -lc -lgcc /usr/lib64/crtn.o"
#else
#define LIB_STANDARD "-lgcc -lc -lgcc /usr/lib/crtn.o"
#endif
#define LINKER "$(CC) -nostdlib"
#endif
#ifdef TERM
#define LIBS_SYSTEM "-lclient"
#define C_SWITCH_SYSTEM "-I/usr/src/term"
#else
/* alane@wozzle.linet.org says that -lipc is not a separate library,
   since libc-4.4.1.  So -lipc was deleted.  */
#define LIBS_SYSTEM
#endif
#ifdef LINUX_ELF
/*
 * everything is pdump now. --SY
 * #define UNEXEC "unexelf.o"
 */
#undef UNEXEC
#define UNEXEC_USE_MAP_PRIVATE
/* Although slb thinks ORDINARY_LINK does not work on linux, ORDINARY_LINK
   has been enabled in 21.5 for some time with no ill effects. */
#define ORDINARY_LINK
/* I still think ORDINARY_LINK should be the default, but since slb
   insists, ORDINARY_LINK will stay on until we expunge the dump code.
   However, the user (i.e. me!) should be able to specify ORDINARY_LINK via
   configure --cppflags=-DORDINARY_LINK ... */
#ifdef ORDINARY_LINK
#undef LIB_STANDARD
#undef START_FILES
#undef LIB_GCC
#endif
#endif				/* LINUX_ELF */
#ifdef LINUX_QMAGIC
#define HAVE_TEXT_START
/* #define UNEXEC "unexsunos4.o" */
#undef UNEXEC
#define N_PAGSIZ(x) PAGE_SIZE
#else				/* not LINUX_QMAGIC */
#define A_TEXT_OFFSET(hdr) (N_MAGIC(hdr) == QMAGIC ? sizeof (struct exec) : 0)
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#define ADJUST_EXEC_HEADER \
  unexec_text_start = N_TXTADDR(ohdr) + A_TEXT_OFFSET(ohdr)
#endif				/* not LINUX_QMAGIC */
/* This is to work around mysterious gcc failures in some system versions.
   It is unlikely that Emacs changes will work around this problem;
   therefore, this should remain permanently.  */
#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif
/* The regex.o routines are a part of the GNU C-library used with Linux.  */
/* However, sometimes they disagree with the src/regex.h that comes with Emacs,
   and that can make trouble in etags.c because it gets the regex.h from Emacs
   and the function definitions in libc.  So turn this off.  */
/* XEmacs: in any case, Mule uses different regex routines. */
/* #define REGEXP_IN_LIBC */
/* XEmacs change: the standard linux libc includes regex routines in
   it.  We have to use our own and have to avoid name conflicts. */
#define re_compile_pattern sys_re_compile_pattern
#define re_search sys_re_search
#define re_search_2 sys_re_search_2
#define re_match_2 sys_re_match_2
#define re_max_failures sys_re_max_failures
#define re_set_syntax sys_re_set_syntax
#define re_set_registers sys_re_set_registers
#define re_compile_fastmap sys_re_compile_fastmap
#define re_match sys_re_match
#define regcomp sys_regcomp
#define regexec sys_regexec
#define regerror sys_regerror
#define regfree sys_regfree
#if 0				/* mrb - if autoconf 2 is wrong, we should fix the test */
/* XEmacs: Damon Lipparelli says that he incorrectly gets this
   defined on his system */
#undef GETTIMEOFDAY_ONE_ARGUMENT
#endif				/* 0 */
/* Use BSD process groups, but use setpgid() instead of setpgrp() to
   actually set a process group. */
/* XEmacs: removed setpgrp() definition because we use setpgid() when
   it's available, and autodetect it. */
