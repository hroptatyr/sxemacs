/* machine description file for Iris-4D machines.  Use with s/irix*.h.
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef mips
#define mips
#endif

#ifndef IRIS_4D
#define IRIS_4D
#endif

/* jg@genmagic.genmagic.com (John Giannandrea) says this is unnecessary.  */
#if 0
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long	/* This doesn't quite work on the 4D */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int)(((double)(x)*100)/1024.0)

/* s-iris3-6.h uses /vmunix */

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"
#endif

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* #define C_ALLOCA */  /* Sjoerd.Mullender@cwi.nl says no need.  */
/* #define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#ifdef USG5_4
#undef UNEXEC
#define UNEXEC "unexelf.o"
#else
#define UNEXEC "unexmips.o"
#endif

#define TEXT_START 0x400000
#define DATA_START 0x10000000

#undef LIBS_MACHINE
/* -lsun in case using Yellow Pages for passwords.  */
#define LIBS_DEBUG

/* Define this if you have a fairly recent system,
   in which crt1.o and crt1.n should be used.  */
#define HAVE_CRTN

#ifndef USG5_4
#ifdef HAVE_CRTN
/* Must define START-FILES so that the linker can find /usr/lib/crt0.o.  */
#define START_FILES "pre-crt0.o /usr/lib/crt1.o"
#define LIB_STANDARD "-lc /usr/lib/crtn.o"
#else
#define START_FILES "pre-crt0.o /usr/lib/crt0.o"
/* The entry-point label (start of text segment) is `start', not `__start'.  */
#define DEFAULT_ENTRY_ADDRESS start
#define LIB_STANDARD "-lc"
#endif
#endif

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'

/* Define STACK_DIRECTION for alloca.c */

#undef STACK_DIRECTION
#define STACK_DIRECTION -1
