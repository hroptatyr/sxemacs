/* Machine description file for intel 386.
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Intel 386 (-machine=intel386 or -machine=is386.h)

  The possibilities for -opsystem are: bsd4-2, usg5-2-2, usg5-3,
  isc2-2, 386-ix, esix, linux, sco3.2v4, and xenix.

  18.58 should support a wide variety of operating systems.
  Use isc2-2 for Interactive 386/ix version 2.2.
  Use 386ix for prior versions.
  Use esix for Esix.
  Use linux for Linux.
  It isn't clear what to do on an SCO system.

  -machine=is386 is used for an Integrated Solutions 386 machine.
  It may also be correct for Microport systems.

Cubix QBx/386 (-machine=intel386 -opsystem=usg5-3)

  Changes merged in 19.1.  Systems before 2/A/0 may fail to compile etags.c
  due to a compiler bug.

Prime EXL (-machine=intel386 -opsystem=usg5-3)

  Minor changes merged in 19.1.
NOTE-END */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define INTEL386

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

/* Mly 16-Jan-96 16:38:32: this is part of a prototype -- same bug present in
   other m*.h files */
#define CRT0_DUMMIES int bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

#ifdef XENIX
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE short

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (((double) (x)) * 100.0 / FSCALE)

#define FSCALE 256.0		/* determined by experimentation...  */
#endif

#ifdef SOLARIS2
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))

/* configure thinks solaris X86 has gethostname, but it does not work,
   so undefine it.  */
#undef HAVE_GETHOSTNAME

#else				/* not SOLARIS2 */
#ifdef USG5_4			/* Older USG systems do not support the load average.  */
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#define FSCALE 256.0
#endif
#endif				/* not SOLARIS2 */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

#ifdef XENIX

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

#define STACK_DIRECTION -1

/* Since cannot purify, use standard Xenix 386 startup code. */

#define START_FILES "/lib/386/Sseg.o pre-crt0.o /lib/386/Scrt0.o"

/* These really use terminfo.  */

#define LIBS_TERMCAP "/lib/386/Slibcurses.a /lib/386/Slibtinfo.a /lib/386/Slibx.a"

/* Standard libraries for this machine.  Since `-l' doesn't work in `ld'.  */
/* '__fltused' is unresolved w/o Slibcfp.a */
#define LIB_STANDARD "/lib/386/Slibcfp.a /lib/386/Slibc.a"
#else				/* not XENIX */

#ifdef USG

#define NO_REMAP
#define TEXT_START 0
#endif				/* USG */
#endif				/* not XENIX */

#ifdef linux
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif

#ifdef __GNUC__
/* GCC's alloca() is semi-broken.  See lisp.h.

   This brokenness has been confirmed under both Linux and NetBSD.
   It may also exist on non-Intel architectures. */
#undef BROKEN_ALLOCA_IN_FUNCTION_CALLS
#define BROKEN_ALLOCA_IN_FUNCTION_CALLS	1
#endif

/* XEmacs change: John Hughes <john@AtlanTech.COM> says using vfork
   under i386-unknown-sysv4.2 makes C-g sometimes cause a SIGSEGV
   in TTY mode; the problem goes away if you use fork */
#ifdef USG5_4_2
#define vfork fork
#endif
