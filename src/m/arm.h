/* Machine description file for digital/intel arm/strongarm
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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef arm
#define arm
#endif

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

/* Mly 16-Jan-96 16:38:32: this is part of a prototype -- same bug present in
   other m*.h files */
#define CRT0_DUMMIES int bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

#ifdef USG5_4			/* Older USG systems do not support the load average.  */
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

/* FSHIFT and FSCALE are defined in param.h, but are required by
   LOAD_AVE_CVT, so they need to be defined here.  */

#ifndef FSHIFT
#define FSHIFT	8		/* bits to right of fixed binary point */
#endif

#ifndef FSCALE
#define FSCALE	(1<<FSHIFT)
#endif

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#endif

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* this brings in alloca() if we're using cc */
#ifdef USG
#define NO_REMAP
#define TEXT_START 0
#endif				/* USG */


#ifdef linux
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif

/* XEmacs change: John Hughes <john@AtlanTech.COM> says using vfork
   under i386-unknown-sysv4.2 makes C-g sometimes cause a SIGSEGV
   in TTY mode; the problem goes away if you use fork */
#ifdef USG5_4_2
#define vfork fork
#endif
