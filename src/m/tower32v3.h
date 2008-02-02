/* machine description file for the NCR Tower 32 running System V.3.
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

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-3"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
#define m68000

/* Data type of load average, as read out of kmem.  */

/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#ifdef __GNUC__
#define HAVE_ALLOCA
#define alloca __builtin_alloca
#define C_OPTIMIZE_SWITCH "-O -fstrength-reduce -fomit-frame-pointer"
#define LIB_STANDARD "-lc /lib/crtn.o"
#else
/* This section is correct if you do *not* change src/ymakefile so that
   CFLAGS includes C_OPTIMIZE_SWITCH rather than C_DEBUG_SWITCH.  */
#define HAVE_ALLOCA
#define "C_DEBUG_SWITCH -g -O0"
#define LIB_STANDARD "-lc -lPW /lib/crtn.o"
/* This section is correct if you do enable C_OPTIMIZE_SWITCH.  */
/* #define C_ALLOCA */
/* #define STACK_DIRECTION -1 */
/* #define C_OPTIMIZE_SWITCH -O2 */
/* #define LIB_STANDARD -lc /lib/crtn.o */
#endif

/* The OS maps the data section far away from the text section.  */
#define NO_REMAP
#define TEXT_START 0
#define START_FILES "pre-crt0.o /lib/crt1.o"

/* The OS has an implementation of symlinks that is semantically different
   from BSD, but for some silly reason it partly has the same syntax.  */
#undef S_IFLNK

/* The OS needs stream.h+ptem.h included in sysdep.c.  */
#define NO_SIOCTL_H
#define NEED_PTEM_H
