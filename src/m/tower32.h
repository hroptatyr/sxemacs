/* machine description file for the NCR Tower 32 running System V.2.
   Copyright (C) 1986 Free Software Foundation, Inc.

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

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-2-2"  */

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

/* #define C_ALLOCA */
#define HAVE_ALLOCA

/* Change some things to avoid bugs in compiler */

#define SWITCH_ENUM_BUG 1

/* The standard C library is -lcieee, not -lc.
   Also use the PW library, which contains alloca.  */

#define LIB_STANDARD "-lPW -lcieee"

/* crt0.c should use the vax-bsd style of entry.  Beware that if you have
   OS release 2.00.00 or later, *and* change src/ymakefile so that CFLAGS
   includes C_OPTIMIZE_SWITCH rather than C_DEBUG_SWITCH, you need to
   uncomment CRT0_DUMMIES and C_OPTIMIZE_SWITCH below.  */

/* With the optimizer OFF */

#define CRT0_DUMMIES zero, bogus_fp,

/* With the optimizer ON */

/* #define CRT0_DUMMIES */
/* #define C_OPTIMIZE_SWITCH -O2 */

/* emacs's magic number isn't temacs's;
   temacs is writable text (the default!).  */

#include <asld.h>
#define EXEC_MAGIC AOUT1MAGIC
#define EXEC_PAGESIZE DATACLICK
