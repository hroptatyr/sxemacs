/* machine description file for hp9000 series 200 or 300 on either HPUX or BSD.
   Copyright (C) 1985, 1994 Free Software Foundation, Inc.

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
HP 9000 series 200 or 300 (-machine=hp9000s300)

  These machines are 68000-series CPUs running HP-UX
  (a derivative of sysV with some BSD features) or BSD 4.3 ported by Utah.

  If you're running HP-UX, specify `-opsystem=hpux'.
  If you're running BSD, specify `-opsystem=bsd4-3'.
NOTE-END */

/* Define this symbol if you are running a version of HP-UX
   which predates version 6.01 */

/* #define HPUX_5 */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef hp9000s300
#define hp9000s300
#endif

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Let the OS header file determine this if not HPUX. */

/*
 * everything is pdump now --SY
 * #ifdef HPUX
 * # define UNEXEC "unexhp9k3.o"
 * #endif
 */
#undef UNEXEC


/* For University of Utah 4.3bsd implementation on HP300s.
   The #ifndef __GNUC__ definitions are required for the "standard" cc,
   a very old, brain-dead version of PCC. */

#ifdef BSD4_3

/* Tell crt0.c that this is an ordinary 68020.  */
#undef hp9000s300
#define m68000

#define CRT0_DUMMIES		bogus_a6,

#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif

#ifndef __GNUC__
#define LIBS_DEBUG		/* don't have -lg that works */
#define C_DEBUG_SWITCH		/* don't support -g */
#endif

#undef LOAD_AVE_TYPE
#undef LOAD_AVE_CVT
#define LOAD_AVE_TYPE long
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) / 2048.0 * 100.0))

#endif				/* BSD4_3 */

#ifndef BSD4_3
/* The following definitions are for HPUX only.  */

/* The symbol in the kernel where the load average is found
   is named _avenrun on this machine.  */

#define LDAV_SYMBOL "_avenrun"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

#ifdef __GNUC__
#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif
#endif

/* This library is needed with -g, on the 200/300 only.  */

#if !defined(__GNUC__) || defined(__HPUX_ASM__)
#define LIBS_DEBUG "/usr/lib/end.o"
#endif

/* Need a TEXT_START.  On the HP9000/s300 that is 0.  */
#ifdef __GNUC__
#define TEXT_START   0
#endif

/* In older versions of hpux, for unknown reasons, S_IFLNK is defined
   even though symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.

   Version 6 of HP-UX has symbolic links.  */

#ifdef HPUX_5
#undef S_IFLNK
#endif

/* Define C_SWITCH_MACHINE to be +X if you want the s200/300
 * Emacs to run on both 68010 and 68020 based hp-ux's.
 *
 * Define OLD_HP_ASSEMBLER if you have an ancient assembler
 *
 * Define HPUX_68010 if you are using the new assembler but
 * compiling for a s200 (upgraded) or s310.  68010 based
 * processor without 68881.
 */

/* These switches increase the size of some internal C compiler tables.
   They are required for compiling the X11 interface files. */

#ifndef HPUX_5
#ifndef __GNUC__
#define C_SWITCH_MACHINE "-Wc,-Nd4000,-Ns3000"
#endif
#endif

/* Define NEED_BSDTTY if you have such. */

#define NEED_BSDTTY

#endif				/* not BSD4_3 */
