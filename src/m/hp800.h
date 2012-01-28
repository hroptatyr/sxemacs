/* machine description file for hp9000 series 800 machines.
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
   USUAL-OPSYS="hpux"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#	define hp9000s800
#endif

#ifdef __hpux
/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#     define hp9000s800
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) (x * 100.0))

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

/* lemacs change:  define HAVE_ALLOCA if gcc is being used */
#ifdef __GNUC__
#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif
#else
#define C_ALLOCA
#endif

/* the data segment on this machine always starts at address 0x40000000. */

#ifdef DATA_START
#undef DATA_START
#endif
#ifdef TEXT_START
#undef TEXT_START
#endif

#define DATA_START    0x40000000
#define TEXT_START    0x00000000

#define STACK_DIRECTION 1

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

/*
 * everything is pdump now --SY
 * #define UNEXEC "unexhp9k800.o"
 */
#undef UNEXEC

#define LIBS_MACHINE
#define LIBS_DEBUG

/* Include the file bsdtty.h, since this machine has job control.  */
#define NEED_BSDTTY

/* The symbol in the kernel where the load average is found
   is named _avenrun.  At this time there are two major flavors
   of hp-ux (there is the s800 and s300 (s200) flavors).  The
   differences are thusly moved to the corresponding machine description file.
*/

/* no underscore please */
#define LDAV_SYMBOL "avenrun"

#if 0				/* Supposedly no longer true.  */
/* In hpux, for unknown reasons, S_IFLNK is defined even though
   symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.  */

#undef S_IFLNK
#endif

#endif				/* __hpux */
