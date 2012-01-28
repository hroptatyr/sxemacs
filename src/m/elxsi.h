/* machine description file for Elxsi machine (running enix).
   Copyright (C) 1986, 1992 Free Software Foundation, Inc.
   Adapted by John Salmon

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
   USUAL-OPSYS="usg5-2"  */

/* This file was modified by Matt Crawford <matt@tank.uchicago.edu>
   to work under Elxsi's 12.0 release of BSD unix. */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

#ifndef elxsi
#define elxsi
#endif

/* Name of kernel load average variable */

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) ((x) * 100.0)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.

   Earlier versions couldn't dump.
   Changes for 12.0 release are in 19.1.
   Dumping should work now.  */

/* #define CANNOT_DUMP */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/*#define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
/*#define NO_REMAP*/

/* This is a guess for an alternate solution to whatever
   problem motivated defining _sobuf in sysdep,c with extern char *_sobuf.  */
#define _sobuf xsobuf

/* Address of start of text segment as loaded.  */

#define TEXT_START	0x800

/* Tell crt0.c not to define environ.  */

#define DONT_NEED_ENVIRON

/* The elxsi has no debugger, so might as well optimize instead
   of trying to make a symbol table.  */

#define C_DEBUG_SWITCH "-O"

/* Elxsi uses COFF under both Sys V and BSD environments */

#define COFF

#define ADJUST_EXEC_HEADER {\
extern int _init_brk;\
_init_brk = bss_start;\
}
