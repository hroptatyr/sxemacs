/* machine description file for Masscomp 5000 series running RTU, ucb universe.
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
   USUAL-OPSYS="rtu"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
/* Masscomp predefines mc68000. */
#define m68000 mc68000

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

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

/* On return from a subroutine, the 68020 compiler restores old contents of
   register variables relative to sp, so alloca() screws up such routines.
   The following definitions should work on all Masscomps.  On the MC-5500
   (a 68000) one can #undef C_ALLOCA and #define HAVE_ALLOCA.  */
#ifdef mc500
#undef C_ALLOCA
#define HAVE_ALLOCA
#else
#define C_ALLOCA
#undef HAVE_ALLOCA
#endif

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#undef NO_REMAP

/* crt0.c should use the vax-bsd style of entry, with a dummy arg.  */

#define CRT0_DUMMIES bogus_fp,

/* Name of file the to look in
   for the kernel symbol table (for load average) */

#define KERNEL_FILE "/unix"

/* This triggers some stuff to avoid a compiler bug */

#define MASSC_REGISTER_BUG

/* Prevent -lg from being used for debugging.  Not implemented?  */

#define LIBS_DEBUG

/* Adjust a header field for the executable file about to be dumped.  */

#define ADJUST_EXEC_HEADER   \
  hdr.a_stamp = STAMP13; /* really want the latest stamp, whatever it is */
