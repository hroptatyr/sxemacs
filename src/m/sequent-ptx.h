/* machine description file for SEQUENT machines running DYNIX/ptx
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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
   USUAL-OPSYS="ptx"  */

#include "intel386.h"

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others.  */

/* BTW: ptx defines _SEQUENT_, i386 */

/* crt0.c should use the vax-bsd style of entry, with these dummy args.  */
/* Already defined.  Assume prior definition works for PTX.  */
#if 0
#undef CRT0_DUMMIES
#define CRT0_DUMMIES dummy1, dummy2, dummy3,
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE unsigned long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define	FSCALE	1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

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

#ifndef __GNUC__
#define C_ALLOCA
#undef HAVE_ALLOCA
#endif

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
/* On PTX, can't seem to get a valid executable unless NO_REMAP is
   defined.  This did work in the Sequent patched Emacs 18.57.  */
#ifndef NO_REMAP
#define NO_REMAP
#endif
/* #undef NO_REMAP */

/* If a valid PTX executable could be dumped without NO_REMAP defined, 
   here's a guess at some defines to make it work.  */
#ifndef NO_REMAP

/* PTX has getpagesize() but it returns 296. Using the default method of
   including getpagesize.h in unexec.c returns 4096 which seems more
   reasonable.  */
#undef HAVE_GETPAGESIZE

/* Override magic number for a.out header.  */
#define EXEC_MAGIC 0411		/* from a.out.h: separated I&D */

#define ADJUST_TEXT_SCNHDR_SIZE
#define ADJUST_TEXTBASE

/* The file sections in the Symmetry a.out must be on 4K boundaries.  */
/* #define DATA_SECTION_ALIGNMENT       (4096-1) */

#endif  /* ifndef NO_REMAP */

/* Avoids a compiler bug.  */
#define TAHOE_REGISTER_BUG

/* (short) negative-int doesn't sign-extend correctly.  */
#define SHORT_CAST_BUG

/* Cause compilations to be done in parallel in ymakefile.  */
#define MAKE_PARALLEL $&

/* Use terminfo library.  */
#define LIBS_TERMCAP "-ltermlib"

