/* machine description file for Data General AViiON.
   Copyright (C) 1985, 1986, 1991 Free Software Foundation, Inc.

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

/*#ifndef m88k*/
/*#define m88k*/
/*#endif*/

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
#define alloca(x) __builtin_alloca(x)

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Define ADDR_CORRECT(ADDR) to be a macro to correct an int which is
   the bit pattern of a pointer to a byte into an int which is the
   number of a byte.

   This macro has a default definition which is usually right.
   This default definition is a no-op on most machines (where a
   pointer looks like an int) but not on all machines. */

#define	ADDR_CORRECT(ADDR) ((int)ADDR)

/* Some machines that use COFF executables require that each section
   start on a certain boundary *in the COFF file*.  Such machines should
   define SECTION_ALIGNMENT to a mask of the low-order bits that must be
   zero on such a boundary.  This mask is used to control padding between
   segments in the COFF file.

   If SECTION_ALIGNMENT is not defined, the segments are written
   consecutively with no attempt at alignment.  This is right for
   unmodified system V.  */

#define SECTION_ALIGNMENT 0x7
