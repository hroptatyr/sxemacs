/* Machine-dependent configuration for GNU Emacs for Tadpole 68k machines
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
   USUAL-OPSYS="usg5-3"  */

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

/* SysV has alloca in the PW library */

#define LIB_STANDARD "-lPW -lc"
#define HAVE_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/* #define NO_REMAP */

/* Use Terminfo, not Termcap.  */

#define TERMINFO

/* TPIX extras */

#define TPIX				/* used in various source files */
#undef ADDR_CORRECT			/* don't need this bug fix */
#define fchmod				/* we don't have fchmod() */
#define SECTION_ALIGNMENT (2048-1)	/* 2k boundaries required in unexec */
#define SEGMENT_MASK (128*1024-1)	/* 128k offsets required in unexec */
#define C_DEBUG_SWITCH "-O"		/* build with -O (TPIX has GCC 1.34) */

#define BROKEN_TIOCGWINSZ		/* Don't try to use TIOCGWINSZ.  */

/* omit next three lines if no TCP installed */

#define select gnu_select		/* avoid select() name clash */
#define HAVE_PTYS			/* we do have PTYs if we have TCP */
#define LIBS_SYSTEM "-lsocket"		/* get TCP networking functions */
