/* Machine description file for Acorn RISCiX machines.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* ARM note - this is done by the Norcroft compiler - symbol is `__arm' */

#ifdef LDAV_SYMBOL
#undef LDAV_SYMBOL
#endif

#define LDAV_SYMBOL "_iavenrun"


/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

/*
 * Scale factor for scaled integers used to count
 * %cpu time and load averages.
 */

/* FSHIFT and FSCALE are defined in param.h, but are required by
   LOAD_AVE_CVT, so they need to be defined here.  */

#ifndef FSHIFT
#define FSHIFT	8	/* bits to right of fixed binary point */
#endif

#ifndef FSCALE
#define FSCALE	(1<<FSHIFT)
#endif

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* This prevents Emacs dumping an unsqueezed binary with the
   SQUEEZE bit set in the magic number. */

#define ADJUST_EXEC_HEADER {hdr.a_magic &= ~MF_SQUEEZED;}

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#ifdef __GNUC__

/* Use builtin alloca. Also be sure that no other ones are tried out. */
#define alloca __builtin_alloca
#define HAVE_ALLOCA

/* Keep gcc/RISCiX happy - it uses __gccmain where other versions of
   gcc use __main, because of a library routine name clash. */
#define __main __gccmain

#else
#define C_ALLOCA
#undef HAVE_ALLOCA
#endif  /* __GNUC__ */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP


#ifndef NOT_C_CODE
#define TEXT_START	0x8000
#define DATA_END	&_edata
extern int _edata;
#define etext _etext
#endif

/* Avoid debugging library */
#define LIBS_DEBUG

/* Avoid sharing libc */
#define LIB_STANDARD "-lc_n"

/* Avoid sharing libX11 */
#define LIB_X11_LIB "-lX11_n"

/* All kinds of symbol definitions, so as to avoid multiply defined symbol
   errors from the RISCiX linker. */

#ifdef __GNUC__
#define C_DEBUG_SWITCH

#define C_OPTIMIZE_SWITCH "-O1 -fomit-frame-pointer -w -g -Dgetopt=gnu_getopt -Dopterr=gnu_opterr -Doptind=gnu_optind -Doptarg=gnu_optarg -Dcfree=gnu_cfree -D___type="

#else
#define C_DEBUG_SWITCH "-O -w -g -Dgetopt=gnu_getopt -Dopterr=gnu_opterr -Doptind=gnu_optind -Doptarg=gnu_optarg -Dcfree=gnu_cfree"
#endif

/* Turn this on to avoid the emacs malloc and use standard one */

#undef SYSTEM_MALLOC

/* For the portable alloca */
#define STACK_DIRECTION -1

#ifdef NO_REMAP
/* CRT0_O is defined in s/riscix1-1.h or s/riscix1-2.h, as appropriate. */
#define START_FILES "pre-crt0.o CRT0_O"
#else
Cannot
do
this
yet
#endif
