/* Machine description file for the Motorola Delta.
   Tested on mvme147 board using R3V7 without X.  Tested with gcc.
   Tested on mvme167 board using R3V7 without X.  Tested with cc, gnucc, gcc.
   Copyright (C) 1986, 1993, 1994 Free Software Foundation, Inc.

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define m68000
#define MOTOROLA_DELTA

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
/* #define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Machine specific stuff */
#define HAVE_PTYS
#define SYSV_PTYS
#define BROKEN_CLOSEDIR		/* builtin closedir is interruptible */
#define memmove(t,f,s) safe_bcopy(f,t,s)	/* for overlapping copies */
#undef KERNEL_FILE
#define KERNEL_FILE "/sysv68"
#undef LDAV_SYMBOL

/* The standard C library is -lc881, not -lc.
   DO NOT USE -lPW. That version of alloca is broken in versions R3V5,
   R3V6, R3V7. -riku@field.fi -pot@cnuce.cnr.it. */

#define LIB_STANDARD "-lc881"
#define LIB_MATH "-lm881"
#define LIBS_TERMCAP "-lcurses"
#define LIBS_SYSTEM "-lbsd"

#ifdef HAVE_X_WINDOWS
# define HAVE_RANDOM
# define HAVE_XSCREENNUMBEROFSCREEN
# undef LIB_X11_LIB		/* no shared libraries */
# define LIB_X11_LIB "-lX11"
# undef USG_SHARED_LIBRARIES	/* once again, no shared libs */
# undef LIBX11_SYSTEM		/* no -lpt as usg5-3.h expects */
# define LIBX11_SYSTEM "-lnls -lnsl_s"
#endif				/* HAVE_X_WINDOWS */

#ifdef __GNUC__
 /* Use builtin alloca. Also be sure that no other ones are tried out. */
# define alloca __builtin_alloca
# define HAVE_ALLOCA

 /* We are assuming here that the `true' GNU gcc has not been
    installed, and we are using the gnucc provided by Motorola.  No
    support exists for compiling with GNU gcc, as I do not have it on
    my machine to try it out.  -pot@cnuce.cnr.it
    If __STDC__ is defined gnucc has been called without the -traditional
    option, that is, we are inside configure.  If THIS_IS_CONFIGURE is
    not defined, then configure is trying to figure out what the right
    option for real compilation are.
    Let us set -traditional, because gmalloc.c includes <stddef.h>, and
    we don't have that (as of SYSV68 R3V7). */
#  define C_SWITCH_MACHINE "-mfp0ret -traditional -Dconst= -fdelayed-branch -fstrength-reduce -fno-inline -fcaller-saves"
#  define LIB_GCC "/lib/gnulib881"

#else
 /* Not __GNUC__, use the alloca in alloca.s. */

 /* Try to guess if we are using the Green Hills Compiler */
# if defined mc68000 && defined MC68000
   /* Required only for use with Green Hills compiler:
      -ga        Because alloca relies on stack frames. This option forces
      the Green Hills compiler to create stack frames even for
      functions with few local variables. */
#  define C_SWITCH_MACHINE "-ga -O"
#  define GAP_USE_BCOPY		/* *++to = *++from  is inefficient */
#  define BCOPY_UPWARD_SAFE 0
#  define BCOPY_DOWNWARD_SAFE 1	/* bcopy does: mov.b (%a1)+,(%a0)+ */
# else
 /* We are using the standard AT&T Portable C Compiler */
#  define SWITCH_ENUM_BUG
# endif

#endif				/* not __GNUC__ */

#ifdef HAVE_INET_SOCKETS	/* this comes from autoconf  */
# define HAVE_SOCKETS		/* NSE may or may not have been installed */
#endif
