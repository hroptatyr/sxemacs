/* amdahl machine description file 
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/*
This file for amdahl_uts created by modifying the template.h
by Jishnu Mukerji 3/1/87

The following line tells the configuration script what sort of
operating system this machine is likely to run.
USUAL-OPSYS="usg5-2-2"

This file works with the Amdahl uts native C compiler. The 5.2u370
compiler is so brain damaged that it is not even worth trying to use it.
*/

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

/* uts gets defined automatically */
/* However for clarity define amdahl_uts */
#define amdahl_uts

/* Data type of load average, as read out of kmem.  */

/* #define LOAD_AVE_TYPE long*/

/* Convert that into an integer that is 100 for a load average of 1.0  */

/*#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0)*/

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/*#define HAVE_ALLOCA */

#ifdef HAVE_ALLOCA
#define LIB_STANDARD "-lPW -lc"
#endif

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/*#define NO_REMAP*/

#define TERMINFO

/* The usual definition of XINT, which involves shifting, does not
   sign-extend properly on this machine.  */

#define XREALINT(i) (((sign_extend_temp=(i)) & 0x00800000) \
		     ? (sign_extend_temp | 0xFF000000) \
		     : (sign_extend_temp & 0x00FFFFFF))

#ifdef emacs /* Don't do this when making xmakefile! */
extern int sign_extend_temp;
#endif

/* The following needed to load the proper crt0.o and to get the
   proper declaration of data_start in the #undef NO_REMAP case */

#ifndef NO_REMAP
#define START_FILES "pre-crt0.o /lib/crt0.o"
#endif

/* Perhaps this means that the optimizer isn't safe to use.  */

#define C_OPTIMIZE_SWITCH

/* Put text and data on non-segment boundary; makes image smaller */

#define LD_SWITCH_MACHINE "-N"

/* When writing the 'xemacs' file, make text segment ro */
#define EXEC_MAGIC	0410

/* Mask for address bits within a memory segment */
#define SEGSIZ 0x10000		/* Should this not be defined elsewhere ? */
#define SEGMENT_MASK (SEGSIZ - 1)

/* Tell alloca.c which direction stack grows.  */
#define STACK_DIRECTION -1

/* Compensate for error in signal.h.  */
#if NSIG==19
#undef NSIG
#define NSIG 20
#endif
