/* machine description file for WICAT machines.
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
   USUAL-OPSYS="usg5-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

#ifndef m68000
#define m68000
#endif

/* This flag is used only in alloca.s.  */
#define WICAT

/* Data type of load average, as read out of kmem.  */

#undef LOAD_AVE_TYPE

/* Convert that into an integer that is 100 for a load average of 1.0  */

#undef LOAD_AVE_CVT

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

/* For the Wicat C compiler version 4.2, this can be removed
   and the alloca in alloca.s used.  */
#define C_ALLOCA
#define	STACK_DIRECTION	-1  /* grows towards lower addresses on WICAT */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#undef NO_REMAP

/* For WICAT, define TAHOE_REGISTER_BUG if you have a pre-4.2 C compiler */

#define TAHOE_REGISTER_BUG

/* pagesize definition */

#define EXEC_PAGESIZE	0x1000

/* Delete this for WICAT sys V releases before 2.0.  */

#define	LIB_STANDARD "-lc-nofp"

/* Special magic number */

#define EXEC_MAGIC	MC68ROMAGIC

/* Special switches to give to ld.  */

#define LD_SWITCH_MACHINE "-e __start -N"

/*
 * Define optimflags if you want to optimize.
 *	- Set to null string for pre-4.2 C compiler
 *	- Set to "-O -Wopt,-O-f" for 4.2
 */

#define C_OPTIMIZE_SWITCH /* -O -Wopt,-O-f */

/* For WICAT version supporting PTYs and select (currently internal only) */

#ifdef HAVE_PTYS
#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'
#endif

/* there is a select() in libcurses.a that causes a conflict so use termlib */
#ifdef HAVE_SELECT
#undef TERMINFO
#define LIBS_TERMCAP "select.o -ltermlib"
#endif
