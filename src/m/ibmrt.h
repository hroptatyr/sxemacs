/* RTPC machine dependent defines 
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
   USUAL-OPSYS="bsd4-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

/* XEmacs change */
#ifndef ibmrt
#define ibmrt
#endif
#ifndef romp
#define romp /* unfortunately old include files are hanging around.  */
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double	/* For AIS (sysV) */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0)

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

#define HAVE_ALLOCA

#define DATA_START    0x10000000

/* The text segment always starts at a fixed address.
   This way we don't need to have a label _start defined.  */
#define TEXT_START 0

/* Taking a pointer to a char casting it as int pointer */
/* and then taking the int which the int pointer points to */
/* is practically guaranteed to give erroneous results */

#define NEED_ERRNO

#define SKTPAIR

/* Special switches to give the C compiler.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE "-Dalloca=_Alloca"
#endif

/* XEmacs addition: */
/* Under Mach at least, gcc doesn't seem to work as the linker. */
#ifdef MACH
#define START_FILES "pre-crt0.o"
#ifdef __GNUC__
#define LINKER "pcc"
#endif
#endif

/* Don't attempt to relabel some of the data as text when dumping.
   It does not work because their virtual addresses are not consecutive.
   This enables us to use the standard crt0.o.  */

#define NO_REMAP

/* Use the bitmap files that come with Emacs.  */
#define EMACS_BITMAP_FILES
