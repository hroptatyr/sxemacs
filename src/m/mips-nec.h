/* m- file for Mips machines.
   Copyright (C) 1987, 1992, 1993, 1995 Free Software Foundation, Inc.

   This file contains some changes for our SVR4 based SINIX-Mips 5.4.
   I hope this is helpful to port the emacs to our RM?00 series and
   maybe to the DC/OSx (Mips-based) machines of Pyramid Inc.
     (Marco.Walther@mch.sni.de)

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
   USUAL-OPSYS="note"

NOTE-START
Use m-mips4.h for RISCOS version 4; use s-bsd4-3.h with the BSD world.
Note that the proper m- file for the Decstation is m-pmax.h.
This is the m- file for SNI RM*00 machines. Use s- sinix5-4.h file!
With this the file mips-siemens.h is obsolete.
NOTE-END  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef mips
#	define mips
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / 256.0)

/* CDC EP/IX 1.4.3 uses /unix */

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"

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

#ifdef __GNUC__
#define HAVE_ALLOCA
#else
#define C_ALLOCA
#endif

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Describe layout of the address space in an executing process.  */
/* MARCO ???
*/
#define TEXT_START 0x400000

#undef ORDINARY_LINK

#undef LIBS_DEBUG

/* Alter some of the options used when linking.  */

#undef LIBS_MACHINE
				   /* #define LIBS_MACHINE "-lmld" *//* mrb */
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o"

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#endif
#define LIB_STANDARD "-lc /usr/ccs/lib/crtn.o"

#ifdef __GNUC__
#define C_DEBUG_SWITCH
#define C_OPTIMIZE_SWITCH "-O"
#define LD_SWITCH_MACHINE
#else
#define C_DEBUG_SWITCH "-DSYSV"
#define C_OPTIMIZE_SWITCH "-DSYSV "
#define LD_SWITCH_MACHINE
#endif
