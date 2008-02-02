/* machine description file for Gould PowerNodes with UTX/32 2.0 and 2.1.
   (See MACHINES for older versions.)

* NOTE: If you are running a pre-release of UTX/32 2.1 you should #define
* RELEASE2_1 in config.h. This may also be necessary with un-updated
* official releases of 2.1

   Copyright (C) 1986 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: FSF 19.31. */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Gould Power Node (-machine=gould -opsystem=bsd4-2 or bsd4-3)
(gould.h; s-bsd4-2.h or s-bsd4-3.h)

  18.36 worked on versions 1.2 and 2.0 of the operating system.

  On UTX/32 2.0, use -opsystem=bsd4-3

  On UTX/32 1.2 and UTX/32S 1.0, use -opsystem=bsd4-2 and note that compiling 
  lib-src/sorted-doc tickles a compiler bug:  remove the -g flag to cc in the 
  makefile.

  Version 19 incorporates support for releases 2.1 and later of UTX/32.
  A site running a pre-release of 2.1 should #define RELEASE2_1 in config.h.
NOTE-END */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically */

#ifndef GOULD
#define GOULD
#endif

/* sel is an old preprocessor name on gould machines 
  - it is no longer needed and interferes with a variable in xmenu.c */
#undef sel

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

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

#define C_ALLOCA
#define	STACK_DIRECTION	-1	/* grows towards lower addresses on Gould UTX/32 */

/* No need to extend the user stack. */

/* If this is a 2.1 system, COFF will be predefined by cpp. If it's */
/* pre-2.1 COFF won't be defined, which is as it should be. */

#ifdef COFF

#define HEADER_INCL_IN_TEXT
#define COFF_BSD_SYMBOLS

/* Seems to be necessary with coff */
#define NO_REMAP

#ifndef GOULD_NP1
/* gould-np1.h includes this file */
/* keep the old value - don't skip over the headers */
#define KEEP_OLD_TEXT_SCNPTR
#define KEEP_OLD_PADDR
#ifndef RELEASE2_1
#define ADJUST_TEXTBASE
#endif				/*RELEASE2_1 */
#endif				/* GOULD_NP1 */

#ifdef IN_UNEXEC
/* make Gould NP and PN COFF look like USG COFF */
/* PN COFF */
#define aouthdr old_exec
/* PN COFF doesn't have a data_start or a_dtbase field in its */
/* optional header, so substitute a junk field */
#define a_dtbase a_ccvers
/* Gould COFF */
#define magic a_magic
#define tsize a_text
#define dsize a_data
#define bsize a_bss
#define entry a_entry
#define text_start a_txbase
#define data_start a_dtbase
#endif				/* IN_UNEXEC */

/* Define how to search all pty names.
 * This is for UTX 2.1 and greater on PN and all NP versions. It is only
 * accident that this happens to correspond to the same versions of UTX
 * as COFF does, but we'll take advantage of that here.
 */

/*#define USE_PTY_PAIR*/

#endif				/* COFF */

/* -g is sometimes broken on the Gould.  */

#define C_DEBUG_SWITCH

/* The GOULD machine counts the a.out file header as part of the text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* Machine-dependent action when about to dump an executable file.  */

#ifndef COFF
#define ADJUST_EXEC_HEADER   \
  unexec_text_start = hdr.a_txbase + sizeof (hdr);
#endif

/* We use the system's crt0.o.  Somehow it avoids losing
   with `environ' the way most standard crt0.o's do.  */

#define START_FILES "pre-crt0.o /lib/crt0.o"
