/* Definitions file for GNU Emacs running on Stride Micro System-V.2.2
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
   USUAL-OPSYS="usg5-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe, APOLLO and STRIDE
   are the ones defined so far.  */

#define m68000			/* because the SGS compiler defines "m68k" */
#ifndef STRIDE
#define STRIDE
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* The STRIDE system is more powerful than standard USG5.  */

#define HAVE_PTYS
#define SKTPAIR

#define MAIL_USE_FLOCK
#undef TERMINFO
#define EXEC_MAGIC 0413

/* USG wins again: Foo! I can't get SIGIO to work properly on the Stride, because I'm
   running a System V variant, and don't have a reliable way to block SIGIO
   signals without losing them.  So, I've gone back to non-SIGIO mode, so
   please append this line to the file "stride.h":
 */
#define BROKEN_SIGIO

/* Specify alignment requirement for start of text and data sections
   in the executable file.  */

#define SECTION_ALIGNMENT (getpagesize() - 1)

