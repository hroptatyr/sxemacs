/* machine description file for Apollo machine.
   Copyright (C) 1985, 1986, 1994, Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-3"  */

/* Say this machine is a 68000 */

		       /* #define m68000 *//* Done by the C compiler */

#define APOLLO

/* Assume we use s-bsd4-3.h for system version 10.  */

#ifdef BSD4_3
#define APOLLO_SR10
#endif

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

/* Define HAVE_ALLOCA because we use the system's version of alloca.  */

#define HAVE_ALLOCA

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* Can't use the system's termcap.  It has compressed data sections that
   interfere with dumping.  That means we won't automatically get a vt100
   when we start up emacs in a dm pad (a dubious feature at best anyway). */

#undef LIBS_TERMCAP

/* Must use the system's malloc and alloca.  */

#define SYSTEM_MALLOC

/* Define the file we use for UNEXEC. */

#define UNEXEC "unexapollo.o"

/* The Apollo linker does not recognize the -X switch, so we remove it here. */

#define LD_SWITCH_SYSTEM

/* Define C_SWITCH_MACHINE to compile for 68020/68030 or PRISM.
   Define LD_SWITCH_MACHINE to save space by stripping symbols
   and use X11 libraries. */

#if _ISP__A88K
#define C_SWITCH_MACHINE "-W0,-ncompress -W0,-opt,2 -A cpu,a88k -A sys,any -A run,bsd4.3"
#define LD_SWITCH_MACHINE "-A cpu,a88k -A sys,any -A run,bsd4.3"
#else
#define C_SWITCH_MACHINE "-W0,-ncompress -W0,-opt,2 -A cpu,3000 -A sys,any -A run,bsd4.3"
#define LD_SWITCH_MACHINE "-A cpu,m68k -A sys,any -A run,bsd4.3"
#endif

#define OLDXMENU_OPTIONS "${C_SWITCH_MACHINE}"

#if 0				/* from FSF Emacs */

/* In SR10.4, unistd.h has bad prototype for getpgrp, so we don't include it. */
#undef HAVE_UNISTD_H
#endif				/* 0 */
