/* RTPC AIX machine/system dependent defines
   Copyright (C) 1988 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-2-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef IBMAIX
#define IBMAIX
#endif

/* No load average information appears in the AIX kernel.  VRM has this
   info, and if anyone desires they should fix fns.c to get it out of VRM */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define addresses, macros, change some setup for dump */

#undef COFF
#define NO_REMAP

#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
#define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
#define STACK_DIRECTION -1	/* tell alloca.c which way it grows */

/* AIX has PTYs, so define here, along with macros needed to make them work. */

#define HAVE_PTYS
#define PTY_ITERATION for (i=0; i<256; i++)
#define PTY_NAME_SPRINTF sprintf (ptyname, "/dev/ptc%d", i);

#define PTY_TTY_NAME_SPRINTF				\
{ /* Check that server side not already open */		\
  if ((ioctl (*ptyv, PTYSTATUS, 0) & 0xFFFF) != 0)	\
    {							\
      close (*ptyv);					\
      continue;						\
    }							\
  /* And finally to be sure we can open it later */	\
  sprintf (ptyname, "/dev/pts%d", i);			\
  signal (SIGHUP,SIG_IGN);				\
}				/* ignore hangup at process end */

/* TIOCNOTTY doesn't occur on AIX, but the rest
   of the conditionalized code in process.c does
   the right thing if we fake this out.  */
#define TIOCNOTTY IOCTYPE

		      /* #define SKTPAIR *//* SKTPAIR works, but what is advantage over pipes? */

/* Specify the font for X to use.  */

#define X_DEFAULT_FONT "Rom14.500"

/* Here override various assumptions in ymakefile */

/* On AIX 2.2.1, use these definitions instead
#define C_SWITCH_MACHINE "-I/usr/include -Nn2000"
#define LIBS_MACHINE "-lX -lrts"
*/

#define C_SWITCH_MACHINE "-I/usr/include -I/usr/include/bsd -Nn2000"
#define LIBS_MACHINE "-lXMenu -lX -lsock -lbsd -lrts"

#define OBJECTS_MACHINE "hftctl.o"
#define START_FILES "/lib/crt0.o"
/* -lXMenu, -lX must precede -lsock, -lbsd */
#define LD_SWITCH_MACHINE "-n -T0x10000000 -K -e start"

#if 0				/* I refuse to promulgate a recommendation that would make
				   users unable to debug - RMS.  */
/* delete the following line to foil optimization, enable debugging */
#define C_DEBUG_SWITCH "-O"
#endif

/* Setup to do some things BSD way - these won't work previous to AIX 2.1.2 */

#include </usr/include/bsd/BSDtoAIX.h>

/* AIX utimes allegedly causes SIGSEGV.  */
#undef HAVE_UTIMES		/* override configuration decision */

/* rocky@watson.ibm.com says this is needed.  */
#define HAVE_FTIME
