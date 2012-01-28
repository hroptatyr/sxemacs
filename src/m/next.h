/* Configuration file for the NeXT machine.
   Copyright (C) 1990 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.31. and Emacs for NeXTstep 4.1 */

/* Say this machine is a next if not previously defined */

#ifndef NeXT
#define NeXT
#endif

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* Mask for address bits within a memory segment */

#define SEGSIZ 0x20000
#define SEGMENT_MASK (SEGSIZ - 1)

#define HAVE_UNIX_DOMAIN

#define LIB_X11_LIB "-L/usr/lib/X11 -lX11"

/* This avoids a problem in Xos.h when using co-Xist 3.01.  */
#define X_NOT_POSIX

/* Conflicts in process.c between ioctl.h & tty.h use of t_foo fields */

#define NO_T_CHARS_DEFINES

/* Use our own unexec routines */
/*
 * everything is pdump now. --SY
 * #define UNEXEC "unexnext.o"
 */
#undef UNEXEC

/* We don't have a g library either, so override the -lg LIBS_DEBUG switch */

#define LIBS_DEBUG

/* We don't have a libgcc.a, so we can't let LIB_GCC default to -lgcc */

#define LIB_GCC

/* Link this program just by running cc.  */
#define ORDINARY_LINK

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START 0
/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext ()
/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata ()

/* Defining KERNEL_FILE causes lossage because sys/file.h
   stupidly gets confused by it.  */
#undef KERNEL_FILE

#define LD_SWITCH_MACHINE

/* #define environ _environ */

/* XEmacs change from Barry Warsaw. */
#ifndef NOT_C_CODE
/* this is only typedef'd in types.h if _POSIX_SOURCE is defined
 * but the problem with that is that compiling with -posix links
 * in -lposix instead of -lsys_s, and the latter defines some
 * important NeXT AppKit symbols.
 */
typedef unsigned short mode_t;
#endif				/* ! NOT_C_CODE */

#define ASSERT_VALID_POINTER(pnt) (assert ((((int) pnt) & 1) == 0))
