/* Definitions file for GNU Emacs running on UMAX 4.2
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

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
/* #define USG */
/* #define BSD4_1 */
#define BSD4_2
/* #define BSD4_3 */
#define BSD
#define UMAX4_2
#define UMAX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "berkeley-unix"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* The file containing the kernel's symbol table is called /vmunix.  */

#define KERNEL_FILE "/vmunix"

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Specify alignment requirement for start of text and data sections
   in the executable file.  */

#define SECTION_ALIGNMENT pagemask


#define SEGMENT_MASK	(64 * 1024 - 1)


/* crt0.c needs this for compilation because it uses asm.  */  

#define C_SWITCH_ASM -q nodirect_code

/* Encore machines with APC processor boards align sections on 4M
   boundaries, so it is not easy to remap the start of the text segment
   in the unexec() routine.  For them you need the following two lines.
   For DPC processors you can enable these or not, as you wish, but
   you will get better performance without them.  */

/* #define NO_REMAP
   #define TEXT_START 0
*/

/* Formerly "BSD_PGRPS" */

#define SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP

