/* Definitions file for GNU Emacs running on IBM AIX version 3.1
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

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

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG			/* System III, System V, etc */
#define USG5

/*      Specify IBM AIX version of system */

#ifndef AIX
#define AIX
#endif

/*      turn off c prototypes */
/* XEmacs change: XEmacs compiles fine with prototypes under AIX, dkeller@vnet.ibm.com
#ifndef _NO_PROTO
#define _NO_PROTO
#endif
*/

/*      This symbol should be defined on AIX Version 3  ??????? */
#ifndef _AIX
#define _AIX
#endif

/*      Specify "_BSD" to invoke Berkeley compatibility in header files */
/*#ifndef _BSD
#define _BSD
#endif
*/

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "aix"

/* In AIX, you allocate a pty by opening /dev/ptc to get the master side.
   To get the name of the slave side, you just ttyname() the master side.  */

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"
/* Special itemss needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

/* #define ADDR_CORRECT(x) (x) */

#ifndef __GNUC__
#define LINKER "cc"
#endif

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* No need to specify -lc when linking.  */

#define LIB_STANDARD

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* The following definition seems to be needed in AIX version 3.1.6.8.
   It may not have been needed in certain earlier versions.  */
#define HAVE_TCATTR

#define SYSTEM_MALLOC

/* AIX doesn't define this.  */
#define unix 1

/* AIX 3.1 has the HFT features.  */
#define AIXHFT

/* For unexaix.c. */
#define ALIGN_DATA_RELOC
