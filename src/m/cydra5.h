/* machine description file for Cydrome's CYDRA 5 mini super computer
   Copyright (C) 1988 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-3"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) x

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/*#define CANNOT_DUMP*/

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
#undef HAVE_ALLOCA

#define DATA_START    0x20000000

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP
#define	LIBS_MACHINE "-lsocket -lnsl"

/* Stack grows downward in memory.  */
#define	STACK_DIRECTION -1

/* The data section in a coff file must be aligned in the file.  */
#define	DATA_SECTION_ALIGNMENT	0xFFF

/* Compiler won't allow switch (x) when x is an enum.  */
#define	SWITCH_ENUM_BUG

/* Explain how pty filenames work.  */

#define PTY_ITERATION  for (i = 47; i >= 0; i--)
#define PTY_NAME_SPRINTF  sprintf (ptyname, "/dev/pty%03x", i);
#define PTY_TTY_NAME_SPRINTF  sprintf (ptyname, "/dev/ptm%03x", i);

/* We can't do interrupt-driven input, so don't let user try.  */

#define BROKEN_SIGIO
