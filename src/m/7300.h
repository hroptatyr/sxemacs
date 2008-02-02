/* machine description file for AT&T UNIX PC model 7300
   Copyright (C) 1986 Free Software Foundation, Inc.
   Modified for this machine by mtxinu!rtech!gonzo!daveb

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
   USUAL-OPSYS="usg5-2-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

# ifndef mc68k
# define mc68k
# endif
#ifndef m68k
#define m68k
#endif

/* Cause crt0.c to define errno.  */

#define NEED_ERRNO

/* Data type of load average, as read out of kmem.  */
/* These are commented out since it is not supported by this machine.  */

/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

#ifdef __GNUC__

#define HAVE_ALLOCA

#else

#define SWITCH_ENUM_BUG
#define C_ALLOCA
#define STACK_DIRECTION -1

#endif

/* If you have the PD pty driver installed, uncomment the following line.  */
/* #define HAVE_PTYS */

/* We don't have memmove.  */
#define memmove(d, s, n) safe_bcopy (s, d, n)

/* These three lines were new in 18.50.  They were said to permit
   a demand-paged executable, but someone else says they don't work.
   Someone else says they do.  They didn't work because errno was an
   initialized variable in crt0.c, and because of %splimit (also therein),
   both of which have been fixed now. */
#define SECTION_ALIGNMENT 0x03ff
#define SEGMENT_MASK 0xffff
#define LD_SWITCH_MACHINE "-z"

/* Insist on using cc when compiling this.  GCC may have been
   configured to use GAS syntax, which causes problems.  */
#define CRT0_COMPILE "cc -c -O -Demacs"
