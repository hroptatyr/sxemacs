/* machine description file for Sun 4 SPARC.
   Copyright (C) 1987, 1994 Free Software Foundation, Inc.

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
Use -opsystem=sunos4 for operating system version 4, and
-opsystem=bsd4-2 for earlier versions.
NOTE-END  */

/* XEmacs change */
/* Say this machine is a sparc if we are not generating the Makefiles.
   In that case say we are a SPARC.  Otherwise people who have sparc
   in a path will not be happy. */

#ifdef NOT_C_CODE
# define SPARC
#else
# ifndef sparc
#  define sparc
# endif
#endif

/* Mask for address bits within a memory segment */

#define SEGMENT_MASK (SEGSIZ - 1)

#if ! defined (__NetBSD__) && ! defined (__linux__) && !defined (__OpenBSD__)
/* This really belongs in s/sun.h.  */

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

#endif				/* not Linux or NetBSD or OpenBSD */
