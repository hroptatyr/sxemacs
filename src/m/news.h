/* machine description file for Sony's NEWS workstations, NEWS-OS 3.0.
   Copyright (C) 1985, 1986, 1989 Free Software Foundation, Inc.

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
Use -opsystem=bsd4-2, or -opsystem=bsd4-3 for system release 3.
NOTE-END  */

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

/* One CRT0 Dummy variable */

#if 0				/* larry@mitra.com says Sony's as doesn't like this.  */
#define CRT0_DUMMIES one_dummy,
#endif

/* The News machine has alloca. */

#define HAVE_ALLOCA

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP "-ltermcap"
