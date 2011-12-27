/* machine description file for Sun's 386-based RoadRunner.  This file borrows heavily from
  "sun2.h", but since that file is heavily cpu-specific, it was easier
  not to include it.

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
   USUAL-OPSYS="note"

NOTE-START
  Use s-sunos4-0.h for operating system version 4.0, and s-sunos4-1.h
  for later versions.  See the file share-lib/SUNBUG for how to solve
  problems caused by bugs in the "export" version of SunOS 4.
NOTE-END  */

/* Say this machine is a bird */
#ifndef roadrunner
#define roadrunner
#endif

/* Actual cpu-specific defs */
#include "intel386.h"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Underscores are not prepended to C symbols on this machine.  */
#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP "-ltermcap"

/* Arrange to link with sun windows, if requested.  */
/* For details on emacstool and sunfns, see etc/SUN-SUPPORT */
/* These programs require Sun UNIX 4.2 Release 3.2 or greater */

#ifdef HAVE_SUN_WINDOWS
#define OTHER_FILES " ${etcdir}emacstool"
#define LIBS_MACHINE "-lsuntool -lsunwindow -lpixrect"
#define OBJECTS_MACHINE "sunfns.o"
#define SYMS_MACHINE syms_of_sunfns ()
#define SYSTEM_PURESIZE_EXTRA 12000
#endif

/* Roadrunner uses 'COFF' format */
#define COFF

/* XEmacs change:  from Thomas.Tornblom@nexus.comm.se */
#ifdef USE_GCC
#define C_SWITCH_MACHINE "-static -Dmode_t=\"unsigned short\""	/* avoid dynamic linking */
#define LD_SWITCH_MACHINE "-Wl,-N -static"
#else
#define C_SWITCH_MACHINE "-Bstatic -Dmode_t=\"unsigned short\""	/* avoid dynamic linking */
#define LD_SWITCH_MACHINE "-N -Bstatic"
#endif

/* Get rid of the -e __start that s-sunos4.h does.  */
#undef LD_SWITCH_SYSTEM

/* XEmacs addition? */
#undef RUN_TIME_REMAP
#undef UNEXEC
<<<<<<< HEAD
<<<<<<< HEAD
#define UNEXEC "unexec.o"
=======
=======
>>>>>>> master

/* 
 * everything is pdump now. --SY
 * #define UNEXEC "unexec.o"
 */
<<<<<<< HEAD
>>>>>>> origin/master
=======
>>>>>>> master
