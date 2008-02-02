/* machine description file for HLH Orion.
   Copyright (C) 1985 Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-2"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef orion
#define orion
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#ifndef FSCALE
#define FSCALE 1.0
#endif
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* It is necessary to use the portable imitation of alloca,
   since a true stack-allocating one is impossible.  */

#define C_ALLOCA
