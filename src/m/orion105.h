/* machine description file for HLH Orion 1/05 (Clipper).
   Copyright (C) 1985 Free Software Foundation, Inc.
   Lee McLoughlin <lmjm%doc.imperial.ac.uk@nss.cs.ucl.ac.uk>

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

/* Data type of load average, as read out of kmem.  */
/* This used to be `double'.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* This used to be 1.0.  */
#ifndef FSCALE
#define FSCALE 256
#endif
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

#define HAVE_ALLOCA

/* Here is where programs actually start running */
#define TEXT_START 0x8000
#define LD_TEXT_START_ADDR 8000

/* Arguments to ignore before argc in crt0.c.  */
#define DUMMIES dummy1, dummy2,
