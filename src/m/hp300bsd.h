/* machine description file for HP300 running BSD 4.3
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
   USUAL-OPSYS="bsd4-3"  */

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

/* One CRT0 Dummy variable */

#define CRT0_DUMMIES one_dummy,

#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif

#define LOAD_AVE_TYPE long
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) / 2048.0 * 100.0))
