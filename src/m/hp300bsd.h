/* machine description file for HP300 running BSD 4.3
   Copyright (C) 1985, 1986, 1989 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

/* One CRT0 Dummy variable */

#define CRT0_DUMMIES one_dummy,

#define HAVE_ALLOCA

#define LOAD_AVE_TYPE long
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) / 2048.0 * 100.0))
