/* machine description file for Dual machines using unisoft port.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
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
   USUAL-OPSYS="note"

NOTE-START
Dual running System V (-machine=dual -opsystem=usg5-2)

  As of 17.46, this works except for a few changes
  needed in unexec.c.

Dual running Uniplus (-machine=dual -opsystem=unipl5-2)

  Works, as of 17.51.
NOTE-END */


/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

#ifndef m68000
#define m68000
#endif

/* Data type of load average, as read out of kmem.  */
/* These are commented out since it does not really work in uniplus */
  
/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

/* Change some things to avoid bugs in compiler */

#define SWITCH_ENUM_BUG 1
