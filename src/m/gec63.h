/* machine description file for gec63
   Copyright (C) 1986 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-2"  */

/* Say this machine is a 68000 */

#define gec63

/* GEC63 has alloca in the PW/ux63 library.  */
#define LIB_STANDARD "-lPW -lc"
#define HAVE_ALLOCA

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

#undef ADDR_CORRECT(x)

#undef TERMCAP
#define TERMINFO

#define NO_REMAP

/* The rest of the file certainly needs updating for Emacs 19.29!  */

/* I'm certain none of this works with XEmacs. -jwz */
#error fixme

/* Define sizes of portions of a Lisp_Object.  */
#define VALBITS 24

#define VALAMASK (((1<<VALBITS) - 1)| 0xF0000000L)

#define XTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#define XSETTYPE(a, b) ((a)  =  ((a) & VALAMASK)  +  ((int)(b) << VALBITS))

#define XPNTR(a) ((a) & VALAMASK)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + ((int) (ptr) & VALAMASK))

/* Move some garbage-collector flag bits to different bit positions.  */
#define ARRAY_MARK_FLAG (1 << 27)
