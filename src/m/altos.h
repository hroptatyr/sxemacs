/* altos machine description file	Altos 3068 Unix System V Release 2
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

#define LIB_STANDARD "-lc"

#ifdef __GNUC__
#define alloca __builtin_alloca
#define HAVE_ALLOCA
#else
#define C_ALLOCA		/* we have -lPW and alloca but it's broken!
				   <vsedev!ron> */
#endif

#define SWITCH_ENUM_BUG

#define NO_REMAP
#define STACK_DIRECTION -1

#undef TERMINFO

#undef CANNOT_DUMP
#define TERMCAP

#define LIBS_TERMCAP "-ltermlib"
#define SYSTEM_PURESIZE_EXTRA 100000
#define ALTOS

#ifdef __GNUC__
#define COFF_ENCAPSULATE
#endif
