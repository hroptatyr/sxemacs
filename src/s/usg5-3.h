/* Definitions file for GNU Emacs running on AT&T's System V Release 3
   Copyright (C) 1987 Free Software Foundation, Inc.

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

#include "usg5-2-2.h"

#define USG5_3

/* Some versions of V.3 have this, but not all.
   #define HAVE_PTYS
   #define SYSV_PTYS  */

/* 5.3 apparently makes close() interruptible */

#define INTERRUPTIBLE_CLOSE

/* Apparently -lg is provided in 5.3 */

#undef LIBS_DEBUG

/* Some variants have TIOCGETC, but the structures to go with it
   are not declared.  */

#define BROKEN_TIOCGETC

/* Some variants have TIOCGWINSZ, but the structures to go with it
   are not declared.  */

#define BROKEN_TIOCGWINSZ

/* Enable support for shared libraries in unexec.  */

#define USG_SHARED_LIBRARIES
