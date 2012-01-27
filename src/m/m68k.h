/* Machine description file for generic Motorola 68k.
   Copyright (C) 1985, 1995 Free Software Foundation, Inc.

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef m68k
#define m68k
#endif

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

#ifdef linux

#define NO_REMAP
#define TEXT_START 0
#endif

/* If compiling with GCC, let GCC implement alloca.  */
#if defined(__GNUC__) && !defined(alloca)
#ifndef HAVE_ALLOCA
#define alloca(n) __builtin_alloca(n)
#define HAVE_ALLOCA
#endif
#endif
#define ASSERT_VALID_POINTER(pnt) (assert ((((int) pnt) & 1) == 0))
