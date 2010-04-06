/* Definitions file for GNU Emacs running on Sony's NEWS-OS 5.0.2
   Copyright (C) 1992, 1994 Free Software Foundation, Inc.

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

/* Use the SysVr4 file for at least base configuration. */

#include "usg5-4.h"

#define NEWSOS5

/* These will be defined by "m-mips.h". */
#undef START_FILES
#undef LIB_STANDARD

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lgen"

/* Disable use of "unexelf.c" and shared libraries, because
   "unexelf.c" doesn't work correctly on NEWS-OS.  "unexmips.c" does
   work correctly if the program is linked statically without ELF. */
#undef UNEXEC
#undef USG_SHARED_LIBRARIES

/* Use `ld' directly rather than ordinary link, because ordinary link
   can't produce a non-ELF executable.  */
#undef ORDINARY_LINK
#define LINKER "/usr/lib/cmplrs/cc/ld"
#define START_FILES "pre-crt0.o /usr/ccs/lib/crt1.o"
#define LIB_STANDARD "-lc /usr/ccs/lib/crtn.o /usr/ccs/lib/values-Xt.o"
