/* Definitions file for GNU Emacs running on the GNU Hurd.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h.  */
#define BSD4_2

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

#define LIBS_DEBUG

/* XXX emacs should not expect TAB3 to be defined.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* GNU needs its own crt0, and libc defines data_start.  */
#define ORDINARY_LINK
#define DATA_START ({ extern int data_start; (char *) &data_start; })

/* GNU now always uses the ELF format.  */
/*
 * everything is pdump now. --SY
 * #define UNEXEC "unexelf.o"
 */
#undef UNEXEC

/* Some losing code fails to include this and then assumes
   that because it is braindead that O_RDONLY==0.  */
#ifndef NOT_C_CODE
#include <fcntl.h>
#endif
