/* Definitions file for GNU Emacs running on Ultrix (bsd 4.3)
   Copyright (C) 1993 Free Software Foundation, Inc.

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


/* Synched up with: Not in FSF. */

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "ultrix"

/* We don't have a built-in strdup() function */
#define NEED_STRDUP
