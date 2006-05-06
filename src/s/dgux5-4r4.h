/* Definitions file for GNU Emacs running on Data General's DG/UX
   version 5.4 Release 4.00 and above.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

#include "dgux5-4r3.h"

/* wait() is better off not operating as a struct.  Things go a bit pear. */
#undef _BSD_WAIT_FLAVOR

/* Some things won't compile for me with these (in dgux.h): */
#undef setpgrp
/*#undef getpgrp*/

/* Symbols missing - I suspect this is the culprit... */
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"

/* Prevent use of a (non-existent) debugging library. */
#define LIBS_DEBUG
