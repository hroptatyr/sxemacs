/* Definitions file for GNU Emacs running on Data General's DG/UX
   5.4 Release 2.xx systems.
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

#include "dgux.h"

/* In DGUX 5.4R2.xx the function inet_addr() returns a `struct
   in_addr' instead of the more common `unsigned long'.
   -pmr@pajato.com */

#define HAVE_BROKEN_INET_ADDR

#if 0  /* Shawn M. Carey <smcarey@mailbox.syr.edu> found this
	  caused trouble on DGUX 5.4.2.  */
#define LIBS_SYSTEM "-ldgc"
#endif
