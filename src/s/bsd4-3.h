/* Definitions file for GNU Emacs running on bsd 4.3
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

#include "bsd4-2.h"

#undef BSD4_2

/* We give these symbols the numeric values found in <sys/param.h> to
   avoid warnings about redefined macros.  */
#ifndef BSD4_3
#define BSD4_3 1
#endif /* BSD4_3 */

#ifdef BSD_WAS_DEFINED
#undef BSD
#endif

#ifndef BSD
#define BSD 43
#endif /* BSD */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Apparently not needed any more? */

#undef SIGIO_REQUIRES_SEPARATE_PROCESS_GROUP
