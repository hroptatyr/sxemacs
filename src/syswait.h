/* Define wait system call interface for Emacs.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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


#ifndef INCLUDED_syswait_h_
#define INCLUDED_syswait_h_

#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(s) ((s) >> 8)
#endif
#ifndef WIFEXITED
#define WIFEXITED(s) (((s) & 0xff) == 0)
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(s) (((s) & 0xff) == 0x7f)
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(s) (((unsigned int)((s)-1) & 0xffff) < 0xff)
#endif
#ifndef WCOREDUMP
#define WCOREDUMP(s) ((s) & 0200)
#endif
#ifndef WTERMSIG
#define WTERMSIG(s) ((s) & 0x7f)
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(s) ((s) >> 8)
#endif
#ifndef WRETCODE
#define WRETCODE(s) ((s) >> 8)
#endif

#endif				/* INCLUDED_syswait_h_ */
