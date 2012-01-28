/* Machine description file for PFU A-series.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* Say this machine is a 68000 */

#define m68000
#define mc68000 1

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Define TEXT_START_ADDR if your linker don't set execute point to _start.
   If it needed, temacs always CORE-DUMP.	*/

#define TEXT_START_ADDR __start

/* Define START_FILES if your machine used _start.
 */

#define START_FILES "crt0.o"

/* Define LD_SWITCH_MACHINE if your linker needs it.
 */

#define LD_SWITCH_MACHINE "-e __start"

/* SX/A has alloca in the PW library.  */

#define LIB_STANDARD "-lPW -lc"
#ifndef HAVE_ALLOCA
#define HAVE_ALLOCA
#endif

/* SX/A uses terminfo and lib/curses   */

#define TERMINFO

#define HAVE_PTYS

/* SX/A use SystemV style getdents/readdir. */

#define NO_SIOCTL_H

#define BROKEN_SIGIO
