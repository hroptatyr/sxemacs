/* Mark end of data space to dump as pure, for XEmacs.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30. */


/* How this works:

 Fdump_emacs dumps everything up to my_edata as text space (pure).

 The files of Emacs are written so as to have no initialized
 data that can ever need to be altered except at the first startup.
 This is so that those words can be dumped as sharable text.

 It is not possible to exercise such control over library files.
 So it is necessary to refrain from making their data areas shared.
 Therefore, this file is loaded following all the files of Emacs
 but before library files.
 As a result, the symbol my_edata indicates the point
 in data space between data coming from Emacs and data
 coming from libraries.
*/

#include <config.h>

char my_edata[] = "End of Emacs initialized data";

/* Ensure there is enough slack in the .bss to pad with. */
#ifdef HEAP_IN_DATA
#define BSS_PADDING 0x1000
#else
#define BSS_PADDING 1
#endif

char my_ebss [BSS_PADDING];

