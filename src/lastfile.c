/* Mark end of data space to dump as pure, for SXEmacs.
   Copyright (C) 1985 Free Software Foundation, Inc.

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

char my_ebss[BSS_PADDING];
