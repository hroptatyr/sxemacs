/* zlib interface for XEmacs.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

/* Synched up with: Not in FSF. */

/* Author: William Perry <wmperry@aventail.com> */

#include <emodules.h>

DEFUN ("compress", Fcompress, 1, 6, 0, /*
Return the compressed version of an object.
OBJECT is either a string or a buffer.
Optional argument LEVEL specifies how much to compress - valid range is 0 - 9.
Optional arguments START and END denote buffer positions for compressing
a portion of OBJECT.  The optional CODING argument specifies the coding
system the text is to be represented in while computing the digest.  This only
has meaning with MULE, and defaults to the current format of the data.
If ERROR-ME-NOT is nil, report an error if the coding system can't be
determined.  Else assume binary coding if all else fails.
*/
       (object, level, start, end, coding, error_me_not))
{
	return(Qnil);
}

DEFUN ("decompress", Fdecompress, 1, 5, 0, /*
Uncompress an object.
OBJECT is either a string or a buffer.
Optional arguments START and END denote buffer positions for decompressing
a portion of OBJECT.  The optional CODING argument specifies the coding
system the text is to be represented in while computing the digest.  This only
has meaning with MULE, and defaults to the current format of the data.
If ERROR-ME-NOT is nil, report an error if the coding system can't be
determined.  Else assume binary coding if all else fails.
*/
       (object, start, end, coding, error_me_not))
{
	return(Qnil);
}

void
syms_of_zlib (void)
{
  DEFSUBR(Fcompress);
  DEFSUBR(Fdecompress);
}

void
vars_of_zlib (void)
{
  Fprovide (intern ("zlib"));
}
