/* Fixed-size block allocator -- include file.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.
*/

/* Synched up with: Not in FSF. */

/* Authorship:

   Ben Wing: December 1994, for 19.12.
 */

#ifndef INCLUDED_blocktype_h_
#define INCLUDED_blocktype_h_

#define Blocktype_declare(type)                                      \
  type *free;                                                        \
  int elsize;                                                        \
  type *tempel

void *Blocktype_newf (size_t elsize);
void Blocktype_allocf (void *b);
void Blocktype_free (void *bbb, void *el);

#define Blocktype_new(structype) \
  (structype *) Blocktype_newf (sizeof(*(((structype *) NULL)->free)))
#define Blocktype_alloc(b) (Blocktype_allocf (b), (b)->tempel)

#endif /* INCLUDED_blocktype_h_ */
