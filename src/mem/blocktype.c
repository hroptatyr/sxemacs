/* Fixed-size block allocator.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* Authorship:

   Ben Wing: December 1994, for 19.12.
 */

/*

------------------------------------------------------------------------------

A "block-type object" is used to efficiently allocate and free blocks
of a particular size.  Freed blocks are remembered in a free list and
are reused as necessary to allocate new blocks, so as to avoid as
much as possible making calls to malloc() and free().

This is a container object.  Declare a block-type object of a specific type
as follows:

struct mytype_blocktype {
  Blocktype_declare (mytype);
};

Use the following functions/macros:

   structype *Blocktype_new(structype)
      [MACRO] Create a new block-type object of the specified type.
      The argument to this call should be the type of object to be
      created, e.g. foobar_blocktype.
   type *Blocktype_alloc(b)
      [MACRO] Allocate a block of the proper type for the specified
      block-type object and return a pointer to it.
   Blocktype_free(b, block)
      Free a block of the type corresponding to the specified block-type
      object.
   Blocktype_delete(b)
      Destroy a block-type object and the memory allocated to it.

*/

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "blocktype.h"

typedef struct blocktype {
	Blocktype_declare(void);
} Blocktype;

struct block_internal {
	void *next;
};

void *Blocktype_newf(size_t elsize)
{
	Blocktype *b = xnew(Blocktype);
	b->elsize = max(elsize, sizeof(void *));
	b->free = 0;
	return (void *)b;
}

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
void Blocktype_allocf(void *bbb)
{
	((Blocktype*)bbb)->tempel = (void*)xmalloc(((Blocktype*)bbb)->elsize);
	return;
}
#else  /* !BDWGC */
void Blocktype_allocf(void *bbb)
{
	Blocktype *b = (Blocktype *) bbb;
	if (b->free) {
		b->tempel = b->free;
		b->free = ((struct block_internal *)(b->free))->next;
	} else
		b->tempel = (void *)xmalloc(b->elsize);
}
#endif	/* BDWGC */

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
void Blocktype_free(void *SXE_UNUSED(bbb), void *el)
{
	xfree(el);
	return;
}
#else  /* !BDWGC */
void Blocktype_free(void *bbb, void *el)
{
	Blocktype *b = (Blocktype *) bbb;
	((struct block_internal *)el)->next = b->free;
	b->free = el;
}
#endif	/* BDWGC */
