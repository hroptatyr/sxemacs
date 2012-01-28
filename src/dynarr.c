/* Simple 'n' stupid dynamic-array module.
   Copyright (C) 1993 Sun Microsystems, Inc.

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


/* Synched up with:  Not in FSF. */

/* Written by Ben Wing, December 1993. */

/*

A "dynamic array" is a contiguous array of fixed-size elements where there
is no upper limit (except available memory) on the number of elements in the
array.  Because the elements are maintained contiguously, space is used
efficiently (no per-element pointers necessary) and random access to a
particular element is in constant time.  At any one point, the block of memory
that holds the array has an upper limit; if this limit is exceeded, the
memory is realloc()ed into a new array that is twice as big.  Assuming that
the time to grow the array is on the order of the new size of the array
block, this scheme has a provably constant amortized time (i.e. average
time over all additions).

When you add elements or retrieve elements, pointers are used.  Note that
the element itself (of whatever size it is), and not the pointer to it,
is stored in the array; thus you do not have to allocate any heap memory
on your own.  Also, returned pointers are only guaranteed to be valid
until the next operation that changes the length of the array.

This is a container object.  Declare a dynamic array of a specific type
as follows:

typedef struct
{
  Dynarr_declare (mytype);
} mytype_dynarr;

Use the following functions/macros:

   void *Dynarr_new(type)
      [MACRO] Create a new dynamic-array object, with each element of the
      specified type.  The return value is cast to (type##_dynarr).
      This requires following the convention that types are declared in
      such a way that this type concatenation works.  In particular, TYPE
      must be a symbol, not an arbitrary C type.

   Dynarr_add(d, el)
      [MACRO] Add an element to the end of a dynamic array.  EL is a pointer
      to the element; the element itself is stored in the array, however.
      No function call is performed unless the array needs to be resized.

   Dynarr_add_many(d, base, len)
      [MACRO] Add LEN elements to the end of the dynamic array.  The elements
      should be contiguous in memory, starting at BASE.

   Dynarr_insert_many_at_start(d, base, len)
      [MACRO] Append LEN elements to the beginning of the dynamic array.
      The elements should be contiguous in memory, starting at BASE.

   Dynarr_insert_many(d, base, len, start)
      Insert LEN elements to the dynamic array starting at position
      START.  The elements should be contiguous in memory, starting at BASE.

   int Dynarr_length(d)
      [MACRO] Return the number of elements currently in a dynamic array.

   int Dynarr_largest(d)
      [MACRO] Return the maximum value that Dynarr_length(d) would
      ever have returned.

   type Dynarr_at(d, i)
      [MACRO] Return the element at the specified index (no bounds checking
      done on the index).  The element itself is returned, not a pointer
      to it.

   type *Dynarr_atp(d, i)
      [MACRO] Return a pointer to the element at the specified index (no
      bounds checking done on the index).  The pointer may not be valid
      after an element is added to or removed from the array.

   Dynarr_reset(d)
      [MACRO] Reset the length of a dynamic array to 0.

   Dynarr_free(d)
      Destroy a dynamic array and the memory allocated to it.

Use the following global variable:

   Dynarr_min_size
      Minimum allowable size for a dynamic array when it is resized.

*/

#include <config.h>
#include "lisp.h"

static int Dynarr_min_size = 8;

static void Dynarr_realloc(Dynarr * dy, int new_size)
{
	if (DUMPEDP(dy->base)) {
		void *new_base = dy->elsize >= (signed int)sizeof(void*)
			? xmalloc(new_size)
			: xmalloc_atomic(new_size);
		int max_bytes = dy->max * dy->elsize;
		memcpy(new_base, dy->base,
		       max_bytes > new_size ? new_size : max_bytes);
		dy->base = new_base;
	} else {
		dy->base = xrealloc(dy->base, new_size);
	}
}

void *Dynarr_newf(int elsize)
{
	Dynarr *d = xnew_and_zero(Dynarr);
	d->elsize = elsize;

	return d;
}

void Dynarr_resize(void *d, int size)
{
       Dynarr *dy = (Dynarr *) d;
       int newsize = max(Dynarr_min_size,dy->max);


       if (dy->max <= 16)
	       while(newsize < size)
		       /* newsize *= 2 */
		       newsize <<= 1;
       else
	       while(newsize < size)
		       /* newsize *= 1.5 */
		       newsize += (newsize>>1);


	/* Don't do anything if the array is already big enough. */
	if (newsize > dy->max) {
		Dynarr_realloc(dy, newsize * dy->elsize);
		dy->max = newsize;
	}
}

/* Add a number of contiguous elements to the array starting at START. */
void Dynarr_insert_many(void *d, const void *el, int len, int start)
{
	Dynarr *dy = (Dynarr *) d;

	Dynarr_resize(dy, dy->cur + len);
	/* Silently adjust start to be valid. */
	if (start > dy->cur)
		start = dy->cur;
	else if (start < 0)
		start = 0;

	if (start != dy->cur) {
		memmove((char *)dy->base + (start + len) * dy->elsize,
			(char *)dy->base + start * dy->elsize,
			(dy->cur - start) * dy->elsize);
	}
	memcpy((char *)dy->base + start * dy->elsize, el, len * dy->elsize);
	dy->cur += len;

	if (dy->cur > dy->largest)
		dy->largest = dy->cur;
}

void Dynarr_delete_many(void *d, int start, int len)
{
	Dynarr *dy = (Dynarr *) d;

	assert(start >= 0 && len >= 0 && start + len <= dy->cur);
	memmove((char *)dy->base + start * dy->elsize,
		(char *)dy->base + (start + len) * dy->elsize,
		(dy->cur - start - len) * dy->elsize);
	dy->cur -= len;
}

void Dynarr_free(void *d)
{
	Dynarr *dy = (Dynarr *) d;

	if (dy->base && !DUMPEDP(dy->base))
		xfree(dy->base);
	if (!DUMPEDP(dy))
		xfree(dy);
}

#if defined MEMORY_USAGE_STATS && !(defined HAVE_BDWGC && defined EF_USE_BDWGC)

/* Return memory usage for Dynarr D.  The returned value is the total
   amount of bytes actually being used for the Dynarr, including all
   overhead.  The extra amount of space in the Dynarr that is
   allocated beyond what was requested is returned in DYNARR_OVERHEAD
   in STATS.  The extra amount of space that malloc() allocates beyond
   what was requested of it is returned in MALLOC_OVERHEAD in STATS.
   See the comment above the definition of this structure. */

size_t Dynarr_memory_usage(void *d, struct overhead_stats *stats)
{
	size_t total = 0;
	Dynarr *dy = (Dynarr *) d;

	/* We have to be a bit tricky here because not all of the
	   memory that malloc() will claim as "requested" was actually
	   requested. */

	if (dy->base) {
		size_t malloc_used = malloced_storage_size(dy->base,
							   dy->elsize * dy->max,
							   0);
		/* #### This may or may not be correct.  Some Dynarrs would
		   prefer that we use dy->cur instead of dy->largest here. */
		int was_requested = dy->elsize * dy->largest;
		int dynarr_overhead = dy->elsize * (dy->max - dy->largest);

		total += malloc_used;
		stats->was_requested += was_requested;
		stats->dynarr_overhead += dynarr_overhead;
		/* And the remainder must be malloc overhead. */
		stats->malloc_overhead +=
			malloc_used - was_requested - dynarr_overhead;
	}

	total += malloced_storage_size(d, sizeof(*dy), stats);

	return total;
}

#endif				/* MEMORY_USAGE_STATS */
