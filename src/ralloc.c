/* Block-relocating memory allocator.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Synched Up with:  FSF 20.2 (non-mmap portion only)
*/

/* NOTES:

   Only relocate the blocs necessary for SIZE in r_alloc_sbrk,
   rather than all of them.  This means allowing for a possible
   hole between the first bloc and the end of malloc storage. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>  /* for getpagesize() */
#endif

#ifdef emacs

#include "lisp.h"

/* The important properties of this type are that 1) it's a pointer, and
   2) arithmetic on it should work as if the size of the object pointed
   to has a size of 1.  */
#if 0 /* Arithmetic on void* is a GCC extension.  */
#ifdef __STDC__
typedef void *POINTER;
#else
typedef unsigned char *POINTER;
#endif
#endif /* 0 */

/* Unconditionally use unsigned char * for this.  */
typedef unsigned char *POINTER;

#ifdef DOUG_LEA_MALLOC
#define M_TOP_PAD -2
#include <malloc.h>
#endif

#include "getpagesize.h"

#include <string.h>
void refill_memory_reserve (void);

#else	/* Not emacs.  */

#include <stddef.h>

typedef void *POINTER;

#include <unistd.h>
#include <malloc.h>
#include <string.h>

#endif	/* emacs.  */

void init_ralloc (void);

#define NIL ((POINTER) 0)


#if !defined(HAVE_MMAP) || defined(DOUG_LEA_MALLOC)

/* A flag to indicate whether we have initialized ralloc yet.  For
   Emacs's sake, please do not make this local to malloc_init; on some
   machines, the dumping procedure makes all static variables
   read-only.  On these machines, the word static is #defined to be
   the empty string, meaning that r_alloc_initialized becomes an
   automatic variable, and loses its value each time Emacs is started up.  */
static int r_alloc_initialized = 0;


/* Declarations for working with the malloc, ralloc, and system breaks.  */

/* Function to set the real break value. */
static POINTER (*real_morecore) (ptrdiff_t size);

/* The break value, as seen by malloc (). */
static POINTER virtual_break_value;

/* The break value, viewed by the relocatable blocs. */
static POINTER break_value;

/* This is the size of a page.  We round memory requests to this boundary.  */
static size_t page_size;

/* Whenever we get memory from the system, get this many extra bytes.  This
   must be a multiple of page_size.  */
static int extra_bytes;

/* Macros for rounding.  Note that rounding to any value is possible
   by changing the definition of PAGE. */
#define PAGE (getpagesize ())
#define ALIGNED(addr) (((unsigned long int) (addr) & (page_size - 1)) == 0)
#define ROUNDUP(size) (((unsigned long int) (size) + page_size - 1) \
                       & ~(page_size - 1))
#define ROUND_TO_PAGE(addr) (addr & (~(page_size - 1)))

#define MEM_ALIGN sizeof(double)
#define MEM_ROUNDUP(addr) (((unsigned long int)(addr) + MEM_ALIGN - 1) \
				   & ~(MEM_ALIGN - 1))

/* Data structures of heaps and blocs.  */

/* The relocatable objects, or blocs, and the malloc data
   both reside within one or more heaps.
   Each heap contains malloc data, running from `start' to `bloc_start',
   and relocatable objects, running from `bloc_start' to `free'.

   Relocatable objects may relocate within the same heap
   or may move into another heap; the heaps themselves may grow
   but they never move.

   We try to make just one heap and make it larger as necessary.
   But sometimes we can't do that, because we can't get contiguous
   space to add onto the heap.  When that happens, we start a new heap.  */

typedef struct heap
{
  struct heap *next;
  struct heap *prev;
  /* Start of memory range of this heap.  */
  POINTER start;
  /* End of memory range of this heap.  */
  POINTER end;
  /* Start of relocatable data in this heap.  */
  POINTER bloc_start;
  /* Start of unused space in this heap.  */
  POINTER free;
  /* First bloc in this heap.  */
  struct bp *first_bloc;
  /* Last bloc in this heap.  */
  struct bp *last_bloc;
} *heap_ptr;

#define NIL_HEAP ((heap_ptr) 0)
#define HEAP_PTR_SIZE (sizeof (struct heap))

/* This is the first heap object.
   If we need additional heap objects, each one resides at the beginning of
   the space it covers.   */
static struct heap heap_base;

/* Head and tail of the list of heaps.  */
static heap_ptr first_heap, last_heap;

/* These structures are allocated in the malloc arena.
   The linked list is kept in order of increasing '.data' members.
   The data blocks abut each other; if b->next is non-nil, then
   b->data + b->size == b->next->data.

   An element with variable==NIL denotes a freed block, which has not yet
   been collected.  They may only appear while r_alloc_freeze > 0, and will be
   freed when the arena is thawed.  Currently, these blocs are not reusable,
   while the arena is frozen.  Very inefficient.  */

typedef struct bp
{
  struct bp *next;
  struct bp *prev;
  POINTER *variable;
  POINTER data;
  size_t size;
  POINTER new_data;		/* temporarily used for relocation */
  struct heap *heap; 		/* Heap this bloc is in.  */
} *bloc_ptr;

#define NIL_BLOC ((bloc_ptr) 0)
#define BLOC_PTR_SIZE (sizeof (struct bp))

/* Head and tail of the list of relocatable blocs. */
static bloc_ptr first_bloc, last_bloc;

static int use_relocatable_buffers;

/* If >0, no relocation whatsoever takes place.  */
static int r_alloc_freeze_level;

/* Obtain SIZE bytes of space.  If enough space is not presently available
   in our process reserve, (i.e., (page_break_value - break_value)),
   this means getting more page-aligned space from the system.

   Return non-zero if all went well, or zero if we couldn't allocate
   the memory.  */

/* Functions to get and return memory from the system.  */

/* Find the heap that ADDRESS falls within.  */

static heap_ptr
find_heap (POINTER address)
{
  heap_ptr heap;

  for (heap = last_heap; heap; heap = heap->prev)
    {
      if (heap->start <= address && address <= heap->end)
	return heap;
    }

  return NIL_HEAP;
}

/* Find SIZE bytes of space in a heap.
   Try to get them at ADDRESS (which must fall within some heap's range)
   if we can get that many within one heap.

   If enough space is not presently available in our reserve, this means
   getting more page-aligned space from the system.  If the returned space
   is not contiguous to the last heap, allocate a new heap, and append it

   obtain does not try to keep track of whether space is in use
   or not in use.  It just returns the address of SIZE bytes that
   fall within a single heap.  If you call obtain twice in a row
   with the same arguments, you typically get the same value.
   to the heap list.  It's the caller's responsibility to keep
   track of what space is in use.

   Return the address of the space if all went well, or zero if we couldn't
   allocate the memory.  */

static POINTER
obtain (POINTER address, size_t size)
{
  heap_ptr heap;
  size_t already_available;

  /* Find the heap that ADDRESS falls within.  */
  for (heap = last_heap; heap; heap = heap->prev)
    {
      if (heap->start <= address && address <= heap->end)
	break;
    }

  if (! heap)
    abort ();

  /* If we can't fit SIZE bytes in that heap,
     try successive later heaps.  */
  while (heap && address + size > heap->end)
    {
      heap = heap->next;
      if (heap == NIL_HEAP)
	break;
      address = heap->bloc_start;
    }

  /* If we can't fit them within any existing heap,
     get more space.  */
  if (heap == NIL_HEAP)
    {
      POINTER new = (*real_morecore)(0);
      size_t get;

      already_available = (char *)last_heap->end - (char *)address;

      if (new != last_heap->end)
	{
	  /* Someone else called sbrk.  Make a new heap.  */

	  heap_ptr new_heap = (heap_ptr) MEM_ROUNDUP (new);
	  POINTER bloc_start = (POINTER) MEM_ROUNDUP ((POINTER)(new_heap + 1));

	  if ((*real_morecore) (bloc_start - new) != new)
	    return 0;

	  new_heap->start = new;
	  new_heap->end = bloc_start;
	  new_heap->bloc_start = bloc_start;
	  new_heap->free = bloc_start;
	  new_heap->next = NIL_HEAP;
	  new_heap->prev = last_heap;
	  new_heap->first_bloc = NIL_BLOC;
	  new_heap->last_bloc = NIL_BLOC;
	  last_heap->next = new_heap;
	  last_heap = new_heap;

	  address = bloc_start;
	  already_available = 0;
	}

      /* Add space to the last heap (which we may have just created).
	 Get some extra, so we can come here less often.  */

      get = size + extra_bytes - already_available;
      get = (char *) ROUNDUP ((char *)last_heap->end + get)
	- (char *) last_heap->end;

      if ((*real_morecore) (get) != last_heap->end)
	return 0;

      last_heap->end += get;
    }

  return address;
}

#if 0
/* Obtain SIZE bytes of space and return a pointer to the new area.
   If we could not allocate the space, return zero.  */

static POINTER
get_more_space (size_t size)
{
  POINTER ptr = break_value;
  if (obtain (size))
    return ptr;
  else
    return 0;
}
#endif

/* Note that SIZE bytes of space have been relinquished by the process.
   If SIZE is more than a page, return the space to the system. */

static void
relinquish (void)
{
  register heap_ptr h;
  int excess = 0;

  /* Add the amount of space beyond break_value
     in all heaps which have extend beyond break_value at all.  */

  for (h = last_heap; h && break_value < h->end; h = h->prev)
    {
      excess += (char *) h->end - (char *) ((break_value < h->bloc_start)
					    ? h->bloc_start : break_value);
    }

  if (excess > extra_bytes * 2 && (*real_morecore) (0) == last_heap->end)
    {
      /* Keep extra_bytes worth of empty space.
	 And don't free anything unless we can free at least extra_bytes.  */
      excess -= extra_bytes;

      if ((char *)last_heap->end - (char *)last_heap->bloc_start <= excess)
	{
	  /* This heap should have no blocs in it.  */
	  if (last_heap->first_bloc != NIL_BLOC
	      || last_heap->last_bloc != NIL_BLOC)
	    abort ();

	  /* Return the last heap, with its header, to the system.  */
	  excess = (char *)last_heap->end - (char *)last_heap->start;
	  last_heap = last_heap->prev;
	  last_heap->next = NIL_HEAP;
	}
      else
	{
	  excess = (char *) last_heap->end
			- (char *) ROUNDUP ((char *)last_heap->end - excess);
	  last_heap->end -= excess;
	}

      if ((*real_morecore) (- excess) == 0)
	abort ();
    }
}

/* Return the total size in use by relocating allocator,
   above where malloc gets space.  */

long r_alloc_size_in_use (void);
long
r_alloc_size_in_use (void)
{
  return break_value - virtual_break_value;
}

/* The meat - allocating, freeing, and relocating blocs.  */


/* Find the bloc referenced by the address in PTR.  Returns a pointer
   to that block. */

static bloc_ptr
find_bloc (POINTER *ptr)
{
  register bloc_ptr p = first_bloc;

  while (p != NIL_BLOC)
    {
      if (p->variable == ptr && p->data == *ptr)
	return p;

      p = p->next;
    }

  return p;
}

/* Allocate a bloc of SIZE bytes and append it to the chain of blocs.
   Returns a pointer to the new bloc, or zero if we couldn't allocate
   memory for the new block.  */

static bloc_ptr
get_bloc (size_t size)
{
  register bloc_ptr new_bloc;
  register heap_ptr heap;

  if (! (new_bloc = (bloc_ptr) malloc (BLOC_PTR_SIZE))
      || ! (new_bloc->data = obtain (break_value, size)))
    {
      if (new_bloc)
	free (new_bloc);

      return 0;
    }

  break_value = new_bloc->data + size;

  new_bloc->size = size;
  new_bloc->next = NIL_BLOC;
  new_bloc->variable = (POINTER *) NIL;
  new_bloc->new_data = 0;

  /* Record in the heap that this space is in use.  */
  heap = find_heap (new_bloc->data);
  heap->free = break_value;

  /* Maintain the correspondence between heaps and blocs.  */
  new_bloc->heap = heap;
  heap->last_bloc = new_bloc;
  if (heap->first_bloc == NIL_BLOC)
    heap->first_bloc = new_bloc;

  /* Put this bloc on the doubly-linked list of blocs.  */
  if (first_bloc)
    {
      new_bloc->prev = last_bloc;
      last_bloc->next = new_bloc;
      last_bloc = new_bloc;
    }
  else
    {
      first_bloc = last_bloc = new_bloc;
      new_bloc->prev = NIL_BLOC;
    }

  return new_bloc;
}

/* Calculate new locations of blocs in the list beginning with BLOC,
   relocating it to start at ADDRESS, in heap HEAP.  If enough space is
   not presently available in our reserve, call obtain for
   more space.

   Store the new location of each bloc in its new_data field.
   Do not touch the contents of blocs or break_value.  */

static int
relocate_blocs (bloc_ptr bloc, heap_ptr heap, POINTER address)
{
  register bloc_ptr b = bloc;

  /* No need to ever call this if arena is frozen, bug somewhere!  */
  if (r_alloc_freeze_level)
    abort();

  while (b)
    {
      /* If bloc B won't fit within HEAP,
	 move to the next heap and try again.  */
      while (heap && address + b->size > heap->end)
	{
	  heap = heap->next;
	  if (heap == NIL_HEAP)
	    break;
	  address = heap->bloc_start;
	}

      /* If BLOC won't fit in any heap,
	 get enough new space to hold BLOC and all following blocs.  */
      if (heap == NIL_HEAP)
	{
	  register bloc_ptr tb = b;
	  register size_t s = 0;

	  /* Add up the size of all the following blocs.  */
	  while (tb != NIL_BLOC)
	    {
	      if (tb->variable)
		s += tb->size;

	      tb = tb->next;
	    }

	  /* Get that space.  */
	  address = obtain (address, s);
	  if (address == 0)
	    return 0;

	  heap = last_heap;
	}

      /* Record the new address of this bloc
	 and update where the next bloc can start.  */
      b->new_data = address;
      if (b->variable)
	address += b->size;
      b = b->next;
    }

  return 1;
}

#if 0 /* unused */
/* Reorder the bloc BLOC to go before bloc BEFORE in the doubly linked list.
   This is necessary if we put the memory of space of BLOC
   before that of BEFORE.  */

static void
reorder_bloc (bloc_ptr bloc, bloc_ptr before)
{
  bloc_ptr prev, next;

  /* Splice BLOC out from where it is.  */
  prev = bloc->prev;
  next = bloc->next;

  if (prev)
    prev->next = next;
  if (next)
    next->prev = prev;

  /* Splice it in before BEFORE.  */
  prev = before->prev;

  if (prev)
    prev->next = bloc;
  bloc->prev = prev;

  before->prev = bloc;
  bloc->next = before;
}
#endif /* unused */

/* Update the records of which heaps contain which blocs, starting
   with heap HEAP and bloc BLOC.  */

static void
update_heap_bloc_correspondence (bloc_ptr bloc, heap_ptr heap)
{
  register bloc_ptr b;

  /* Initialize HEAP's status to reflect blocs before BLOC.  */
  if (bloc != NIL_BLOC && bloc->prev != NIL_BLOC && bloc->prev->heap == heap)
    {
      /* The previous bloc is in HEAP.  */
      heap->last_bloc = bloc->prev;
      heap->free = bloc->prev->data + bloc->prev->size;
    }
  else
    {
      /* HEAP contains no blocs before BLOC.  */
      heap->first_bloc = NIL_BLOC;
      heap->last_bloc = NIL_BLOC;
      heap->free = heap->bloc_start;
    }

  /* Advance through blocs one by one.  */
  for (b = bloc; b != NIL_BLOC; b = b->next)
    {
      /* Advance through heaps, marking them empty,
	 till we get to the one that B is in.  */
      while (heap)
	{
	  if (heap->bloc_start <= b->data && b->data <= heap->end)
	    break;
	  heap = heap->next;
	  /* We know HEAP is not null now,
	     because there has to be space for bloc B.  */
	  heap->first_bloc = NIL_BLOC;
	  heap->last_bloc = NIL_BLOC;
	  heap->free = heap->bloc_start;
	}

      /* Update HEAP's status for bloc B.  */
      heap->free = b->data + b->size;
      heap->last_bloc = b;
      if (heap->first_bloc == NIL_BLOC)
	heap->first_bloc = b;

      /* Record that B is in HEAP.  */
      b->heap = heap;
    }

  /* If there are any remaining heaps and no blocs left,
     mark those heaps as empty.  */
  heap = heap->next;
  while (heap)
    {
      heap->first_bloc = NIL_BLOC;
      heap->last_bloc = NIL_BLOC;
      heap->free = heap->bloc_start;
      heap = heap->next;
    }
}

/* Resize BLOC to SIZE bytes.  This relocates the blocs
   that come after BLOC in memory.  */

static int
resize_bloc (bloc_ptr bloc, size_t size)
{
  register bloc_ptr b;
  heap_ptr heap;
  POINTER address;
  size_t old_size;

  /* No need to ever call this if arena is frozen, bug somewhere!  */
  if (r_alloc_freeze_level)
    abort();

  if (bloc == NIL_BLOC || size == bloc->size)
    return 1;

  for (heap = first_heap; heap != NIL_HEAP; heap = heap->next)
    {
      if (heap->bloc_start <= bloc->data && bloc->data <= heap->end)
	break;
    }

  if (heap == NIL_HEAP)
    abort ();

  old_size = bloc->size;
  bloc->size = size;

  /* Note that bloc could be moved into the previous heap.  */
  address = (bloc->prev ? bloc->prev->data + bloc->prev->size
	     : first_heap->bloc_start);
  while (heap)
    {
      if (heap->bloc_start <= address && address <= heap->end)
	break;
      heap = heap->prev;
    }

  if (! relocate_blocs (bloc, heap, address))
    {
      bloc->size = old_size;
      return 0;
    }

  if (size > old_size)
    {
      for (b = last_bloc; b != bloc; b = b->prev)
	{
	  if (!b->variable)
	    {
	      b->size = 0;
	      b->data = b->new_data;
            }
	  else
	    {
	      memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
            }
	}
      if (!bloc->variable)
	{
	  bloc->size = 0;
	  bloc->data = bloc->new_data;
	}
      else
	{
	  memmove (bloc->new_data, bloc->data, old_size);
	  memset (bloc->new_data + old_size, 0, size - old_size);
	  *bloc->variable = bloc->data = bloc->new_data;
	}
    }
  else
    {
      for (b = bloc; b != NIL_BLOC; b = b->next)
	{
	  if (!b->variable)
	    {
	      b->size = 0;
	      b->data = b->new_data;
            }
	  else
	    {
	      memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }
	}
    }

  update_heap_bloc_correspondence (bloc, heap);

  break_value = (last_bloc ? last_bloc->data + last_bloc->size
		 : first_heap->bloc_start);
  return 1;
}

/* Free BLOC from the chain of blocs, relocating any blocs above it
   and returning BLOC->size bytes to the free area. */

static void
free_bloc (bloc_ptr bloc)
{
  heap_ptr heap = bloc->heap;

  if (r_alloc_freeze_level)
    {
      bloc->variable = (POINTER *) NIL;
      return;
    }

  resize_bloc (bloc, 0);

  if (bloc == first_bloc && bloc == last_bloc)
    {
      first_bloc = last_bloc = NIL_BLOC;
    }
  else if (bloc == last_bloc)
    {
      last_bloc = bloc->prev;
      last_bloc->next = NIL_BLOC;
    }
  else if (bloc == first_bloc)
    {
      first_bloc = bloc->next;
      first_bloc->prev = NIL_BLOC;
    }
  else
    {
      bloc->next->prev = bloc->prev;
      bloc->prev->next = bloc->next;
    }

  /* Update the records of which blocs are in HEAP.  */
  if (heap->first_bloc == bloc)
    {
      if (bloc->next != 0 && bloc->next->heap == heap)
	heap->first_bloc = bloc->next;
      else
	heap->first_bloc = heap->last_bloc = NIL_BLOC;
    }
  if (heap->last_bloc == bloc)
    {
      if (bloc->prev != 0 && bloc->prev->heap == heap)
	heap->last_bloc = bloc->prev;
      else
	heap->first_bloc = heap->last_bloc = NIL_BLOC;
    }

  relinquish ();
  free (bloc);
}

/* Interface routines.  */

/* Obtain SIZE bytes of storage from the free pool, or the system, as
   necessary.  If relocatable blocs are in use, this means relocating
   them.  This function gets plugged into the GNU malloc's __morecore
   hook.

   We provide hysteresis, never relocating by less than extra_bytes.

   If we're out of memory, we should return zero, to imitate the other
   __morecore hook values - in particular, __default_morecore in the
   GNU malloc package.  */

POINTER r_alloc_sbrk (ptrdiff_t size);
POINTER
r_alloc_sbrk (ptrdiff_t size)
{
  register bloc_ptr b;
  POINTER address;

  if (! r_alloc_initialized)
    init_ralloc ();

  if (! use_relocatable_buffers)
    return (*real_morecore) (size);

  if (size == 0)
    return virtual_break_value;

  if (size > 0)
    {
      /* Allocate a page-aligned space.  GNU malloc would reclaim an
	 extra space if we passed an unaligned one.  But we could
	 not always find a space which is contiguous to the previous.  */
      POINTER new_bloc_start;
      heap_ptr h = first_heap;
      size_t get = ROUNDUP (size);

      address = (POINTER) ROUNDUP (virtual_break_value);

      /* Search the list upward for a heap which is large enough.  */
      while ((char *) h->end < (char *) MEM_ROUNDUP ((char *)address + get))
	{
	  h = h->next;
	  if (h == NIL_HEAP)
	    break;
	  address = (POINTER) ROUNDUP (h->start);
	}

      /* If not found, obtain more space.  */
      if (h == NIL_HEAP)
	{
	  get += extra_bytes + page_size;

	  if (! obtain (address, get))
	    return 0;

	  if (first_heap == last_heap)
	    address = (POINTER) ROUNDUP (virtual_break_value);
	  else
	    address = (POINTER) ROUNDUP (last_heap->start);
	  h = last_heap;
	}

      new_bloc_start = (POINTER) MEM_ROUNDUP ((char *)address + get);

      if (first_heap->bloc_start < new_bloc_start)
	{
	  /* This is no clean solution - no idea how to do it better.  */
	  if (r_alloc_freeze_level)
	    return NIL;

	  /* There is a bug here: if the above obtain call succeeded, but the
	     relocate_blocs call below does not succeed, we need to free
	     the memory that we got with obtain.  */

	  /* Move all blocs upward.  */
	  if (! relocate_blocs (first_bloc, h, new_bloc_start))
	    return 0;

	  /* Note that (POINTER)(h+1) <= new_bloc_start since
	     get >= page_size, so the following does not destroy the heap
	     header.  */
	  for (b = last_bloc; b != NIL_BLOC; b = b->prev)
	    {
	      memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }

	  h->bloc_start = new_bloc_start;

	  update_heap_bloc_correspondence (first_bloc, h);
	}
      if (h != first_heap)
	{
	  /* Give up managing heaps below the one the new
	     virtual_break_value points to.  */
	  first_heap->prev = NIL_HEAP;
	  first_heap->next = h->next;
	  first_heap->start = h->start;
	  first_heap->end = h->end;
	  first_heap->free = h->free;
	  first_heap->first_bloc = h->first_bloc;
	  first_heap->last_bloc = h->last_bloc;
	  first_heap->bloc_start = h->bloc_start;

	  if (first_heap->next)
	    first_heap->next->prev = first_heap;
	  else
	    last_heap = first_heap;
	}

      memset (address, 0, size);
    }
  else /* size < 0 */
    {
      EMACS_INT excess = (char *)first_heap->bloc_start
		      - ((char *)virtual_break_value + size);

      address = virtual_break_value;

      if (r_alloc_freeze_level == 0 && excess > 2 * extra_bytes)
	{
	  excess -= extra_bytes;
	  first_heap->bloc_start
	    = (POINTER) MEM_ROUNDUP ((char *)first_heap->bloc_start - excess);

	  relocate_blocs (first_bloc, first_heap, first_heap->bloc_start);

	  for (b = first_bloc; b != NIL_BLOC; b = b->next)
	    {
	      memmove (b->new_data, b->data, b->size);
	      *b->variable = b->data = b->new_data;
	    }
	}

      if ((char *)virtual_break_value + size < (char *)first_heap->start)
	{
	  /* We found an additional space below the first heap */
	  first_heap->start = (POINTER) ((char *)virtual_break_value + size);
	}
    }

  virtual_break_value = (POINTER) ((char *)address + size);
  break_value = (last_bloc
		 ? last_bloc->data + last_bloc->size
		 : first_heap->bloc_start);
  if (size < 0)
    relinquish ();

  return address;
}

/* Allocate a relocatable bloc of storage of size SIZE.  A pointer to
   the data is returned in *PTR.  PTR is thus the address of some variable
   which will use the data area.

   The allocation of 0 bytes is valid.
   In case r_alloc_freeze is set, a best fit of unused blocs could be done
   before allocating a new area.  Not yet done.

   If we can't allocate the necessary memory, set *PTR to zero, and
   return zero.  */

POINTER r_alloc (POINTER *ptr, size_t size);
POINTER
r_alloc (POINTER *ptr, size_t size)
{
  bloc_ptr new_bloc;

  if (! r_alloc_initialized)
    init_ralloc ();

  new_bloc = get_bloc (size);
  if (new_bloc)
    {
      new_bloc->variable = ptr;
      *ptr = new_bloc->data;
    }
  else
    *ptr = 0;

  return *ptr;
}

/* Free a bloc of relocatable storage whose data is pointed to by PTR.
   Store 0 in *PTR to show there's no block allocated.  */

void r_alloc_free (POINTER *ptr);
void
r_alloc_free (POINTER *ptr)
{
  register bloc_ptr dead_bloc;

  if (! r_alloc_initialized)
    init_ralloc ();

  dead_bloc = find_bloc (ptr);
  if (dead_bloc == NIL_BLOC)
    abort ();

  free_bloc (dead_bloc);
  *ptr = 0;

#ifdef emacs
  refill_memory_reserve ();
#endif
}

/* Given a pointer at address PTR to relocatable data, resize it to SIZE.
   Do this by shifting all blocks above this one up in memory, unless
   SIZE is less than or equal to the current bloc size, in which case
   do nothing.

   In case r_alloc_freeze is set, a new bloc is allocated, and the
   memory copied to it.  Not very efficient.  We could traverse the
   bloc_list for a best fit of free blocs first.

   Change *PTR to reflect the new bloc, and return this value.

   If more memory cannot be allocated, then leave *PTR unchanged, and
   return zero.  */

POINTER r_re_alloc (POINTER *ptr, size_t size);
POINTER
r_re_alloc (POINTER *ptr, size_t size)
{
  register bloc_ptr bloc;

  if (! r_alloc_initialized)
    init_ralloc ();

  if (!*ptr)
    return r_alloc (ptr, size);
  if (!size)
    {
      r_alloc_free (ptr);
      return r_alloc (ptr, 0);
    }

  bloc = find_bloc (ptr);
  if (bloc == NIL_BLOC)
    abort ();

  if (size < bloc->size)
    {
      /* Wouldn't it be useful to actually resize the bloc here?  */
      /* I think so too, but not if it's too expensive...  */
      if ((bloc->size - MEM_ROUNDUP (size) >= page_size)
          && r_alloc_freeze_level == 0)
	{
	  resize_bloc (bloc, MEM_ROUNDUP (size));
	  /* Never mind if this fails, just do nothing...  */
	  /* It *should* be infallible!  */
	}
    }
  else if (size > bloc->size)
    {
      if (r_alloc_freeze_level)
	{
	  bloc_ptr new_bloc;
	  new_bloc = get_bloc (MEM_ROUNDUP (size));
	  if (new_bloc)
	    {
	      new_bloc->variable = ptr;
	      *ptr = new_bloc->data;
	      bloc->variable = (POINTER *) NIL;
	    }
          else
	    return NIL;
	}
      else
	{
	  if (! resize_bloc (bloc, MEM_ROUNDUP (size)))
	    return NIL;
        }
    }
  return *ptr;
}

/* Disable relocations, after making room for at least SIZE bytes
   of non-relocatable heap if possible.  The relocatable blocs are
   guaranteed to hold still until thawed, even if this means that
   malloc must return a null pointer.  */

void r_alloc_freeze (long size);
void
r_alloc_freeze (long size)
{
  if (! r_alloc_initialized)
    init_ralloc ();

  /* If already frozen, we can't make any more room, so don't try.  */
  if (r_alloc_freeze_level > 0)
    size = 0;
  /* If we can't get the amount requested, half is better than nothing.  */
  while (size > 0 && r_alloc_sbrk (size) == 0)
    size /= 2;
  ++r_alloc_freeze_level;
  if (size > 0)
    r_alloc_sbrk (-size);
}

void r_alloc_thaw (void);
void
r_alloc_thaw (void)
{

  if (! r_alloc_initialized)
    init_ralloc ();

  if (--r_alloc_freeze_level < 0)
    abort ();

  /* This frees all unused blocs.  It is not too inefficient, as the resize
     and memmove is done only once.  Afterwards, all unreferenced blocs are
     already shrunk to zero size.  */
  if (!r_alloc_freeze_level)
    {
      bloc_ptr *b = &first_bloc;
      while (*b)
	if (!(*b)->variable)
	  free_bloc (*b);
	else
	  b = &(*b)->next;
    }
}


/* The hook `malloc' uses for the function which gets more space
   from the system.  */
#ifndef DOUG_LEA_MALLOC
extern POINTER (*__morecore) (ptrdiff_t size);
#endif

/* Initialize various things for memory allocation. */

void
init_ralloc (void)
{
  if (r_alloc_initialized)
    return;

  r_alloc_initialized = 1;
  real_morecore = (POINTER (*) (ptrdiff_t)) __morecore;
  __morecore =
#ifdef __GNUC__
    (__typeof__ (__morecore))
#endif
    r_alloc_sbrk;

  first_heap = last_heap = &heap_base;
  first_heap->next = first_heap->prev = NIL_HEAP;
  first_heap->start = first_heap->bloc_start
    = virtual_break_value = break_value = (*real_morecore) (0);
  if (break_value == NIL)
    abort ();

  page_size = PAGE;
  extra_bytes = ROUNDUP (50000);

#ifdef DOUG_LEA_MALLOC
    mallopt (M_TOP_PAD, 64 * 4096);
#else
#if 0 /* Hasn't been synched yet */
  /* Give GNU malloc's morecore some hysteresis
     so that we move all the relocatable blocks much less often.  */
  __malloc_extra_blocks = 64;
#endif
#endif

  first_heap->end = (POINTER) ROUNDUP (first_heap->start);

  /* The extra call to real_morecore guarantees that the end of the
     address space is a multiple of page_size, even if page_size is
     not really the page size of the system running the binary in
     which page_size is stored.  This allows a binary to be built on a
     system with one page size and run on a system with a smaller page
     size.  */
  (*real_morecore) (first_heap->end - first_heap->start);

  /* Clear the rest of the last page; this memory is in our address space
     even though it is after the sbrk value.  */
  /* Doubly true, with the additional call that explicitly adds the
     rest of that page to the address space.  */
  memset (first_heap->start, 0, first_heap->end - first_heap->start);
  virtual_break_value = break_value = first_heap->bloc_start = first_heap->end;
  use_relocatable_buffers = 1;
}

#if defined (emacs) && defined (DOUG_LEA_MALLOC)

/* Reinitialize the morecore hook variables after restarting a dumped
   Emacs.  This is needed when using Doug Lea's malloc from GNU libc.  */
void r_alloc_reinit (void);
void
r_alloc_reinit (void)
{
  /* Only do this if the hook has been reset, so that we don't get an
     infinite loop, in case Emacs was linked statically.  */
  if ( (POINTER (*) (ptrdiff_t)) __morecore !=  r_alloc_sbrk)
    {
      real_morecore = (POINTER (*) (ptrdiff_t)) __morecore;
      __morecore =
#ifdef __GNUC__
	(__typeof__ (__morecore))
#endif
	r_alloc_sbrk;
    }
}
#if 0
#ifdef DEBUG

void
r_alloc_check (void)
{
  int found = 0;
  heap_ptr h, ph = 0;
  bloc_ptr b, pb = 0;

  if (!r_alloc_initialized)
    return;

  assert (first_heap);
  assert (last_heap->end <= (POINTER) sbrk (0));
  assert ((POINTER) first_heap < first_heap->start);
  assert (first_heap->start <= virtual_break_value);
  assert (virtual_break_value <= first_heap->end);

  for (h = first_heap; h; h = h->next)
    {
      assert (h->prev == ph);
      assert ((POINTER) ROUNDUP (h->end) == h->end);
#if 0 /* ??? The code in ralloc.c does not really try to ensure
	 the heap start has any sort of alignment.
	 Perhaps it should.  */
      assert ((POINTER) MEM_ROUNDUP (h->start) == h->start);
#endif
      assert ((POINTER) MEM_ROUNDUP (h->bloc_start) == h->bloc_start);
      assert (h->start <= h->bloc_start && h->bloc_start <= h->end);

      if (ph)
	{
	  assert (ph->end < h->start);
	  assert (h->start <= (POINTER)h && (POINTER)(h+1) <= h->bloc_start);
	}

      if (h->bloc_start <= break_value && break_value <= h->end)
	found = 1;

      ph = h;
    }

  assert (found);
  assert (last_heap == ph);

  for (b = first_bloc; b; b = b->next)
    {
      assert (b->prev == pb);
      assert ((POINTER) MEM_ROUNDUP (b->data) == b->data);
      assert ((size_t) MEM_ROUNDUP (b->size) == b->size);

      ph = 0;
      for (h = first_heap; h; h = h->next)
	{
	  if (h->bloc_start <= b->data && b->data + b->size <= h->end)
	    break;
	  ph = h;
	}

      assert (h);

      if (pb && pb->data + pb->size != b->data)
	{
	  assert (ph && b->data == h->bloc_start);
	  while (ph)
	    {
	      if (ph->bloc_start <= pb->data
		  && pb->data + pb->size <= ph->end)
		{
		  assert (pb->data + pb->size + b->size > ph->end);
		  break;
		}
	      else
		{
		  assert (ph->bloc_start + b->size > ph->end);
		}
	      ph = ph->prev;
	    }
	}
      pb = b;
    }

  assert (last_bloc == pb);

  if (last_bloc)
    assert (last_bloc->data + last_bloc->size == break_value);
  else
    assert (first_heap->bloc_start == break_value);
}
#endif /* DEBUG */
#endif /* 0 */

#endif

#else /* HAVE_MMAP */

/*
   A relocating allocator built using the mmap(2) facility available
   in some OSes.  Based on another version written by Paul Flinders,
   from which code (and comments) are snarfed.

   The OS should support mmap() with MAP_ANONYMOUS attribute, or have
   /dev/zero.  It should support private memory mapping.

   Paul Flinders wrote a version which works well for systems that
   allow callers to specify (virtual) addresses to mmap().
   Unfortunately, such a scheme doesn't work for certain systems like
   HP-UX that have a system-wide virtual->real address map, and
   consequently impose restrictions on the virtual address values
   permitted.

   NB: The mapping scheme in HP-UX is motivated by the inverted page
   table design in some HP processors.

   This alternate implementation allows for the addresses to be
   optionally chosen by the system.  Fortunately, buffer allocation
   doesn't insist upon contiguous memory which Flinders' scheme
   provides, and this one doesn't.

   We don't really provide for hysteresis here, but add some metering
   to monitor how poorly the allocator actually works.  See the
   documentation for `mmap-hysteresis'.

   This implementation actually cycles through the blocks allocated
   via mmap() and only sends it to free() if it wasn't one of them.
   Unfortunately, this is O(n) in the number of mmapped blocks.  (Not
   really, as we have a hash table which tries to reduce the cost.)
   Also, this dereferences the pointer passed, so it would cause a
   segfault if garbage was passed to it.  */

#include <fcntl.h>
#include <sys/mman.h>
#include <stdio.h>

typedef void *VM_ADDR;		/* VM addresses */
static const VM_ADDR VM_FAILURE_ADDR = (VM_ADDR) -1; /* mmap returns this when it fails. */

/* Configuration for relocating allocator. */

/* #define MMAP_GENERATE_ADDRESSES */
/* Define this if you want Emacs to manage the address table.
   It is not recommended unless you have major problems with the
   default scheme, which allows the OS to pick addresses. */

/* USELESS_LOWER_ADDRESS_BITS defines the number of bits which can be
   discarded while computing the hash, as they're always zero.  The
   default is appropriate for a page size of 4096 bytes. */

#define USELESS_LOWER_ADDRESS_BITS 12


/* Size of hash table for inverted VM_ADDR->MMAP_HANDLE lookup */

#define MHASH_PRIME 89


/* Whether we want to enable metering of some ralloc performance.
   This incurs a constant penalty for each mmap operation. */

#define MMAP_METERING


/* Rename the following to protect against a some smartness elsewhere.
   We need access to the allocator used for non-mmap allocation
   elsewhere, in case we get passed a handle that we didn't allocate
   ourselves.  Currently, this default allocator is also used to
   maintain local structures for relocatable blocks. */

#define UNDERLYING_MALLOC   malloc
#define UNDERLYING_FREE     free
#define UNDERLYING_REALLOC  realloc

/* MAP_ADDRCHOICE_FLAG is set to MAP_FIXED if MMAP_GENERATE_ADDRESSES
   is defined, and MAP_VARIABLE otherwise.  Some losing systems don't
   define the _FIXED/_VARIABLE flags, in which case it is set to 0 */

#ifdef MMAP_GENERATE_ADDRESSES
# ifdef MAP_FIXED
#    define MAP_ADDRCHOICE_FLAG MAP_FIXED
# endif
#else /* !MMAP_GENERATE_ADDRESSES */
# ifdef MAP_VARIABLE
#    define MAP_ADDRCHOICE_FLAG MAP_VARIABLE
# endif
#endif /* MMAP_GENERATE_ADDRESSES */

/* Default case. */
#ifndef MAP_ADDRCHOICE_FLAG
#  define MAP_ADDRCHOICE_FLAG 0
#endif /* MAP_ADDRCHOICE_FLAG */

#ifdef MAP_ANONYMOUS
#  define MAP_FLAGS (MAP_PRIVATE | MAP_ADDRCHOICE_FLAG | MAP_ANONYMOUS)
#else
#  define MAP_FLAGS (MAP_PRIVATE | MAP_ADDRCHOICE_FLAG)
#endif /* MAP_ANONYMOUS */


/* (ptf): A flag to indicate whether we have initialized ralloc yet.  For
   Emacs's sake, please do not make this local to malloc_init; on some
   machines, the dumping procedure makes all static variables
   read-only.  On these machines, the word static is #defined to be
   the empty string, meaning that r_alloc_initialized becomes an
   automatic variable, and loses its value each time Emacs is started up.

   If we're using mmap this flag has three possible values
   0 - initial value
   1 - Normal value when running temacs. In this case buffers
       are allocated using malloc so that any data that they
       contain becomes part of the undumped executable.
   2 - Normal value when running emacs */
static int r_alloc_initialized = 0;

/* (ptf): Macros for rounding.  Note that rounding to any value is possible
   by changing the definition of PAGE. */
#define PAGE (getpagesize ())
#define PAGES_FOR(size) (((unsigned long int) (size) + page_size - 1)/page_size)
#define ROUNDUP(size) ((unsigned long int)PAGES_FOR(size)*page_size)


/* DEV_ZERO_FD is -1 normally, but for systems without MAP_ANONYMOUS
   points to a file descriptor opened on /dev/zero */

static int DEV_ZERO_FD = -1;


/* We actually need a data structure that can be usefully structured
   based on the VM address, and allows an ~O(1) lookup on an arbitrary
   address, i.e. a hash table.  Maybe the XEmacs hash table can be
   coaxed enough.  At the moment, we use lookup on a hash table to
   decide whether to do an O(n) search on the malloced block list.
   Addresses are hashed to a bucket modulo MHASH_PRIME. */


/* We settle for a standard doubly-linked-list.  The dynarr type isn't
   very amenable to deletion of items in the middle, so we conjure up
   yet another stupid datastructure.  The structure is maintained as a
   ring, and the singleton ring has the sole element as its left and
   right neighbours. */

static void init_MHASH_table (void); /* Forward reference */

typedef struct alloc_dll
{
  size_t size;			/* #bytes currently in use */
  size_t space_for;		/* #bytes we really have */
  POINTER* aliased_address;	/* Address of aliased variable, to tweak if relocating */
  VM_ADDR vm_addr;		/* VM address returned by mmap */
  struct alloc_dll *left;	/* Left link in circular doubly linked list */
  struct alloc_dll *right;
} *MMAP_HANDLE;

static MMAP_HANDLE mmap_start = 0; /* Head of linked list */
static size_t page_size = 0;	/* Size of VM pages */
static Fixnum mmap_hysteresis;	/* Logically a "size_t" */

/* Get a new handle for a fresh block. */
static MMAP_HANDLE
new_mmap_handle (size_t nsiz)
{
  MMAP_HANDLE h = (MMAP_HANDLE) UNDERLYING_MALLOC( sizeof (struct alloc_dll));
  if ( h == 0) return 0;
  h->size = nsiz;
  if (mmap_start == 0)
    {
      init_MHASH_table ();
      mmap_start = h; mmap_start->left = h; mmap_start->right = h;
    }
  {
    MMAP_HANDLE prev = mmap_start->left;
    MMAP_HANDLE nex = mmap_start;

    /* Four pointers need fixing. */
    h->right = nex;
    h->left = prev;
    prev->right = h;
    nex->left = h;
  }
  return h;
}

/* Find a handle given the aliased address using linear search. */
static MMAP_HANDLE
find_mmap_handle_lsearch (POINTER *alias)
{
  MMAP_HANDLE h = mmap_start;
  if (h == 0) return 0;
  do {
    if (h->aliased_address == alias && *alias == h->vm_addr)
      return h;
    h = h->right;
  } while( h != mmap_start );
  return 0;			/* Bogus alias passed. */
}

/* Free a handle. */
static void
free_mmap_handle (MMAP_HANDLE h)
{
  MMAP_HANDLE prev = h->left;
  MMAP_HANDLE nex = h->right;
  if (prev == h || nex == h)	/* In fact, this should be && */
    {				/* We're the singleton dll */
      UNDERLYING_FREE( h );		/* Free the sole item */
      mmap_start = 0; return;
    }
  else if (h == mmap_start)
    {
      mmap_start = nex;		/* Make sure mmap_start isn't bogus. */
    }
  prev->right = nex;
  nex->left = prev;
  UNDERLYING_FREE( h );
}

/* A simple hash table to speed up the inverted lookup of
   VM_ADDR->MMAP_HANDLE. We maintain the number of hits for a
   particular bucket.  We invalidate a hash table entry during block
   deletion if the hash has cached the deleted block's address. */

/* Simple hash check. */
struct {
  int n_hits;			/* How many addresses map to this? */
  MMAP_HANDLE handle;		/* What is the current handle? */
  VM_ADDR addr;			/* What is its VM address? */
} MHASH_HITS[ MHASH_PRIME ];

static void
init_MHASH_table (void)
{
  int i = 0;
  for (; i < MHASH_PRIME; i++)
    {
      MHASH_HITS[i].n_hits = 0;
      MHASH_HITS[i].addr = 0;
      MHASH_HITS[i].handle = 0;
    }
}

/* Compute the hash value for an address. */
static int
MHASH (VM_ADDR addr)
{
#if (LONGBITS == 64)
  unsigned long int addr_shift = (unsigned long int)(addr) >> USELESS_LOWER_ADDRESS_BITS;
#else
  unsigned int addr_shift = (unsigned int)(addr) >> USELESS_LOWER_ADDRESS_BITS;
#endif
  int hval = addr_shift % MHASH_PRIME; /* We could have addresses which are -ve
					  when converted to signed ints */
  return ((hval >= 0) ? hval : MHASH_PRIME + hval);
}

/* Add a VM address with its corresponding handle to the table. */
static void
MHASH_ADD (VM_ADDR addr, MMAP_HANDLE h)
{
  int kVal = MHASH( addr );
  if (MHASH_HITS[kVal].n_hits++ == 0)
    { /* Only overwrite the table if there were no hits so far. */
      MHASH_HITS[kVal].addr = addr;
      MHASH_HITS[kVal].handle = h;
    }
}

/* Delete a VM address entry from the hash table. */
static void
MHASH_DEL (VM_ADDR addr)
{
  int kVal = MHASH( addr );
  MHASH_HITS[kVal].n_hits--;
  if (addr == MHASH_HITS[kVal].addr)
    {
      MHASH_HITS[kVal].addr = 0; /* Invalidate cache. */
      MHASH_HITS[kVal].handle = 0;
    }
}

/* End of hash buckets */

/* Metering malloc performance. */
#ifdef MMAP_METERING
/* If we're metering, we introduce some extra symbols to aid the noble
   cause of bloating XEmacs core size. */

static Lisp_Object Qmmap_times_mapped;
static Lisp_Object Qmmap_pages_mapped;
static Lisp_Object Qmmap_times_unmapped;
static Lisp_Object Qmmap_times_remapped;
static Lisp_Object Qmmap_didnt_copy;
static Lisp_Object Qmmap_pages_copied;
static Lisp_Object Qmmap_average_bumpval;
static Lisp_Object Qmmap_wastage;
static Lisp_Object Qmmap_live_pages;
static Lisp_Object Qmmap_addr_looked_up;
static Lisp_Object Qmmap_hash_worked;
static Lisp_Object Qmmap_addrlist_size;

#define M_Map 0			/* How many times allocated? */
#define M_Pages_Map 1		/* How many pages allocated? */
#define M_Unmap 2		/* How many times freed? */
#define M_Remap 3		/* How many times increased in size? */
#define M_Didnt_Copy 4		/* How many times didn't need to copy? */
#define M_Copy_Pages 5		/* Total # pages copied */
#define M_Average_Bumpval 6	/* Average bump value */
#define M_Wastage 7		/* Remaining (unused space) */
#define M_Live_Pages 8		/* #live pages */
#define M_Address_Lookup 9	/* How many times did we need to check if an addr is in the block? */
#define M_Hash_Worked   10      /* How many times did the simple hash check work? */
#define M_Addrlist_Size 11	/* What is the size of the XEmacs memory map? */

#define N_Meterables 12		/* Total number of meterables */
#define MEMMETER(x) {x;}
#define MVAL(x) (meter[x])
#define MLVAL(x) (make_int (meter[x]))
static int meter[N_Meterables];

DEFUN ("mmap-allocator-status", Fmmap_allocator_status, 0, 0, 0, /*
Return some information about mmap-based allocator.

mmap-times-mapped:    number of times r_alloc was called.
mmap-pages-mapped:    number of pages mapped by r_alloc calls only.
mmap-times-unmapped:  number of times r_free was called.
mmap-times-remapped:  number of times r_re_alloc was called.
mmap-didnt-copy:      number of times re-alloc did NOT have to move the block.
mmap-pages-copied:    total number of pages copied.
mmap-average-bumpval: average increase in size demanded to re-alloc.
mmap-wastage:         total number of bytes allocated, but not currently in use.
mmap-live-pages:      total number of pages live.
mmap-addr-looked-up:  total number of times needed to check if addr is in block.
mmap-hash-worked:     total number of times the simple hash check worked.
mmap-addrlist-size:   number of entries in address picking list.
*/
       ())
{
  Lisp_Object result = Qnil;

  result = cons3 (Qmmap_addrlist_size,	MLVAL (M_Addrlist_Size),   result);
  result = cons3 (Qmmap_hash_worked,	MLVAL (M_Hash_Worked),	   result);
  result = cons3 (Qmmap_addr_looked_up,	MLVAL (M_Address_Lookup),  result);
  result = cons3 (Qmmap_live_pages,	MLVAL (M_Live_Pages),	   result);
  result = cons3 (Qmmap_wastage,	MLVAL (M_Wastage),	   result);
  result = cons3 (Qmmap_average_bumpval,MLVAL (M_Average_Bumpval), result);
  result = cons3 (Qmmap_pages_copied,	MLVAL (M_Copy_Pages),	   result);
  result = cons3 (Qmmap_didnt_copy,	MLVAL (M_Didnt_Copy),	   result);
  result = cons3 (Qmmap_times_remapped,	MLVAL (M_Remap),	   result);
  result = cons3 (Qmmap_times_unmapped,	MLVAL (M_Unmap),	   result);
  result = cons3 (Qmmap_pages_mapped,	MLVAL (M_Pages_Map),	   result);
  result = cons3 (Qmmap_times_mapped,	MLVAL (M_Map),		   result);

  return result;
}

#else /* !MMAP_METERING */

#define MEMMETER(x)
#define MVAL(x)

#endif /* MMAP_METERING */

static MMAP_HANDLE
find_mmap_handle (POINTER *alias)
{
  int kval  = MHASH( *alias );
  MEMMETER( MVAL(M_Address_Lookup)++ )
  switch( MHASH_HITS[kval].n_hits)
    {
    case 0:
      MEMMETER( MVAL( M_Hash_Worked )++ )
      return 0;

    case 1:
      if (*alias == MHASH_HITS[kval].addr)
	{
	  MEMMETER( MVAL( M_Hash_Worked) ++ );
	  return MHASH_HITS[kval].handle;
	}
      /* FALL THROUGH */
    default:
      return find_mmap_handle_lsearch( alias );
    } /* switch */
}

/*
   Some kernels don't like being asked to pick addresses for mapping
   themselves---IRIX is known to become extremely slow if mmap is
   passed a ZERO as the first argument.  In such cases, we use an
   address map which is managed local to the XEmacs process.  The
   address map maintains an ordered linked list of (address, size,
   occupancy) triples ordered by the absolute address.  Initially, a
   large address area is marked as being empty.  The address picking
   scheme takes bites off the first block which is still empty and
   large enough.  If mmap with the specified address fails, it is
   marked unavailable and not attempted thereafter.  The scheme will
   keep fragmenting the large empty block until it finds an address
   which can be successfully mmapped, or until there are no free
   blocks of the given size left.

   Note that this scheme, given its first-fit strategy, is prone to
   fragmentation of the first part of memory earmarked for this
   purpose. [ACP Vol I].  We can't use the workaround of using a
   randomized first fit because we don't want to presume too much
   about the memory map.  Instead, we try to coalesce empty or
   unavailable blocks at any available opportunity.  */

/* Initialization procedure for address picking scheme */
static void Addr_Block_initialize(void);

/* Get a suitable VM_ADDR via mmap */
static VM_ADDR New_Addr_Block (size_t sz);

/* Free a VM_ADDR allocated via New_Addr_Block */
static void Free_Addr_Block (VM_ADDR addr, size_t sz);

#ifdef MMAP_GENERATE_ADDRESSES
/* Implementation of the three calls for address picking when XEmacs is incharge */

/* The enum denotes the status of the following block. */
typedef enum { empty = 0, occupied, unavailable } addr_status;

typedef struct addr_chain
{
  POINTER addr;
  size_t sz;
  addr_status flag;
  struct addr_chain *next;
} ADDRESS_BLOCK, *ADDRESS_CHAIN;
/* NB: empty and unavailable blocks are concatenated. */

static ADDRESS_CHAIN addr_chain = 0;
/* Start off the address block chain with a humongous address block
   which is empty to start with.  Note that addr_chain is invariant
   WRT the addition/deletion of address blocks because of the assert
   in Coalesce() and the strict ordering of blocks by their address
   */
static void
Addr_Block_initialize (void)
{
  MEMMETER( MVAL( M_Addrlist_Size )++)
  addr_chain = (ADDRESS_CHAIN) UNDERLYING_MALLOC( sizeof( ADDRESS_BLOCK ));
  addr_chain->next = 0;		/* Last block in chain */
  addr_chain->sz = 0x0c000000;	/* Size */
  addr_chain->addr = (POINTER) (0x04000000);
  addr_chain->flag = empty;
}

/* Coalesce address blocks if they are contiguous.  Only empty and
   unavailable slots are coalesced. */
static void
Coalesce_Addr_Blocks (void)
{
  ADDRESS_CHAIN p;
  for (p = addr_chain; p; p = p->next)
    {
      while (p->next && p->flag == p->next->flag)
	{
	  ADDRESS_CHAIN np;
	  np = p->next;

	  if (p->flag == occupied) break; /* No cigar */

	  /* Check if the addresses are contiguous. */
	  if (p->addr + p->sz != np->addr) break;

	  MEMMETER( MVAL( M_Addrlist_Size )--)
	  /* We can coalesce these two. */
	  p->sz += np->sz;
	  p->next = np->next;
	  assert( np != addr_chain ); /* We're not freeing the head of the list. */
	  UNDERLYING_FREE( np );
	}
    } /* for all p */
}

/* Get an empty address block of specified size. */
static VM_ADDR
New_Addr_Block (size_t sz)
{
  ADDRESS_CHAIN p = addr_chain;
  VM_ADDR new_addr = VM_FAILURE_ADDR;
  for (; p; p = p->next)
    {
      if (p->flag == empty && p->sz > sz)
	{
	  /* Create a new entry following p which is empty. */
	  ADDRESS_CHAIN remainder = (ADDRESS_CHAIN) UNDERLYING_MALLOC( sizeof( ADDRESS_BLOCK ) );
	  remainder->next = p->next;
	  remainder->flag = empty;
	  remainder->addr = p->addr + sz;
	  remainder->sz = p->sz - sz;

	  MEMMETER( MVAL( M_Addrlist_Size )++)

	  /* Now make p become an occupied block with the appropriate size */
	  p->next = remainder;
	  p->sz = sz;
	  new_addr = mmap( (VM_ADDR) p->addr, p->sz, PROT_READ|PROT_WRITE,
			   MAP_FLAGS, DEV_ZERO_FD, 0 );
	  if (new_addr == VM_FAILURE_ADDR)
	    {
	      p->flag = unavailable;
	      continue;
	    }
	  p->flag = occupied;
	  break;
	}
    }
  Coalesce_Addr_Blocks();
  return new_addr;
}

/* Free an address block.  We mark the block as being empty, and attempt to
   do any coalescing that may have resulted from this. */
static void
Free_Addr_Block (VM_ADDR addr, size_t sz)
{
  ADDRESS_CHAIN p = addr_chain;
  for (; p; p = p->next )
    {
      if (p->addr == addr)
	{
	  if (p->sz != sz) abort(); /* ACK! Shouldn't happen at all. */
	  munmap( (VM_ADDR) p->addr, p->sz );
	  p->flag = empty;
	  break;
	}
    }
  if (!p) abort(); /* Can't happen... we've got a block to free which is not in
		      the address list. */
  Coalesce_Addr_Blocks();
}
#else /* !MMAP_GENERATE_ADDRESSES */
/* This is an alternate (simpler) implementation in cases where the
   address is picked by the kernel. */

static void
Addr_Block_initialize (void)
{
  /* Nothing. */
}

static VM_ADDR
New_Addr_Block (size_t sz)
{
  return mmap (0, sz, PROT_READ|PROT_WRITE, MAP_FLAGS,
	       DEV_ZERO_FD, 0 );
}

static void
Free_Addr_Block (VM_ADDR addr, size_t sz)
{
  munmap ((caddr_t) addr, sz );
}

#endif /* MMAP_GENERATE_ADDRESSES */


/* IMPLEMENTATION OF EXPORTED RELOCATOR INTERFACE */

/*
 r_alloc (POINTER, SIZE): Allocate a relocatable area with the start
 address aliased to the first parameter.
 */

POINTER r_alloc (POINTER *ptr, size_t size);
POINTER
r_alloc (POINTER *ptr, size_t size)
{
  MMAP_HANDLE mh;

  switch(r_alloc_initialized)
    {
    case 0:
      abort();
    case 1:
      *ptr = (POINTER) UNDERLYING_MALLOC(size);
      break;
    default:
      mh = new_mmap_handle( size );
      if (mh)
	{
	  size_t hysteresis = (mmap_hysteresis > 0 ?  mmap_hysteresis  : 0);
	  size_t mmapped_size = ROUNDUP( size + hysteresis );
	  MEMMETER( MVAL(M_Map)++ )
	  MEMMETER( MVAL(M_Pages_Map) += (mmapped_size/page_size) )
	  MEMMETER( MVAL(M_Wastage) += mmapped_size - size )
          MEMMETER( MVAL(M_Live_Pages) += (mmapped_size/page_size) )
	  mh->vm_addr = New_Addr_Block( mmapped_size );
	  if (mh->vm_addr == VM_FAILURE_ADDR) {
	    free_mmap_handle( mh ); /* Free the loser */
	    *ptr = 0;
	    return 0;		/* ralloc failed due to mmap() failure. */
	  }
	  MHASH_ADD( mh->vm_addr, mh );
	  mh->space_for = mmapped_size;
	  mh->aliased_address = ptr;
	  *ptr = (POINTER) mh->vm_addr;
	}
      else
	*ptr = 0;		/* Malloc of block failed */
      break;
    }
  return *ptr;
}

/* Free a bloc of relocatable storage whose data is pointed to by PTR.
   Store 0 in *PTR to show there's no block allocated.  */

void r_alloc_free (POINTER *ptr);
void
r_alloc_free (POINTER *ptr)
{
  switch( r_alloc_initialized) {
    case 0:
      abort();

    case 1:
      UNDERLYING_FREE( *ptr );		/* Certain this is from the heap. */
      break;

    default:
      {
	MMAP_HANDLE dead_handle = find_mmap_handle( ptr );
	/* Check if we've got it. */
	if (dead_handle == 0)	/* Didn't find it in the list of mmap handles */
	  {
	    UNDERLYING_FREE( *ptr );
	  }
	else
	  {
	    MEMMETER( MVAL( M_Wastage ) -= (dead_handle->space_for - dead_handle->size) )
	    MEMMETER( MVAL( M_Live_Pages ) -= (dead_handle->space_for / page_size ))
	    MEMMETER(MVAL(M_Unmap)++)
	    MHASH_DEL( dead_handle->vm_addr );
	    Free_Addr_Block( dead_handle->vm_addr, dead_handle->space_for );
	    free_mmap_handle (dead_handle);
	  }
      }
      break;
    } /* r_alloc_initialized */
  *ptr = 0;			/* Zap the pointer's contents. */
}

/* Given a pointer at address PTR to relocatable data, resize it to SIZE.

   Change *PTR to reflect the new bloc, and return this value.

   If more memory cannot be allocated, then leave *PTR unchanged, and
   return zero.  */

POINTER r_re_alloc (POINTER *ptr, size_t sz);
POINTER
r_re_alloc (POINTER *ptr, size_t sz)
{
  if (r_alloc_initialized == 0)
    {
      abort ();
      return 0; /* suppress compiler warning */
    }
  else if (r_alloc_initialized == 1)
    {
      POINTER tmp = (POINTER) realloc(*ptr, sz);
      if (tmp)
	*ptr = tmp;
      return tmp;
    }
  else
    {
      size_t hysteresis = (mmap_hysteresis > 0 ?  mmap_hysteresis : 0);
      size_t actual_sz = ROUNDUP( sz + hysteresis );
      MMAP_HANDLE h = find_mmap_handle( ptr );
      VM_ADDR new_vm_addr;

      if ( h == 0 )		/* Was allocated using malloc. */
	{
	  POINTER tmp = (POINTER) UNDERLYING_REALLOC(*ptr, sz);
	  if (tmp)
	    *ptr = tmp;
	  return tmp;
	}

      MEMMETER(
	       MVAL(M_Average_Bumpval) =
	       (((double) MVAL(M_Remap) * MVAL(M_Average_Bumpval)) + (sz - h->size))
	       / (double) (MVAL(M_Remap) + 1))
      MEMMETER(MVAL(M_Remap)++)
      if (h->space_for > sz)	/* We've got some more room */
	{			/* Also, if a shrinkage was asked for. */
	  MEMMETER( MVAL(M_Didnt_Copy)++ )
          MEMMETER( MVAL(M_Wastage) -= (sz - h->size))
	  /* We're pretty dumb at handling shrinkage.  We should check for
	     a larger gap than the standard hysteresis allowable, and if so,
	     shrink the number of pages.  Right now, we simply reset the size
	     component and return. */
	  h->size = sz;
	  return *ptr;
	}

      new_vm_addr = New_Addr_Block( actual_sz );
      if (new_vm_addr == VM_FAILURE_ADDR)
	{/* Failed to realloc. */
          /* *ptr = 0; */
	  return 0;
	}

      MHASH_ADD( new_vm_addr, h );
      /* We got a block OK: now we should move the old contents to the
	 new address.  We use the old size of this block.  */
      memmove(new_vm_addr, h->vm_addr, h->size);
      MHASH_DEL( h->vm_addr );
      Free_Addr_Block( h->vm_addr, h->space_for ); /* Unmap old area. */

      MEMMETER( MVAL( M_Copy_Pages ) += (h->space_for/page_size) )
      MEMMETER( MVAL( M_Live_Pages ) -= (h->space_for / page_size))
      MEMMETER( MVAL( M_Live_Pages ) += (actual_sz / page_size))
      MEMMETER( MVAL( M_Wastage ) -= (h->space_for - h->size))
      MEMMETER( MVAL( M_Wastage ) += (actual_sz - sz) )

      /* Update block datastructure. */
      h->space_for = actual_sz;	/* New total space */
      h->size = sz;		/* New (requested) size */
      h->vm_addr = new_vm_addr;	/* New VM start address */
      h->aliased_address = ptr;	/* Change alias to reflect block relocation. */
      *ptr = (POINTER) h->vm_addr;
      return *ptr;
    }
}


/* Initialize various things for memory allocation.
 */
void
init_ralloc (void)
{
  int i = 0;
  if (r_alloc_initialized > 1)
    return;	/* used to return 1 */

#ifdef PDUMP
  /* Under pdump, we need to activate ralloc on the first go. */
  ++r_alloc_initialized;
#endif
  if (++r_alloc_initialized == 1)
    return;	/* used to return 1 */

  Addr_Block_initialize();	/* Initialize the address picker, if required. */
  page_size = PAGE;
  assert( page_size > 0 );	/* getpagesize() bogosity check. */

#ifndef MAP_ANONYMOUS
  DEV_ZERO_FD = open( "/dev/zero", O_RDWR );
  if (DEV_ZERO_FD < 0)
    /* Failed.  Perhaps we should abort here? */
    return;	/* used to return 0 */
#endif

#ifdef MMAP_METERING
  for(i = 0; i < N_Meterables; i++ )
    {
      meter[i] = 0;
    }
#endif /* MMAP_METERING */
}

void
syms_of_ralloc (void)
{
#ifdef MMAP_METERING
  defsymbol (&Qmmap_times_mapped, "mmap-times-mapped");
  defsymbol (&Qmmap_pages_mapped, "mmap-pages-mapped");
  defsymbol (&Qmmap_times_unmapped, "mmap-times-unmapped");
  defsymbol (&Qmmap_times_remapped, "mmap-times-remapped");
  defsymbol (&Qmmap_didnt_copy, "mmap-didnt-copy");
  defsymbol (&Qmmap_pages_copied, "mmap-pages-copied");
  defsymbol (&Qmmap_average_bumpval, "mmap-average-bumpval");
  defsymbol (&Qmmap_wastage, "mmap-wastage");
  defsymbol (&Qmmap_live_pages, "mmap-live-pages");
  defsymbol (&Qmmap_addr_looked_up, "mmap-addr-looked-up");
  defsymbol (&Qmmap_hash_worked, "mmap-hash-worked");
  defsymbol (&Qmmap_addrlist_size, "mmap-addrlist-size");
  DEFSUBR (Fmmap_allocator_status);
#endif /* MMAP_METERING */
}

void
vars_of_ralloc (void)
{
  DEFVAR_INT ("mmap-hysteresis", &mmap_hysteresis /*
Extra room left at the end of an allocated arena,
so that a re-alloc requesting extra space smaller than this
does not actually cause a new arena to be allocated.

A negative value is considered equal to zero.  This is the
minimum amount of space guaranteed to be left at the end of
the arena.  Because allocation happens in multiples of the OS
page size, it is possible for more space to be left unused.
*/ );
  mmap_hysteresis = 0;
}

#endif /* HAVE_MMAP */
