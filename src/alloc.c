/* Storage allocation and gc for XEmacs Lisp interpreter.
   Copyright (C) 1985-1998 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: FSF 19.28, Mule 2.0.  Substantially different from
   FSF. */

/* Authorship:

   FSF: Original version; a long time ago.
   Mly: Significantly rewritten to use new 3-bit tags and
        nicely abstracted object definitions, for 19.8.
   JWZ: Improved code to keep track of purespace usage and
        issue nice purespace and GC stats.
   Ben Wing: Cleaned up frob-block lrecord code, added error-checking
        and various changes for Mule, for 19.12.
        Added bit vectors for 19.13.
	Added lcrecord lists for 19.14.
   slb: Lots of work on the purification and dump time code.
        Synched Doug Lea malloc support from Emacs 20.2.
   og:  Killed the purespace.  Portable dumper (moved to dumper.c)
*/

#include <config.h>
#include "lisp.h"

#include "backtrace.h"
#include "buffer.h"
#include "bytecode.h"
#include "chartab.h"
#include "device.h"
#include "elhash.h"
#include "events.h"
#include "extents.h"
#include "frame.h"
#include "glyphs.h"
#include "opaque.h"
#include "redisplay.h"
#include "specifier.h"
#include "sysfile.h"
#include "sysdep.h"
#include "window.h"
#include "console-stream.h"

#ifdef DOUG_LEA_MALLOC
#include <malloc.h>
#endif

#ifdef PDUMP
#include "dumper.h"
#endif

EXFUN (Fgarbage_collect, 0);

#if 0 /* this is _way_ too slow to be part of the standard debug options */
#if defined(DEBUG_XEMACS) && defined(MULE)
#define VERIFY_STRING_CHARS_INTEGRITY
#endif
#endif

/* Define this to use malloc/free with no freelist for all datatypes,
   the hope being that some debugging tools may help detect
   freed memory references */
#ifdef USE_DEBUG_MALLOC	/* Taking the above comment at face value -slb */
#include <dmalloc.h>
#define ALLOC_NO_POOLS
#endif

#ifdef DEBUG_XEMACS
static Fixnum debug_allocation;
static Fixnum debug_allocation_backtrace_length;
#endif

/* Number of bytes of consing done since the last gc */
EMACS_INT consing_since_gc;
#define INCREMENT_CONS_COUNTER_1(size) (consing_since_gc += (size))

#define debug_allocation_backtrace()				\
do {								\
  if (debug_allocation_backtrace_length > 0)			\
    debug_short_backtrace (debug_allocation_backtrace_length);	\
} while (0)

#ifdef DEBUG_XEMACS
#define INCREMENT_CONS_COUNTER(foosize, type)			\
  do {								\
    if (debug_allocation)					\
      {								\
	stderr_out ("allocating %s (size %ld)\n", type, (long)foosize);	\
	debug_allocation_backtrace ();				\
      }								\
    INCREMENT_CONS_COUNTER_1 (foosize);				\
  } while (0)
#define NOSEEUM_INCREMENT_CONS_COUNTER(foosize, type)		\
  do {								\
    if (debug_allocation > 1)					\
      {								\
	stderr_out ("allocating noseeum %s (size %ld)\n", type, (long)foosize); \
	debug_allocation_backtrace ();				\
      }								\
    INCREMENT_CONS_COUNTER_1 (foosize);				\
  } while (0)
#else
#define INCREMENT_CONS_COUNTER(size, type) INCREMENT_CONS_COUNTER_1 (size)
#define NOSEEUM_INCREMENT_CONS_COUNTER(size, type) \
  INCREMENT_CONS_COUNTER_1 (size)
#endif

#define DECREMENT_CONS_COUNTER(size) do {	\
  consing_since_gc -= (size);			\
  if (consing_since_gc < 0)			\
    consing_since_gc = 0;			\
} while (0)

/* Number of bytes of consing since gc before another gc should be done. */
EMACS_INT gc_cons_threshold;

/* Nonzero during gc */
int gc_in_progress;

/* Number of times GC has happened at this level or below.
 * Level 0 is most volatile, contrary to usual convention.
 *  (Of course, there's only one level at present) */
EMACS_INT gc_generation_number[1];

/* This is just for use by the printer, to allow things to print uniquely */
static int lrecord_uid_counter;

/* Nonzero when calling certain hooks or doing other things where
   a GC would be bad */
int gc_currently_forbidden;

/* Hooks. */
Lisp_Object Vpre_gc_hook, Qpre_gc_hook;
Lisp_Object Vpost_gc_hook, Qpost_gc_hook;

/* "Garbage collecting" */
Lisp_Object Vgc_message;
Lisp_Object Vgc_pointer_glyph;
static const char gc_default_message[] = "Garbage collecting";
Lisp_Object Qgarbage_collecting;

/* Non-zero means we're in the process of doing the dump */
int purify_flag;

#ifdef ERROR_CHECK_TYPECHECK

Error_behavior ERROR_ME, ERROR_ME_NOT, ERROR_ME_WARN;

#endif

int
c_readonly (Lisp_Object obj)
{
  return POINTER_TYPE_P (XTYPE (obj)) && C_READONLY (obj);
}

int
lisp_readonly (Lisp_Object obj)
{
  return POINTER_TYPE_P (XTYPE (obj)) && LISP_READONLY (obj);
}


/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 0 /* 16000 */
#endif

/* Non-zero means ignore malloc warnings.  Set during initialization.  */
int ignore_malloc_warnings;


static void *breathing_space;

void
release_breathing_space (void)
{
  if (breathing_space)
    {
      void *tmp = breathing_space;
      breathing_space = 0;
      xfree (tmp);
    }
}

/* malloc calls this if it finds we are near exhausting storage */
void
malloc_warning (const char *str)
{
  if (ignore_malloc_warnings)
    return;

  warn_when_safe
    (Qmemory, Qcritical,
     "%s\n"
     "Killing some buffers may delay running out of memory.\n"
     "However, certainly by the time you receive the 95%% warning,\n"
     "you should clean up, kill this Emacs, and start a new one.",
     str);
}

/* Called if malloc returns zero */
DOESNT_RETURN
memory_full (void)
{
  /* Force a GC next time eval is called.
     It's better to loop garbage-collecting (we might reclaim enough
     to win) than to loop beeping and barfing "Memory exhausted"
   */
  consing_since_gc = gc_cons_threshold + 1;
  release_breathing_space ();

  /* Flush some histories which might conceivably contain garbalogical
     inhibitors.  */
  if (!NILP (Fboundp (Qvalues)))
    Fset (Qvalues, Qnil);
  Vcommand_history = Qnil;

  error ("Memory exhausted");
}

/* like malloc and realloc but check for no memory left, and block input. */

#undef xmalloc
void *
xmalloc (size_t size)
{
  void *val = malloc (size);

  if (!val && (size != 0)) memory_full ();
  return val;
}

#undef xcalloc
static void *
xcalloc (size_t nelem, size_t elsize)
{
  void *val = calloc (nelem, elsize);

  if (!val && (nelem != 0)) memory_full ();
  return val;
}

void *
xmalloc_and_zero (size_t size)
{
  return xcalloc (size, sizeof (char));
}

#undef xrealloc
void *
xrealloc (void *block, size_t size)
{
  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  void *val = block ? realloc (block, size) : malloc (size);

  if (!val && (size != 0)) memory_full ();
  return val;
}

void
#ifdef ERROR_CHECK_MALLOC
xfree_1 (void *block)
#else
xfree (void *block)
#endif
{
#ifdef ERROR_CHECK_MALLOC
  /* Unbelievably, calling free() on 0xDEADBEEF doesn't cause an
     error until much later on for many system mallocs, such as
     the one that comes with Solaris 2.3.  FMH!! */
  assert (block != (void *) 0xDEADBEEF);
  assert (block);
#endif /* ERROR_CHECK_MALLOC */
  free (block);
}

#ifdef ERROR_CHECK_GC

#if SIZEOF_INT == 4
typedef unsigned int four_byte_t;
#elif SIZEOF_LONG == 4
typedef unsigned long four_byte_t;
#elif SIZEOF_SHORT == 4
typedef unsigned short four_byte_t;
#else
What kind of strange-ass system are we running on?
#endif

static void
deadbeef_memory (void *ptr, size_t size)
{
  four_byte_t *ptr4 = (four_byte_t *) ptr;
  size_t beefs = size >> 2;

  /* In practice, size will always be a multiple of four.  */
  while (beefs--)
    (*ptr4++) = 0xDEADBEEF;
}

#else /* !ERROR_CHECK_GC */


#define deadbeef_memory(ptr, size)

#endif /* !ERROR_CHECK_GC */

#undef xstrdup
char *
xstrdup (const char *str)
{
  int len = strlen (str) + 1;   /* for stupid terminating 0 */

  void *val = xmalloc (len);
  if (val == 0) return 0;
  return (char *) memcpy (val, str, len);
}

#ifdef NEED_STRDUP
char *
strdup (const char *s)
{
  return xstrdup (s);
}
#endif /* NEED_STRDUP */


static void *
allocate_lisp_storage (size_t size)
{
  return xmalloc (size);
}


/* lcrecords are chained together through their "next" field.
   After doing the mark phase, GC will walk this linked list
   and free any lcrecord which hasn't been marked. */
static struct lcrecord_header *all_lcrecords;

void *
alloc_lcrecord (size_t size, const struct lrecord_implementation *implementation)
{
  struct lcrecord_header *lcheader;

  type_checking_assert
    ((implementation->static_size == 0 ?
      implementation->size_in_bytes_method != NULL :
      implementation->static_size == size)
     &&
     (! implementation->basic_p)
     &&
     (! (implementation->hash == NULL && implementation->equal != NULL)));

  lcheader = (struct lcrecord_header *) allocate_lisp_storage (size);
  set_lheader_implementation (&lcheader->lheader, implementation);
  lcheader->next = all_lcrecords;
#if 1                           /* mly prefers to see small ID numbers */
  lcheader->uid = lrecord_uid_counter++;
#else				/* jwz prefers to see real addrs */
  lcheader->uid = (int) &lcheader;
#endif
  lcheader->free = 0;
  all_lcrecords = lcheader;
  INCREMENT_CONS_COUNTER (size, implementation->name);
  return lcheader;
}

#if 0 /* Presently unused */
/* Very, very poor man's EGC?
 * This may be slow and thrash pages all over the place.
 *  Only call it if you really feel you must (and if the
 *  lrecord was fairly recently allocated).
 * Otherwise, just let the GC do its job -- that's what it's there for
 */
void
free_lcrecord (struct lcrecord_header *lcrecord)
{
  if (all_lcrecords == lcrecord)
    {
      all_lcrecords = lcrecord->next;
    }
  else
    {
      struct lrecord_header *header = all_lcrecords;
      for (;;)
	{
	  struct lrecord_header *next = header->next;
	  if (next == lcrecord)
	    {
	      header->next = lrecord->next;
	      break;
	    }
	  else if (next == 0)
	    abort ();
	  else
	    header = next;
	}
    }
  if (lrecord->implementation->finalizer)
    lrecord->implementation->finalizer (lrecord, 0);
  xfree (lrecord);
  return;
}
#endif /* Unused */


static void
disksave_object_finalization_1 (void)
{
  struct lcrecord_header *header;

  for (header = all_lcrecords; header; header = header->next)
    {
      if (LHEADER_IMPLEMENTATION (&header->lheader)->finalizer &&
	  !header->free)
	LHEADER_IMPLEMENTATION (&header->lheader)->finalizer (header, 1);
    }
}


/************************************************************************/
/*			  Debugger support				*/
/************************************************************************/
/* Give gdb/dbx enough information to decode Lisp Objects.  We make
   sure certain symbols are always defined, so gdb doesn't complain
   about expressions in src/.gdbinit.  See src/.gdbinit or src/.dbxrc
   to see how this is used.  */

EMACS_UINT dbg_valmask = ((1UL << VALBITS) - 1) << GCBITS;
EMACS_UINT dbg_typemask = (1UL << GCTYPEBITS) - 1;

#ifdef USE_UNION_TYPE
unsigned char dbg_USE_UNION_TYPE = 1;
#else
unsigned char dbg_USE_UNION_TYPE = 0;
#endif

unsigned char dbg_valbits = VALBITS;
unsigned char dbg_gctypebits = GCTYPEBITS;

/* On some systems, the above definitions will be optimized away by
   the compiler or linker unless they are referenced in some function. */
long dbg_inhibit_dbg_symbol_deletion (void);
long
dbg_inhibit_dbg_symbol_deletion (void)
{
  return
    (dbg_valmask +
     dbg_typemask +
     dbg_USE_UNION_TYPE +
     dbg_valbits +
     dbg_gctypebits);
}

/* Macros turned into functions for ease of debugging.
   Debuggers don't know about macros! */
int dbg_eq (Lisp_Object obj1, Lisp_Object obj2);
int
dbg_eq (Lisp_Object obj1, Lisp_Object obj2)
{
  return EQ (obj1, obj2);
}


/************************************************************************/
/*			  Fixed-size type macros			*/
/************************************************************************/

/* For fixed-size types that are commonly used, we malloc() large blocks
   of memory at a time and subdivide them into chunks of the correct
   size for an object of that type.  This is more efficient than
   malloc()ing each object separately because we save on malloc() time
   and overhead due to the fewer number of malloc()ed blocks, and
   also because we don't need any extra pointers within each object
   to keep them threaded together for GC purposes.  For less common
   (and frequently large-size) types, we use lcrecords, which are
   malloc()ed individually and chained together through a pointer
   in the lcrecord header.  lcrecords do not need to be fixed-size
   (i.e. two objects of the same type need not have the same size;
   however, the size of a particular object cannot vary dynamically).
   It is also much easier to create a new lcrecord type because no
   additional code needs to be added to alloc.c.  Finally, lcrecords
   may be more efficient when there are only a small number of them.

   The types that are stored in these large blocks (or "frob blocks")
   are cons, float, compiled-function, symbol, marker, extent, event,
   and string.

   Note that strings are special in that they are actually stored in
   two parts: a structure containing information about the string, and
   the actual data associated with the string.  The former structure
   (a struct Lisp_String) is a fixed-size structure and is managed the
   same way as all the other such types.  This structure contains a
   pointer to the actual string data, which is stored in structures of
   type struct string_chars_block.  Each string_chars_block consists
   of a pointer to a struct Lisp_String, followed by the data for that
   string, followed by another pointer to a Lisp_String, followed by
   the data for that string, etc.  At GC time, the data in these
   blocks is compacted by searching sequentially through all the
   blocks and compressing out any holes created by unmarked strings.
   Strings that are more than a certain size (bigger than the size of
   a string_chars_block, although something like half as big might
   make more sense) are malloc()ed separately and not stored in
   string_chars_blocks.  Furthermore, no one string stretches across
   two string_chars_blocks.

   Vectors are each malloc()ed separately, similar to lcrecords.

   In the following discussion, we use conses, but it applies equally
   well to the other fixed-size types.

   We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc() whenever necessary.  Cons cells reclaimed
   by GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.  Each cons_block is
   just under 2^n - MALLOC_OVERHEAD bytes long, since malloc (at least
   the versions in malloc.c and gmalloc.c) really allocates in units
   of powers of two and uses 4 bytes for its own overhead.

   What GC actually does is to search through all the cons_blocks,
   from the most recently allocated to the oldest, and put all
   cons cells that are not marked (whether or not they're already
   free) on a cons_free_list.  The cons_free_list is a stack, and
   so the cons cells in the oldest-allocated cons_block end up
   at the head of the stack and are the first to be reallocated.
   If any cons_block is entirely free, it is freed with free()
   and its cons cells removed from the cons_free_list.  Because
   the cons_free_list ends up basically in memory order, we have
   a high locality of reference (assuming a reasonable turnover
   of allocating and freeing) and have a reasonable probability
   of entirely freeing up cons_blocks that have been more recently
   allocated.  This stage is called the "sweep stage" of GC, and
   is executed after the "mark stage", which involves starting
   from all places that are known to point to in-use Lisp objects
   (e.g. the obarray, where are all symbols are stored; the
   current catches and condition-cases; the backtrace list of
   currently executing functions; the gcpro list; etc.) and
   recursively marking all objects that are accessible.

   At the beginning of the sweep stage, the conses in the cons blocks
   are in one of three states: in use and marked, in use but not
   marked, and not in use (already freed).  Any conses that are marked
   have been marked in the mark stage just executed, because as part
   of the sweep stage we unmark any marked objects.  The way we tell
   whether or not a cons cell is in use is through the LRECORD_FREE_P
   macro.  This uses a special lrecord type `lrecord_type_free',
   which is never associated with any valid object.

   Conses on the free_cons_list are threaded through a pointer stored
   in the conses themselves.  Because the cons is still in a
   cons_block and needs to remain marked as not in use for the next
   time that GC happens, we need room to store both the "free"
   indicator and the chaining pointer.  So this pointer is stored
   after the lrecord header (actually where C places a pointer after
   the lrecord header; they are not necessarily contiguous).  This
   implies that all fixed-size types must be big enough to contain at
   least one pointer.  This is true for all current fixed-size types,
   with the possible exception of Lisp_Floats, for which we define the
   meat of the struct using a union of a pointer and a double to
   ensure adequate space for the free list chain pointer.

   Some types of objects need additional "finalization" done
   when an object is converted from in use to not in use;
   this is the purpose of the ADDITIONAL_FREE_type macro.
   For example, markers need to be removed from the chain
   of markers that is kept in each buffer.  This is because
   markers in a buffer automatically disappear if the marker
   is no longer referenced anywhere (the same does not
   apply to extents, however).

   WARNING: Things are in an extremely bizarre state when
   the ADDITIONAL_FREE_type macros are called, so beware!

   When ERROR_CHECK_GC is defined, we do things differently so as to
   maximize our chances of catching places where there is insufficient
   GCPROing.  The thing we want to avoid is having an object that
   we're using but didn't GCPRO get freed by GC and then reallocated
   while we're in the process of using it -- this will result in
   something seemingly unrelated getting trashed, and is extremely
   difficult to track down.  If the object gets freed but not
   reallocated, we can usually catch this because we set most of the
   bytes of a freed object to 0xDEADBEEF. (The lisp object type is set
   to the invalid type `lrecord_type_free', however, and a pointer
   used to chain freed objects together is stored after the lrecord
   header; we play some tricks with this pointer to make it more
   bogus, so crashes are more likely to occur right away.)

   We want freed objects to stay free as long as possible,
   so instead of doing what we do above, we maintain the
   free objects in a first-in first-out queue.  We also
   don't recompute the free list each GC, unlike above;
   this ensures that the queue ordering is preserved.
   [This means that we are likely to have worse locality
   of reference, and that we can never free a frob block
   once it's allocated. (Even if we know that all cells
   in it are free, there's no easy way to remove all those
   cells from the free list because the objects on the
   free list are unlikely to be in memory order.)]
   Furthermore, we never take objects off the free list
   unless there's a large number (usually 1000, but
   varies depending on type) of them already on the list.
   This way, we ensure that an object that gets freed will
   remain free for the next 1000 (or whatever) times that
   an object of that type is allocated.  */

#ifndef MALLOC_OVERHEAD
#ifdef GNU_MALLOC
#define MALLOC_OVERHEAD 0
#elif defined (rcheck)
#define MALLOC_OVERHEAD 20
#else
#define MALLOC_OVERHEAD 8
#endif
#endif /* MALLOC_OVERHEAD */

#if !defined(HAVE_MMAP) || defined(DOUG_LEA_MALLOC)
/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c.  */
void refill_memory_reserve (void);
void
refill_memory_reserve (void)
{
  if (breathing_space == 0)
    breathing_space = (char *) malloc (4096 - MALLOC_OVERHEAD);
}
#endif

#ifdef ALLOC_NO_POOLS
# define TYPE_ALLOC_SIZE(type, structtype) 1
#else
# define TYPE_ALLOC_SIZE(type, structtype)			\
    ((2048 - MALLOC_OVERHEAD - sizeof (struct type##_block *))	\
     / sizeof (structtype))
#endif /* ALLOC_NO_POOLS */

#define DECLARE_FIXED_TYPE_ALLOC(type, structtype)	\
							\
struct type##_block					\
{							\
  struct type##_block *prev;				\
  structtype block[TYPE_ALLOC_SIZE (type, structtype)];	\
};							\
							\
static struct type##_block *current_##type##_block;	\
static int current_##type##_block_index;		\
							\
static Lisp_Free *type##_free_list;			\
static Lisp_Free *type##_free_list_tail;		\
							\
static void						\
init_##type##_alloc (void)				\
{							\
  current_##type##_block = 0;				\
  current_##type##_block_index =			\
    countof (current_##type##_block->block);		\
  type##_free_list = 0;					\
  type##_free_list_tail = 0;				\
}							\
							\
static int gc_count_num_##type##_in_use;		\
static int gc_count_num_##type##_freelist

#define ALLOCATE_FIXED_TYPE_FROM_BLOCK(type, result) do {		\
  if (current_##type##_block_index					\
      == countof (current_##type##_block->block))			\
    {									\
      struct type##_block *AFTFB_new = (struct type##_block *)		\
	allocate_lisp_storage (sizeof (struct type##_block));		\
      AFTFB_new->prev = current_##type##_block;				\
      current_##type##_block = AFTFB_new;				\
      current_##type##_block_index = 0;					\
    }									\
  (result) =								\
    &(current_##type##_block->block[current_##type##_block_index++]);	\
} while (0)

/* Allocate an instance of a type that is stored in blocks.
   TYPE is the "name" of the type, STRUCTTYPE is the corresponding
   structure type. */

#ifdef ERROR_CHECK_GC

/* Note: if you get crashes in this function, suspect incorrect calls
   to free_cons() and friends.  This happened once because the cons
   cell was not GC-protected and was getting collected before
   free_cons() was called. */

#define ALLOCATE_FIXED_TYPE_1(type, structtype, result) do {	\
  if (gc_count_num_##type##_freelist >				\
      MINIMUM_ALLOWED_FIXED_TYPE_CELLS_##type)			\
    {								\
      result = (structtype *) type##_free_list;			\
      /* Before actually using the chain pointer,		\
	 we complement all its bits; see FREE_FIXED_TYPE(). */	\
      type##_free_list = (Lisp_Free *)				\
	(~ (EMACS_UINT) (type##_free_list->chain));		\
      gc_count_num_##type##_freelist--;				\
    }								\
  else								\
    ALLOCATE_FIXED_TYPE_FROM_BLOCK (type, result);		\
  MARK_LRECORD_AS_NOT_FREE (result);				\
} while (0)

#else /* !ERROR_CHECK_GC */

#define ALLOCATE_FIXED_TYPE_1(type, structtype, result) do {	\
  if (type##_free_list)						\
    {								\
      result = (structtype *) type##_free_list;			\
      type##_free_list = type##_free_list->chain;		\
    }								\
  else								\
    ALLOCATE_FIXED_TYPE_FROM_BLOCK (type, result);		\
  MARK_LRECORD_AS_NOT_FREE (result);				\
} while (0)

#endif /* !ERROR_CHECK_GC */


#define ALLOCATE_FIXED_TYPE(type, structtype, result)	\
do							\
{							\
  ALLOCATE_FIXED_TYPE_1 (type, structtype, result);	\
  INCREMENT_CONS_COUNTER (sizeof (structtype), #type);	\
} while (0)

#define NOSEEUM_ALLOCATE_FIXED_TYPE(type, structtype, result)	\
do								\
{								\
  ALLOCATE_FIXED_TYPE_1 (type, structtype, result);		\
  NOSEEUM_INCREMENT_CONS_COUNTER (sizeof (structtype), #type);	\
} while (0)


/* Lisp_Free is the type to represent a free list member inside a frob
   block of any lisp object type.  */
typedef struct Lisp_Free
{
  struct lrecord_header lheader;
  struct Lisp_Free *chain;
} Lisp_Free;

#define LRECORD_FREE_P(ptr) \
((ptr)->lheader.type == lrecord_type_free)

#define MARK_LRECORD_AS_FREE(ptr) \
((void) ((ptr)->lheader.type = lrecord_type_free))

#ifdef ERROR_CHECK_GC
#define MARK_LRECORD_AS_NOT_FREE(ptr) \
((void) ((ptr)->lheader.type = lrecord_type_undefined))
#else
#define MARK_LRECORD_AS_NOT_FREE(ptr) DO_NOTHING
#endif

#ifdef ERROR_CHECK_GC

#define PUT_FIXED_TYPE_ON_FREE_LIST(type, structtype, ptr) do {	\
  if (type##_free_list_tail)					\
    {								\
      /* When we store the chain pointer, we complement all	\
	 its bits; this should significantly increase its	\
	 bogosity in case someone tries to use the value, and	\
	 should make us crash faster if someone overwrites the	\
	 pointer because when it gets un-complemented in	\
	 ALLOCATED_FIXED_TYPE(), the resulting pointer will be	\
	 extremely bogus. */					\
      type##_free_list_tail->chain =				\
	(Lisp_Free *) ~ (EMACS_UINT) (ptr);			\
    }								\
  else								\
    type##_free_list = (Lisp_Free *) (ptr);			\
  type##_free_list_tail = (Lisp_Free *) (ptr);			\
} while (0)

#else /* !ERROR_CHECK_GC */

#define PUT_FIXED_TYPE_ON_FREE_LIST(type, structtype, ptr) do {	\
  ((Lisp_Free *) (ptr))->chain = type##_free_list;		\
  type##_free_list = (Lisp_Free *) (ptr);			\
} while (0)							\

#endif /* !ERROR_CHECK_GC */

/* TYPE and STRUCTTYPE are the same as in ALLOCATE_FIXED_TYPE(). */

#define FREE_FIXED_TYPE(type, structtype, ptr) do {		\
  structtype *FFT_ptr = (ptr);					\
  ADDITIONAL_FREE_##type (FFT_ptr);				\
  deadbeef_memory (FFT_ptr, sizeof (structtype));		\
  PUT_FIXED_TYPE_ON_FREE_LIST (type, structtype, FFT_ptr);	\
  MARK_LRECORD_AS_FREE (FFT_ptr);				\
} while (0)

/* Like FREE_FIXED_TYPE() but used when we are explicitly
   freeing a structure through free_cons(), free_marker(), etc.
   rather than through the normal process of sweeping.
   We attempt to undo the changes made to the allocation counters
   as a result of this structure being allocated.  This is not
   completely necessary but helps keep things saner: e.g. this way,
   repeatedly allocating and freeing a cons will not result in
   the consing-since-gc counter advancing, which would cause a GC
   and somewhat defeat the purpose of explicitly freeing. */

#define FREE_FIXED_TYPE_WHEN_NOT_IN_GC(type, structtype, ptr)	\
do { FREE_FIXED_TYPE (type, structtype, ptr);			\
     DECREMENT_CONS_COUNTER (sizeof (structtype));		\
     gc_count_num_##type##_freelist++;				\
   } while (0)



/************************************************************************/
/*			   Cons allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (cons, Lisp_Cons);
/* conses are used and freed so often that we set this really high */
/* #define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_cons 20000 */
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_cons 2000

static Lisp_Object
mark_cons (Lisp_Object obj)
{
  if (NILP (XCDR (obj)))
    return XCAR (obj);

  mark_object (XCAR (obj));
  return XCDR (obj);
}

static int
cons_equal (Lisp_Object ob1, Lisp_Object ob2, int depth)
{
  depth++;
  while (internal_equal (XCAR (ob1), XCAR (ob2), depth))
    {
      ob1 = XCDR (ob1);
      ob2 = XCDR (ob2);
      if (! CONSP (ob1) || ! CONSP (ob2))
	return internal_equal (ob1, ob2, depth);
    }
  return 0;
}

static const struct lrecord_description cons_description[] = {
  { XD_LISP_OBJECT, offsetof (Lisp_Cons, car) },
  { XD_LISP_OBJECT, offsetof (Lisp_Cons, cdr) },
  { XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION ("cons", cons,
				     mark_cons, print_cons, 0,
				     cons_equal,
				     /*
				      * No `hash' method needed.
				      * internal_hash knows how to
				      * handle conses.
				      */
				     0,
				     cons_description,
				     Lisp_Cons);

DEFUN ("cons", Fcons, 2, 2, 0, /*
Create a new cons, give it CAR and CDR as components, and return it.
*/
       (car, cdr))
{
  /* This cannot GC. */
  Lisp_Object val;
  Lisp_Cons *c;

  ALLOCATE_FIXED_TYPE (cons, Lisp_Cons, c);
  set_lheader_implementation (&c->lheader, &lrecord_cons);
  XSETCONS (val, c);
  c->car = car;
  c->cdr = cdr;
  return val;
}

/* This is identical to Fcons() but it used for conses that we're
   going to free later, and is useful when trying to track down
   "real" consing. */
Lisp_Object
noseeum_cons (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object val;
  Lisp_Cons *c;

  NOSEEUM_ALLOCATE_FIXED_TYPE (cons, Lisp_Cons, c);
  set_lheader_implementation (&c->lheader, &lrecord_cons);
  XSETCONS (val, c);
  XCAR (val) = car;
  XCDR (val) = cdr;
  return val;
}

DEFUN ("list", Flist, 0, MANY, 0, /*
Return a newly created list with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object val = Qnil;
  Lisp_Object *argp = args + nargs;

  while (argp > args)
    val = Fcons (*--argp, val);
  return val;
}

Lisp_Object
list1 (Lisp_Object obj0)
{
  /* This cannot GC. */
  return Fcons (obj0, Qnil);
}

Lisp_Object
list2 (Lisp_Object obj0, Lisp_Object obj1)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Qnil));
}

Lisp_Object
list3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Qnil)));
}

Lisp_Object
cons3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, obj2));
}

Lisp_Object
acons (Lisp_Object key, Lisp_Object value, Lisp_Object alist)
{
  return Fcons (Fcons (key, value), alist);
}

Lisp_Object
list4 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Qnil))));
}

Lisp_Object
list5 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3,
       Lisp_Object obj4)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Fcons (obj4, Qnil)))));
}

Lisp_Object
list6 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2, Lisp_Object obj3,
       Lisp_Object obj4, Lisp_Object obj5)
{
  /* This cannot GC. */
  return Fcons (obj0, Fcons (obj1, Fcons (obj2, Fcons (obj3, Fcons (obj4, Fcons (obj5, Qnil))))));
}

DEFUN ("make-list", Fmake_list, 2, 2, 0, /*
Return a new list of length LENGTH, with each element being OBJECT.
*/
       (length, object))
{
  CHECK_NATNUM (length);

  {
    Lisp_Object val = Qnil;
    size_t size = XINT (length);

    while (size--)
      val = Fcons (object, val);
    return val;
  }
}


/************************************************************************/
/*			  Float allocation				*/
/************************************************************************/

#ifdef LISP_FLOAT_TYPE

DECLARE_FIXED_TYPE_ALLOC (float, Lisp_Float);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_float 1000

Lisp_Object
make_float (double float_value)
{
  Lisp_Object val;
  Lisp_Float *f;

  ALLOCATE_FIXED_TYPE (float, Lisp_Float, f);

  /* Avoid dump-time `uninitialized memory read' purify warnings. */
  if (sizeof (struct lrecord_header) + sizeof (double) != sizeof (*f))
    xzero (*f);

  set_lheader_implementation (&f->lheader, &lrecord_float);
  float_data (f) = float_value;
  XSETFLOAT (val, f);
  return val;
}

#endif /* LISP_FLOAT_TYPE */


/************************************************************************/
/*			   Vector allocation				*/
/************************************************************************/

static Lisp_Object
mark_vector (Lisp_Object obj)
{
  Lisp_Vector *ptr = XVECTOR (obj);
  int len = vector_length (ptr);
  int i;

  for (i = 0; i < len - 1; i++)
    mark_object (ptr->contents[i]);
  return (len > 0) ? ptr->contents[len - 1] : Qnil;
}

static size_t
size_vector (const void *lheader)
{
  return FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Vector, Lisp_Object, contents,
				       ((Lisp_Vector *) lheader)->size);
}

static int
vector_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  int len = XVECTOR_LENGTH (obj1);
  if (len != XVECTOR_LENGTH (obj2))
    return 0;

  {
    Lisp_Object *ptr1 = XVECTOR_DATA (obj1);
    Lisp_Object *ptr2 = XVECTOR_DATA (obj2);
    while (len--)
      if (!internal_equal (*ptr1++, *ptr2++, depth + 1))
	return 0;
  }
  return 1;
}

static hashcode_t
vector_hash (Lisp_Object obj, int depth)
{
  return HASH2 (XVECTOR_LENGTH (obj),
		internal_array_hash (XVECTOR_DATA (obj),
				     XVECTOR_LENGTH (obj),
				     depth + 1));
}

static const struct lrecord_description vector_description[] = {
  { XD_LONG,              offsetof (Lisp_Vector, size) },
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Vector, contents), XD_INDIRECT(0, 0) },
  { XD_END }
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION("vector", vector,
				       mark_vector, print_vector, 0,
				       vector_equal,
				       vector_hash,
				       vector_description,
				       size_vector, Lisp_Vector);

/* #### should allocate `small' vectors from a frob-block */
static Lisp_Vector *
make_vector_internal (size_t sizei)
{
  /* no vector_next */
  size_t sizem = FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Vector, Lisp_Object,
					       contents, sizei);
  Lisp_Vector *p = (Lisp_Vector *) alloc_lcrecord (sizem, &lrecord_vector);

  p->size = sizei;
  return p;
}

Lisp_Object
make_vector (size_t length, Lisp_Object object)
{
  Lisp_Vector *vecp = make_vector_internal (length);
  Lisp_Object *p = vector_data (vecp);

  while (length--)
    *p++ = object;

  {
    Lisp_Object vector;
    XSETVECTOR (vector, vecp);
    return vector;
  }
}

DEFUN ("make-vector", Fmake_vector, 2, 2, 0, /*
Return a new vector of length LENGTH, with each element being OBJECT.
See also the function `vector'.
*/
       (length, object))
{
  CONCHECK_NATNUM (length);
  return make_vector (XINT (length), object);
}

DEFUN ("vector", Fvector, 0, MANY, 0, /*
Return a newly created vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Vector *vecp = make_vector_internal (nargs);
  Lisp_Object *p = vector_data (vecp);

  while (nargs--)
    *p++ = *args++;

  {
    Lisp_Object vector;
    XSETVECTOR (vector, vecp);
    return vector;
  }
}

Lisp_Object
vector1 (Lisp_Object obj0)
{
  return Fvector (1, &obj0);
}

Lisp_Object
vector2 (Lisp_Object obj0, Lisp_Object obj1)
{
  Lisp_Object args[2];
  args[0] = obj0;
  args[1] = obj1;
  return Fvector (2, args);
}

Lisp_Object
vector3 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2)
{
  Lisp_Object args[3];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  return Fvector (3, args);
}

#if 0 /* currently unused */

Lisp_Object
vector4 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3)
{
  Lisp_Object args[4];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  return Fvector (4, args);
}

Lisp_Object
vector5 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4)
{
  Lisp_Object args[5];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  return Fvector (5, args);
}

Lisp_Object
vector6 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5)
{
  Lisp_Object args[6];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  return Fvector (6, args);
}

Lisp_Object
vector7 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5,
	 Lisp_Object obj6)
{
  Lisp_Object args[7];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  args[6] = obj6;
  return Fvector (7, args);
}

Lisp_Object
vector8 (Lisp_Object obj0, Lisp_Object obj1, Lisp_Object obj2,
	 Lisp_Object obj3, Lisp_Object obj4, Lisp_Object obj5,
	 Lisp_Object obj6, Lisp_Object obj7)
{
  Lisp_Object args[8];
  args[0] = obj0;
  args[1] = obj1;
  args[2] = obj2;
  args[3] = obj3;
  args[4] = obj4;
  args[5] = obj5;
  args[6] = obj6;
  args[7] = obj7;
  return Fvector (8, args);
}
#endif /* unused */

/************************************************************************/
/*			 Bit Vector allocation				*/
/************************************************************************/

static Lisp_Object all_bit_vectors;

/* #### should allocate `small' bit vectors from a frob-block */
static Lisp_Bit_Vector *
make_bit_vector_internal (size_t sizei)
{
  size_t num_longs = BIT_VECTOR_LONG_STORAGE (sizei);
  size_t sizem = FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Bit_Vector, unsigned long,
					       bits, num_longs);
  Lisp_Bit_Vector *p = (Lisp_Bit_Vector *) allocate_lisp_storage (sizem);
  set_lheader_implementation (&p->lheader, &lrecord_bit_vector);

  INCREMENT_CONS_COUNTER (sizem, "bit-vector");

  bit_vector_length (p) = sizei;
  bit_vector_next   (p) = all_bit_vectors;
  /* make sure the extra bits in the last long are 0; the calling
     functions might not set them. */
  p->bits[num_longs - 1] = 0;
  XSETBIT_VECTOR (all_bit_vectors, p);
  return p;
}

Lisp_Object
make_bit_vector (size_t length, Lisp_Object bit)
{
  Lisp_Bit_Vector *p = make_bit_vector_internal (length);
  size_t num_longs = BIT_VECTOR_LONG_STORAGE (length);

  CHECK_BIT (bit);

  if (ZEROP (bit))
    memset (p->bits, 0, num_longs * sizeof (long));
  else
    {
      size_t bits_in_last = length & (LONGBITS_POWER_OF_2 - 1);
      memset (p->bits, ~0, num_longs * sizeof (long));
      /* But we have to make sure that the unused bits in the
	 last long are 0, so that equal/hash is easy. */
      if (bits_in_last)
	p->bits[num_longs - 1] &= (1 << bits_in_last) - 1;
    }

  {
    Lisp_Object bit_vector;
    XSETBIT_VECTOR (bit_vector, p);
    return bit_vector;
  }
}

Lisp_Object
make_bit_vector_from_byte_vector (unsigned char *bytevec, size_t length)
{
  size_t i;
  Lisp_Bit_Vector *p = make_bit_vector_internal (length);

  for (i = 0; i < length; i++)
    set_bit_vector_bit (p, i, bytevec[i]);

  {
    Lisp_Object bit_vector;
    XSETBIT_VECTOR (bit_vector, p);
    return bit_vector;
  }
}

DEFUN ("make-bit-vector", Fmake_bit_vector, 2, 2, 0, /*
Return a new bit vector of length LENGTH. with each bit set to BIT.
BIT must be one of the integers 0 or 1.  See also the function `bit-vector'.
*/
       (length, bit))
{
  CONCHECK_NATNUM (length);

  return make_bit_vector (XINT (length), bit);
}

DEFUN ("bit-vector", Fbit_vector, 0, MANY, 0, /*
Return a newly created bit vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
Each argument must be one of the integers 0 or 1.
*/
       (int nargs, Lisp_Object *args))
{
  int i;
  Lisp_Bit_Vector *p = make_bit_vector_internal (nargs);

  for (i = 0; i < nargs; i++)
    {
      CHECK_BIT (args[i]);
      set_bit_vector_bit (p, i, !ZEROP (args[i]));
    }

  {
    Lisp_Object bit_vector;
    XSETBIT_VECTOR (bit_vector, p);
    return bit_vector;
  }
}


/************************************************************************/
/*		     Compiled-function allocation			*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (compiled_function, Lisp_Compiled_Function);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_compiled_function 1000

static Lisp_Object
make_compiled_function (void)
{
  Lisp_Compiled_Function *f;
  Lisp_Object fun;

  ALLOCATE_FIXED_TYPE (compiled_function, Lisp_Compiled_Function, f);
  set_lheader_implementation (&f->lheader, &lrecord_compiled_function);

  f->stack_depth = 0;
  f->specpdl_depth = 0;
  f->flags.documentationp = 0;
  f->flags.interactivep = 0;
  f->flags.domainp = 0; /* I18N3 */
  f->instructions = Qzero;
  f->constants = Qzero;
  f->arglist = Qnil;
  f->doc_and_interactive = Qnil;
#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  f->annotated = Qnil;
#endif
  XSETCOMPILED_FUNCTION (fun, f);
  return fun;
}

DEFUN ("make-byte-code", Fmake_byte_code, 4, MANY, 0, /*
Return a new compiled-function object.
Usage: (arglist instructions constants stack-depth
	&optional doc-string interactive)
Note that, unlike all other emacs-lisp functions, calling this with five
arguments is NOT the same as calling it with six arguments, the last of
which is nil.  If the INTERACTIVE arg is specified as nil, then that means
that this function was defined with `(interactive)'.  If the arg is not
specified, then that means the function is not interactive.
This is terrible behavior which is retained for compatibility with old
`.elc' files which expect these semantics.
*/
       (int nargs, Lisp_Object *args))
{
/* In a non-insane world this function would have this arglist...
   (arglist instructions constants stack_depth &optional doc_string interactive)
 */
  Lisp_Object fun = make_compiled_function ();
  Lisp_Compiled_Function *f = XCOMPILED_FUNCTION (fun);

  Lisp_Object arglist      = args[0];
  Lisp_Object instructions = args[1];
  Lisp_Object constants    = args[2];
  Lisp_Object stack_depth  = args[3];
  Lisp_Object doc_string   = (nargs > 4) ? args[4] : Qnil;
  Lisp_Object interactive  = (nargs > 5) ? args[5] : Qunbound;

  if (nargs < 4 || nargs > 6)
    return Fsignal (Qwrong_number_of_arguments,
		    list2 (intern ("make-byte-code"), make_int (nargs)));

  /* Check for valid formal parameter list now, to allow us to use
     SPECBIND_FAST_UNSAFE() later in funcall_compiled_function(). */
  {
    EXTERNAL_LIST_LOOP_3 (symbol, arglist, tail)
      {
	CHECK_SYMBOL (symbol);
	if (EQ (symbol, Qt)   ||
	    EQ (symbol, Qnil) ||
	    SYMBOL_IS_KEYWORD (symbol))
	  signal_simple_error_2
	    ("Invalid constant symbol in formal parameter list",
	     symbol, arglist);
      }
  }
  f->arglist = arglist;

  /* `instructions' is a string or a cons (string . int) for a
     lazy-loaded function. */
  if (CONSP (instructions))
    {
      CHECK_STRING (XCAR (instructions));
      CHECK_INT (XCDR (instructions));
    }
  else
    {
      CHECK_STRING (instructions);
    }
  f->instructions = instructions;

  if (!NILP (constants))
    CHECK_VECTOR (constants);
  f->constants = constants;

  CHECK_NATNUM (stack_depth);
  f->stack_depth = (unsigned short) XINT (stack_depth);

#ifdef COMPILED_FUNCTION_ANNOTATION_HACK
  if (!NILP (Vcurrent_compiled_function_annotation))
    f->annotated = Fcopy (Vcurrent_compiled_function_annotation);
  else if (!NILP (Vload_file_name_internal_the_purecopy))
    f->annotated = Vload_file_name_internal_the_purecopy;
  else if (!NILP (Vload_file_name_internal))
    {
      struct gcpro gcpro1;
      GCPRO1 (fun);		/* don't let fun get reaped */
      Vload_file_name_internal_the_purecopy =
	Ffile_name_nondirectory (Vload_file_name_internal);
      f->annotated = Vload_file_name_internal_the_purecopy;
      UNGCPRO;
    }
#endif /* COMPILED_FUNCTION_ANNOTATION_HACK */

  /* doc_string may be nil, string, int, or a cons (string . int).
     interactive may be list or string (or unbound). */
  f->doc_and_interactive = Qunbound;
#ifdef I18N3
  if ((f->flags.domainp = !NILP (Vfile_domain)) != 0)
    f->doc_and_interactive = Vfile_domain;
#endif
  if ((f->flags.interactivep = !UNBOUNDP (interactive)) != 0)
    {
      f->doc_and_interactive
	= (UNBOUNDP (f->doc_and_interactive) ? interactive :
	   Fcons (interactive, f->doc_and_interactive));
    }
  if ((f->flags.documentationp = !NILP (doc_string)) != 0)
    {
      f->doc_and_interactive
	= (UNBOUNDP (f->doc_and_interactive) ? doc_string :
	   Fcons (doc_string, f->doc_and_interactive));
    }
  if (UNBOUNDP (f->doc_and_interactive))
    f->doc_and_interactive = Qnil;

  return fun;
}


/************************************************************************/
/*			    Symbol allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (symbol, Lisp_Symbol);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_symbol 1000

DEFUN ("make-symbol", Fmake_symbol, 1, 1, 0, /*
Return a newly allocated uninterned symbol whose name is NAME.
Its value and function definition are void, and its property list is nil.
*/
       (name))
{
  Lisp_Object val;
  Lisp_Symbol *p;

  CHECK_STRING (name);

  ALLOCATE_FIXED_TYPE (symbol, Lisp_Symbol, p);
  set_lheader_implementation (&p->lheader, &lrecord_symbol);
  p->name     = XSTRING (name);
  p->plist    = Qnil;
  p->value    = Qunbound;
  p->function = Qunbound;
  symbol_next (p) = 0;
  XSETSYMBOL (val, p);
  return val;
}


/************************************************************************/
/*			   Extent allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (extent, struct extent);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_extent 1000

struct extent *
allocate_extent (void)
{
  struct extent *e;

  ALLOCATE_FIXED_TYPE (extent, struct extent, e);
  set_lheader_implementation (&e->lheader, &lrecord_extent);
  extent_object (e) = Qnil;
  set_extent_start (e, -1);
  set_extent_end (e, -1);
  e->plist = Qnil;

  xzero (e->flags);

  extent_face (e) = Qnil;
  e->flags.end_open = 1;  /* default is for endpoints to behave like markers */
  e->flags.detachable = 1;

  return e;
}


/************************************************************************/
/*			   Event allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (event, Lisp_Event);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_event 1000

Lisp_Object
allocate_event (void)
{
  Lisp_Object val;
  Lisp_Event *e;

  ALLOCATE_FIXED_TYPE (event, Lisp_Event, e);
  set_lheader_implementation (&e->lheader, &lrecord_event);

  XSETEVENT (val, e);
  return val;
}


/************************************************************************/
/*			 Marker allocation				*/
/************************************************************************/

DECLARE_FIXED_TYPE_ALLOC (marker, Lisp_Marker);
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_marker 1000

DEFUN ("make-marker", Fmake_marker, 0, 0, 0, /*
Return a new marker which does not point at any place.
*/
       ())
{
  Lisp_Object val;
  Lisp_Marker *p;

  ALLOCATE_FIXED_TYPE (marker, Lisp_Marker, p);
  set_lheader_implementation (&p->lheader, &lrecord_marker);
  p->buffer = 0;
  p->memind = 0;
  marker_next (p) = 0;
  marker_prev (p) = 0;
  p->insertion_type = 0;
  XSETMARKER (val, p);
  return val;
}

Lisp_Object
noseeum_make_marker (void)
{
  Lisp_Object val;
  Lisp_Marker *p;

  NOSEEUM_ALLOCATE_FIXED_TYPE (marker, Lisp_Marker, p);
  set_lheader_implementation (&p->lheader, &lrecord_marker);
  p->buffer = 0;
  p->memind = 0;
  marker_next (p) = 0;
  marker_prev (p) = 0;
  p->insertion_type = 0;
  XSETMARKER (val, p);
  return val;
}


/************************************************************************/
/*			  String allocation				*/
/************************************************************************/

/* The data for "short" strings generally resides inside of structs of type
   string_chars_block. The Lisp_String structure is allocated just like any
   other Lisp object (except for vectors), and these are freelisted when
   they get garbage collected. The data for short strings get compacted,
   but the data for large strings do not.

   Previously Lisp_String structures were relocated, but this caused a lot
   of bus-errors because the C code didn't include enough GCPRO's for
   strings (since EVERY REFERENCE to a short string needed to be GCPRO'd so
   that the reference would get relocated).

   This new method makes things somewhat bigger, but it is MUCH safer.  */

DECLARE_FIXED_TYPE_ALLOC (string, Lisp_String);
/* strings are used and freed quite often */
/* #define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_string 10000 */
#define MINIMUM_ALLOWED_FIXED_TYPE_CELLS_string 1000

static Lisp_Object
mark_string (Lisp_Object obj)
{
  Lisp_String *ptr = XSTRING (obj);

  if (CONSP (ptr->plist) && EXTENT_INFOP (XCAR (ptr->plist)))
    flush_cached_extent_info (XCAR (ptr->plist));
  return ptr->plist;
}

static int
string_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
  Bytecount len;
  return (((len = XSTRING_LENGTH (obj1)) == XSTRING_LENGTH (obj2)) &&
	  !memcmp (XSTRING_DATA (obj1), XSTRING_DATA (obj2), len));
}

static const struct lrecord_description string_description[] = {
  { XD_BYTECOUNT,       offsetof (Lisp_String, size) },
  { XD_OPAQUE_DATA_PTR, offsetof (Lisp_String, data), XD_INDIRECT(0, 1) },
  { XD_LISP_OBJECT,     offsetof (Lisp_String, plist) },
  { XD_END }
};

/* We store the string's extent info as the first element of the string's
   property list; and the string's MODIFF as the first or second element
   of the string's property list (depending on whether the extent info
   is present), but only if the string has been modified.  This is ugly
   but it reduces the memory allocated for the string in the vast
   majority of cases, where the string is never modified and has no
   extent info.

   #### This means you can't use an int as a key in a string's plist. */

static Lisp_Object *
string_plist_ptr (Lisp_Object string)
{
  Lisp_Object *ptr = &XSTRING (string)->plist;

  if (CONSP (*ptr) && EXTENT_INFOP (XCAR (*ptr)))
    ptr = &XCDR (*ptr);
  if (CONSP (*ptr) && INTP (XCAR (*ptr)))
    ptr = &XCDR (*ptr);
  return ptr;
}

static Lisp_Object
string_getprop (Lisp_Object string, Lisp_Object property)
{
  return external_plist_get (string_plist_ptr (string), property, 0, ERROR_ME);
}

static int
string_putprop (Lisp_Object string, Lisp_Object property, Lisp_Object value)
{
  external_plist_put (string_plist_ptr (string), property, value, 0, ERROR_ME);
  return 1;
}

static int
string_remprop (Lisp_Object string, Lisp_Object property)
{
  return external_remprop (string_plist_ptr (string), property, 0, ERROR_ME);
}

static Lisp_Object
string_plist (Lisp_Object string)
{
  return *string_plist_ptr (string);
}

/* No `finalize', or `hash' methods.
   internal_hash() already knows how to hash strings and finalization
   is done with the ADDITIONAL_FREE_string macro, which is the
   standard way to do finalization when using
   SWEEP_FIXED_TYPE_BLOCK(). */
DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS ("string", string,
						mark_string, print_string,
						0, string_equal, 0,
						string_description,
						string_getprop,
						string_putprop,
						string_remprop,
						string_plist,
						Lisp_String);

/* String blocks contain this many useful bytes. */
#define STRING_CHARS_BLOCK_SIZE					\
((Bytecount) (8192 - MALLOC_OVERHEAD -				\
	      ((2 * sizeof (struct string_chars_block *))	\
	       + sizeof (EMACS_INT))))
/* Block header for small strings. */
struct string_chars_block
{
  EMACS_INT pos;
  struct string_chars_block *next;
  struct string_chars_block *prev;
  /* Contents of string_chars_block->string_chars are interleaved
     string_chars structures (see below) and the actual string data */
  unsigned char string_chars[STRING_CHARS_BLOCK_SIZE];
};

static struct string_chars_block *first_string_chars_block;
static struct string_chars_block *current_string_chars_block;

/* If SIZE is the length of a string, this returns how many bytes
 *  the string occupies in string_chars_block->string_chars
 *  (including alignment padding).
 */
#define STRING_FULLSIZE(size) \
   ALIGN_SIZE (((size) + 1 + sizeof (Lisp_String *)),\
               ALIGNOF (Lisp_String *))

#define BIG_STRING_FULLSIZE_P(fullsize) ((fullsize) >= STRING_CHARS_BLOCK_SIZE)
#define BIG_STRING_SIZE_P(size) (BIG_STRING_FULLSIZE_P (STRING_FULLSIZE(size)))

#define STRING_CHARS_FREE_P(ptr) ((ptr)->string == NULL)
#define MARK_STRING_CHARS_AS_FREE(ptr) ((void) ((ptr)->string = NULL))

struct string_chars
{
  Lisp_String *string;
  unsigned char chars[1];
};

struct unused_string_chars
{
  Lisp_String *string;
  EMACS_INT fullsize;
};

static void
init_string_chars_alloc (void)
{
  first_string_chars_block = xnew (struct string_chars_block);
  first_string_chars_block->prev = 0;
  first_string_chars_block->next = 0;
  first_string_chars_block->pos = 0;
  current_string_chars_block = first_string_chars_block;
}

static struct string_chars *
allocate_string_chars_struct (Lisp_String *string_it_goes_with,
			      EMACS_INT fullsize)
{
  struct string_chars *s_chars;

  if (fullsize <=
      (countof (current_string_chars_block->string_chars)
       - current_string_chars_block->pos))
    {
      /* This string can fit in the current string chars block */
      s_chars = (struct string_chars *)
	(current_string_chars_block->string_chars
	 + current_string_chars_block->pos);
      current_string_chars_block->pos += fullsize;
    }
  else
    {
      /* Make a new current string chars block */
      struct string_chars_block *new_scb = xnew (struct string_chars_block);

      current_string_chars_block->next = new_scb;
      new_scb->prev = current_string_chars_block;
      new_scb->next = 0;
      current_string_chars_block = new_scb;
      new_scb->pos = fullsize;
      s_chars = (struct string_chars *)
	current_string_chars_block->string_chars;
    }

  s_chars->string = string_it_goes_with;

  INCREMENT_CONS_COUNTER (fullsize, "string chars");

  return s_chars;
}

Lisp_Object
make_uninit_string (Bytecount length)
{
  Lisp_String *s;
  EMACS_INT fullsize = STRING_FULLSIZE (length);
  Lisp_Object val;

  assert (length >= 0 && fullsize > 0);

  /* Allocate the string header */
  ALLOCATE_FIXED_TYPE (string, Lisp_String, s);
  set_lheader_implementation (&s->lheader, &lrecord_string);

  set_string_data (s, BIG_STRING_FULLSIZE_P (fullsize)
		   ? xnew_array (Bufbyte, length + 1)
		   : allocate_string_chars_struct (s, fullsize)->chars);

  set_string_length (s, length);
  s->plist = Qnil;

  set_string_byte (s, length, 0);

  XSETSTRING (val, s);
  return val;
}

#ifdef VERIFY_STRING_CHARS_INTEGRITY
static void verify_string_chars_integrity (void);
#endif

/* Resize the string S so that DELTA bytes can be inserted starting
   at POS.  If DELTA < 0, it means deletion starting at POS.  If
   POS < 0, resize the string but don't copy any characters.  Use
   this if you're planning on completely overwriting the string.
*/

void
resize_string (Lisp_String *s, Bytecount pos, Bytecount delta)
{
  Bytecount oldfullsize, newfullsize;
#ifdef VERIFY_STRING_CHARS_INTEGRITY
  verify_string_chars_integrity ();
#endif

#ifdef ERROR_CHECK_BUFPOS
  if (pos >= 0)
    {
      assert (pos <= string_length (s));
      if (delta < 0)
	assert (pos + (-delta) <= string_length (s));
    }
  else
    {
      if (delta < 0)
	assert ((-delta) <= string_length (s));
    }
#endif /* ERROR_CHECK_BUFPOS */

  if (delta == 0)
    /* simplest case: no size change. */
    return;

  if (pos >= 0 && delta < 0)
    /* If DELTA < 0, the functions below will delete the characters
       before POS.  We want to delete characters *after* POS, however,
       so convert this to the appropriate form. */
    pos += -delta;

  oldfullsize = STRING_FULLSIZE (string_length (s));
  newfullsize = STRING_FULLSIZE (string_length (s) + delta);

  if (BIG_STRING_FULLSIZE_P (oldfullsize))
    {
      if (BIG_STRING_FULLSIZE_P (newfullsize))
	{
	  /* Both strings are big.  We can just realloc().
	     But careful!  If the string is shrinking, we have to
	     memmove() _before_ realloc(), and if growing, we have to
	     memmove() _after_ realloc() - otherwise the access is
	     illegal, and we might crash. */
	  Bytecount len = string_length (s) + 1 - pos;

	  if (delta < 0 && pos >= 0)
	    memmove (string_data (s) + pos + delta, string_data (s) + pos, len);
	  set_string_data (s, (Bufbyte *) xrealloc (string_data (s),
						    string_length (s) + delta + 1));
	  if (delta > 0 && pos >= 0)
	    memmove (string_data (s) + pos + delta, string_data (s) + pos, len);
	}
      else /* String has been demoted from BIG_STRING. */
	{
	  Bufbyte *new_data =
	    allocate_string_chars_struct (s, newfullsize)->chars;
	  Bufbyte *old_data = string_data (s);

	  if (pos >= 0)
	    {
	      memcpy (new_data, old_data, pos);
	      memcpy (new_data + pos + delta, old_data + pos,
		      string_length (s) + 1 - pos);
	    }
	  set_string_data (s, new_data);
	  xfree (old_data);
	}
    }
  else /* old string is small */
    {
      if (oldfullsize == newfullsize)
	{
	  /* special case; size change but the necessary
	     allocation size won't change (up or down; code
	     somewhere depends on there not being any unused
	     allocation space, modulo any alignment
	     constraints). */
	  if (pos >= 0)
	    {
	      Bufbyte *addroff = pos + string_data (s);

	      memmove (addroff + delta, addroff,
		       /* +1 due to zero-termination. */
		       string_length (s) + 1 - pos);
	    }
	}
      else
	{
	  Bufbyte *old_data = string_data (s);
	  Bufbyte *new_data =
	    BIG_STRING_FULLSIZE_P (newfullsize)
	    ? xnew_array (Bufbyte, string_length (s) + delta + 1)
	    : allocate_string_chars_struct (s, newfullsize)->chars;

	  if (pos >= 0)
	    {
	      memcpy (new_data, old_data, pos);
	      memcpy (new_data + pos + delta, old_data + pos,
		      string_length (s) + 1 - pos);
	    }
	  set_string_data (s, new_data);

	  {
	    /* We need to mark this chunk of the string_chars_block
	       as unused so that compact_string_chars() doesn't
	       freak. */
	    struct string_chars *old_s_chars = (struct string_chars *)
	      ((char *) old_data - offsetof (struct string_chars, chars));
	    /* Sanity check to make sure we aren't hosed by strange
	       alignment/padding. */
	    assert (old_s_chars->string == s);
	    MARK_STRING_CHARS_AS_FREE (old_s_chars);
	    ((struct unused_string_chars *) old_s_chars)->fullsize =
	      oldfullsize;
	  }
	}
    }

  set_string_length (s, string_length (s) + delta);
  /* If pos < 0, the string won't be zero-terminated.
     Terminate now just to make sure. */
  string_data (s)[string_length (s)] = '\0';

  if (pos >= 0)
    {
      Lisp_Object string;

      XSETSTRING (string, s);
      /* We also have to adjust all of the extent indices after the
	 place we did the change.  We say "pos - 1" because
	 adjust_extents() is exclusive of the starting position
	 passed to it. */
      adjust_extents (string, pos - 1, string_length (s),
		      delta);
    }

#ifdef VERIFY_STRING_CHARS_INTEGRITY
  verify_string_chars_integrity ();
#endif
}

#ifdef MULE

void
set_string_char (Lisp_String *s, Charcount i, Emchar c)
{
  Bufbyte newstr[MAX_EMCHAR_LEN];
  Bytecount bytoff = charcount_to_bytecount (string_data (s), i);
  Bytecount oldlen = charcount_to_bytecount (string_data (s) + bytoff, 1);
  Bytecount newlen = set_charptr_emchar (newstr, c);

  if (oldlen != newlen)
    resize_string (s, bytoff, newlen - oldlen);
  /* Remember, string_data (s) might have changed so we can't cache it. */
  memcpy (string_data (s) + bytoff, newstr, newlen);
}

#endif /* MULE */

DEFUN ("make-string", Fmake_string, 2, 2, 0, /*
Return a new string consisting of LENGTH copies of CHARACTER.
LENGTH must be a non-negative integer.
*/
       (length, character))
{
  CHECK_NATNUM (length);
  CHECK_CHAR_COERCE_INT (character);
  {
    Bufbyte init_str[MAX_EMCHAR_LEN];
    int len = set_charptr_emchar (init_str, XCHAR (character));
    Lisp_Object val = make_uninit_string (len * XINT (length));

    if (len == 1)
      /* Optimize the single-byte case */
      memset (XSTRING_DATA (val), XCHAR (character), XSTRING_LENGTH (val));
    else
      {
	size_t i;
	Bufbyte *ptr = XSTRING_DATA (val);

	for (i = XINT (length); i; i--)
	  {
	    Bufbyte *init_ptr = init_str;
	    switch (len)
	      {
	      case 4: *ptr++ = *init_ptr++;
	      case 3: *ptr++ = *init_ptr++;
	      case 2: *ptr++ = *init_ptr++;
	      case 1: *ptr++ = *init_ptr++;
	      }
	  }
      }
    return val;
  }
}

DEFUN ("string", Fstring, 0, MANY, 0, /*
Concatenate all the argument characters and make the result a string.
*/
       (int nargs, Lisp_Object *args))
{
  Bufbyte *storage = alloca_array (Bufbyte, nargs * MAX_EMCHAR_LEN);
  Bufbyte *p = storage;

  for (; nargs; nargs--, args++)
    {
      Lisp_Object lisp_char = *args;
      CHECK_CHAR_COERCE_INT (lisp_char);
      p += set_charptr_emchar (p, XCHAR (lisp_char));
    }
  return make_string (storage, p - storage);
}


/* Take some raw memory, which MUST already be in internal format,
   and package it up into a Lisp string. */
Lisp_Object
make_string (const Bufbyte *contents, Bytecount length)
{
  Lisp_Object val;

  /* Make sure we find out about bad make_string's when they happen */
#if defined (ERROR_CHECK_BUFPOS) && defined (MULE)
  bytecount_to_charcount (contents, length); /* Just for the assertions */
#endif

  val = make_uninit_string (length);
  memcpy (XSTRING_DATA (val), contents, length);
  return val;
}

/* Take some raw memory, encoded in some external data format,
   and convert it into a Lisp string. */
Lisp_Object
make_ext_string (const Extbyte *contents, EMACS_INT length,
		 Lisp_Object coding_system)
{
  Lisp_Object string;
  TO_INTERNAL_FORMAT (DATA, (contents, length),
		      LISP_STRING, string,
		      coding_system);
  return string;
}

Lisp_Object
build_string (const char *str)
{
  /* Some strlen's crash and burn if passed null. */
  return make_string ((const Bufbyte *) str, (str ? strlen(str) : 0));
}

Lisp_Object
build_ext_string (const char *str, Lisp_Object coding_system)
{
  /* Some strlen's crash and burn if passed null. */
  return make_ext_string ((const Extbyte *) str, (str ? strlen(str) : 0),
			  coding_system);
}

Lisp_Object
build_translated_string (const char *str)
{
  return build_string (GETTEXT (str));
}

Lisp_Object
make_string_nocopy (const Bufbyte *contents, Bytecount length)
{
  Lisp_String *s;
  Lisp_Object val;

  /* Make sure we find out about bad make_string_nocopy's when they happen */
#if defined (ERROR_CHECK_BUFPOS) && defined (MULE)
  bytecount_to_charcount (contents, length); /* Just for the assertions */
#endif

  /* Allocate the string header */
  ALLOCATE_FIXED_TYPE (string, Lisp_String, s);
  set_lheader_implementation (&s->lheader, &lrecord_string);
  SET_C_READONLY_RECORD_HEADER (&s->lheader);
  s->plist = Qnil;
  set_string_data (s, (Bufbyte *)contents);
  set_string_length (s, length);

  XSETSTRING (val, s);
  return val;
}


/************************************************************************/
/*                           lcrecord lists                             */
/************************************************************************/

/* Lcrecord lists are used to manage the allocation of particular
   sorts of lcrecords, to avoid calling alloc_lcrecord() (and thus
   malloc() and garbage-collection junk) as much as possible.
   It is similar to the Blocktype class.

   It works like this:

   1) Create an lcrecord-list object using make_lcrecord_list().
      This is often done at initialization.  Remember to staticpro_nodump
      this object!  The arguments to make_lcrecord_list() are the
      same as would be passed to alloc_lcrecord().
   2) Instead of calling alloc_lcrecord(), call allocate_managed_lcrecord()
      and pass the lcrecord-list earlier created.
   3) When done with the lcrecord, call free_managed_lcrecord().
      The standard freeing caveats apply: ** make sure there are no
      pointers to the object anywhere! **
   4) Calling free_managed_lcrecord() is just like kissing the
      lcrecord goodbye as if it were garbage-collected.  This means:
      -- the contents of the freed lcrecord are undefined, and the
         contents of something produced by allocate_managed_lcrecord()
	 are undefined, just like for alloc_lcrecord().
      -- the mark method for the lcrecord's type will *NEVER* be called
         on freed lcrecords.
      -- the finalize method for the lcrecord's type will be called
         at the time that free_managed_lcrecord() is called.

   */

static Lisp_Object
mark_lcrecord_list (Lisp_Object obj)
{
  struct lcrecord_list *list = XLCRECORD_LIST (obj);
  Lisp_Object chain = list->free;

  while (!NILP (chain))
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (chain);
      struct free_lcrecord_header *free_header =
	(struct free_lcrecord_header *) lheader;

      gc_checking_assert
	(/* There should be no other pointers to the free list. */
	 ! MARKED_RECORD_HEADER_P (lheader)
	 &&
	 /* Only lcrecords should be here. */
	 ! LHEADER_IMPLEMENTATION (lheader)->basic_p
	 &&
	 /* Only free lcrecords should be here. */
	 free_header->lcheader.free
	 &&
	 /* The type of the lcrecord must be right. */
	 LHEADER_IMPLEMENTATION (lheader) == list->implementation
	 &&
	 /* So must the size. */
	 (LHEADER_IMPLEMENTATION (lheader)->static_size == 0 ||
	  LHEADER_IMPLEMENTATION (lheader)->static_size == list->size)
	 );

      MARK_RECORD_HEADER (lheader);
      chain = free_header->chain;
    }

  return Qnil;
}

DEFINE_LRECORD_IMPLEMENTATION ("lcrecord-list", lcrecord_list,
			       mark_lcrecord_list, internal_object_printer,
			       0, 0, 0, 0, struct lcrecord_list);
Lisp_Object
make_lcrecord_list (size_t size,
		    const struct lrecord_implementation *implementation)
{
  struct lcrecord_list *p = alloc_lcrecord_type (struct lcrecord_list,
						 &lrecord_lcrecord_list);
  Lisp_Object val;

  p->implementation = implementation;
  p->size = size;
  p->free = Qnil;
  XSETLCRECORD_LIST (val, p);
  return val;
}

Lisp_Object
allocate_managed_lcrecord (Lisp_Object lcrecord_list)
{
  struct lcrecord_list *list = XLCRECORD_LIST (lcrecord_list);
  if (!NILP (list->free))
    {
      Lisp_Object val = list->free;
      struct free_lcrecord_header *free_header =
	(struct free_lcrecord_header *) XPNTR (val);

#ifdef ERROR_CHECK_GC
      struct lrecord_header *lheader = &free_header->lcheader.lheader;

      /* There should be no other pointers to the free list. */
      assert (! MARKED_RECORD_HEADER_P (lheader));
      /* Only lcrecords should be here. */
      assert (! LHEADER_IMPLEMENTATION (lheader)->basic_p);
      /* Only free lcrecords should be here. */
      assert (free_header->lcheader.free);
      /* The type of the lcrecord must be right. */
      assert (LHEADER_IMPLEMENTATION (lheader) == list->implementation);
      /* So must the size. */
      assert (LHEADER_IMPLEMENTATION (lheader)->static_size == 0 ||
	      LHEADER_IMPLEMENTATION (lheader)->static_size == list->size);
#endif /* ERROR_CHECK_GC */

      list->free = free_header->chain;
      free_header->lcheader.free = 0;
      return val;
    }
  else
    {
      Lisp_Object val;

      XSETOBJ (val, alloc_lcrecord (list->size, list->implementation));
      return val;
    }
}

void
free_managed_lcrecord (Lisp_Object lcrecord_list, Lisp_Object lcrecord)
{
  struct lcrecord_list *list = XLCRECORD_LIST (lcrecord_list);
  struct free_lcrecord_header *free_header =
    (struct free_lcrecord_header *) XPNTR (lcrecord);
  struct lrecord_header *lheader = &free_header->lcheader.lheader;
  const struct lrecord_implementation *implementation
    = LHEADER_IMPLEMENTATION (lheader);

  /* Make sure the size is correct.  This will catch, for example,
     putting a window configuration on the wrong free list. */
  gc_checking_assert ((implementation->size_in_bytes_method ?
		       implementation->size_in_bytes_method (lheader) :
		       implementation->static_size)
		      == list->size);

  if (implementation->finalizer)
    implementation->finalizer (lheader, 0);
  free_header->chain = list->free;
  free_header->lcheader.free = 1;
  list->free = lcrecord;
}




DEFUN ("purecopy", Fpurecopy, 1, 1, 0, /*
Kept for compatibility, returns its argument.
Old:
Make a copy of OBJECT in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.
*/
       (object))
{
  return object;
}


/************************************************************************/
/*			   Garbage Collection				*/
/************************************************************************/

/* All the built-in lisp object types are enumerated in `enum lrecord_type'.
   Additional ones may be defined by a module (none yet).  We leave some
   room in `lrecord_implementations_table' for such new lisp object types. */
const struct lrecord_implementation *lrecord_implementations_table[(unsigned int)lrecord_type_last_built_in_type + MODULE_DEFINABLE_TYPE_COUNT];
unsigned int lrecord_type_count = (unsigned int)lrecord_type_last_built_in_type;
/* Object marker functions are in the lrecord_implementation structure.
   But copying them to a parallel array is much more cache-friendly.
   This hack speeds up (garbage-collect) by about 5%. */
Lisp_Object (*lrecord_markers[countof (lrecord_implementations_table)]) (Lisp_Object);

struct gcpro *gcprolist;

/* We want the staticpros relocated, but not the pointers found therein.
   Hence we use a trivial description, as for pointerless objects. */
static const struct lrecord_description staticpro_description_1[] = {
  { XD_END }
};

static const struct struct_description staticpro_description = {
  sizeof (Lisp_Object *),
  staticpro_description_1
};

static const struct lrecord_description staticpros_description_1[] = {
  XD_DYNARR_DESC (Lisp_Object_ptr_dynarr, &staticpro_description),
  { XD_END }
};

static const struct struct_description staticpros_description = {
  sizeof (Lisp_Object_ptr_dynarr),
  staticpros_description_1
};

Lisp_Object_ptr_dynarr *staticpros;

/* Mark the Lisp_Object at non-heap VARADDRESS as a root object for
   garbage collection, and for dumping. */
void
staticpro (Lisp_Object *varaddress)
{
  Dynarr_add (staticpros, varaddress);
  dump_add_root_object (varaddress);
}


Lisp_Object_ptr_dynarr *staticpros_nodump;

/* Mark the Lisp_Object at non-heap VARADDRESS as a root object for
   garbage collection, but not for dumping. */
void
staticpro_nodump (Lisp_Object *varaddress)
{
  Dynarr_add (staticpros_nodump, varaddress);
}

#ifdef ERROR_CHECK_GC
#define GC_CHECK_LHEADER_INVARIANTS(lheader) do {		\
  struct lrecord_header * GCLI_lh = (lheader);			\
  assert (GCLI_lh != 0);					\
  assert (GCLI_lh->type < lrecord_type_count);			\
  assert (! C_READONLY_RECORD_HEADER_P (GCLI_lh) ||		\
	  (MARKED_RECORD_HEADER_P (GCLI_lh) &&			\
	   LISP_READONLY_RECORD_HEADER_P (GCLI_lh)));		\
} while (0)
#else
#define GC_CHECK_LHEADER_INVARIANTS(lheader)
#endif


/* Mark reference to a Lisp_Object.  If the object referred to has not been
   seen yet, recursively mark all the references contained in it. */

void
mark_object (Lisp_Object obj)
{
 tail_recurse:

  /* Checks we used to perform */
  /* if (EQ (obj, Qnull_pointer)) return; */
  /* if (!POINTER_TYPE_P (XGCTYPE (obj))) return; */
  /* if (PURIFIED (XPNTR (obj))) return; */

  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);

      GC_CHECK_LHEADER_INVARIANTS (lheader);

      gc_checking_assert (LHEADER_IMPLEMENTATION (lheader)->basic_p ||
			  ! ((struct lcrecord_header *) lheader)->free);

      /* All c_readonly objects have their mark bit set,
	 so that we only need to check the mark bit here. */
      if (! MARKED_RECORD_HEADER_P (lheader))
	{
	  MARK_RECORD_HEADER (lheader);

	  if (RECORD_MARKER (lheader))
	    {
	      obj = RECORD_MARKER (lheader) (obj);
	      if (!NILP (obj)) goto tail_recurse;
	    }
	}
    }
}

/* mark all of the conses in a list and mark the final cdr; but
   DO NOT mark the cars.

   Use only for internal lists!  There should never be other pointers
   to the cons cells, because if so, the cars will remain unmarked
   even when they maybe should be marked. */
void
mark_conses_in_list (Lisp_Object obj)
{
  Lisp_Object rest;

  for (rest = obj; CONSP (rest); rest = XCDR (rest))
    {
      if (CONS_MARKED_P (XCONS (rest)))
	return;
      MARK_CONS (XCONS (rest));
    }

  mark_object (rest);
}


/* Find all structures not marked, and free them. */

static int gc_count_num_bit_vector_used, gc_count_bit_vector_total_size;
static int gc_count_bit_vector_storage;
static int gc_count_num_short_string_in_use;
static int gc_count_string_total_size;
static int gc_count_short_string_total_size;

/* static int gc_count_total_records_used, gc_count_records_total_size; */


/* stats on lcrecords in use - kinda kludgy */

static struct
{
  int instances_in_use;
  int bytes_in_use;
  int instances_freed;
  int bytes_freed;
  int instances_on_free_list;
} lcrecord_stats [countof (lrecord_implementations_table)
		  + MODULE_DEFINABLE_TYPE_COUNT];

static void
tick_lcrecord_stats (const struct lrecord_header *h, int free_p)
{
  unsigned int type_index = h->type;

  if (((struct lcrecord_header *) h)->free)
    {
      gc_checking_assert (!free_p);
      lcrecord_stats[type_index].instances_on_free_list++;
    }
  else
    {
      const struct lrecord_implementation *implementation =
	LHEADER_IMPLEMENTATION (h);

      size_t sz = (implementation->size_in_bytes_method ?
		   implementation->size_in_bytes_method (h) :
		   implementation->static_size);
      if (free_p)
	{
	  lcrecord_stats[type_index].instances_freed++;
	  lcrecord_stats[type_index].bytes_freed += sz;
	}
      else
	{
	  lcrecord_stats[type_index].instances_in_use++;
	  lcrecord_stats[type_index].bytes_in_use += sz;
	}
    }
}


/* Free all unmarked records */
static void
sweep_lcrecords_1 (struct lcrecord_header **prev, int *used)
{
  struct lcrecord_header *header;
  int num_used = 0;
  /* int total_size = 0; */

  xzero (lcrecord_stats); /* Reset all statistics to 0. */

  /* First go through and call all the finalize methods.
     Then go through and free the objects.  There used to
     be only one loop here, with the call to the finalizer
     occurring directly before the xfree() below.  That
     is marginally faster but much less safe -- if the
     finalize method for an object needs to reference any
     other objects contained within it (and many do),
     we could easily be screwed by having already freed that
     other object. */

  for (header = *prev; header; header = header->next)
    {
      struct lrecord_header *h = &(header->lheader);

      GC_CHECK_LHEADER_INVARIANTS (h);

      if (! MARKED_RECORD_HEADER_P (h) && ! header->free)
	{
	  if (LHEADER_IMPLEMENTATION (h)->finalizer)
	    LHEADER_IMPLEMENTATION (h)->finalizer (h, 0);
	}
    }

  for (header = *prev; header; )
    {
      struct lrecord_header *h = &(header->lheader);
      if (MARKED_RECORD_HEADER_P (h))
	{
	  if (! C_READONLY_RECORD_HEADER_P (h))
	    UNMARK_RECORD_HEADER (h);
	  num_used++;
	  /* total_size += n->implementation->size_in_bytes (h);*/
	  /* #### May modify header->next on a C_READONLY lcrecord */
	  prev = &(header->next);
	  header = *prev;
	  tick_lcrecord_stats (h, 0);
	}
      else
	{
	  struct lcrecord_header *next = header->next;
          *prev = next;
	  tick_lcrecord_stats (h, 1);
	  /* used to call finalizer right here. */
	  xfree (header);
	  header = next;
	}
    }
  *used = num_used;
  /* *total = total_size; */
}


static void
sweep_bit_vectors_1 (Lisp_Object *prev,
		     int *used, int *total, int *storage)
{
  Lisp_Object bit_vector;
  int num_used = 0;
  int total_size = 0;
  int total_storage = 0;

  /* BIT_VECTORP fails because the objects are marked, which changes
     their implementation */
  for (bit_vector = *prev; !EQ (bit_vector, Qzero); )
    {
      Lisp_Bit_Vector *v = XBIT_VECTOR (bit_vector);
      int len = v->size;
      if (MARKED_RECORD_P (bit_vector))
	{
	  if (! C_READONLY_RECORD_HEADER_P(&(v->lheader)))
	    UNMARK_RECORD_HEADER (&(v->lheader));
	  total_size += len;
          total_storage +=
	    MALLOC_OVERHEAD +
	    FLEXIBLE_ARRAY_STRUCT_SIZEOF (Lisp_Bit_Vector, unsigned long,
					  bits, BIT_VECTOR_LONG_STORAGE (len));
	  num_used++;
	  /* #### May modify next on a C_READONLY bitvector */
	  prev = &(bit_vector_next (v));
	  bit_vector = *prev;
	}
      else
	{
          Lisp_Object next = bit_vector_next (v);
          *prev = next;
	  xfree (v);
	  bit_vector = next;
	}
    }
  *used = num_used;
  *total = total_size;
  *storage = total_storage;
}

/* And the Lord said: Thou shalt use the `c-backslash-region' command
   to make macros prettier. */

#ifdef ERROR_CHECK_GC

#define SWEEP_FIXED_TYPE_BLOCK(typename, obj_type)			\
do {									\
  struct typename##_block *SFTB_current;				\
  int SFTB_limit;							\
  int num_free = 0, num_used = 0;					\
									\
  for (SFTB_current = current_##typename##_block,			\
       SFTB_limit = current_##typename##_block_index;			\
       SFTB_current;							\
       )								\
    {									\
      int SFTB_iii;							\
									\
      for (SFTB_iii = 0; SFTB_iii < SFTB_limit; SFTB_iii++)		\
	{								\
	  obj_type *SFTB_victim = &(SFTB_current->block[SFTB_iii]);	\
									\
	  if (LRECORD_FREE_P (SFTB_victim))				\
	    {								\
	      num_free++;						\
	    }								\
	  else if (C_READONLY_RECORD_HEADER_P (&SFTB_victim->lheader))	\
	    {								\
	      num_used++;						\
	    }								\
	  else if (! MARKED_RECORD_HEADER_P (&SFTB_victim->lheader))	\
	    {								\
	      num_free++;						\
	      FREE_FIXED_TYPE (typename, obj_type, SFTB_victim);	\
	    }								\
	  else								\
	    {								\
	      num_used++;						\
	      UNMARK_##typename (SFTB_victim);				\
	    }								\
	}								\
      SFTB_current = SFTB_current->prev;				\
      SFTB_limit = countof (current_##typename##_block->block);		\
    }									\
									\
  gc_count_num_##typename##_in_use = num_used;				\
  gc_count_num_##typename##_freelist = num_free;			\
} while (0)

#else /* !ERROR_CHECK_GC */

#define SWEEP_FIXED_TYPE_BLOCK(typename, obj_type)				\
do {										\
  struct typename##_block *SFTB_current;					\
  struct typename##_block **SFTB_prev;						\
  int SFTB_limit;								\
  int num_free = 0, num_used = 0;						\
										\
  typename##_free_list = 0;							\
										\
  for (SFTB_prev = &current_##typename##_block,					\
       SFTB_current = current_##typename##_block,				\
       SFTB_limit = current_##typename##_block_index;				\
       SFTB_current;								\
       )									\
    {										\
      int SFTB_iii;								\
      int SFTB_empty = 1;							\
      Lisp_Free *SFTB_old_free_list = typename##_free_list;			\
										\
      for (SFTB_iii = 0; SFTB_iii < SFTB_limit; SFTB_iii++)			\
	{									\
	  obj_type *SFTB_victim = &(SFTB_current->block[SFTB_iii]);		\
										\
	  if (LRECORD_FREE_P (SFTB_victim))					\
	    {									\
	      num_free++;							\
	      PUT_FIXED_TYPE_ON_FREE_LIST (typename, obj_type, SFTB_victim);	\
	    }									\
	  else if (C_READONLY_RECORD_HEADER_P (&SFTB_victim->lheader))		\
	    {									\
	      SFTB_empty = 0;							\
	      num_used++;							\
	    }									\
	  else if (! MARKED_RECORD_HEADER_P (&SFTB_victim->lheader))		\
	    {									\
	      num_free++;							\
	      FREE_FIXED_TYPE (typename, obj_type, SFTB_victim);		\
	    }									\
	  else									\
	    {									\
	      SFTB_empty = 0;							\
	      num_used++;							\
	      UNMARK_##typename (SFTB_victim);					\
	    }									\
	}									\
      if (!SFTB_empty)								\
	{									\
	  SFTB_prev = &(SFTB_current->prev);					\
	  SFTB_current = SFTB_current->prev;					\
	}									\
      else if (SFTB_current == current_##typename##_block			\
	       && !SFTB_current->prev)						\
	{									\
	  /* No real point in freeing sole allocation block */			\
	  break;								\
	}									\
      else									\
	{									\
	  struct typename##_block *SFTB_victim_block = SFTB_current;		\
	  if (SFTB_victim_block == current_##typename##_block)			\
	    current_##typename##_block_index					\
	      = countof (current_##typename##_block->block);			\
	  SFTB_current = SFTB_current->prev;					\
	  {									\
	    *SFTB_prev = SFTB_current;						\
	    xfree (SFTB_victim_block);						\
	    /* Restore free list to what it was before victim was swept */	\
	    typename##_free_list = SFTB_old_free_list;				\
	    num_free -= SFTB_limit;						\
	  }									\
	}									\
      SFTB_limit = countof (current_##typename##_block->block);			\
    }										\
										\
  gc_count_num_##typename##_in_use = num_used;					\
  gc_count_num_##typename##_freelist = num_free;				\
} while (0)

#endif /* !ERROR_CHECK_GC */




static void
sweep_conses (void)
{
#define UNMARK_cons(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_cons(ptr)

  SWEEP_FIXED_TYPE_BLOCK (cons, Lisp_Cons);
}

/* Explicitly free a cons cell.  */
void
free_cons (Lisp_Cons *ptr)
{
#ifdef ERROR_CHECK_GC
  /* If the CAR is not an int, then it will be a pointer, which will
     always be four-byte aligned.  If this cons cell has already been
     placed on the free list, however, its car will probably contain
     a chain pointer to the next cons on the list, which has cleverly
     had all its 0's and 1's inverted.  This allows for a quick
     check to make sure we're not freeing something already freed. */
  if (POINTER_TYPE_P (XTYPE (ptr->car)))
    ASSERT_VALID_POINTER (XPNTR (ptr->car));
#endif /* ERROR_CHECK_GC */

#ifndef ALLOC_NO_POOLS
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (cons, Lisp_Cons, ptr);
#endif /* ALLOC_NO_POOLS */
}

/* explicitly free a list.  You **must make sure** that you have
   created all the cons cells that make up this list and that there
   are no pointers to any of these cons cells anywhere else.  If there
   are, you will lose. */

void
free_list (Lisp_Object list)
{
  Lisp_Object rest, next;

  for (rest = list; !NILP (rest); rest = next)
    {
      next = XCDR (rest);
      free_cons (XCONS (rest));
    }
}

/* explicitly free an alist.  You **must make sure** that you have
   created all the cons cells that make up this alist and that there
   are no pointers to any of these cons cells anywhere else.  If there
   are, you will lose. */

void
free_alist (Lisp_Object alist)
{
  Lisp_Object rest, next;

  for (rest = alist; !NILP (rest); rest = next)
    {
      next = XCDR (rest);
      free_cons (XCONS (XCAR (rest)));
      free_cons (XCONS (rest));
    }
}

static void
sweep_compiled_functions (void)
{
#define UNMARK_compiled_function(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_compiled_function(ptr)

  SWEEP_FIXED_TYPE_BLOCK (compiled_function, Lisp_Compiled_Function);
}


#ifdef LISP_FLOAT_TYPE
static void
sweep_floats (void)
{
#define UNMARK_float(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_float(ptr)

  SWEEP_FIXED_TYPE_BLOCK (float, Lisp_Float);
}
#endif /* LISP_FLOAT_TYPE */

static void
sweep_symbols (void)
{
#define UNMARK_symbol(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_symbol(ptr)

  SWEEP_FIXED_TYPE_BLOCK (symbol, Lisp_Symbol);
}

static void
sweep_extents (void)
{
#define UNMARK_extent(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_extent(ptr)

  SWEEP_FIXED_TYPE_BLOCK (extent, struct extent);
}

static void
sweep_events (void)
{
#define UNMARK_event(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_event(ptr)

  SWEEP_FIXED_TYPE_BLOCK (event, Lisp_Event);
}

static void
sweep_markers (void)
{
#define UNMARK_marker(ptr) UNMARK_RECORD_HEADER (&((ptr)->lheader))
#define ADDITIONAL_FREE_marker(ptr)					\
  do { Lisp_Object tem;							\
       XSETMARKER (tem, ptr);						\
       unchain_marker (tem);						\
     } while (0)

  SWEEP_FIXED_TYPE_BLOCK (marker, Lisp_Marker);
}

/* Explicitly free a marker.  */
void
free_marker (Lisp_Marker *ptr)
{
  /* Perhaps this will catch freeing an already-freed marker. */
  gc_checking_assert (ptr->lheader.type == lrecord_type_marker);

#ifndef ALLOC_NO_POOLS
  FREE_FIXED_TYPE_WHEN_NOT_IN_GC (marker, Lisp_Marker, ptr);
#endif /* ALLOC_NO_POOLS */
}


#if defined (MULE) && defined (VERIFY_STRING_CHARS_INTEGRITY)

static void
verify_string_chars_integrity (void)
{
  struct string_chars_block *sb;

  /* Scan each existing string block sequentially, string by string.  */
  for (sb = first_string_chars_block; sb; sb = sb->next)
    {
      int pos = 0;
      /* POS is the index of the next string in the block.  */
      while (pos < sb->pos)
        {
          struct string_chars *s_chars =
            (struct string_chars *) &(sb->string_chars[pos]);
          Lisp_String *string;
	  int size;
	  int fullsize;

	  /* If the string_chars struct is marked as free (i.e. the
	     STRING pointer is NULL) then this is an unused chunk of
	     string storage. (See below.) */

	  if (STRING_CHARS_FREE_P (s_chars))
	    {
	      fullsize = ((struct unused_string_chars *) s_chars)->fullsize;
	      pos += fullsize;
	      continue;
            }

          string = s_chars->string;
	  /* Must be 32-bit aligned. */
	  assert ((((int) string) & 3) == 0);

          size = string_length (string);
          fullsize = STRING_FULLSIZE (size);

          assert (!BIG_STRING_FULLSIZE_P (fullsize));
	  assert (string_data (string) == s_chars->chars);
	  pos += fullsize;
        }
      assert (pos == sb->pos);
    }
}

#endif /* MULE && ERROR_CHECK_GC */

/* Compactify string chars, relocating the reference to each --
   free any empty string_chars_block we see. */
static void
compact_string_chars (void)
{
  struct string_chars_block *to_sb = first_string_chars_block;
  int to_pos = 0;
  struct string_chars_block *from_sb;

  /* Scan each existing string block sequentially, string by string.  */
  for (from_sb = first_string_chars_block; from_sb; from_sb = from_sb->next)
    {
      int from_pos = 0;
      /* FROM_POS is the index of the next string in the block.  */
      while (from_pos < from_sb->pos)
        {
          struct string_chars *from_s_chars =
            (struct string_chars *) &(from_sb->string_chars[from_pos]);
          struct string_chars *to_s_chars;
          Lisp_String *string;
	  int size;
	  int fullsize;

	  /* If the string_chars struct is marked as free (i.e. the
	     STRING pointer is NULL) then this is an unused chunk of
	     string storage.  This happens under Mule when a string's
	     size changes in such a way that its fullsize changes.
	     (Strings can change size because a different-length
	     character can be substituted for another character.)
	     In this case, after the bogus string pointer is the
	     "fullsize" of this entry, i.e. how many bytes to skip. */

	  if (STRING_CHARS_FREE_P (from_s_chars))
	    {
	      fullsize = ((struct unused_string_chars *) from_s_chars)->fullsize;
	      from_pos += fullsize;
	      continue;
            }

          string = from_s_chars->string;
	  assert (!(LRECORD_FREE_P (string)));

          size = string_length (string);
          fullsize = STRING_FULLSIZE (size);

          gc_checking_assert (! BIG_STRING_FULLSIZE_P (fullsize));

          /* Just skip it if it isn't marked.  */
	  if (! MARKED_RECORD_HEADER_P (&(string->lheader)))
            {
              from_pos += fullsize;
              continue;
            }

          /* If it won't fit in what's left of TO_SB, close TO_SB out
             and go on to the next string_chars_block.  We know that TO_SB
             cannot advance past FROM_SB here since FROM_SB is large enough
             to currently contain this string. */
          if ((to_pos + fullsize) > countof (to_sb->string_chars))
            {
              to_sb->pos = to_pos;
              to_sb = to_sb->next;
              to_pos = 0;
            }

          /* Compute new address of this string
             and update TO_POS for the space being used.  */
          to_s_chars = (struct string_chars *) &(to_sb->string_chars[to_pos]);

          /* Copy the string_chars to the new place.  */
          if (from_s_chars != to_s_chars)
            memmove (to_s_chars, from_s_chars, fullsize);

          /* Relocate FROM_S_CHARS's reference */
          set_string_data (string, &(to_s_chars->chars[0]));

          from_pos += fullsize;
          to_pos += fullsize;
        }
    }

  /* Set current to the last string chars block still used and
     free any that follow. */
  {
    struct string_chars_block *victim;

    for (victim = to_sb->next; victim; )
      {
	struct string_chars_block *next = victim->next;
	xfree (victim);
	victim = next;
      }

    current_string_chars_block = to_sb;
    current_string_chars_block->pos = to_pos;
    current_string_chars_block->next = 0;
  }
}

#if 1 /* Hack to debug missing purecopy's */
static int debug_string_purity;

static void
debug_string_purity_print (Lisp_String *p)
{
  Charcount i;
  Charcount s = string_char_length (p);
  stderr_out ("\"");
  for (i = 0; i < s; i++)
  {
    Emchar ch = string_char (p, i);
    if (ch < 32 || ch >= 126)
      stderr_out ("\\%03o", ch);
    else if (ch == '\\' || ch == '\"')
      stderr_out ("\\%c", ch);
    else
      stderr_out ("%c", ch);
  }
  stderr_out ("\"\n");
}
#endif /* 1 */


static void
sweep_strings (void)
{
  int num_small_used = 0, num_small_bytes = 0, num_bytes = 0;
  int debug = debug_string_purity;

#define UNMARK_string(ptr) do {			\
    Lisp_String *p = (ptr);			\
    size_t size = string_length (p);		\
    UNMARK_RECORD_HEADER (&(p->lheader));	\
    num_bytes += size;				\
    if (!BIG_STRING_SIZE_P (size))		\
      {						\
	num_small_bytes += size;		\
        num_small_used++;			\
      }						\
    if (debug)					\
      debug_string_purity_print (p);		\
  } while (0)
#define ADDITIONAL_FREE_string(ptr) do {	\
    size_t size = string_length (ptr);		\
    if (BIG_STRING_SIZE_P (size))		\
      xfree (ptr->data);			\
  } while (0)

  SWEEP_FIXED_TYPE_BLOCK (string, Lisp_String);

  gc_count_num_short_string_in_use = num_small_used;
  gc_count_string_total_size = num_bytes;
  gc_count_short_string_total_size = num_small_bytes;
}


/* I hate duplicating all this crap! */
int
marked_p (Lisp_Object obj)
{
  /* Checks we used to perform. */
  /* if (EQ (obj, Qnull_pointer)) return 1; */
  /* if (!POINTER_TYPE_P (XGCTYPE (obj))) return 1; */
  /* if (PURIFIED (XPNTR (obj))) return 1; */

  if (XTYPE (obj) == Lisp_Type_Record)
    {
      struct lrecord_header *lheader = XRECORD_LHEADER (obj);

      GC_CHECK_LHEADER_INVARIANTS (lheader);

      return MARKED_RECORD_HEADER_P (lheader);
    }
  return 1;
}

static void
gc_sweep (void)
{
  /* Free all unmarked records.  Do this at the very beginning,
     before anything else, so that the finalize methods can safely
     examine items in the objects.  sweep_lcrecords_1() makes
     sure to call all the finalize methods *before* freeing anything,
     to complete the safety. */
  {
    int ignored;
    sweep_lcrecords_1 (&all_lcrecords, &ignored);
  }

  compact_string_chars ();

  /* Finalize methods below (called through the ADDITIONAL_FREE_foo
     macros) must be *extremely* careful to make sure they're not
     referencing freed objects.  The only two existing finalize
     methods (for strings and markers) pass muster -- the string
     finalizer doesn't look at anything but its own specially-
     created block, and the marker finalizer only looks at live
     buffers (which will never be freed) and at the markers before
     and after it in the chain (which, by induction, will never be
     freed because if so, they would have already removed themselves
     from the chain). */

  /* Put all unmarked strings on free list, free'ing the string chars
     of large unmarked strings */
  sweep_strings ();

  /* Put all unmarked conses on free list */
  sweep_conses ();

  /* Free all unmarked bit vectors */
  sweep_bit_vectors_1 (&all_bit_vectors,
		       &gc_count_num_bit_vector_used,
		       &gc_count_bit_vector_total_size,
		       &gc_count_bit_vector_storage);

  /* Free all unmarked compiled-function objects */
  sweep_compiled_functions ();

#ifdef LISP_FLOAT_TYPE
  /* Put all unmarked floats on free list */
  sweep_floats ();
#endif

  /* Put all unmarked symbols on free list */
  sweep_symbols ();

  /* Put all unmarked extents on free list */
  sweep_extents ();

  /* Put all unmarked markers on free list.
     Dechain each one first from the buffer into which it points. */
  sweep_markers ();

  sweep_events ();

#ifdef PDUMP
  pdump_objects_unmark ();
#endif
}

/* Clearing for disksave. */

void
disksave_object_finalization (void)
{
  /* It's important that certain information from the environment not get
     dumped with the executable (pathnames, environment variables, etc.).
     To make it easier to tell when this has happened with strings(1) we
     clear some known-to-be-garbage blocks of memory, so that leftover
     results of old evaluation don't look like potential problems.
     But first we set some notable variables to nil and do one more GC,
     to turn those strings into garbage.
  */

  /* Yeah, this list is pretty ad-hoc... */
  Vprocess_environment = Qnil;
  Vexec_directory = Qnil;
  Vdata_directory = Qnil;
  Vsite_directory = Qnil;
  Vdoc_directory = Qnil;
  Vconfigure_info_directory = Qnil;
  Vexec_path = Qnil;
  Vload_path = Qnil;
  /* Vdump_load_path = Qnil; */
  /* Release hash tables for locate_file */
  Flocate_file_clear_hashing (Qt);
  uncache_home_directory();

#if defined(LOADHIST) && !(defined(LOADHIST_DUMPED) || \
			   defined(LOADHIST_BUILTIN))
  Vload_history = Qnil;
#endif
  Vshell_file_name = Qnil;

  garbage_collect_1 ();

  /* Run the disksave finalization methods of all live objects. */
  disksave_object_finalization_1 ();

  /* Zero out the uninitialized (really, unused) part of the containers
     for the live strings. */
  {
    struct string_chars_block *scb;
    for (scb = first_string_chars_block; scb; scb = scb->next)
      {
	int count = sizeof (scb->string_chars) - scb->pos;

	assert (count >= 0 && count < STRING_CHARS_BLOCK_SIZE);
	if (count != 0)
	  {
	    /* from the block's fill ptr to the end */
	    memset ((scb->string_chars + scb->pos), 0, count);
	  }
      }
  }

  /* There, that ought to be enough... */

}


Lisp_Object
restore_gc_inhibit (Lisp_Object val)
{
  gc_currently_forbidden = XINT (val);
  return val;
}

/* Maybe we want to use this when doing a "panic" gc after memory_full()? */
static int gc_hooks_inhibited;

struct post_gc_action
{
  void (*fun) (void *);
  void *arg;
};

typedef struct post_gc_action post_gc_action;

typedef struct
{
  Dynarr_declare (post_gc_action);
} post_gc_action_dynarr;

static post_gc_action_dynarr *post_gc_actions;

/* Register an action to be called at the end of GC.
   gc_in_progress is 0 when this is called.
   This is used when it is discovered that an action needs to be taken,
   but it's during GC, so it's not safe. (e.g. in a finalize method.)

   As a general rule, do not use Lisp objects here.
   And NEVER signal an error.
*/

void
register_post_gc_action (void (*fun) (void *), void *arg)
{
  post_gc_action action;

  if (!post_gc_actions)
    post_gc_actions = Dynarr_new (post_gc_action);

  action.fun = fun;
  action.arg = arg;

  Dynarr_add (post_gc_actions, action);
}

static void
run_post_gc_actions (void)
{
  int i;

  if (post_gc_actions)
    {
      for (i = 0; i < Dynarr_length (post_gc_actions); i++)
	{
	  post_gc_action action = Dynarr_at (post_gc_actions, i);
	  (action.fun) (action.arg);
	}

      Dynarr_reset (post_gc_actions);
    }
}


void
garbage_collect_1 (void)
{
#if MAX_SAVE_STACK > 0
  char stack_top_variable;
  extern char *stack_bottom;
#endif
  struct frame *f;
  int speccount;
  int cursor_changed;
  Lisp_Object pre_gc_cursor;
  struct gcpro gcpro1;

  if (gc_in_progress
      || gc_currently_forbidden
      || in_display
      || preparing_for_armageddon)
    return;

  /* We used to call selected_frame() here.

     The following functions cannot be called inside GC
     so we move to after the above tests. */
  {
    Lisp_Object frame;
    Lisp_Object device = Fselected_device (Qnil);
    if (NILP (device)) /* Could happen during startup, eg. if always_gc */
      return;
    frame = DEVICE_SELECTED_FRAME (XDEVICE (device));
    if (NILP (frame))
      signal_simple_error ("No frames exist on device", device);
    f = XFRAME (frame);
  }

  pre_gc_cursor = Qnil;
  cursor_changed = 0;

  GCPRO1 (pre_gc_cursor);

  /* Very important to prevent GC during any of the following
     stuff that might run Lisp code; otherwise, we'll likely
     have infinite GC recursion. */
  speccount = specpdl_depth ();
  record_unwind_protect (restore_gc_inhibit,
                         make_int (gc_currently_forbidden));
  gc_currently_forbidden = 1;

  if (!gc_hooks_inhibited)
    run_hook_trapping_errors ("Error in pre-gc-hook", Qpre_gc_hook);

  /* Now show the GC cursor/message. */
  if (!noninteractive)
    {
      if (FRAME_WIN_P (f))
	{
	  Lisp_Object frame = make_frame (f);
	  Lisp_Object cursor = glyph_image_instance (Vgc_pointer_glyph,
						     FRAME_SELECTED_WINDOW (f),
						     ERROR_ME_NOT, 1);
	  pre_gc_cursor = f->pointer;
	  if (POINTER_IMAGE_INSTANCEP (cursor)
	      /* don't change if we don't know how to change back. */
	      && POINTER_IMAGE_INSTANCEP (pre_gc_cursor))
	    {
	      cursor_changed = 1;
	      Fset_frame_pointer (frame, cursor);
	    }
	}

      /* Don't print messages to the stream device. */
      if (!cursor_changed && !FRAME_STREAM_P (f))
	{
	  char *msg = (STRINGP (Vgc_message)
		       ? GETTEXT ((char *) XSTRING_DATA (Vgc_message))
		       : 0);
	  Lisp_Object args[2], whole_msg;
	  args[0] = build_string (msg ? msg :
				  GETTEXT ((const char *) gc_default_message));
	  args[1] = build_string ("...");
	  whole_msg = Fconcat (2, args);
	  echo_area_message (f, (Bufbyte *) 0, whole_msg, 0, -1,
			     Qgarbage_collecting);
	}
    }

  /***** Now we actually start the garbage collection. */

  gc_in_progress = 1;
  inhibit_non_essential_printing_operations = 1;

  gc_generation_number[0]++;

#if MAX_SAVE_STACK > 0

  /* Save a copy of the contents of the stack, for debugging.  */
  if (!purify_flag)
    {
      /* Static buffer in which we save a copy of the C stack at each GC.  */
      static char *stack_copy;
      static size_t stack_copy_size;

      ptrdiff_t stack_diff = &stack_top_variable - stack_bottom;
      size_t stack_size = (stack_diff > 0 ? stack_diff : -stack_diff);
      if (stack_size < MAX_SAVE_STACK)
	{
	  if (stack_copy_size < stack_size)
	    {
	      stack_copy = (char *) xrealloc (stack_copy, stack_size);
	      stack_copy_size = stack_size;
	    }

	  memcpy (stack_copy,
		  stack_diff > 0 ? stack_bottom : &stack_top_variable,
		  stack_size);
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  /* Do some totally ad-hoc resource clearing. */
  /* #### generalize this? */
  clear_event_resource ();
  cleanup_specifiers ();

  /* Mark all the special slots that serve as the roots of accessibility. */

  { /* staticpro() */
    Lisp_Object **p = Dynarr_begin (staticpros);
    size_t count;
    for (count = Dynarr_length (staticpros); count; count--)
      mark_object (**p++);
  }

  { /* staticpro_nodump() */
    Lisp_Object **p = Dynarr_begin (staticpros_nodump);
    size_t count;
    for (count = Dynarr_length (staticpros_nodump); count; count--)
      mark_object (**p++);
  }

  { /* GCPRO() */
    struct gcpro *tail;
    int i;
    for (tail = gcprolist; tail; tail = tail->next)
      for (i = 0; i < tail->nvars; i++)
	mark_object (tail->var[i]);
  }

  { /* specbind() */
    struct specbinding *bind;
    for (bind = specpdl; bind != specpdl_ptr; bind++)
      {
	mark_object (bind->symbol);
	mark_object (bind->old_value);
      }
  }

  {
    struct catchtag *catch;
    for (catch = catchlist; catch; catch = catch->next)
      {
	mark_object (catch->tag);
	mark_object (catch->val);
      }
  }

  {
    struct backtrace *backlist;
    for (backlist = backtrace_list; backlist; backlist = backlist->next)
      {
	int nargs = backlist->nargs;
	int i;

	mark_object (*backlist->function);
	if (nargs < 0 /* nargs == UNEVALLED || nargs == MANY */)
	  mark_object (backlist->args[0]);
	else
	  for (i = 0; i < nargs; i++)
	    mark_object (backlist->args[i]);
      }
  }

  mark_redisplay ();
  mark_profiling_info ();

  /* OK, now do the after-mark stuff.  This is for things that
     are only marked when something else is marked (e.g. weak hash tables).
     There may be complex dependencies between such objects -- e.g.
     a weak hash table might be unmarked, but after processing a later
     weak hash table, the former one might get marked.  So we have to
     iterate until nothing more gets marked. */

  while (finish_marking_weak_hash_tables () > 0 ||
	 finish_marking_weak_lists       () > 0)
    ;

  /* And prune (this needs to be called after everything else has been
     marked and before we do any sweeping). */
  /* #### this is somewhat ad-hoc and should probably be an object
     method */
  prune_weak_hash_tables ();
  prune_weak_lists ();
  prune_specifiers ();
  prune_syntax_tables ();

  gc_sweep ();

  consing_since_gc = 0;
#ifndef DEBUG_XEMACS
  /* Allow you to set it really fucking low if you really want ... */
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;
#endif

  inhibit_non_essential_printing_operations = 0;
  gc_in_progress = 0;

  run_post_gc_actions ();

  /******* End of garbage collection ********/

  run_hook_trapping_errors ("Error in post-gc-hook", Qpost_gc_hook);

  /* Now remove the GC cursor/message */
  if (!noninteractive)
    {
      if (cursor_changed)
	Fset_frame_pointer (make_frame (f), pre_gc_cursor);
      else if (!FRAME_STREAM_P (f))
	{
	  char *msg = (STRINGP (Vgc_message)
		       ? GETTEXT ((char *) XSTRING_DATA (Vgc_message))
		       : 0);

	  /* Show "...done" only if the echo area would otherwise be empty. */
	  if (NILP (clear_echo_area (selected_frame (),
				     Qgarbage_collecting, 0)))
	    {
	      Lisp_Object args[2], whole_msg;
	      args[0] = build_string (msg ? msg :
				      GETTEXT ((const char *)
					       gc_default_message));
	      args[1] = build_string ("... done");
	      whole_msg = Fconcat (2, args);
	      echo_area_message (selected_frame (), (Bufbyte *) 0,
				 whole_msg, 0, -1,
				 Qgarbage_collecting);
	    }
	}
    }

  /* now stop inhibiting GC */
  unbind_to (speccount, Qnil);

  if (!breathing_space)
    {
      breathing_space = malloc (4096 - MALLOC_OVERHEAD);
    }

  UNGCPRO;
  return;
}

/* Debugging aids.  */

static Lisp_Object
gc_plist_hack (const char *name, int value, Lisp_Object tail)
{
  /* C doesn't have local functions (or closures, or GC, or readable syntax,
     or portable numeric datatypes, or bit-vectors, or characters, or
     arrays, or exceptions, or ...) */
  return cons3 (intern (name), make_int (value), tail);
}

#define HACK_O_MATIC(type, name, pl) do {				\
  int s = 0;								\
  struct type##_block *x = current_##type##_block;			\
  while (x) { s += sizeof (*x) + MALLOC_OVERHEAD; x = x->prev; }	\
  (pl) = gc_plist_hack ((name), s, (pl));				\
} while (0)

DEFUN ("garbage-collect", Fgarbage_collect, 0, 0, "", /*
Reclaim storage for Lisp objects no longer needed.
Return info on amount of space in use:
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS
  PLIST)
  where `PLIST' is a list of alternating keyword/value pairs providing
  more detailed information.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
*/
       ())
{
  Lisp_Object pl = Qnil;
  unsigned int i;
  int gc_count_vector_total_size = 0;

  garbage_collect_1 ();

  for (i = 0; i < lrecord_type_count; i++)
    {
      if (lcrecord_stats[i].bytes_in_use != 0
          || lcrecord_stats[i].bytes_freed != 0
	  || lcrecord_stats[i].instances_on_free_list != 0)
        {
          char buf [255];
          const char *name = lrecord_implementations_table[i]->name;
	  int len = strlen (name);
	  /* save this for the FSFmacs-compatible part of the summary */
	  if (i == lrecord_type_vector)
	    gc_count_vector_total_size =
	      lcrecord_stats[i].bytes_in_use + lcrecord_stats[i].bytes_freed;

          sprintf (buf, "%s-storage", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].bytes_in_use, pl);
	  /* Okay, simple pluralization check for `symbol-value-varalias' */
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-freed", name);
	  else
	    sprintf (buf, "%ss-freed", name);
          if (lcrecord_stats[i].instances_freed != 0)
            pl = gc_plist_hack (buf, lcrecord_stats[i].instances_freed, pl);
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-on-free-list", name);
	  else
	    sprintf (buf, "%ss-on-free-list", name);
          if (lcrecord_stats[i].instances_on_free_list != 0)
            pl = gc_plist_hack (buf, lcrecord_stats[i].instances_on_free_list,
				pl);
	  if (name[len-1] == 's')
	    sprintf (buf, "%ses-used", name);
	  else
	    sprintf (buf, "%ss-used", name);
          pl = gc_plist_hack (buf, lcrecord_stats[i].instances_in_use, pl);
        }
    }

  HACK_O_MATIC (extent, "extent-storage", pl);
  pl = gc_plist_hack ("extents-free", gc_count_num_extent_freelist, pl);
  pl = gc_plist_hack ("extents-used", gc_count_num_extent_in_use, pl);
  HACK_O_MATIC (event, "event-storage", pl);
  pl = gc_plist_hack ("events-free", gc_count_num_event_freelist, pl);
  pl = gc_plist_hack ("events-used", gc_count_num_event_in_use, pl);
  HACK_O_MATIC (marker, "marker-storage", pl);
  pl = gc_plist_hack ("markers-free", gc_count_num_marker_freelist, pl);
  pl = gc_plist_hack ("markers-used", gc_count_num_marker_in_use, pl);
#ifdef LISP_FLOAT_TYPE
  HACK_O_MATIC (float, "float-storage", pl);
  pl = gc_plist_hack ("floats-free", gc_count_num_float_freelist, pl);
  pl = gc_plist_hack ("floats-used", gc_count_num_float_in_use, pl);
#endif /* LISP_FLOAT_TYPE */
  HACK_O_MATIC (string, "string-header-storage", pl);
  pl = gc_plist_hack ("long-strings-total-length",
                      gc_count_string_total_size
		      - gc_count_short_string_total_size, pl);
  HACK_O_MATIC (string_chars, "short-string-storage", pl);
  pl = gc_plist_hack ("short-strings-total-length",
                      gc_count_short_string_total_size, pl);
  pl = gc_plist_hack ("strings-free", gc_count_num_string_freelist, pl);
  pl = gc_plist_hack ("long-strings-used",
                      gc_count_num_string_in_use
		      - gc_count_num_short_string_in_use, pl);
  pl = gc_plist_hack ("short-strings-used",
                      gc_count_num_short_string_in_use, pl);

  HACK_O_MATIC (compiled_function, "compiled-function-storage", pl);
  pl = gc_plist_hack ("compiled-functions-free",
		      gc_count_num_compiled_function_freelist, pl);
  pl = gc_plist_hack ("compiled-functions-used",
		      gc_count_num_compiled_function_in_use, pl);

  pl = gc_plist_hack ("bit-vector-storage", gc_count_bit_vector_storage, pl);
  pl = gc_plist_hack ("bit-vectors-total-length",
                      gc_count_bit_vector_total_size, pl);
  pl = gc_plist_hack ("bit-vectors-used", gc_count_num_bit_vector_used, pl);

  HACK_O_MATIC (symbol, "symbol-storage", pl);
  pl = gc_plist_hack ("symbols-free", gc_count_num_symbol_freelist, pl);
  pl = gc_plist_hack ("symbols-used", gc_count_num_symbol_in_use, pl);

  HACK_O_MATIC (cons, "cons-storage", pl);
  pl = gc_plist_hack ("conses-free", gc_count_num_cons_freelist, pl);
  pl = gc_plist_hack ("conses-used", gc_count_num_cons_in_use, pl);

  /* The things we do for backwards-compatibility */
  return
    list6 (Fcons (make_int (gc_count_num_cons_in_use),
		  make_int (gc_count_num_cons_freelist)),
	   Fcons (make_int (gc_count_num_symbol_in_use),
		  make_int (gc_count_num_symbol_freelist)),
	   Fcons (make_int (gc_count_num_marker_in_use),
		  make_int (gc_count_num_marker_freelist)),
	   make_int (gc_count_string_total_size),
	   make_int (gc_count_vector_total_size),
	   pl);
}
#undef HACK_O_MATIC

DEFUN ("consing-since-gc", Fconsing_since_gc, 0, 0, "", /*
Return the number of bytes consed since the last garbage collection.
\"Consed\" is a misnomer in that this actually counts allocation
of all different kinds of objects, not just conses.

If this value exceeds `gc-cons-threshold', a garbage collection happens.
*/
       ())
{
  return make_int (consing_since_gc);
}

#if 0
DEFUN ("memory-limit", Fmemory_limit, 0, 0, 0, /*
Return the address of the last byte Emacs has allocated, divided by 1024.
This may be helpful in debugging Emacs's memory usage.
The value is divided by 1024 to make sure it will fit in a lisp integer.
*/
       ())
{
  return make_int ((EMACS_INT) sbrk (0) / 1024);
}
#endif


int
object_dead_p (Lisp_Object obj)
{
  return ((BUFFERP  (obj) && !BUFFER_LIVE_P  (XBUFFER  (obj))) ||
	  (FRAMEP   (obj) && !FRAME_LIVE_P   (XFRAME   (obj))) ||
	  (WINDOWP  (obj) && !WINDOW_LIVE_P  (XWINDOW  (obj))) ||
	  (DEVICEP  (obj) && !DEVICE_LIVE_P  (XDEVICE  (obj))) ||
	  (CONSOLEP (obj) && !CONSOLE_LIVE_P (XCONSOLE (obj))) ||
	  (EVENTP   (obj) && !EVENT_LIVE_P   (XEVENT   (obj))) ||
	  (EXTENTP  (obj) && !EXTENT_LIVE_P  (XEXTENT  (obj))));
}

#ifdef MEMORY_USAGE_STATS

/* Attempt to determine the actual amount of space that is used for
   the block allocated starting at PTR, supposedly of size "CLAIMED_SIZE".

   It seems that the following holds:

   1. When using the old allocator (malloc.c):

      -- blocks are always allocated in chunks of powers of two.  For
	 each block, there is an overhead of 8 bytes if rcheck is not
	 defined, 20 bytes if it is defined.  In other words, a
	 one-byte allocation needs 8 bytes of overhead for a total of
	 9 bytes, and needs to have 16 bytes of memory chunked out for
	 it.

   2. When using the new allocator (gmalloc.c):

      -- blocks are always allocated in chunks of powers of two up
         to 4096 bytes.  Larger blocks are allocated in chunks of
	 an integral multiple of 4096 bytes.  The minimum block
         size is 2*sizeof (void *), or 16 bytes if SUNOS_LOCALTIME_BUG
	 is defined.  There is no per-block overhead, but there
	 is an overhead of 3*sizeof (size_t) for each 4096 bytes
	 allocated.

    3. When using the system malloc, anything goes, but they are
       generally slower and more space-efficient than the GNU
       allocators.  One possibly reasonable assumption to make
       for want of better data is that sizeof (void *), or maybe
       2 * sizeof (void *), is required as overhead and that
       blocks are allocated in the minimum required size except
       that some minimum block size is imposed (e.g. 16 bytes). */

size_t
malloced_storage_size (void *ptr, size_t claimed_size,
		       struct overhead_stats *stats)
{
  size_t orig_claimed_size = claimed_size;

#ifdef GNU_MALLOC

  if (claimed_size < 2 * sizeof (void *))
    claimed_size = 2 * sizeof (void *);
# ifdef SUNOS_LOCALTIME_BUG
  if (claimed_size < 16)
    claimed_size = 16;
# endif
  if (claimed_size < 4096)
    {
      int log = 1;

      /* compute the log base two, more or less, then use it to compute
	 the block size needed. */
      claimed_size--;
      /* It's big, it's heavy, it's wood! */
      while ((claimed_size /= 2) != 0)
	++log;
      claimed_size = 1;
      /* It's better than bad, it's good! */
      while (log > 0)
        {
	  claimed_size *= 2;
          log--;
        }
      /* We have to come up with some average about the amount of
	 blocks used. */
      if ((size_t) (rand () & 4095) < claimed_size)
	claimed_size += 3 * sizeof (void *);
    }
  else
    {
      claimed_size += 4095;
      claimed_size &= ~4095;
      claimed_size += (claimed_size / 4096) * 3 * sizeof (size_t);
    }

#elif defined (SYSTEM_MALLOC)

  if (claimed_size < 16)
    claimed_size = 16;
  claimed_size += 2 * sizeof (void *);

#else /* old GNU allocator */

# ifdef rcheck /* #### may not be defined here */
  claimed_size += 20;
# else
  claimed_size += 8;
# endif
  {
    int log = 1;

    /* compute the log base two, more or less, then use it to compute
       the block size needed. */
    claimed_size--;
    /* It's big, it's heavy, it's wood! */
    while ((claimed_size /= 2) != 0)
      ++log;
    claimed_size = 1;
    /* It's better than bad, it's good! */
    while (log > 0)
      {
	claimed_size *= 2;
        log--;
      }
  }

#endif /* old GNU allocator */

  if (stats)
    {
      stats->was_requested += orig_claimed_size;
      stats->malloc_overhead += claimed_size - orig_claimed_size;
    }
  return claimed_size;
}

size_t
fixed_type_block_overhead (size_t size)
{
  size_t per_block = TYPE_ALLOC_SIZE (cons, unsigned char);
  size_t overhead = 0;
  size_t storage_size = malloced_storage_size (0, per_block, 0);
  while (size >= per_block)
    {
      size -= per_block;
      overhead += sizeof (void *) + per_block - storage_size;
    }
  if (rand () % per_block < size)
    overhead += sizeof (void *) + per_block - storage_size;
  return overhead;
}

#endif /* MEMORY_USAGE_STATS */


/* Initialization */
void
reinit_alloc_once_early (void)
{
  gc_generation_number[0] = 0;
  breathing_space = 0;
  XSETINT (all_bit_vectors, 0); /* Qzero may not be set yet. */
  XSETINT (Vgc_message, 0);
  all_lcrecords = 0;
  ignore_malloc_warnings = 1;
#ifdef DOUG_LEA_MALLOC
  mallopt (M_TRIM_THRESHOLD, 128*1024); /* trim threshold */
  mallopt (M_MMAP_THRESHOLD, 64*1024); /* mmap threshold */
#if 1 /* Moved to emacs.c */
  mallopt (M_MMAP_MAX, 0); /* max. number of mmap'ed areas */
#endif
#endif
  init_string_alloc ();
  init_string_chars_alloc ();
  init_cons_alloc ();
  init_symbol_alloc ();
  init_compiled_function_alloc ();
#ifdef LISP_FLOAT_TYPE
  init_float_alloc ();
#endif /* LISP_FLOAT_TYPE */
  init_marker_alloc ();
  init_extent_alloc ();
  init_event_alloc ();

  ignore_malloc_warnings = 0;

  if (staticpros_nodump)
    Dynarr_free (staticpros_nodump);
  staticpros_nodump = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_resize (staticpros_nodump, 100); /* merely a small optimization */

  consing_since_gc = 0;
#if 1
  gc_cons_threshold = 500000; /* XEmacs change */
#else
  gc_cons_threshold = 15000; /* debugging */
#endif
  lrecord_uid_counter = 259;
  debug_string_purity = 0;
  gcprolist = 0;

  gc_currently_forbidden = 0;
  gc_hooks_inhibited = 0;

#ifdef ERROR_CHECK_TYPECHECK
  ERROR_ME.really_unlikely_name_to_have_accidentally_in_a_non_errb_structure =
    666;
  ERROR_ME_NOT.
    really_unlikely_name_to_have_accidentally_in_a_non_errb_structure = 42;
  ERROR_ME_WARN.
    really_unlikely_name_to_have_accidentally_in_a_non_errb_structure =
      3333632;
#endif /* ERROR_CHECK_TYPECHECK */
}

void
init_alloc_once_early (void)
{
  reinit_alloc_once_early ();

  {
    int i;
    for (i = 0; i < countof (lrecord_implementations_table); i++)
      lrecord_implementations_table[i] = 0;
  }

  INIT_LRECORD_IMPLEMENTATION (cons);
  INIT_LRECORD_IMPLEMENTATION (vector);
  INIT_LRECORD_IMPLEMENTATION (string);
  INIT_LRECORD_IMPLEMENTATION (lcrecord_list);

  staticpros = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_resize (staticpros, 1410); /* merely a small optimization */
  dump_add_root_struct_ptr (&staticpros, &staticpros_description);
}

void
reinit_alloc (void)
{
  gcprolist = 0;
}

void
syms_of_alloc (void)
{
  DEFSYMBOL (Qpre_gc_hook);
  DEFSYMBOL (Qpost_gc_hook);
  DEFSYMBOL (Qgarbage_collecting);

  DEFSUBR (Fcons);
  DEFSUBR (Flist);
  DEFSUBR (Fvector);
  DEFSUBR (Fbit_vector);
  DEFSUBR (Fmake_byte_code);
  DEFSUBR (Fmake_list);
  DEFSUBR (Fmake_vector);
  DEFSUBR (Fmake_bit_vector);
  DEFSUBR (Fmake_string);
  DEFSUBR (Fstring);
  DEFSUBR (Fmake_symbol);
  DEFSUBR (Fmake_marker);
  DEFSUBR (Fpurecopy);
  DEFSUBR (Fgarbage_collect);
#if 0
  DEFSUBR (Fmemory_limit);
#endif
  DEFSUBR (Fconsing_since_gc);
}

void
vars_of_alloc (void)
{
  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold /*
*Number of bytes of consing between garbage collections.
\"Consing\" is a misnomer in that this actually counts allocation
of all different kinds of objects, not just conses.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically when `eval' or `funcall' are
called.  (Note that `funcall' is called implicitly as part of evaluation.)
By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.

See also `consing-since-gc'.
*/ );

#ifdef DEBUG_XEMACS
  DEFVAR_INT ("debug-allocation", &debug_allocation /*
If non-zero, print out information to stderr about all objects allocated.
See also `debug-allocation-backtrace-length'.
*/ );
  debug_allocation = 0;

  DEFVAR_INT ("debug-allocation-backtrace-length",
	      &debug_allocation_backtrace_length /*
Length (in stack frames) of short backtrace printed out by `debug-allocation'.
*/ );
  debug_allocation_backtrace_length = 2;
#endif

  DEFVAR_BOOL ("purify-flag", &purify_flag /*
Non-nil means loading Lisp code in order to dump an executable.
This means that certain objects should be allocated in readonly space.
*/ );

  DEFVAR_LISP ("pre-gc-hook", &Vpre_gc_hook /*
Function or functions to be run just before each garbage collection.
Interrupts, garbage collection, and errors are inhibited while this hook
runs, so be extremely careful in what you add here.  In particular, avoid
consing, and do not interact with the user.
*/ );
  Vpre_gc_hook = Qnil;

  DEFVAR_LISP ("post-gc-hook", &Vpost_gc_hook /*
Function or functions to be run just after each garbage collection.
Interrupts, garbage collection, and errors are inhibited while this hook
runs, so be extremely careful in what you add here.  In particular, avoid
consing, and do not interact with the user.
*/ );
  Vpost_gc_hook = Qnil;

  DEFVAR_LISP ("gc-message", &Vgc_message /*
String to print to indicate that a garbage collection is in progress.
This is printed in the echo area.  If the selected frame is on a
window system and `gc-pointer-glyph' specifies a value (i.e. a pointer
image instance) in the domain of the selected frame, the mouse pointer
will change instead of this message being printed.
*/ );
  Vgc_message = build_string (gc_default_message);

  DEFVAR_LISP ("gc-pointer-glyph", &Vgc_pointer_glyph /*
Pointer glyph used to indicate that a garbage collection is in progress.
If the selected window is on a window system and this glyph specifies a
value (i.e. a pointer image instance) in the domain of the selected
window, the pointer will be changed as specified during garbage collection.
Otherwise, a message will be printed in the echo area, as controlled
by `gc-message'.
*/ );
}

void
complex_vars_of_alloc (void)
{
  Vgc_pointer_glyph = Fmake_glyph_internal (Qpointer);
}
