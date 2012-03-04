/* Copyright (c) 1994, 1995 Free Software Foundation, Inc.
   Copyright (c) 1995 Sun Microsystems, Inc.
   Copyright (c) 1995, 1996, 2000 Ben Wing.

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

/* This file has been Mule-ized. */

/* Written by Ben Wing <ben@xemacs.org>.

   [Originally written by some people at Lucid.
   Hacked on by jwz.
   Start/end-open stuff added by John Rose (john.rose@eng.sun.com).
   Rewritten from scratch by Ben Wing, December 1994.] */

/* Commentary:

   Extents are regions over a buffer, with a start and an end position
   denoting the region of the buffer included in the extent.  In
   addition, either end can be closed or open, meaning that the endpoint
   is or is not logically included in the extent.  Insertion of a character
   at a closed endpoint causes the character to go inside the extent;
   insertion at an open endpoint causes the character to go outside.

   Extent endpoints are stored using memory indices (see insdel.c),
   to minimize the amount of adjusting that needs to be done when
   characters are inserted or deleted.

   (Formerly, extent endpoints at the gap could be either before or
   after the gap, depending on the open/closedness of the endpoint.
   The intent of this was to make it so that insertions would
   automatically go inside or out of extents as necessary with no
   further work needing to be done.  It didn't work out that way,
   however, and just ended up complexifying and buggifying all the
   rest of the code.)

   Extents are compared using memory indices.  There are two orderings
   for extents and both orders are kept current at all times.  The normal
   or "display" order is as follows:

   Extent A is "less than" extent B, that is, earlier in the display order,
   if:    A-start < B-start,
   or if: A-start = B-start, and A-end > B-end

   So if two extents begin at the same position, the larger of them is the
   earlier one in the display order (EXTENT_LESS is true).

   For the e-order, the same thing holds: Extent A is "less than" extent B
   in e-order, that is, later in the buffer,
   if:    A-end < B-end,
   or if: A-end = B-end, and A-start > B-start

   So if two extents end at the same position, the smaller of them is the
   earlier one in the e-order (EXTENT_E_LESS is true).

   The display order and the e-order are complementary orders: any
   theorem about the display order also applies to the e-order if you
   swap all occurrences of "display order" and "e-order", "less than"
   and "greater than", and "extent start" and "extent end".

   Extents can be zero-length, and will end up that way if their endpoints
   are explicitly set that way or if their detachable property is nil
   and all the text in the extent is deleted. (The exception is open-open
   zero-length extents, which are barred from existing because there is
   no sensible way to define their properties.  Deletion of the text in
   an open-open extent causes it to be converted into a closed-open
   extent.)  Zero-length extents are primarily used to represent
   annotations, and behave as follows:

   1) Insertion at the position of a zero-length extent expands the extent
   if both endpoints are closed; goes after the extent if it is closed-open;
   and goes before the extent if it is open-closed.

   2) Deletion of a character on a side of a zero-length extent whose
   corresponding endpoint is closed causes the extent to be detached if
   it is detachable; if the extent is not detachable or the corresponding
   endpoint is open, the extent remains in the buffer, moving as necessary.

   Note that closed-open, non-detachable zero-length extents behave exactly
   like markers and that open-closed, non-detachable zero-length extents
   behave like the "point-type" marker in Mule.

   #### The following information is wrong in places.

   More about the different orders:
   --------------------------------

   The extents in a buffer are ordered by "display order" because that
   is that order that the redisplay mechanism needs to process them in.
   The e-order is an auxiliary ordering used to facilitate operations
   over extents.  The operations that can be performed on the ordered
   list of extents in a buffer are

   1) Locate where an extent would go if inserted into the list.
   2) Insert an extent into the list.
   3) Remove an extent from the list.
   4) Map over all the extents that overlap a range.

   (4) requires being able to determine the first and last extents
   that overlap a range.

   NOTE: "overlap" is used as follows:

   -- two ranges overlap if they have at least one point in common.
      Whether the endpoints are open or closed makes a difference here.
   -- a point overlaps a range if the point is contained within the
      range; this is equivalent to treating a point P as the range
      [P, P].
   -- In the case of an *extent* overlapping a point or range, the
      extent is normally treated as having closed endpoints.  This
      applies consistently in the discussion of stacks of extents
      and such below.  Note that this definition of overlap is not
      necessarily consistent with the extents that `map-extents'
      maps over, since `map-extents' sometimes pays attention to
      whether the endpoints of an extents are open or closed.
      But for our purposes, it greatly simplifies things to treat
      all extents as having closed endpoints.

   First, define >, <, <=, etc. as applied to extents to mean
     comparison according to the display order.  Comparison between an
     extent E and an index I means comparison between E and the range
     [I, I].
   Also define e>, e<, e<=, etc. to mean comparison according to the
     e-order.
   For any range R, define R(0) to be the starting index of the range
     and R(1) to be the ending index of the range.
   For any extent E, define E(next) to be the extent directly following
     E, and E(prev) to be the extent directly preceding E.  Assume
     E(next) and E(prev) can be determined from E in constant time.
     (This is because we store the extent list as a doubly linked
     list.)
   Similarly, define E(e-next) and E(e-prev) to be the extents
     directly following and preceding E in the e-order.

   Now:

   Let R be a range.
   Let F be the first extent overlapping R.
   Let L be the last extent overlapping R.

   Theorem 1: R(1) lies between L and L(next), i.e. L <= R(1) < L(next).

   This follows easily from the definition of display order.  The
   basic reason that this theorem applies is that the display order
   sorts by increasing starting index.

   Therefore, we can determine L just by looking at where we would
   insert R(1) into the list, and if we know F and are moving forward
   over extents, we can easily determine when we've hit L by comparing
   the extent we're at to R(1).

   Theorem 2: F(e-prev) e< [1, R(0)] e<= F.

   This is the analog of Theorem 1, and applies because the e-order
   sorts by increasing ending index.

   Therefore, F can be found in the same amount of time as operation (1),
   i.e. the time that it takes to locate where an extent would go if
   inserted into the e-order list.

   If the lists were stored as balanced binary trees, then operation (1)
   would take logarithmic time, which is usually quite fast.  However,
   currently they're stored as simple doubly-linked lists, and instead
   we do some caching to try to speed things up.

   Define a "stack of extents" (or "SOE") as the set of extents
   (ordered in the display order) that overlap an index I, together with
   the SOE's "previous" extent, which is an extent that precedes I in
   the e-order. (Hopefully there will not be very many extents between
   I and the previous extent.)

   Now:

   Let I be an index, let S be the stack of extents on I, let F be
   the first extent in S, and let P be S's previous extent.

   Theorem 3: The first extent in S is the first extent that overlaps
   any range [I, J].

   Proof: Any extent that overlaps [I, J] but does not include I must
   have a start index > I, and thus be greater than any extent in S.

   Therefore, finding the first extent that overlaps a range R is the
   same as finding the first extent that overlaps R(0).

   Theorem 4: Let I2 be an index such that I2 > I, and let F2 be the
   first extent that overlaps I2.  Then, either F2 is in S or F2 is
   greater than any extent in S.

   Proof: If F2 does not include I then its start index is greater
   than I and thus it is greater than any extent in S, including F.
   Otherwise, F2 includes I and thus is in S, and thus F2 >= F.

*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "debug.h"
#include "ui/device.h"
#include "elhash.h"
#include "extents.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "ui/glyphs.h"
#include "ui/insdel.h"
#include "ui/keymap.h"
#include "opaque.h"
#include "process.h"
#include "ui/redisplay.h"
#include "ui/gutter.h"

/* ------------------------------- */
/*            gap array            */
/* ------------------------------- */

/* Note that this object is not extent-specific and should perhaps be
   moved into another file. */

typedef struct gap_array_marker_s *gap_array_marker_t;
typedef struct gap_array_s *gap_array_t;

/* Holds a marker that moves as elements in the array are inserted and
   deleted, similar to standard markers. */

struct gap_array_marker_s {
	int pos;
	gap_array_marker_t next;
};

/* Holds a "gap array", which is an array of elements with a gap located
   in it.  Insertions and deletions with a high degree of locality
   are very fast, essentially in constant time.  Array positions as
   used and returned in the gap array functions are independent of
   the gap. */

struct gap_array_s {
	char *array;
	int gap;
	int gapsize;
	int numels;
	int elsize;
	gap_array_marker_t markers;
};

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
static gap_array_marker_t gap_array_marker_freelist;
#endif	/* !BDWGC */

/* Convert a "memory position" (i.e. taking the gap into account) into
   the address of the element at (i.e. after) that position.  "Memory
   positions" are only used internally and are of type Memind.
   "Array positions" are used externally and are of type int. */
#define GAP_ARRAY_MEMEL_ADDR(ga, memel) ((ga)->array + (ga)->elsize*(memel))

/* Number of elements currently in a gap array */
#define GAP_ARRAY_NUM_ELS(ga) ((ga)->numels)

#define GAP_ARRAY_ARRAY_TO_MEMORY_POS(ga, pos)			\
	((pos) <= (ga)->gap ? (pos) : (pos) + (ga)->gapsize)

#define GAP_ARRAY_MEMORY_TO_ARRAY_POS(ga, pos)			\
	((pos) <= (ga)->gap ? (pos) : (pos) - (ga)->gapsize)

/* Convert an array position into the address of the element at
   (i.e. after) that position. */
#define GAP_ARRAY_EL_ADDR(ga, pos)					\
	((pos) < (ga)->gap						\
	 ? GAP_ARRAY_MEMEL_ADDR(ga, pos)				\
	 : GAP_ARRAY_MEMEL_ADDR(ga, (pos) + (ga)->gapsize))

/* ------------------------------- */
/*          extent list            */
/* ------------------------------- */

typedef struct extent_list_marker_s *extent_list_marker_t;
typedef struct extent_list_s *extent_list_t;

struct extent_list_marker_s {
	gap_array_marker_t m;
	int endp;
	extent_list_marker_t next;
};

struct extent_list_s {
	gap_array_t start;
	gap_array_t end;
	extent_list_marker_t markers;
};

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
static extent_list_marker_t extent_list_marker_freelist;
#endif	/* !BDWGC */

#define EXTENT_LESS_VALS(e,st,nd)		\
	((extent_start (e) < (st)) ||		\
	 ((extent_start (e) == (st)) &&		\
	  (extent_end (e) > (nd))))

#define EXTENT_EQUAL_VALS(e,st,nd)		\
	((extent_start (e) == (st)) &&		\
	 (extent_end (e) == (nd)))

#define EXTENT_LESS_EQUAL_VALS(e,st,nd)		\
	((extent_start (e) < (st)) ||		\
	 ((extent_start (e) == (st)) &&		\
	  (extent_end (e) >= (nd))))

/* Is extent E1 less than extent E2 in the display order? */
#define EXTENT_LESS(e1,e2)						\
	EXTENT_LESS_VALS (e1, extent_start (e2), extent_end (e2))

/* Is extent E1 equal to extent E2? */
#define EXTENT_EQUAL(e1,e2)						\
	EXTENT_EQUAL_VALS (e1, extent_start (e2), extent_end (e2))

/* Is extent E1 less than or equal to extent E2 in the display order? */
#define EXTENT_LESS_EQUAL(e1,e2)					\
	EXTENT_LESS_EQUAL_VALS (e1, extent_start (e2), extent_end (e2))

#define EXTENT_E_LESS_VALS(e,st,nd)		\
	((extent_end (e) < (nd)) ||		\
	 ((extent_end (e) == (nd)) &&		\
	  (extent_start (e) > (st))))

#define EXTENT_E_LESS_EQUAL_VALS(e,st,nd)	\
	((extent_end (e) < (nd)) ||		\
	 ((extent_end (e) == (nd)) &&		\
	  (extent_start (e) >= (st))))

/* Is extent E1 less than extent E2 in the e-order? */
#define EXTENT_E_LESS(e1,e2)						\
	EXTENT_E_LESS_VALS(e1, extent_start (e2), extent_end (e2))

/* Is extent E1 less than or equal to extent E2 in the e-order? */
#define EXTENT_E_LESS_EQUAL(e1,e2)					\
	EXTENT_E_LESS_EQUAL_VALS (e1, extent_start (e2), extent_end (e2))

#define EXTENT_GAP_ARRAY_AT(ga, pos) (*(EXTENT*)GAP_ARRAY_EL_ADDR(ga, pos))

/* ------------------------------- */
/*    auxiliary extent structure   */
/* ------------------------------- */

struct extent_auxiliary extent_auxiliary_defaults;

/* ------------------------------- */
/*     buffer-extent primitives    */
/* ------------------------------- */
typedef struct extent_stack_s *extent_stack_t;

struct extent_stack_s {
	extent_list_t extents;
	/* Position of stack of extents.  EXTENTS is the list of
	   all extents that overlap this position.  This position
	   can be -1 if the stack of extents is invalid (this
	   happens when a buffer is first created or a string's
	   stack of extents is created [a string's stack of extents
	   is nuked when a GC occurs, to conserve memory]). */
	Memind pos;
};

/* ------------------------------- */
/*           map-extents           */
/* ------------------------------- */

typedef int Endpoint_Index;

#define memind_to_startind(x, start_open)		\
	((Endpoint_Index) (((x) << 1) + !!(start_open)))
#define memind_to_endind(x, end_open)			\
	((Endpoint_Index) (((x) << 1) - !!(end_open)))

/* Combination macros */
#define bytind_to_startind(buf, x, start_open)			\
	memind_to_startind (bytind_to_memind (buf, x), start_open)
#define bytind_to_endind(buf, x, end_open)			\
	memind_to_endind (bytind_to_memind (buf, x), end_open)

/* ------------------------------- */
/*    buffer-or-string primitives  */
/* ------------------------------- */

/* Similar for Bytinds and start/end indices. */

#define buffer_or_string_bytind_to_startind(obj, ind, start_open)	\
	memind_to_startind(buffer_or_string_bytind_to_memind (obj, ind), \
			   start_open)

#define buffer_or_string_bytind_to_endind(obj, ind, end_open)		\
	memind_to_endind(buffer_or_string_bytind_to_memind (obj, ind),	\
			 end_open)

/* ------------------------------- */
/*      Lisp-level functions       */
/* ------------------------------- */

/* flags for decode_extent() */
#define DE_MUST_HAVE_BUFFER 1
#define DE_MUST_BE_ATTACHED 2

Lisp_Object Vlast_highlighted_extent;
Fixnum mouse_highlight_priority;

Lisp_Object Qextentp;
Lisp_Object Qextent_live_p;

Lisp_Object Qall_extents_closed;
Lisp_Object Qall_extents_open;
Lisp_Object Qall_extents_closed_open;
Lisp_Object Qall_extents_open_closed;
Lisp_Object Qstart_in_region;
Lisp_Object Qend_in_region;
Lisp_Object Qstart_and_end_in_region;
Lisp_Object Qstart_or_end_in_region;
Lisp_Object Qnegate_in_region;

Lisp_Object Qdetached;
Lisp_Object Qdestroyed;
Lisp_Object Qbegin_glyph;
Lisp_Object Qend_glyph;
Lisp_Object Qstart_open;
Lisp_Object Qend_open;
Lisp_Object Qstart_closed;
Lisp_Object Qend_closed;
Lisp_Object Qread_only;
/* Qhighlight defined in general.c */
Lisp_Object Qunique;
Lisp_Object Qduplicable;
Lisp_Object Qdetachable;
Lisp_Object Qpriority;
Lisp_Object Qmouse_face;
Lisp_Object Qinitial_redisplay_function;

/* This exists only for backwards compatibility. */
Lisp_Object Qglyph_layout;
Lisp_Object Qbegin_glyph_layout, Qend_glyph_layout;
Lisp_Object Qoutside_margin;
Lisp_Object Qinside_margin;
Lisp_Object Qwhitespace;
/* Qtext defined in general.c */

Lisp_Object Qcopy_function;
Lisp_Object Qpaste_function;

/* The idea here is that if we're given a list of faces, we
   need to "memoize" this so that two lists of faces that are `equal'
   turn into the same object.  When `set-extent-face' is called, we
   "memoize" into a list of actual faces; when `extent-face' is called,
   we do a reverse lookup to get the list of symbols. */

static Lisp_Object canonicalize_extent_property(Lisp_Object prop,
						Lisp_Object value);
Lisp_Object Vextent_face_memoize_hash_table;
Lisp_Object Vextent_face_reverse_memoize_hash_table;
Lisp_Object Vextent_face_reusable_list;
/* FSFmacs bogosity */
Lisp_Object Vdefault_text_properties;

EXFUN(Fextent_properties, 1);
EXFUN(Fset_extent_property, 3);

/* if true, we don't want to set any redisplay flags on modeline extent
   changes */
int in_modeline_generation;

/************************************************************************/
/*                       Generalized gap array                          */
/************************************************************************/

/* This generalizes the "array with a gap" model used to store buffer
   characters.  This is based on the stuff in insdel.c and should
   probably be merged with it.  This is not extent-specific and should
   perhaps be moved into a separate file. */

/* ------------------------------- */
/*        internal functions       */
/* ------------------------------- */

/* Adjust the gap array markers in the range (FROM, TO].  Parallel to
   adjust_markers() in insdel.c. */

static void
gap_array_adjust_markers(gap_array_t ga, Memind from, Memind to, int amount)
{
	gap_array_marker_t m;

	for (m = ga->markers; m; m = m->next) {
		m->pos = do_marker_adjustment(m->pos, from, to, amount);
	}
	return;
}

/* Move the gap to array position POS.  Parallel to move_gap() in
   insdel.c but somewhat simplified. */

static void
gap_array_move_gap(gap_array_t ga, int pos)
{
	int gap = ga->gap;
	int gapsize = ga->gapsize;

	assert(ga->array);
	if (pos < gap) {
		memmove(GAP_ARRAY_MEMEL_ADDR(ga, pos + gapsize),
			GAP_ARRAY_MEMEL_ADDR(ga, pos),
			(gap - pos) * ga->elsize);
		gap_array_adjust_markers(ga, (Memind) pos, (Memind) gap,
					 gapsize);
	} else if (pos > gap) {
		memmove(GAP_ARRAY_MEMEL_ADDR(ga, gap),
			GAP_ARRAY_MEMEL_ADDR(ga, gap + gapsize),
			(pos - gap) * ga->elsize);
		gap_array_adjust_markers(ga, (Memind) (gap + gapsize),
					 (Memind) (pos + gapsize), -gapsize);
	}
	ga->gap = pos;
	return;
}

/* Make the gap INCREMENT characters longer.  Parallel to make_gap() in
   insdel.c. */

static void
gap_array_make_gap(gap_array_t ga, int increment)
{
	char *ptr = ga->array;
	int real_gap_loc;
	int old_gap_size;

	/* If we have to get more space, get enough to last a while.  We use
	   a geometric progression that saves on realloc space. */
	increment += 100 + ga->numels / 8;

	ptr = (char*)xrealloc(ptr,
			      (ga->numels + ga->gapsize +
			       increment) * ga->elsize);
	if (ptr == 0) {
		memory_full();
	}
	ga->array = ptr;

	real_gap_loc = ga->gap;
	old_gap_size = ga->gapsize;

	/* Call the newly allocated space a gap at the end of the whole
	   space.  */
	ga->gap = ga->numels + ga->gapsize;
	ga->gapsize = increment;

	/* Move the new gap down to be consecutive with the end of the old one.
	   This adjusts the markers properly too.  */
	gap_array_move_gap(ga, real_gap_loc + old_gap_size);

	/* Now combine the two into one large gap.  */
	ga->gapsize += old_gap_size;
	ga->gap = real_gap_loc;
	return;
}

/* ------------------------------- */
/*        external functions       */
/* ------------------------------- */

/* Insert NUMELS elements (pointed to by ELPTR) into the specified
   gap array at POS. */

static void
gap_array_insert_els(gap_array_t ga, int pos, void *elptr, int numels)
{
	assert(pos >= 0 && pos <= ga->numels);
	if (ga->gapsize < numels) {
		gap_array_make_gap(ga, numels - ga->gapsize);
	}
	if (pos != ga->gap) {
		gap_array_move_gap(ga, pos);
	}
	memcpy(GAP_ARRAY_MEMEL_ADDR(ga, ga->gap), (char *)elptr,
	       numels * ga->elsize);
	ga->gapsize -= numels;
	ga->gap += numels;
	ga->numels += numels;
	/* This is the equivalent of insert-before-markers.

	   #### Should only happen if marker is "moves forward at insert" type.
	 */

	gap_array_adjust_markers(ga, pos - 1, pos, numels);
	return;
}

/* Delete NUMELS elements from the specified gap array, starting at FROM. */

static void
gap_array_delete_els(gap_array_t ga, int from, int numdel)
{
	int to = from + numdel;
	int gapsize = ga->gapsize;

	assert(from >= 0);
	assert(numdel >= 0);
	assert(to <= ga->numels);

	/* Make sure the gap is somewhere in or next to what we are deleting.  */
	if (to < ga->gap) {
		gap_array_move_gap(ga, to);
	}
	if (from > ga->gap) {
		gap_array_move_gap(ga, from);
	}
	/* Relocate all markers pointing into the new, larger gap
	   to point at the end of the text before the gap.  */
	gap_array_adjust_markers(ga, to + gapsize, to + gapsize,
				 -numdel - gapsize);

	ga->gapsize += numdel;
	ga->numels -= numdel;
	ga->gap = from;
	return;
}

static gap_array_marker_t
gap_array_make_marker(gap_array_t ga, int pos)
{
	gap_array_marker_t m;

	assert(pos >= 0 && pos <= ga->numels);
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	m = xnew(struct gap_array_marker_s);
#else  /* !BDWGC */
	if (gap_array_marker_freelist) {
		m = gap_array_marker_freelist;
		gap_array_marker_freelist = gap_array_marker_freelist->next;
	} else {
		m = xnew(struct gap_array_marker_s);
	}
#endif	/* BDWGC */

	m->pos = GAP_ARRAY_ARRAY_TO_MEMORY_POS(ga, pos);
	m->next = ga->markers;
	ga->markers = m;
	return m;
}

static void
gap_array_delete_marker(gap_array_t ga, gap_array_marker_t m)
{
	gap_array_marker_t p, prev;

	for (prev = 0, p = ga->markers; p && p != m; prev = p, p = p->next);
	if (UNLIKELY(p == NULL)) {
		return;
	}
	if (prev) {
		prev->next = p->next;
	} else {
		ga->markers = p->next;
	}
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	xfree(m);
#else  /* !BDWGC */
	m->next = gap_array_marker_freelist;
	m->pos = 0xDEADBEEF;
	gap_array_marker_freelist = m;
#endif	/* BDWGC */
	return;
}

static void
gap_array_delete_all_markers(gap_array_t ga)
{
	for (gap_array_marker_t p = ga->markers, next; p; p = next) {
		next = p->next;
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
		xfree(p);
#else  /* !BDWGC */
		p->next = gap_array_marker_freelist;
		p->pos = 0xDEADBEEF;
		gap_array_marker_freelist = p;
#endif	/* BDWGC */
	}
	ga->markers = NULL;
	return;
}

static void
gap_array_move_marker(gap_array_t ga, gap_array_marker_t m, int pos)
{
	assert(pos >= 0 && pos <= ga->numels);
	m->pos = GAP_ARRAY_ARRAY_TO_MEMORY_POS(ga, pos);
}

#define gap_array_marker_pos(ga, m)			\
	GAP_ARRAY_MEMORY_TO_ARRAY_POS (ga, (m)->pos)

static gap_array_t
make_gap_array(int elsize)
{
	gap_array_t ga = xnew_and_zero(struct gap_array_s);
	ga->elsize = elsize;
	return ga;
}

static void
free_gap_array(gap_array_t ga)
{
	if (ga->array) {
		xfree(ga->array);
	}
	gap_array_delete_all_markers(ga);
	xfree(ga);
	return;
}

/************************************************************************/
/*                       Extent list primitives                         */
/************************************************************************/

/* A list of extents is maintained as a double gap array: one gap array
   is ordered by start index (the "display order") and the other is
   ordered by end index (the "e-order").  Note that positions in an
   extent list should logically be conceived of as referring *to*
   a particular extent (as is the norm in programs) rather than
   sitting between two extents.  Note also that callers of these
   functions should not be aware of the fact that the extent list is
   implemented as an array, except for the fact that positions are
   integers (this should be generalized to handle integers and linked
   list equally well).
*/

/* Number of elements in an extent list */
#define extent_list_num_els(el) GAP_ARRAY_NUM_ELS(el->start)

/* Return the position at which EXTENT is located in the specified extent
   list (in the display order if ENDP is 0, in the e-order otherwise).
   If the extent is not found, the position where the extent would
   be inserted is returned.  If ENDP is 0, the insertion would go after
   all other equal extents.  If ENDP is not 0, the insertion would go
   before all other equal extents.  If FOUNDP is not 0, then whether
   the extent was found will get written into it. */

static int
extent_list_locate(extent_list_t el, EXTENT extent, int endp, bool *foundp)
{
	gap_array_t ga = endp ? el->end : el->start;
	int left = 0, right = GAP_ARRAY_NUM_ELS(ga);
	int oldfoundpos, foundpos;
	bool found;

	while (left != right) {
		/* RIGHT might not point to a valid extent (i.e. it's at the end
		   of the list), so NEWPOS must round down. */
		unsigned int newpos = (left + right) >> 1;
		EXTENT e = EXTENT_GAP_ARRAY_AT(ga, (int)newpos);

		if (endp ? EXTENT_E_LESS(e, extent) : EXTENT_LESS(e, extent)) {
			left = newpos + 1;
		} else {
			right = newpos;
		}
	}

	/* Now we're at the beginning of all equal extents. */
	found = false;
	oldfoundpos = foundpos = left;
	while (foundpos < GAP_ARRAY_NUM_ELS(ga)) {
		EXTENT e = EXTENT_GAP_ARRAY_AT(ga, foundpos);
		if (e == extent) {
			found = 1;
			break;
		}
		if (!EXTENT_EQUAL(e, extent)) {
			break;
		}
		foundpos++;
	}
	if (foundp) {
		*foundp = found;
	}
	if (found || !endp) {
		return foundpos;
	} else {
		return oldfoundpos;
	}
}

/* Return the position of the first extent that begins at or after POS
   (or ends at or after POS, if ENDP is not 0).

   An out-of-range value for POS is allowed, and guarantees that the
   position at the beginning or end of the extent list is returned. */

static int
extent_list_locate_from_pos(extent_list_t el, Memind pos, int endp)
{
	struct extent fake_extent;
	/*

	   Note that if we search for [POS, POS], then we get the following:

	   -- if ENDP is 0, then all extents whose start position is <= POS
	   lie before the returned position, and all extents whose start
	   position is > POS lie at or after the returned position.

	   -- if ENDP is not 0, then all extents whose end position is < POS
	   lie before the returned position, and all extents whose end
	   position is >= POS lie at or after the returned position.

	 */
	set_extent_start(&fake_extent, endp ? pos : pos - 1);
	set_extent_end(&fake_extent, endp ? pos : pos - 1);
	return extent_list_locate(el, &fake_extent, endp, 0);
}

/* Return the extent at POS. */

static EXTENT
extent_list_at(extent_list_t el, Memind pos, int endp)
{
	gap_array_t ga = endp ? el->end : el->start;

	assert(pos >= 0 && pos < GAP_ARRAY_NUM_ELS(ga));
	return EXTENT_GAP_ARRAY_AT(ga, pos);
}

/* Insert an extent into an extent list. */

static void
extent_list_insert(extent_list_t el, EXTENT extent)
{
	int pos;
	bool foundp;

	pos = extent_list_locate(el, extent, 0, &foundp);
	assert(!foundp);
	gap_array_insert_els(el->start, pos, &extent, 1);
	pos = extent_list_locate(el, extent, 1, &foundp);
	assert(!foundp);
	gap_array_insert_els(el->end, pos, &extent, 1);
	return;
}

/* Delete an extent from an extent list. */

static void
extent_list_delete(extent_list_t el, EXTENT extent)
{
	int pos;
	bool foundp;

	pos = extent_list_locate(el, extent, 0, &foundp);
	assert(foundp);
	gap_array_delete_els(el->start, pos, 1);
	pos = extent_list_locate(el, extent, 1, &foundp);
	assert(foundp);
	gap_array_delete_els(el->end, pos, 1);
	return;
}

static void
extent_list_delete_all(extent_list_t el)
{
	gap_array_delete_els(el->start, 0, GAP_ARRAY_NUM_ELS(el->start));
	gap_array_delete_els(el->end, 0, GAP_ARRAY_NUM_ELS(el->end));
	return;
}

static extent_list_marker_t
extent_list_make_marker(extent_list_t el, int pos, int endp)
{
	extent_list_marker_t m;

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	m = xnew(struct extent_list_marker_s);
#else  /* !BDWGC */
	if (extent_list_marker_freelist) {
		m = extent_list_marker_freelist;
		extent_list_marker_freelist = extent_list_marker_freelist->next;
	} else {
		m = xnew(struct extent_list_marker_s);
	}
#endif	/* BDWGC */

	m->m = gap_array_make_marker(endp ? el->end : el->start, pos);
	m->endp = endp;
	m->next = el->markers;
	el->markers = m;
	return m;
}

#define extent_list_move_marker(el, mkr, pos)				\
	gap_array_move_marker((mkr)->endp				\
			      ? (el)->end				\
			      : (el)->start, (mkr)->m, pos)

static void
extent_list_delete_marker(extent_list_t el, extent_list_marker_t m)
{
	extent_list_marker_t p, prev;

	for (prev = 0, p = el->markers; p && p != m; prev = p, p = p->next);
	if( !p ) {
		abort();
		return;
	}

	if (prev) {
		prev->next = p->next;
	} else {
		el->markers = p->next;
	}
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	xfree(m);
#else  /* !BDWGC */
	m->next = extent_list_marker_freelist;
	extent_list_marker_freelist = m;
#endif	/* BDWGC */
	gap_array_delete_marker(m->endp ? el->end : el->start, m->m);
	return;
}

#define extent_list_marker_pos(el, mkr)					\
	gap_array_marker_pos ((mkr)->endp				\
			      ? (el)->end				\
			      : (el)->start, (mkr)->m)

static extent_list_t
allocate_extent_list(void)
{
	extent_list_t el = xnew(struct extent_list_s);
	el->start = make_gap_array(sizeof(EXTENT));
	el->end = make_gap_array(sizeof(EXTENT));
	el->markers = 0;
	return el;
}

static void
free_extent_list(extent_list_t el)
{
	free_gap_array(el->start);
	free_gap_array(el->end);
	xfree(el);
	return;
}

/************************************************************************/
/*                       Auxiliary extent structure                     */
/************************************************************************/

static Lisp_Object mark_extent_auxiliary(Lisp_Object obj)
{
	struct extent_auxiliary *data = XEXTENT_AUXILIARY(obj);
	mark_object(data->begin_glyph);
	mark_object(data->end_glyph);
	mark_object(data->invisible);
	mark_object(data->children);
	mark_object(data->read_only);
	mark_object(data->mouse_face);
	mark_object(data->initial_redisplay_function);
	mark_object(data->before_change_functions);
	mark_object(data->after_change_functions);
	return data->parent;
}

DEFINE_LRECORD_IMPLEMENTATION("extent-auxiliary", extent_auxiliary,
			      mark_extent_auxiliary, internal_object_printer,
			      0, 0, 0, 0, struct extent_auxiliary);

void allocate_extent_auxiliary(EXTENT ext)
{
	Lisp_Object extent_aux;
	struct extent_auxiliary *data =
		alloc_lcrecord_type(struct extent_auxiliary,
				    &lrecord_extent_auxiliary);

	copy_lcrecord(data, &extent_auxiliary_defaults);
	XSETEXTENT_AUXILIARY(extent_aux, data);
	ext->plist = Fcons(extent_aux, ext->plist);
	ext->flags.has_aux = 1;
	return;
}

/************************************************************************/
/*                         Extent info structure                        */
/************************************************************************/

/* An extent-info structure consists of a list of the buffer or string's
   extents and a "stack of extents" that lists all of the extents over
   a particular position.  The stack-of-extents info is used for
   optimization purposes -- it basically caches some info that might
   be expensive to compute.  Certain otherwise hard computations are easy
   given the stack of extents over a particular position, and if the
   stack of extents over a nearby position is known (because it was
   calculated at some prior point in time), it's easy to move the stack
   of extents to the proper position.

   Given that the stack of extents is an optimization, and given that
   it requires memory, a string's stack of extents is wiped out each
   time a garbage collection occurs.  Therefore, any time you retrieve
   the stack of extents, it might not be there.  If you need it to
   be there, use the _force version.

   Similarly, a string may or may not have an extent_info structure.
   (Generally it won't if there haven't been any extents added to the
   string.) So use the _force version if you need the extent_info
   structure to be there. */

static extent_stack_t allocate_soe(void);
static void free_soe(extent_stack_t);
static void soe_invalidate(Lisp_Object obj);

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
static Lisp_Object
mark_extent_info(Lisp_Object obj)
{
	struct extent_info *data = (struct extent_info *)XEXTENT_INFO(obj);
	int i;
	extent_list_t list = data->extents;

	/* Vbuffer_defaults and Vbuffer_local_symbols are buffer-like
	   objects that are created specially and never have their extent
	   list initialized (or rather, it is set to zero in
	   nuke_all_buffer_slots()).  However, these objects get
	   garbage-collected so we have to deal.

	   (Also the list can be zero when we're dealing with a destroyed
	   buffer.) */

	if (list) {
		for (i = 0; i < extent_list_num_els(list); i++) {
			struct extent *extent = extent_list_at(list, i, 0);
			Lisp_Object exobj;

			XSETEXTENT(exobj, extent);
			mark_object(exobj);
		}
	}

	return Qnil;
}

static void
finalize_extent_info(void *header, int for_disksave)
{
	struct extent_info *data = (struct extent_info *)header;

	if (for_disksave)
		return;

	if (data->soe) {
		free_soe(data->soe);
		data->soe = 0;
	}
	if (data->extents) {
		free_extent_list(data->extents);
		data->extents = 0;
	}
}
#else  /* BDWGC */
/* just define dummies */
static Lisp_Object
mark_extent_info(Lisp_Object SXE_UNUSED(obj))
{
	return Qnil;
}

static void
finalize_extent_info(void *SXE_UNUSED(header), int SXE_UNUSED(for_disksave))
{
	return;
}
#endif	/* !BDWGC */

DEFINE_LRECORD_IMPLEMENTATION("extent-info", extent_info,
			      mark_extent_info, internal_object_printer,
			      finalize_extent_info, 0, 0, 0,
			      struct extent_info);

static Lisp_Object
allocate_extent_info(void)
{
	Lisp_Object extent_info;
	struct extent_info *data =
		alloc_lcrecord_type(struct extent_info, &lrecord_extent_info);

	XSETEXTENT_INFO(extent_info, data);
	data->extents = allocate_extent_list();
	data->soe = 0;
	return extent_info;
}

void
flush_cached_extent_info(Lisp_Object extent_info)
{
	struct extent_info *data = XEXTENT_INFO(extent_info);

	if (data->soe) {
		free_soe(data->soe);
		data->soe = 0;
	}
}

/************************************************************************/
/*                    Buffer/string extent primitives                   */
/************************************************************************/

/* The functions in this section are the ONLY ones that should know
   about the internal implementation of the extent lists.  Other functions
   should only know that there are two orderings on extents, the "display"
   order (sorted by start position, basically) and the e-order (sorted
   by end position, basically), and that certain operations are provided
   to manipulate the list. */

/* ------------------------------- */
/*        basic primitives         */
/* ------------------------------- */

static Lisp_Object
decode_buffer_or_string(Lisp_Object object)
{
	if (LIKELY(NILP(object))) {
		XSETBUFFER(object, current_buffer);
	} else if (BUFFERP(object)) {
		CHECK_LIVE_BUFFER(object);
	} else if (STRINGP(object)) {
		;
	} else {
		dead_wrong_type_argument(Qbuffer_or_string_p, object);
	}
	return object;
}

EXTENT extent_ancestor_1(EXTENT e)
{
	while (e->flags.has_parent) {
		/* There should be no circularities except in case of a logic
		   error somewhere in the extent code */
		e = XEXTENT(XEXTENT_AUXILIARY(XCAR(e->plist))->parent);
	}
	return e;
}

/* Given an extent object (string or buffer or nil), return its extent info.
   This may be 0 for a string. */

static struct extent_info*
buffer_or_string_extent_info(Lisp_Object object)
{
	if (STRINGP(object)) {
		Lisp_Object plist = XSTRING(object)->plist;
		if (!CONSP(plist) || !EXTENT_INFOP(XCAR(plist))) {
			return NULL;
		}
		return XEXTENT_INFO(XCAR(plist));
	} else if (NILP(object)) {
		return NULL;
	} else {
		return XEXTENT_INFO(XBUFFER(object)->extent_info);
	}
}

/* Given a string or buffer, return its extent list.  This may be
   0 for a string. */

static extent_list_t
buffer_or_string_extent_list(Lisp_Object object)
{
	struct extent_info *info = buffer_or_string_extent_info(object);

	if (!info) {
		return 0;
	}
	return info->extents;
}

/* Given a string or buffer, return its extent info.  If it's not there,
   create it. */

static struct extent_info*
buffer_or_string_extent_info_force(Lisp_Object object)
{
	struct extent_info *info = buffer_or_string_extent_info(object);

	if (!info) {
		Lisp_Object extent_info;

		/* should never happen for buffers --
		   the only buffers without an extent
		   info are those after finalization,
		   destroyed buffers, or special
		   Lisp-inaccessible buffer objects. */
		assert(STRINGP(object));

		extent_info = allocate_extent_info();
		XSTRING(object)->plist =
			Fcons(extent_info, XSTRING(object)->plist);
		return XEXTENT_INFO(extent_info);
	}
	return info;
}

/* Detach all the extents in OBJECT.  Called from redisplay. */

void
detach_all_extents(Lisp_Object object)
{
	struct extent_info *data = buffer_or_string_extent_info(object);

	if (data) {
		if (data->extents) {
			for (int i = 0;
			     i < extent_list_num_els(data->extents);
			     i++) {
				EXTENT e = extent_list_at(data->extents, i, 0);
				/* No need to do detach_extent().  Just nuke the
				   damn things, which results in the equivalent
				   but faster. */
				set_extent_start(e, -1);
				set_extent_end(e, -1);
			}
			/* But we need to clear all the lists containing extents
			   or havoc will result. */
			extent_list_delete_all(data->extents);
		}
		soe_invalidate(object);
	}
	return;
}

void
init_buffer_extents(struct buffer *b)
{
	b->extent_info = allocate_extent_info();
	return;
}

void
uninit_buffer_extents(struct buffer *b)
{
	struct extent_info *data = XEXTENT_INFO(b->extent_info);

	/* Don't destroy the extents here -- there may still be children
	   extents pointing to the extents. */
	detach_all_extents(make_buffer(b));
	finalize_extent_info(data, 0);
	return;
}

/* Retrieve the extent list that an extent is a member of; the
   return value will never be 0 except in destroyed buffers (in which
   case the only extents that can refer to this buffer are detached
   ones). */

#define extent_extent_list(e) buffer_or_string_extent_list (extent_object (e))

/* ------------------------------- */
/*        stack of extents         */
/* ------------------------------- */

#ifdef ERROR_CHECK_EXTENTS

void
sledgehammer_extent_check(Lisp_Object object)
{
	extent_list_t el = buffer_or_string_extent_list(object);
	struct buffer *buf = 0;

	if (!el) {
		return;
	}
	if (BUFFERP(object)) {
		buf = XBUFFER(object);
	}
	for (int endp = 0; endp < 2; endp++) {
		for (int i = 1; i < extent_list_num_els(el); i++) {
			EXTENT e1 = extent_list_at(el, i - 1, endp);
			EXTENT e2 = extent_list_at(el, i, endp);
			if (buf) {
				assert(extent_start(e1) <= buf->text->gpt ||
				       extent_start(e1) >
				       buf->text->gpt + buf->text->gap_size);
				assert(extent_end(e1) <= buf->text->gpt
				       || extent_end(e1) >
				       buf->text->gpt + buf->text->gap_size);
			}
			assert(extent_start(e1) <= extent_end(e1));
			assert(endp
			       ? (EXTENT_E_LESS_EQUAL(e1, e2))
			       : (EXTENT_LESS_EQUAL(e1, e2)));
		}
	}
}

#endif	/* ERROR_CHECK_EXTENTS */

static extent_stack_t
buffer_or_string_stack_of_extents(Lisp_Object object)
{
	struct extent_info *info = buffer_or_string_extent_info(object);
	if (!info) {
		return NULL;
	}
	return info->soe;
}

static extent_stack_t
buffer_or_string_stack_of_extents_force(Lisp_Object object)
{
	struct extent_info *info = buffer_or_string_extent_info_force(object);
	if (!info->soe) {
		info->soe = allocate_soe();
	}
	return info->soe;
}

/* #define SOE_DEBUG */

#ifdef SOE_DEBUG

static void print_extent_1(char *buf, Lisp_Object extent);

static void
print_extent_2(EXTENT e)
{
	Lisp_Object extent;
	char buf[200];

	XSETEXTENT(extent, e);
	print_extent_1(buf, extent);
	fputs(buf, stdout);
}

static void
soe_dump(Lisp_Object obj)
{
	int i;
	extent_stack_t soe = buffer_or_string_stack_of_extents(obj);
	extent_list_t sel;
	int endp;

	if (!soe) {
		printf("No SOE");
		return;
	}
	sel = soe->extents;
	printf("SOE pos is %d (memind %d)\n",
	       soe->pos < 0 ? soe->pos :
	       buffer_or_string_memind_to_bytind(obj, soe->pos), soe->pos);
	for (endp = 0; endp < 2; endp++) {
		printf(endp ? "SOE end:" : "SOE start:");
		for (i = 0; i < extent_list_num_els(sel); i++) {
			EXTENT e = extent_list_at(sel, i, endp);
			putchar('\t');
			print_extent_2(e);
		}
		putchar('\n');
	}
	putchar('\n');
}

#endif	/* SOE_DEBUG */

/* Insert EXTENT into OBJ's stack of extents, if necessary. */

static void
soe_insert(Lisp_Object obj, EXTENT extent)
{
	extent_stack_t soe = buffer_or_string_stack_of_extents(obj);

#ifdef SOE_DEBUG
	printf("Inserting into SOE: ");
	print_extent_2(extent);
	putchar('\n');
#endif
	if (!soe || soe->pos < extent_start(extent) ||
	    soe->pos > extent_end(extent)) {
#ifdef SOE_DEBUG
		printf("(not needed)\n\n");
#endif
		return;
	}
	extent_list_insert(soe->extents, extent);
#ifdef SOE_DEBUG
	puts("SOE afterwards is:");
	soe_dump(obj);
#endif
	return;
}

/* Delete EXTENT from OBJ's stack of extents, if necessary. */

static void
soe_delete(Lisp_Object obj, EXTENT extent)
{
	extent_stack_t soe = buffer_or_string_stack_of_extents(obj);

#ifdef SOE_DEBUG
	printf("Deleting from SOE: ");
	print_extent_2(extent);
	putchar('\n');
#endif
	if (!soe || soe->pos < extent_start(extent) ||
	    soe->pos > extent_end(extent)) {
#ifdef SOE_DEBUG
		puts("(not needed)\n");
#endif
		return;
	}
	extent_list_delete(soe->extents, extent);
#ifdef SOE_DEBUG
	puts("SOE afterwards is:");
	soe_dump(obj);
#endif
	return;
}

/* Move OBJ's stack of extents to lie over the specified position. */

static void
soe_move(Lisp_Object obj, Memind pos)
{
	extent_stack_t soe = buffer_or_string_stack_of_extents_force(obj);
	extent_list_t sel = soe->extents;
	int numsoe = extent_list_num_els(sel);
	extent_list_t bel = buffer_or_string_extent_list(obj);
	int direction;
	int endp;

#ifdef ERROR_CHECK_EXTENTS
	assert(bel);
#endif

#ifdef SOE_DEBUG
	printf("Moving SOE from %d (memind %d) to %d (memind %d)\n",
	       soe->pos < 0 ? soe->pos :
	       buffer_or_string_memind_to_bytind(obj, soe->pos), soe->pos,
	       buffer_or_string_memind_to_bytind(obj, pos), pos);
#endif
	if (soe->pos < pos) {
		direction = 1;
		endp = 0;
	} else if (soe->pos > pos) {
		direction = -1;
		endp = 1;
	} else {
#ifdef SOE_DEBUG
		puts("(not needed)\n");
#endif
		return;
	}

	/* For DIRECTION = 1: Any extent that overlaps POS is either in the
	   SOE (if the extent starts at or before SOE->POS) or is greater
	   (in the display order) than any extent in the SOE (if it starts
	   after SOE->POS).

	   For DIRECTION = -1: Any extent that overlaps POS is either in the
	   SOE (if the extent ends at or after SOE->POS) or is less (in the
	   e-order) than any extent in the SOE (if it ends before SOE->POS).

	   We proceed in two stages:

	   1) delete all extents in the SOE that don't overlap POS.
	   2) insert all extents into the SOE that start (or end, when
	   DIRECTION = -1) in (SOE->POS, POS] and that overlap
	   POS. (Don't include SOE->POS in the range because those
	   extents would already be in the SOE.)
	 */

	/* STAGE 1. */

	if (numsoe > 0) {
		/* Delete all extents in the SOE that don't overlap POS.
		   This is all extents that end before (or start after,
		   if DIRECTION = -1) POS.
		 */

		/* Deleting extents from the SOE is tricky because it changes
		   the positions of extents.  If we are deleting in the forward
		   direction we have to call extent_list_at() on the same position
		   over and over again because positions after the deleted element
		   get shifted back by 1.  To make life simplest, we delete forward
		   irrespective of DIRECTION.
		 */
		int start, end;
		int i;

		if (direction > 0) {
			start = 0;
			end = extent_list_locate_from_pos(sel, pos, 1);
		} else {
			start = extent_list_locate_from_pos(sel, pos + 1, 0);
			end = numsoe;
		}

		for (i = start; i < end; i++) {
			extent_list_delete(
				sel, extent_list_at(sel, start, !endp));
		}
	}

	/* STAGE 2. */

	{
		int start_pos;

		if (direction < 0) {
			start_pos =
				extent_list_locate_from_pos(
					bel, soe->pos, endp) - 1;
		} else {
			start_pos =
				extent_list_locate_from_pos(
					bel, soe->pos + 1, endp);
		}

		for (; start_pos >= 0 && start_pos < extent_list_num_els(bel);
		     start_pos += direction) {
			EXTENT e = extent_list_at(bel, start_pos, endp);
			if ((direction > 0)
			    ? (extent_start(e) > pos)
			    : (extent_end(e) < pos)) {
				/* All further extents lie on the far side of
				   POS and thus can't overlap. */
				break;
			}
			if ((direction > 0)
			    ? (extent_end(e) >= pos)
			    : (extent_start(e) <= pos)) {
				extent_list_insert(sel, e);
			}
		}
	}

	soe->pos = pos;
#ifdef SOE_DEBUG
	puts("SOE afterwards is:");
	soe_dump(obj);
#endif
	return;
}

static void
soe_invalidate(Lisp_Object obj)
{
	extent_stack_t soe = buffer_or_string_stack_of_extents(obj);

	if (soe) {
		extent_list_delete_all(soe->extents);
		soe->pos = -1;
	}
	return;
}

static extent_stack_t
allocate_soe(void)
{
	extent_stack_t soe = xnew_and_zero(struct extent_stack_s);
	soe->extents = allocate_extent_list();
	soe->pos = -1;
	return soe;
}

static void
free_soe(extent_stack_t soe)
{
	free_extent_list(soe->extents);
	xfree(soe);
	return;
}

/* ------------------------------- */
/*        other primitives         */
/* ------------------------------- */

/* Return the start (endp == 0) or end (endp == 1) of an extent as
   a byte index.  If you want the value as a memory index, use
   extent_endpoint().  If you want the value as a buffer position,
   use extent_endpoint_bufpos(). */

static Bytind extent_endpoint_bytind(EXTENT extent, int endp)
{
	assert(EXTENT_LIVE_P(extent));
	assert(!extent_detached_p(extent));
	{
		Memind i = endp ? extent_end(extent) : extent_start(extent);
		Lisp_Object obj = extent_object(extent);
		return buffer_or_string_memind_to_bytind(obj, i);
	}
}

static Bufpos extent_endpoint_bufpos(EXTENT extent, int endp)
{
	assert(EXTENT_LIVE_P(extent));
	assert(!extent_detached_p(extent));
	{
		Memind i = endp ? extent_end(extent) : extent_start(extent);
		Lisp_Object obj = extent_object(extent);
		return buffer_or_string_memind_to_bufpos(obj, i);
	}
}

/* A change to an extent occurred that will change the display, so
   notify redisplay.  Maybe also recurse over all the extent's
   descendants. */

static void
extent_changed_for_redisplay(EXTENT extent, int descendants_too,
			     int invisibility_change)
{
	Lisp_Object object;
	Lisp_Object rest;

	/* we could easily encounter a detached extent while traversing the
	   children, but we should never be able to encounter a dead extent. */
	assert(EXTENT_LIVE_P(extent));

	if (descendants_too) {
		Lisp_Object children = extent_children(extent);

		if (!NILP(children)) {
			/* first mark all of the extent's children.  We will
			   lose big-time if there are any circularities here, so
			   we sure as hell better ensure that there aren't. */
			LIST_LOOP(rest, XWEAK_LIST_LIST(children)) {
				extent_changed_for_redisplay(
					XEXTENT(XCAR(rest)), 1,
					invisibility_change);
			}
		}
	}

	/* now mark the extent itself. */

	object = extent_object(extent);

	if (extent_detached_p(extent)) {
		return;

	} else if (STRINGP(object)) {
		/* #### Changes to string extents can affect redisplay if they
		   are in the modeline or in the gutters.

		   If the extent is in some generated-modeline-string: when we
		   change an extent in generated-modeline-string, this changes
		   its parent, which is in `modeline-format', so we should force
		   the modeline to be updated.  But how to determine whether a
		   string is a `generated-modeline-string'?  Looping through all
		   buffers is not very efficient.  Should we add all
		   `generated-modeline-string' strings to a hash table?  Maybe
		   efficiency is not the greatest concern here and there's no
		   big loss in looping over the buffers.

		   If the extent is in a gutter we mark the gutter as
		   changed. This means (a) we can update extents in the gutters
		   when we need it. (b) we don't have to update the gutters when
		   only extents attached to buffers have changed. */

		if (!in_modeline_generation) {
			MARK_EXTENTS_CHANGED;
		}
		gutter_extent_signal_changed_region_maybe(
			object,
			extent_endpoint_bufpos(extent, 0),
			extent_endpoint_bufpos(extent, 1));

	} else if (BUFFERP(object)) {
		struct buffer *b;
		b = XBUFFER(object);
		BUF_FACECHANGE(b)++;
		MARK_EXTENTS_CHANGED;
		if (invisibility_change) {
			MARK_CLIP_CHANGED;
		}
		buffer_extent_signal_changed_region(
			b,
			extent_endpoint_bufpos(extent, 0),
			extent_endpoint_bufpos(extent, 1));
	}
}

/* A change to an extent occurred that might affect redisplay.
   This is called when properties such as the endpoints, the layout,
   or the priority changes.  Redisplay will be affected only if
   the extent has any displayable attributes. */

static void
extent_maybe_changed_for_redisplay(EXTENT extent, int descendants_too,
				   int invisibility_change)
{
	/* Retrieve the ancestor for efficiency */
	EXTENT anc = extent_ancestor(extent);
	if (!NILP(extent_face(anc)) ||
	    !NILP(extent_begin_glyph(anc)) ||
	    !NILP(extent_end_glyph(anc)) ||
	    !NILP(extent_mouse_face(anc)) ||
	    !NILP(extent_invisible(anc)) ||
	    !NILP(extent_initial_redisplay_function(anc)) ||
	    invisibility_change)
		extent_changed_for_redisplay(extent, descendants_too,
					     invisibility_change);
}

static EXTENT
make_extent_detached(Lisp_Object object)
{
	EXTENT extent = allocate_extent();

	assert(NILP(object) || STRINGP(object) ||
	       (BUFFERP(object) && BUFFER_LIVE_P(XBUFFER(object))));
	extent_object(extent) = object;
	/* Now make sure the extent info exists. */
	if (!NILP(object)) {
		buffer_or_string_extent_info_force(object);
	}
	return extent;
}

/* A "real" extent is any extent other than the internal (not-user-visible)
   extents used by `map-extents'. */

static EXTENT
real_extent_at_forward(extent_list_t el, int pos, int endp)
{
	for (; pos < extent_list_num_els(el); pos++) {
		EXTENT e = extent_list_at(el, pos, endp);
		if (!extent_internal_p(e)) {
			return e;
		}
	}
	return NULL;
}

static EXTENT
real_extent_at_backward(extent_list_t el, int pos, int endp)
{
	for (; pos >= 0; pos--) {
		EXTENT e = extent_list_at(el, pos, endp);
		if (!extent_internal_p(e)) {
			return e;
		}
	}
	return NULL;
}

static EXTENT
extent_first(Lisp_Object obj)
{
	extent_list_t el = buffer_or_string_extent_list(obj);

	if (!el) {
		return NULL;
	}
	return real_extent_at_forward(el, 0, 0);
}

#ifdef DEBUG_SXEMACS
static EXTENT
extent_e_first(Lisp_Object obj)
{
	extent_list_t el = buffer_or_string_extent_list(obj);

	if (!el) {
		return 0;
	}
	return real_extent_at_forward(el, 0, 1);
}
#endif	/* DEBUG_SXEMACS */

static EXTENT
extent_next(EXTENT e)
{
	extent_list_t el = extent_extent_list(e);
	bool foundp;
	int pos = extent_list_locate(el, e, 0, &foundp);
	assert(foundp);
	return real_extent_at_forward(el, pos + 1, 0);
}

#ifdef DEBUG_SXEMACS
static EXTENT
extent_e_next(EXTENT e)
{
	extent_list_t el = extent_extent_list(e);
	bool foundp;
	int pos = extent_list_locate(el, e, 1, &foundp);
	assert(foundp);
	return real_extent_at_forward(el, pos + 1, 1);
}
#endif	/* DEBUG_SXEMACS */

static EXTENT
extent_last(Lisp_Object obj)
{
	extent_list_t el = buffer_or_string_extent_list(obj);

	if (!el) {
		return 0;
	}
	return real_extent_at_backward(el, extent_list_num_els(el) - 1, 0);
}

#ifdef DEBUG_SXEMACS
static EXTENT
extent_e_last(Lisp_Object obj)
{
	extent_list_t el = buffer_or_string_extent_list(obj);

	if (!el) {
		return 0;
	}
	return real_extent_at_backward(el, extent_list_num_els(el) - 1, 1);
}
#endif	/* DEBUG_SXEMACS */

static EXTENT
extent_previous(EXTENT e)
{
	extent_list_t el = extent_extent_list(e);
	bool foundp;
	int pos = extent_list_locate(el, e, 0, &foundp);
	assert(foundp);
	return real_extent_at_backward(el, pos - 1, 0);
}

#ifdef DEBUG_SXEMACS
static EXTENT
extent_e_previous(EXTENT e)
{
	extent_list_t el = extent_extent_list(e);
	bool foundp;
	int pos = extent_list_locate(el, e, 1, &foundp);
	assert(foundp);
	return real_extent_at_backward(el, pos - 1, 1);
}
#endif	/* DEBUG_SXEMACS */

static void
extent_attach(EXTENT extent)
{
	extent_list_t el = extent_extent_list(extent);

	extent_list_insert(el, extent);
	soe_insert(extent_object(extent), extent);
	/* only this extent changed */
	extent_maybe_changed_for_redisplay(
		extent, 0, !NILP(extent_invisible(extent)));
	return;
}

static void
extent_detach(EXTENT extent)
{
	extent_list_t el;

	if (extent_detached_p(extent)) {
		return;
	}
	el = extent_extent_list(extent);

	/* call this before messing with the extent. */
	extent_maybe_changed_for_redisplay(
		extent, 0, !NILP(extent_invisible(extent)));
	extent_list_delete(el, extent);
	soe_delete(extent_object(extent), extent);
	set_extent_start(extent, -1);
	set_extent_end(extent, -1);
	return;
}

/* ------------------------------- */
/*        map-extents et al.       */
/* ------------------------------- */

/* Returns true iff map_extents() would visit the given extent.
   See the comments at map_extents() for info on the overlap rule.
   Assumes that all validation on the extent and buffer positions has
   already been performed (see Fextent_in_region_p ()).
 */
static bool
extent_in_region_p(EXTENT extent, Bytind from, Bytind to, unsigned int flags)
{
	Lisp_Object obj = extent_object(extent);
	Endpoint_Index start, end, exs, exe;
	int start_open, end_open;
	unsigned int all_extents_flags = flags & ME_ALL_EXTENTS_MASK;
	unsigned int in_region_flags = flags & ME_IN_REGION_MASK;
	int retval;

	/* A zero-length region is treated as closed-closed. */
	if (from == to) {
		flags |= ME_END_CLOSED;
		flags &= ~ME_START_OPEN;
	}

	/* So is a zero-length extent. */
	if (extent_start(extent) == extent_end(extent)) {
		start_open = 0, end_open = 0;
	} else if (LIKELY(all_extents_flags == 0)) {
		/* `all_extents_flags' will almost always be zero. */
		start_open = extent_start_open_p(extent);
		end_open = extent_end_open_p(extent);
	} else {
		switch (all_extents_flags) {
		case ME_ALL_EXTENTS_CLOSED:
			start_open = 0, end_open = 0;
			break;
		case ME_ALL_EXTENTS_OPEN:
			start_open = 1, end_open = 1;
			break;
		case ME_ALL_EXTENTS_CLOSED_OPEN:
			start_open = 0, end_open = 1;
			break;
		case ME_ALL_EXTENTS_OPEN_CLOSED:
			start_open = 1, end_open = 0;
			break;
		default:
			abort();
			return false;
		}
	}
	start = buffer_or_string_bytind_to_startind(obj, from,
						    flags & ME_START_OPEN);
	end = buffer_or_string_bytind_to_endind(obj, to,
						!(flags & ME_END_CLOSED));
	exs = memind_to_startind(extent_start(extent), start_open);
	exe = memind_to_endind(extent_end(extent), end_open);

	/* It's easy to determine whether an extent lies *outside* the
	   region -- just determine whether it's completely before
	   or completely after the region.  Reject all such extents, so
	   we're now left with only the extents that overlap the region.
	 */

	if (exs > end || exe < start) {
		return false;
	}
	/* See if any further restrictions are called for. */
	/* in_region_flags will almost always be zero. */
	if (in_region_flags == 0) {
		retval = 1;
	} else {
		switch (in_region_flags) {
		case ME_START_IN_REGION:
			retval = start <= exs && exs <= end;
			break;
		case ME_END_IN_REGION:
			retval = start <= exe && exe <= end;
			break;
		case ME_START_AND_END_IN_REGION:
			retval = start <= exs && exe <= end;
			break;
		case ME_START_OR_END_IN_REGION:
			retval = (start <= exs && exs <= end) ||
				(start <= exe && exe <= end);
			break;
		default:
			abort();
			return false;
		}
	}
	return flags & ME_NEGATE_IN_REGION ? !retval : retval;
}

struct map_extents_struct {
	extent_list_t el;
	extent_list_marker_t mkr;
	EXTENT range;
};

static Lisp_Object
map_extents_unwind(Lisp_Object obj)
{
	struct map_extents_struct *closure =
		(struct map_extents_struct *)get_opaque_ptr(obj);
	free_opaque_ptr(obj);
	if (closure->range) {
		extent_detach(closure->range);
	}
	if (closure->mkr) {
		extent_list_delete_marker(closure->el, closure->mkr);
	}
	return Qnil;
}

/* This is the guts of `map-extents' and the other functions that
   map over extents.  In theory the operation of this function is
   simple: just figure out what extents we're mapping over, and
   call the function on each one of them in the range.  Unfortunately
   there are a wide variety of things that the mapping function
   might do, and we have to be very tricky to avoid getting messed
   up.  Furthermore, this function needs to be very fast (it is
   called multiple times every time text is inserted or deleted
   from a buffer), and so we can't always afford the overhead of
   dealing with all the possible things that the mapping function
   might do; thus, there are many flags that can be specified
   indicating what the mapping function might or might not do.

   The result of all this is that this is the most complicated
   function in this file.  Change it at your own risk!

   A potential simplification to the logic below is to determine
   all the extents that the mapping function should be called on
   before any calls are actually made and save them in an array.
   That introduces its own complications, however (the array
   needs to be marked for garbage-collection, and a static array
   cannot be used because map_extents() needs to be reentrant).
   Furthermore, the results might be a little less sensible than
   the logic below. */

static void
map_extents_bytind(Bytind from, Bytind to, map_extents_fun fn, void *arg,
		   Lisp_Object obj, EXTENT after, unsigned int flags)
{
	Memind st, en;		/* range we're mapping over */
	EXTENT range = 0;	/* extent for this, if ME_MIGHT_MODIFY_TEXT */
	extent_list_t el = 0;	/* extent list we're iterating over */
	extent_list_marker_t posm = 0;	/* marker for extent list,
					   if ME_MIGHT_MODIFY_EXTENTS */
	/* count and struct for unwind-protect, if ME_MIGHT_THROW */
	int count = 0;
	struct map_extents_struct closure;

#ifdef ERROR_CHECK_EXTENTS
	assert(from <= to);
	assert(from >= buffer_or_string_absolute_begin_byte(obj) &&
	       from <= buffer_or_string_absolute_end_byte(obj) &&
	       to >= buffer_or_string_absolute_begin_byte(obj) &&
	       to <= buffer_or_string_absolute_end_byte(obj));
#endif

	if (after) {
		assert(EQ(obj, extent_object(after)));
		assert(!extent_detached_p(after));
	}

	el = buffer_or_string_extent_list(obj);
	if (!el || !extent_list_num_els(el))
		return;
	el = 0;

	st = buffer_or_string_bytind_to_memind(obj, from);
	en = buffer_or_string_bytind_to_memind(obj, to);

	if (flags & ME_MIGHT_MODIFY_TEXT) {
		/* The mapping function might change the text in the buffer,
		   so make an internal extent to hold the range we're mapping
		   over. */
		range = make_extent_detached(obj);
		set_extent_start(range, st);
		set_extent_end(range, en);
		range->flags.start_open = flags & ME_START_OPEN;
		range->flags.end_open = !(flags & ME_END_CLOSED);
		range->flags.internal = 1;
		range->flags.detachable = 0;
		extent_attach(range);
	}

	if (flags & ME_MIGHT_THROW) {
		/* The mapping function might throw past us so we need to use an
		   unwind_protect() to eliminate the internal extent and range
		   that we use. */
		count = specpdl_depth();
		closure.range = range;
		closure.mkr = 0;
		record_unwind_protect(map_extents_unwind,
				      make_opaque_ptr(&closure));
	}

	/* ---------- Figure out where we start and what direction
	   we move in.  This is the trickiest part of this
	   function. ---------- */

	/* If ME_START_IN_REGION, ME_END_IN_REGION or ME_START_AND_END_IN_REGION
	   was specified and ME_NEGATE_IN_REGION was not specified, our job
	   is simple because of the presence of the display order and e-order.
	   (Note that theoretically do something similar for
	   ME_START_OR_END_IN_REGION, but that would require more trickiness
	   than it's worth to avoid hitting the same extent twice.)

	   In the general case, all the extents that overlap a range can be
	   divided into two classes: those whose start position lies within
	   the range (including the range's end but not including the
	   range's start), and those that overlap the start position,
	   i.e. those in the SOE for the start position.  Or equivalently,
	   the extents can be divided into those whose end position lies
	   within the range and those in the SOE for the end position.  Note
	   that for this purpose we treat both the range and all extents in
	   the buffer as closed on both ends.  If this is not what the ME_
	   flags specified, then we've mapped over a few too many extents,
	   but no big deal because extent_in_region_p() will filter them
	   out.   Ideally, we could move the SOE to the closer of the range's
	   two ends and work forwards or backwards from there.  However, in
	   order to make the semantics of the AFTER argument work out, we
	   have to always go in the same direction; so we choose to always
	   move the SOE to the start position.

	   When it comes time to do the SOE stage, we first call soe_move()
	   so that the SOE gets set up.  Note that the SOE might get
	   changed while we are mapping over its contents.  If we can
	   guarantee that the SOE won't get moved to a new position, we
	   simply need to put a marker in the SOE and we will track deletions
	   and insertions of extents in the SOE.  If the SOE might get moved,
	   however (this would happen as a result of a recursive invocation
	   of map-extents or a call to a redisplay-type function), then
	   trying to track its changes is hopeless, so we just keep a
	   marker to the first (or last) extent in the SOE and use that as
	   our bound.

	   Finally, if DONT_USE_SOE is defined, we don't use the SOE at all
	   and instead just map from the beginning of the buffer.  This is
	   used for testing purposes and allows the SOE to be calculated
	   using map_extents() instead of the other way around. */

	{
		int range_flag;	/* ME_*_IN_REGION subset of flags */
		int do_soe_stage = 0;	/* Are we mapping over the SOE? */
		/* Does the range stage map over start or end positions? */
		int range_endp;
		/* If type == 0, we include the start position in the range
		   stage mapping.
		   If type == 1, we exclude the start position in the range
		   stage mapping.
		   If type == 2, we begin at range_start_pos, an extent-list
		   position.
		 */
		int range_start_type = 0;
		int range_start_pos = 0;
		int stage;

		range_flag = flags & ME_IN_REGION_MASK;
		if ((range_flag == ME_START_IN_REGION ||
		     range_flag == ME_START_AND_END_IN_REGION) &&
		    !(flags & ME_NEGATE_IN_REGION)) {
			/* map over start position in [range-start, range-end].
			   No SOE stage. */
			range_endp = 0;
		} else if (range_flag == ME_END_IN_REGION
			   && !(flags & ME_NEGATE_IN_REGION)) {
			/* map over end position in [range-start, range-end].
			   No SOE stage. */
			range_endp = 1;
		} else {
			/* Need to include the SOE extents. */
#ifdef DONT_USE_SOE
			/* Just brute-force it: start from the beginning. */
			range_endp = 0;
			range_start_type = 2;
			range_start_pos = 0;
#else
			extent_stack_t soe =
				buffer_or_string_stack_of_extents_force(obj);
			int numsoe;

			/* Move the SOE to the closer end of the range.  This
			   dictates whether we map over start positions or end
			   positions. */
			range_endp = 0;
			soe_move(obj, st);
			numsoe = extent_list_num_els(soe->extents);
			if (numsoe) {
				if (flags & ME_MIGHT_MOVE_SOE) {
					bool foundp;
					/* Can't map over SOE, so just extend
					   range to cover the SOE. */
					EXTENT e = extent_list_at(
						soe->extents, 0, 0);
					range_start_pos = extent_list_locate
						(buffer_or_string_extent_list
						 (obj), e, 0, &foundp);
					assert(foundp);
					range_start_type = 2;
				} else {
					/* We can map over the SOE. */
					do_soe_stage = 1;
					range_start_type = 1;
				}
			} else {
				/* No extents in the SOE to map over, so we act
				   just as if ME_START_IN_REGION or
				   ME_END_IN_REGION was specified.  RANGE_ENDP
				   already specified so no need to do anything
				   else. */
			}
		}
#endif

		/* ---------- Now loop over the extents. ---------- */

		/* We combine the code for the two stages because much of it
		   overlaps. */
		for (stage = 0; stage < 2; stage++) {
			int pos = 0;	/* Position in extent list */

			/* First set up start conditions */
			if (stage == 0) {	/* The SOE stage */
				if (!do_soe_stage)
					continue;
				el = buffer_or_string_stack_of_extents_force
				    (obj)->extents;
				/* We will always be looping over start extents
				   here. */
				assert(!range_endp);
				pos = 0;
			} else {	/* The range stage */
				el = buffer_or_string_extent_list(obj);
				switch (range_start_type) {
				case 0:
					pos = extent_list_locate_from_pos
						(el, st, range_endp);
					break;
				case 1:
					pos = extent_list_locate_from_pos
						(el, st + 1, range_endp);
					break;
				case 2:
					pos = range_start_pos;
					break;
				default:
					break;
				}
			}

			if (flags & ME_MIGHT_MODIFY_EXTENTS) {
				/* Create a marker to track changes to the
				   extent list */
				if (posm)
					/* Delete the marker used in the SOE
					   stage. */
					extent_list_delete_marker
						(buffer_or_string_stack_of_extents_force
						 (obj)->extents, posm);
				posm = extent_list_make_marker(
					el, pos, range_endp);
				/* tell the unwind function about the marker. */
				closure.el = el;
				closure.mkr = posm;
			}

			/* Now loop! */
			for (;;) {
				EXTENT e;
				Lisp_Object obj2;

				/* ----- update position in extent list
				   and fetch next extent ----- */

				if (posm) {
					/* fetch POS again to track extent
					   insertions or deletions */
					pos = extent_list_marker_pos(el, posm);
				}
				if (pos >= extent_list_num_els(el)) {
					break;
				}
				e = extent_list_at(el, pos, range_endp);
				pos++;
				if (posm) {
					/* now point the marker to the next one
					   we're going to process.  This ensures
					   graceful behavior if this extent is
					   deleted. */
					extent_list_move_marker(el, posm, pos);
				}
				/* ----- deal with internal extents ----- */

				if (extent_internal_p(e)) {
					if (!(flags & ME_INCLUDE_INTERNAL)) {
						continue;
					} else if (e == range) {
						/* We're processing internal
						   extents and we've come across
						   our own special range extent.
						   (This happens only in
						   adjust_extents*() and
						   process_extents*(), which
						   handle text insertion and
						   deletion.) We need to omit
						   processing of this extent;
						   otherwise we will probably
						   end up prematurely
						   terminating this loop. */
						continue;
					}
				}

				/* ----- deal with AFTER condition ----- */

				if (after) {
					/* if e > after, then we can stop
					   skipping extents. */
					if (EXTENT_LESS(after, e)) {
						after = 0;
					} else {
						/* otherwise, skip this
						   extent. */
						continue;
					}
				}

				/* ----- stop if we're completely outside the
					 range ----- */

				/* fetch ST and EN again to track text
				   insertions or deletions */
				if (range) {
					st = extent_start(range);
					en = extent_end(range);
				}
				if (extent_endpoint(e, range_endp) > en) {
					/* Can't be mapping over SOE because all
					   extents in there should overlap ST */
					assert(stage == 1);
					break;
				}

				/* ----- Now actually call the function ----- */

				obj2 = extent_object(e);
				if (extent_in_region_p(
					    e,
					    buffer_or_string_memind_to_bytind
					    (obj2, st),
					    buffer_or_string_memind_to_bytind
					    (obj2, en), flags)) {
					if ((*fn) (e, arg)) {
						/* Function wants us to stop
						   mapping. */
						stage = 1;
						/* so outer for loop will
						   terminate */
						break;
					}
				}
			}
		}
		/* ---------- Finished looping. ---------- */
	}

	if (flags & ME_MIGHT_THROW) {
		/* This deletes the range extent and frees the marker. */
		unbind_to(count, Qnil);
	} else {
		/* Delete them ourselves */
		if (range) {
			extent_detach(range);
		}
		if (posm) {
			extent_list_delete_marker(el, posm);
		}
	}
}

void
map_extents(Bufpos from, Bufpos to, map_extents_fun fn,
	    void *arg, Lisp_Object obj, EXTENT after, unsigned int flags)
{
	map_extents_bytind(buffer_or_string_bufpos_to_bytind(obj, from),
			   buffer_or_string_bufpos_to_bytind(obj, to), fn, arg,
			   obj, after, flags);
}

/* ------------------------------- */
/*         adjust_extents()        */
/* ------------------------------- */

/* Add AMOUNT to all extent endpoints in the range (FROM, TO].  This
   happens whenever the gap is moved or (under Mule) a character in a
   string is substituted for a different-length one.  The reason for
   this is that extent endpoints behave just like markers (all memory
   indices do) and this adjustment correct for markers -- see
   adjust_markers().  Note that it is important that we visit all
   extent endpoints in the range, irrespective of whether the
   endpoints are open or closed.

   We could use map_extents() for this (and in fact the function
   was originally written that way), but the gap is in an incoherent
   state when this function is called and this function plays
   around with extent endpoints without detaching and reattaching
   the extents (this is provably correct and saves lots of time),
   so for safety we make it just look at the extent lists directly. */

void
adjust_extents(Lisp_Object obj, Memind from, Memind to, int amount)
{
	int endp;
	int pos;
	int startpos[2];
	extent_list_t el;
	extent_stack_t soe;

#ifdef ERROR_CHECK_EXTENTS
	sledgehammer_extent_check(obj);
#endif
	el = buffer_or_string_extent_list(obj);

	if (!el || !extent_list_num_els(el)) {
		return;
	}
	/* IMPORTANT! Compute the starting positions of the extents to
	   modify BEFORE doing any modification!  Otherwise the starting
	   position for the second time through the loop might get
	   incorrectly calculated (I got bit by this bug real bad). */
	startpos[0] = extent_list_locate_from_pos(el, from + 1, 0);
	startpos[1] = extent_list_locate_from_pos(el, from + 1, 1);
	for (endp = 0; endp < 2; endp++) {
		for (pos = startpos[endp]; pos < extent_list_num_els(el);
		     pos++) {
			EXTENT e = extent_list_at(el, pos, endp);
			if (extent_endpoint(e, endp) > to) {
				break;
			}
			set_extent_endpoint(
				e,
				do_marker_adjustment(
					extent_endpoint(e, endp),
					from, to, amount),
				endp);
		}
	}

	/* The index for the buffer's SOE is a memory index and thus
	   needs to be adjusted like a marker. */
	soe = buffer_or_string_stack_of_extents(obj);
	if (soe && soe->pos >= 0) {
		soe->pos = do_marker_adjustment(soe->pos, from, to, amount);
	}
	return;
}

/* ------------------------------- */
/*  adjust_extents_for_deletion()  */
/* ------------------------------- */

struct adjust_extents_for_deletion_arg {
	EXTENT_dynarr *list;
};

static int adjust_extents_for_deletion_mapper(EXTENT extent, void *arg)
{
	struct adjust_extents_for_deletion_arg *closure =
		(struct adjust_extents_for_deletion_arg *)arg;

	Dynarr_add(closure->list, extent);
	/* continue mapping */
	return 0;
}

/* For all extent endpoints in the range (FROM, TO], move them to the beginning
   of the new gap.   Note that it is important that we visit all extent
   endpoints in the range, irrespective of whether the endpoints are open or
   closed.

   This function deals with weird stuff such as the fact that extents
   may get reordered.

   There is no string correspondent for this because you can't
   delete characters from a string.
 */

void
adjust_extents_for_deletion(Lisp_Object object, Bytind from,
			    Bytind to, int gapsize, int numdel, int movegapsize)
{
	struct adjust_extents_for_deletion_arg closure;
	int i;
	Memind adjust_to = (Memind) (to + gapsize);
	Bytecount amount = -numdel - movegapsize;
	Memind oldsoe = 0, newsoe = 0;
	extent_stack_t soe = buffer_or_string_stack_of_extents(object);

#ifdef ERROR_CHECK_EXTENTS
	sledgehammer_extent_check(object);
#endif
	closure.list = Dynarr_new(EXTENT);

	/* We're going to be playing weird games below with extents and the SOE
	   and such, so compute the list now of all the extents that we're going
	   to muck with.  If we do the mapping and adjusting together, things
	   can get all screwed up. */

	map_extents_bytind(from, to, adjust_extents_for_deletion_mapper,
			   (void *)&closure, object, 0,
			   /* extent endpoints move like markers regardless
			      of their open/closeness. */
			   ME_ALL_EXTENTS_CLOSED | ME_END_CLOSED |
			   ME_START_OR_END_IN_REGION | ME_INCLUDE_INTERNAL);

	/*
	   Old and new values for the SOE's position. (It gets adjusted
	   like a marker, just like extent endpoints.)
	 */

	if (soe) {
		oldsoe = soe->pos;
		if (soe->pos >= 0) {
			newsoe = do_marker_adjustment(
				soe->pos, adjust_to, adjust_to, amount);
		} else {
			newsoe = soe->pos;
		}
	}

	for (i = 0; i < Dynarr_length(closure.list); i++) {
		EXTENT extent = Dynarr_at(closure.list, i);
		Memind new_start = extent_start(extent);
		Memind new_end = extent_end(extent);

		/* do_marker_adjustment() will not adjust values that should not
		   be adjusted.  We're passing the same funky arguments to
		   do_marker_adjustment() as buffer_delete_range() does. */
		new_start = do_marker_adjustment(
			new_start, adjust_to, adjust_to, amount);
		new_end = do_marker_adjustment(
			new_end, adjust_to, adjust_to, amount);

		/* We need to be very careful here so that the SOE doesn't get
		   corrupted.  We are shrinking extents out of the deleted
		   region and simultaneously moving the SOE's pos out of the
		   deleted region, so the SOE should contain the same extents at
		   the end as at the beginning.  However, extents may get
		   reordered by this process, so we have to operate by pulling
		   the extents out of the buffer and SOE, changing their bounds,
		   and then reinserting them.  In order for the SOE not to get
		   screwed up, we have to make sure that the SOE's pos points to
		   its old location whenever we pull an extent out, and points
		   to its new location whenever we put the extent back in.
		 */

		if (new_start != extent_start(extent) ||
		    new_end != extent_end(extent)) {
			extent_detach(extent);
			set_extent_start(extent, new_start);
			set_extent_end(extent, new_end);
			if (soe) {
				soe->pos = newsoe;
			}
			extent_attach(extent);
			if (soe) {
				soe->pos = oldsoe;
			}
		}
	}

	if (soe) {
		soe->pos = newsoe;
	}

#ifdef ERROR_CHECK_EXTENTS
	sledgehammer_extent_check(object);
#endif
	Dynarr_free(closure.list);
	return;
}

/* ------------------------------- */
/*         extent fragments        */
/* ------------------------------- */

/* Imagine that the buffer is divided up into contiguous,
   nonoverlapping "runs" of text such that no extent
   starts or ends within a run (extents that abut the
   run don't count).

   An extent fragment is a structure that holds data about
   the run that contains a particular buffer position (if
   the buffer position is at the junction of two runs, the
   run after the position is used) -- the beginning and
   end of the run, a list of all of the extents in that
   run, the "merged face" that results from merging all of
   the faces corresponding to those extents, the begin and
   end glyphs at the beginning of the run, etc.  This is
   the information that redisplay needs in order to
   display this run.

   Extent fragments have to be very quick to update to
   a new buffer position when moving linearly through
   the buffer.  They rely on the stack-of-extents code,
   which does the heavy-duty algorithmic work of determining
   which extents overly a particular position. */

/* This function returns the position of the beginning of
   the first run that begins after POS, or returns POS if
   there are no such runs. */

static Bytind
extent_find_end_of_run(Lisp_Object obj, Bytind pos, int outside_accessible)
{
	extent_list_t sel;
	extent_list_t bel = buffer_or_string_extent_list(obj);
	Bytind pos1, pos2;
	int elind1, elind2;
	Memind mempos = buffer_or_string_bytind_to_memind(obj, pos);
	Bytind limit = outside_accessible ?
	    buffer_or_string_absolute_end_byte(obj) :
	    buffer_or_string_accessible_end_byte(obj);

	if (!bel || !extent_list_num_els(bel)) {
		return limit;
	}
	sel = buffer_or_string_stack_of_extents_force(obj)->extents;
	soe_move(obj, mempos);

	/* Find the first start position after POS. */
	elind1 = extent_list_locate_from_pos(bel, mempos + 1, 0);
	if (elind1 < extent_list_num_els(bel)) {
		pos1 = buffer_or_string_memind_to_bytind(
			obj, extent_start(extent_list_at(bel, elind1, 0)));
	} else {
		pos1 = limit;
	}

	/* Find the first end position after POS.  The extent corresponding
	   to this position is either in the SOE or is greater than or
	   equal to POS1, so we just have to look in the SOE. */
	elind2 = extent_list_locate_from_pos(sel, mempos + 1, 1);
	if (elind2 < extent_list_num_els(sel)) {
		pos2 = buffer_or_string_memind_to_bytind(
			obj, extent_end(extent_list_at(sel, elind2, 1)));
	} else {
		pos2 = limit;
	}
	return min(min(pos1, pos2), limit);
}

static Bytind
extent_find_beginning_of_run(Lisp_Object obj, Bytind pos,
			     int outside_accessible)
{
	extent_list_t sel;
	extent_list_t bel = buffer_or_string_extent_list(obj);
	Bytind pos1, pos2;
	int elind1, elind2;
	Memind mempos = buffer_or_string_bytind_to_memind(obj, pos);
	Bytind limit = outside_accessible
		? buffer_or_string_absolute_begin_byte(obj)
		: buffer_or_string_accessible_begin_byte(obj);

	if (!bel || !extent_list_num_els(bel)) {
		return limit;
	}
	sel = buffer_or_string_stack_of_extents_force(obj)->extents;
	soe_move(obj, mempos);

	/* Find the first end position before POS. */
	elind1 = extent_list_locate_from_pos(bel, mempos, 1);
	if (elind1 > 0) {
		pos1 = buffer_or_string_memind_to_bytind(
			obj, extent_end(extent_list_at(bel, elind1 - 1, 1)));
	} else {
		pos1 = limit;
	}
	/* Find the first start position before POS.  The extent corresponding
	   to this position is either in the SOE or is less than or
	   equal to POS1, so we just have to look in the SOE. */
	elind2 = extent_list_locate_from_pos(sel, mempos, 0);
	if (elind2 > 0) {
		pos2 = buffer_or_string_memind_to_bytind(
			obj, extent_start(extent_list_at(sel, elind2 - 1, 0)));
	} else {
		pos2 = limit;
	}
	return max(max(pos1, pos2), limit);
}

struct extent_fragment*
extent_fragment_new(Lisp_Object buffer_or_string, struct frame *frm)
{
	struct extent_fragment *ef = xnew_and_zero(struct extent_fragment);

	ef->object = buffer_or_string;
	ef->frm = frm;
	ef->extents = Dynarr_new(EXTENT);
	ef->glyphs = Dynarr_new(glyph_block);

	return ef;
}

void extent_fragment_delete(struct extent_fragment *ef)
{
	Dynarr_free(ef->extents);
	Dynarr_free(ef->glyphs);
	xfree(ef);
}

static int
extent_priority_sort_function(const void *humpty, const void *dumpty)
{
	const EXTENT foo = *(const EXTENT *)humpty;
	const EXTENT bar = *(const EXTENT *)dumpty;
	if (extent_priority(foo) < extent_priority(bar)) {
		return -1;
	}
	return extent_priority(foo) > extent_priority(bar);
}

static void
extent_fragment_sort_by_priority(EXTENT_dynarr * extarr)
{
	int i;

	/* Sort our copy of the stack by extent_priority.  We use a bubble
	   sort here because it's going to be faster than qsort() for small
	   numbers of extents (less than 10 or so), and 99.999% of the time
	   there won't ever be more extents than this in the stack. */
	if (Dynarr_length(extarr) < 10) {
		for (i = 1; i < Dynarr_length(extarr); i++) {
			int j = i - 1;
			while (j >= 0 &&
			       (extent_priority(Dynarr_at(extarr, j)) >
				extent_priority(Dynarr_at(extarr, j + 1)))) {
				EXTENT tmp = Dynarr_at(extarr, j);
				Dynarr_at(extarr, j) = Dynarr_at(extarr, j + 1);
				Dynarr_at(extarr, j + 1) = tmp;
				j--;
			}
		}
	} else {
		/* But some loser programs mess up and may create a large number
		   of extents overlapping the same spot.  This will result in
		   catastrophic behavior if we use the bubble sort above. */
		qsort(Dynarr_atp(extarr, 0), Dynarr_length(extarr),
		      sizeof(EXTENT), extent_priority_sort_function);
	}
}

/* If PROP is the `invisible' property of an extent,
   this is 1 if the extent should be treated as invisible.  */

#define EXTENT_PROP_MEANS_INVISIBLE(buf, prop)			\
  (EQ (buf->invisibility_spec, Qt)				\
   ? ! NILP (prop)						\
   : invisible_p (prop, buf->invisibility_spec))

/* If PROP is the `invisible' property of a extent,
   this is 1 if the extent should be treated as invisible
   and should have an ellipsis.  */

#define EXTENT_PROP_MEANS_INVISIBLE_WITH_ELLIPSIS(buf, prop)	\
  (EQ (buf->invisibility_spec, Qt)				\
   ? 0								\
   : invisible_ellipsis_p (prop, buf->invisibility_spec))

/* This is like a combination of memq and assq.
   Return 1 if PROPVAL appears as an element of LIST
   or as the car of an element of LIST.
   If PROPVAL is a list, compare each element against LIST
   in that way, and return 1 if any element of PROPVAL is found in LIST.
   Otherwise return 0.
   This function cannot quit.  */

static int
invisible_p(REGISTER Lisp_Object propval, Lisp_Object list)
{
	REGISTER Lisp_Object tail, proptail;
	for (tail = list; CONSP(tail); tail = XCDR(tail)) {
		REGISTER Lisp_Object tem;
		tem = XCAR(tail);
		if (EQ(propval, tem))
			return 1;
		if (CONSP(tem) && EQ(propval, XCAR(tem)))
			return 1;
	}
	if (CONSP(propval)) {
		for (proptail = propval; CONSP(proptail);
		     proptail = XCDR(proptail)) {
			Lisp_Object propelt;
			propelt = XCAR(proptail);
			for (tail = list; CONSP(tail); tail = XCDR(tail)) {
				REGISTER Lisp_Object tem;
				tem = XCAR(tail);
				if (EQ(propelt, tem)) {
					return 1;
				}
				if (CONSP(tem) && EQ(propelt, XCAR(tem))) {
					return 1;
				}
			}
		}
	}
	return 0;
}

/* Return 1 if PROPVAL appears as the car of an element of LIST
   and the cdr of that element is non-nil.
   If PROPVAL is a list, check each element of PROPVAL in that way,
   and the first time some element is found,
   return 1 if the cdr of that element is non-nil.
   Otherwise return 0.
   This function cannot quit.  */

static int
invisible_ellipsis_p(REGISTER Lisp_Object propval, Lisp_Object list)
{
	REGISTER Lisp_Object tail, proptail;

	for (tail = list; CONSP(tail); tail = XCDR(tail)) {
		REGISTER Lisp_Object tem;
		tem = XCAR(tail);
		if (CONSP(tem) && EQ(propval, XCAR(tem))) {
			return !NILP(XCDR(tem));
		}
	}
	if (CONSP(propval)) {
		for (proptail = propval; CONSP(proptail);
		     proptail = XCDR(proptail)) {
			Lisp_Object propelt;
			propelt = XCAR(proptail);
			for (tail = list; CONSP(tail); tail = XCDR(tail)) {
				REGISTER Lisp_Object tem;
				tem = XCAR(tail);
				if (CONSP(tem) && EQ(propelt, XCAR(tem))) {
					return !NILP(XCDR(tem));
				}
			}
		}
	}
	return 0;
}

face_index
extent_fragment_update(struct window * w, struct extent_fragment * ef,
		       Bytind pos, Lisp_Object last_glyph)
{
	int i, j;
	int seen_glyph = NILP(last_glyph) ? 1 : 0;
	extent_list_t sel =
		buffer_or_string_stack_of_extents_force(ef->object)->extents;
	EXTENT lhe = 0;
	struct extent dummy_lhe_extent;
	Memind mempos = buffer_or_string_bytind_to_memind(ef->object, pos);
	glyph_block_dynarr *glyphs; /* List of glyphs to post process */
	int invis_before = 0;  /* Exiting an invisible extent.  */
	int invis_after = 0;   /* Entering an invisible extent. */
	int insert_empty = 0;  /* Position to insert empty extent glyphs  */
	int queuing_begin = 0;	/* Queuing begin glyphs. */

#ifdef ERROR_CHECK_EXTENTS
	assert(pos >= buffer_or_string_accessible_begin_byte(ef->object)
	       && pos <= buffer_or_string_accessible_end_byte(ef->object));
#endif

	Dynarr_reset(ef->extents);
	Dynarr_reset(ef->glyphs);

	ef->previously_invisible = ef->invisible;
	if (ef->invisible) {
		if (ef->invisible_ellipses)
			ef->invisible_ellipses_already_displayed = 1;
	} else {
		ef->invisible_ellipses_already_displayed = 0;
	}
	ef->invisible = 0;
	ef->invisible_ellipses = 0;

	/* Set up the begin and end positions. */
	ef->pos = pos;
	ef->end = extent_find_end_of_run(ef->object, pos, 0);

	/* Note that extent_find_end_of_run() already moved the SOE for us. */
	/* soe_move (ef->object, mempos); */

	/* We tried determining all the charsets used in the run here,
	   but that fails even if we only do the current line -- display
	   tables or non-printable characters might cause other charsets
	   to be used. */

	/* Determine whether the last-highlighted-extent is present. */
	if (EXTENTP(Vlast_highlighted_extent))
		lhe = XEXTENT(Vlast_highlighted_extent);

	/* Now add all extents that overlap the character after POS and
	   have a non-nil face.  Also check if the character is
	   invisible.  We also queue begin and end glyphs of extents
	   that being/end at just before POS.  These are ordered as
	   follows. 1) end glyphs of non-empty extents in reverse
	   display order.  2) begin glyphs of empty extents.  3) end
	   glyphs of empty extents.  4) begin glyphs of non-empty
	   extents in display order.  Empty extents are shown nested,
	   but the invisibility property of an empty extent is
	   ignored and not used to determine whether an 'interior'
	   empty extent's glyphs should be shown or not.  */
	glyphs = Dynarr_new(glyph_block);
	for (i = 0; i < extent_list_num_els(sel); i++) {
		EXTENT e = extent_list_at(sel, i, 0);
		int zero_width = extent_start(e) == extent_end(e);
		Lisp_Object invis_prop = extent_invisible(e);
		Lisp_Object glyph;

		if (extent_start(e) == mempos) {
			/* The extent starts here.  If we are queuing
			   end glyphs, we should display all the end
			   glyphs we've pushed.  */

			if (!queuing_begin) {
				/* Append any already seen end glyphs */
				for (j = Dynarr_length(glyphs); j--;) {
					struct glyph_block *gbp
					  = Dynarr_atp(glyphs, j);

					if (seen_glyph)
						Dynarr_add(ef->glyphs, *gbp);
					else if (EQ(gbp->glyph, last_glyph))
						seen_glyph = 1;
				}

				/* Pop the end glyphs just displayed. */
				Dynarr_set_size(glyphs, 0);
				/* We are now queuing begin glyphs. */
				queuing_begin = 1;
				/* And will insert empty extent glyphs
				   just here.  */
				insert_empty = Dynarr_length (ef->glyphs);
			}

			glyph = extent_begin_glyph(e);

			if (!NILP(glyph)) {
				struct glyph_block gb;

				memset(&gb,0,sizeof(gb));

				gb.glyph = glyph;
				gb.active = 0; /* BEGIN_GLYPH */
				gb.width = 0;
				XSETEXTENT(gb.extent, e);

				if (zero_width) {
					if (insert_empty
					    == Dynarr_length (ef->glyphs))
						Dynarr_add (ef->glyphs, gb);
					else
						Dynarr_insert_many
						  (ef->glyphs, &gb,
						   1, insert_empty);
				} else if (!invis_after)
					Dynarr_add (glyphs, gb);
			}
		}

		if (extent_end(e) == mempos) {
			/* The extend ends here.  Push the end glyph.  */
			glyph = extent_end_glyph(e);

			if (!NILP (glyph)) {
				struct glyph_block gb;

				gb.width = gb.findex = 0; /* just init */
				gb.glyph = glyph;
				gb.active = 1; /* END_GLYPH */
				XSETEXTENT(gb.extent, e);

				if (zero_width)
				  Dynarr_add (ef->glyphs, gb);
				else if (!invis_before)
				  Dynarr_add(glyphs, gb);
			}
			/* If this extent is not empty, any inner
			   extents ending here will not be visible.  */
			if (extent_start (e) < mempos && !NILP (invis_prop))
			  invis_before = 1;
		}

		if (extent_end(e) > mempos) {
			/* This extent covers POS. */
			if (!NILP(invis_prop)) {
				invis_after = 1;
				/* If this extend spans POS, all
				   glyphs are invisible.  */
				if (extent_start (e) < mempos)
					Dynarr_set_size (glyphs, 0);

				if (!BUFFERP(ef->object))
					/* #### no `string-invisibility-spec' */
					ef->invisible = 1;
				else {
					if (!ef->
					    invisible_ellipses_already_displayed
					    &&
					    EXTENT_PROP_MEANS_INVISIBLE_WITH_ELLIPSIS
					    (XBUFFER(ef->object), invis_prop)) {
						ef->invisible = 1;
						ef->invisible_ellipses = 1;
					} else if (EXTENT_PROP_MEANS_INVISIBLE
						   (XBUFFER(ef->object),
						    invis_prop))
						ef->invisible = 1;
				}
			}

			/* Remember that one of the extents in the list might be
			   our dummy extent representing the highlighting that
			   is attached to some other extent that is currently
			   mouse-highlighted.  When an extent is
			   mouse-highlighted, it is as if there are two extents
			   there, of potentially different priorities: the
			   extent being highlighted, with whatever face and
			   priority it has; and an ephemeral extent in the
			   `mouse-face' face with `mouse-highlight-priority'.
			 */

			if (!NILP(extent_face(e)))
				Dynarr_add(ef->extents, e);
			if (e == lhe) {
				Lisp_Object f;
				/* zeroing isn't really necessary; we only deref
				   `priority' and `face' */
				xzero(dummy_lhe_extent);
				set_extent_priority(&dummy_lhe_extent,
						    mouse_highlight_priority);
				/* Need to break up the following expression,
				   due to an */
				/* error in the Digital UNIX 3.2g C compiler
				   (Digital */
				/* UNIX Compiler Driver 3.11). */
				f = extent_mouse_face(lhe);
				extent_face(&dummy_lhe_extent) = f;
				Dynarr_add(ef->extents, &dummy_lhe_extent);
			}
			/* since we are looping anyway, we might as well do this
			   here */
			if ((!NILP(extent_initial_redisplay_function(e))) &&
			    !extent_in_red_event_p(e)) {
				Lisp_Object function =
					extent_initial_redisplay_function(e);
				Lisp_Object obj;

				/* print_extent_2 (e);
				   printf ("\n"); */

				/* FIXME: One should probably inhibit the
				   displaying of this extent to reduce
				   flicker */
				extent_in_red_event_p(e) = 1;

				/* call the function */
				XSETEXTENT(obj, e);
				if (!NILP(function)) {
					Fenqueue_eval_event(function, obj);
				}
			}
		}
	}

	if (!queuing_begin) {
		/* Append end glyphs in reverse order */
		for (j = Dynarr_length(glyphs); j--;) {
			struct glyph_block *gbp = Dynarr_atp(glyphs, j);

			if (seen_glyph)
				Dynarr_add(ef->glyphs, *gbp);
			else if (EQ(gbp->glyph, last_glyph))
				seen_glyph = 1;
		}
	} else {
		if (!seen_glyph) {
			/* Scan the zero length glyphs and see where we
			   start a glyph that has not been displayed yet.  */
			for (j = insert_empty;
			     j != Dynarr_length (ef->glyphs); j++) {
				struct glyph_block *gbp
					= Dynarr_atp(ef->glyphs, j);

				if (EQ(gbp->glyph, last_glyph)) {
					seen_glyph = 1;
					j++;
					break;
				}
			}
			Dynarr_delete_many (ef->glyphs, insert_empty,
					    j - insert_empty);
		}

		/* Now copy the begin glyphs. */
		for (j = 0; j != Dynarr_length (glyphs); j++) {
			struct glyph_block *gbp = Dynarr_atp(glyphs, j);

			if (seen_glyph)
				Dynarr_add(ef->glyphs, *gbp);
			else if (EQ(gbp->glyph, last_glyph))
				seen_glyph = 1;
		}
	}

	Dynarr_free(glyphs);

	extent_fragment_sort_by_priority(ef->extents);

	/* Now merge the faces together into a single face.  The code to
	   do this is in faces.c because it involves manipulating faces. */
	return get_extent_fragment_face_cache_index(w, ef);
}

/************************************************************************/
/*		        extent-object methods				*/
/************************************************************************/

/* These are the basic helper functions for handling the allocation of
   extent objects.  They are similar to the functions for other
   lrecord objects.  allocate_extent() is in alloc.c, not here. */

static Lisp_Object mark_extent(Lisp_Object obj)
{
	struct extent *extent = XEXTENT(obj);

	mark_object(extent_object(extent));
	mark_object(extent_no_chase_normal_field(extent, face));
	return extent->plist;
}

static void
print_extent_1(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	EXTENT ext = XEXTENT(obj);
	EXTENT anc = extent_ancestor(ext);
	Lisp_Object tail;
	char buf[100], *bp = buf;
	int sz;

	/* Retrieve the ancestor and use it, for faster retrieval of properties */

	if (!NILP(extent_begin_glyph(anc)))
		*bp++ = '*';
	*bp++ = (extent_start_open_p(anc) ? '(' : '[');
	if (extent_detached_p(ext))
		xstrncpy(bp, "detached", sizeof(buf)-6);
	else {
		sz=snprintf(bp, sizeof(buf)-6, "%ld, %ld",
			    XINT(Fextent_start_position(obj)),
			    XINT(Fextent_end_position(obj)));
		assert(sz>=0 && (size_t)sz<(sizeof(buf)-6));
	}
	bp += strlen(bp);
	*bp++ = (extent_end_open_p(anc) ? ')' : ']');
	if (!NILP(extent_end_glyph(anc)))
		*bp++ = '*';
	*bp++ = ' ';

	if (!NILP(extent_read_only(anc)))
		*bp++ = '%';
	if (!NILP(extent_mouse_face(anc)))
		*bp++ = 'H';
	if (extent_unique_p(anc))
		*bp++ = 'U';
	else if (extent_duplicable_p(anc))
		*bp++ = 'D';
	if (!NILP(extent_invisible(anc)))
		*bp++ = 'I';

	if (!NILP(extent_read_only(anc)) || !NILP(extent_mouse_face(anc)) ||
	    extent_unique_p(anc) ||
	    extent_duplicable_p(anc) || !NILP(extent_invisible(anc)))
		*bp++ = ' ';
	*bp = '\0';
	write_c_string(buf, printcharfun);

	tail = extent_plist_slot(anc);

	for (; !NILP(tail); tail = Fcdr(Fcdr(tail))) {
		Lisp_Object v = XCAR(XCDR(tail));
		if (NILP(v))
			continue;
		print_internal(XCAR(tail), printcharfun, escapeflag);
		write_c_string(" ", printcharfun);
	}

	write_fmt_str(printcharfun, "0x%lx", (long)ext);
}

static void
print_extent(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	if (escapeflag) {
		const char *title = "";
		const char *name = "";
		const char *posttitle = "";
		Lisp_Object obj2 = Qnil;

		/* Destroyed extents have 't' in the object field, causing
		   extent_object() to abort (maybe). */
		if (EXTENT_LIVE_P(XEXTENT(obj)))
			obj2 = extent_object(XEXTENT(obj));

		if (NILP(obj2))
			title = "no buffer";
		else if (BUFFERP(obj2)) {
			if (BUFFER_LIVE_P(XBUFFER(obj2))) {
				title = "buffer ";
				name =
				    (char *)XSTRING_DATA(XBUFFER(obj2)->name);
			} else {
				title = "Killed Buffer";
				name = "";
			}
		} else {
			assert(STRINGP(obj2));
			title = "string \"";
			posttitle = "\"";
			name = (char *)XSTRING_DATA(obj2);
		}

		if (print_readably) {
			if (!EXTENT_LIVE_P(XEXTENT(obj))) {
				error("printing unreadable object "
				      "#<destroyed extent>");
			} else {
				error("printing unreadable object "
				      "#<extent %p>", XEXTENT(obj));
			}
		}

		if (!EXTENT_LIVE_P(XEXTENT(obj))) {
			write_c_string("#<destroyed extent", printcharfun);
		} else {
			write_c_string("#<extent ", printcharfun);
			print_extent_1(obj, printcharfun, escapeflag);
			write_c_string(extent_detached_p(XEXTENT(obj))
				       ? " from " : " in ", printcharfun);
			write_fmt_string(printcharfun, "%s%s%s", title, name, posttitle);
		}
	} else {
		if (print_readably)
			error("printing unreadable object #<extent>");
		write_c_string("#<extent", printcharfun);
	}
	write_c_string(">", printcharfun);
}

static int properties_equal(EXTENT e1, EXTENT e2, int depth)
{
	/* When this function is called, all indirections have been followed.
	   Thus, the indirection checks in the various macros below will not
	   amount to anything, and could be removed.  However, the time
	   savings would probably not be significant. */
	if (!(EQ(extent_face(e1), extent_face(e2)) &&
	      extent_priority(e1) == extent_priority(e2) &&
	      internal_equal(extent_begin_glyph(e1), extent_begin_glyph(e2),
			     depth + 1) &&
	      internal_equal(extent_end_glyph(e1), extent_end_glyph(e2),
			     depth + 1)))
		return 0;

	/* compare the bit flags. */
	{
		/* The has_aux field should not be relevant. */
		int e1_has_aux = e1->flags.has_aux;
		int e2_has_aux = e2->flags.has_aux;
		int value;

		e1->flags.has_aux = e2->flags.has_aux = 0;
		value = memcmp(&e1->flags, &e2->flags, sizeof(e1->flags));
		e1->flags.has_aux = e1_has_aux;
		e2->flags.has_aux = e2_has_aux;
		if (value)
			return 0;
	}

	/* compare the random elements of the plists. */
	return !plists_differ(extent_no_chase_plist(e1),
			      extent_no_chase_plist(e2), 0, 0, depth + 1);
}

static int extent_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	struct extent *e1 = XEXTENT(obj1);
	struct extent *e2 = XEXTENT(obj2);
	return
	    (extent_start(e1) == extent_start(e2) &&
	     extent_end(e1) == extent_end(e2) &&
	     internal_equal(extent_object(e1), extent_object(e2), depth + 1) &&
	     properties_equal(extent_ancestor(e1), extent_ancestor(e2), depth));
}

static unsigned long extent_hash(Lisp_Object obj, int depth)
{
	struct extent *e = XEXTENT(obj);
	/* No need to hash all of the elements; that would take too long.
	   Just hash the most common ones. */
	return HASH3(extent_start(e), extent_end(e),
		     internal_hash(extent_object(e), depth + 1));
}

static const struct lrecord_description extent_description[] = {
	{XD_LISP_OBJECT, offsetof(struct extent, object)},
	{XD_LISP_OBJECT, offsetof(struct extent, flags.face)},
	{XD_LISP_OBJECT, offsetof(struct extent, plist)},
	{XD_END}
};

static Lisp_Object extent_getprop(Lisp_Object obj, Lisp_Object prop)
{
	return Fextent_property(obj, prop, Qunbound);
}

static int extent_putprop(Lisp_Object obj, Lisp_Object prop, Lisp_Object value)
{
	Fset_extent_property(obj, prop, value);
	return 1;
}

static int extent_remprop(Lisp_Object obj, Lisp_Object prop)
{
	EXTENT ext = XEXTENT(obj);

	/* This list is taken from Fset_extent_property, and should be kept
	   in synch.  */
	if (EQ(prop, Qread_only)
	    || EQ(prop, Qunique)
	    || EQ(prop, Qduplicable)
	    || EQ(prop, Qinvisible)
	    || EQ(prop, Qdetachable)
	    || EQ(prop, Qdetached)
	    || EQ(prop, Qdestroyed)
	    || EQ(prop, Qpriority)
	    || EQ(prop, Qface)
	    || EQ(prop, Qinitial_redisplay_function)
	    || EQ(prop, Qafter_change_functions)
	    || EQ(prop, Qbefore_change_functions)
	    || EQ(prop, Qmouse_face)
	    || EQ(prop, Qhighlight)
	    || EQ(prop, Qbegin_glyph_layout)
	    || EQ(prop, Qend_glyph_layout)
	    || EQ(prop, Qglyph_layout)
	    || EQ(prop, Qbegin_glyph)
	    || EQ(prop, Qend_glyph)
	    || EQ(prop, Qstart_open)
	    || EQ(prop, Qend_open)
	    || EQ(prop, Qstart_closed)
	    || EQ(prop, Qend_closed)
	    || EQ(prop, Qkeymap)) {
		/* #### Is this correct, anyway?  */
		return -1;
	}

	return external_remprop(extent_plist_addr(ext), prop, 0, ERROR_ME);
}

static Lisp_Object extent_plist(Lisp_Object obj)
{
	return Fextent_properties(obj);
}

DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS("extent", extent,
					       mark_extent, print_extent,
					       /* NOTE: If you declare a
						  finalization method here,
						  it will NOT be called.
						  Shaft city. */
					       0,
					       extent_equal, extent_hash,
					       extent_description,
					       extent_getprop, extent_putprop,
					       extent_remprop, extent_plist,
					       struct extent);

/************************************************************************/
/*			basic extent accessors				*/
/************************************************************************/

/* These functions are for checking externally-passed extent objects
   and returning an extent's basic properties, which include the
   buffer the extent is associated with, the endpoints of the extent's
   range, the open/closed-ness of those endpoints, and whether the
   extent is detached.  Manipulating these properties requires
   manipulating the ordered lists that hold extents; thus, functions
   to do that are in a later section. */

/* Given a Lisp_Object that is supposed to be an extent, make sure it
   is OK and return an extent pointer.  Extents can be in one of four
   states:

   1) destroyed
   2) detached and not associated with a buffer
   3) detached and associated with a buffer
   4) attached to a buffer

   If FLAGS is 0, types 2-4 are allowed.  If FLAGS is DE_MUST_HAVE_BUFFER,
   types 3-4 are allowed.  If FLAGS is DE_MUST_BE_ATTACHED, only type 4
   is allowed.
   */

static EXTENT decode_extent(Lisp_Object extent_obj, unsigned int flags)
{
	EXTENT extent;
	Lisp_Object obj;

	CHECK_LIVE_EXTENT(extent_obj);
	extent = XEXTENT(extent_obj);
	obj = extent_object(extent);

	/* the following condition will fail if we're dealing with a freed extent */
	assert(NILP(obj) || BUFFERP(obj) || STRINGP(obj));

	if (flags & DE_MUST_BE_ATTACHED)
		flags |= DE_MUST_HAVE_BUFFER;

	/* if buffer is dead, then convert extent to have no buffer. */
	if (BUFFERP(obj) && !BUFFER_LIVE_P(XBUFFER(obj)))
		obj = extent_object(extent) = Qnil;

	assert(!NILP(obj) || extent_detached_p(extent));

	if ((NILP(obj) && (flags & DE_MUST_HAVE_BUFFER))
	    || (extent_detached_p(extent) && (flags & DE_MUST_BE_ATTACHED))) {
		invalid_argument("extent doesn't belong to a buffer or string",
				 extent_obj);
	}

	return extent;
}

/* Note that the returned value is a buffer position, not a byte index. */

static Lisp_Object extent_endpoint_external(Lisp_Object extent_obj, int endp)
{
	EXTENT extent = decode_extent(extent_obj, 0);

	if (extent_detached_p(extent))
		return Qnil;
	else
		return make_int(extent_endpoint_bufpos(extent, endp));
}

DEFUN("extentp", Fextentp, 1, 1, 0,	/*
Return t if OBJECT is an extent.
*/
      (object))
{
	return EXTENTP(object) ? Qt : Qnil;
}

DEFUN("extent-live-p", Fextent_live_p, 1, 1, 0,	/*
Return t if OBJECT is an extent that has not been destroyed.
*/
      (object))
{
	return EXTENTP(object) && EXTENT_LIVE_P(XEXTENT(object)) ? Qt : Qnil;
}

DEFUN("extent-detached-p", Fextent_detached_p, 1, 1, 0,	/*
Return t if EXTENT is detached.
*/
      (extent))
{
	return extent_detached_p(decode_extent(extent, 0)) ? Qt : Qnil;
}

DEFUN("extent-object", Fextent_object, 1, 1, 0,	/*
Return object (buffer or string) that EXTENT refers to.
*/
      (extent))
{
	return extent_object(decode_extent(extent, 0));
}

DEFUN("extent-start-position", Fextent_start_position, 1, 1, 0,	/*
Return start position of EXTENT, or nil if EXTENT is detached.
*/
      (extent))
{
	return extent_endpoint_external(extent, 0);
}

DEFUN("extent-end-position", Fextent_end_position, 1, 1, 0,	/*
Return end position of EXTENT, or nil if EXTENT is detached.
*/
      (extent))
{
	return extent_endpoint_external(extent, 1);
}

DEFUN("extent-length", Fextent_length, 1, 1, 0,	/*
Return length of EXTENT in characters.
*/
      (extent))
{
	EXTENT e = decode_extent(extent, DE_MUST_BE_ATTACHED);
	return make_int(extent_endpoint_bufpos(e, 1)
			- extent_endpoint_bufpos(e, 0));
}

DEFUN("next-extent", Fnext_extent, 1, 1, 0,	/*
Find next extent after EXTENT.
If EXTENT is a buffer return the first extent in the buffer; likewise
for strings.
Extents in a buffer are ordered in what is called the "display"
order, which sorts by increasing start positions and then by *decreasing*
end positions.
If you want to perform an operation on a series of extents, use
`map-extents' instead of this function; it is much more efficient.
The primary use of this function should be to enumerate all the
extents in a buffer.
Note: The display order is not necessarily the order that `map-extents'
processes extents in!
*/
      (extent))
{
	Lisp_Object val;
	EXTENT next;

	if (EXTENTP(extent))
		next = extent_next(decode_extent(extent, DE_MUST_BE_ATTACHED));
	else
		next = extent_first(decode_buffer_or_string(extent));

	if (!next)
		return Qnil;
	XSETEXTENT(val, next);
	return val;
}

DEFUN("previous-extent", Fprevious_extent, 1, 1, 0,	/*
Find last extent before EXTENT.
If EXTENT is a buffer return the last extent in the buffer; likewise
for strings.
This function is analogous to `next-extent'.
*/
      (extent))
{
	Lisp_Object val;
	EXTENT prev;

	if (EXTENTP(extent))
		prev =
		    extent_previous(decode_extent(extent, DE_MUST_BE_ATTACHED));
	else
		prev = extent_last(decode_buffer_or_string(extent));

	if (!prev)
		return Qnil;
	XSETEXTENT(val, prev);
	return val;
}

#ifdef DEBUG_SXEMACS

DEFUN("next-e-extent", Fnext_e_extent, 1, 1, 0,	/*
Find next extent after EXTENT using the "e" order.
If EXTENT is a buffer return the first extent in the buffer; likewise
for strings.
*/
      (extent))
{
	Lisp_Object val;
	EXTENT next;

	if (EXTENTP(extent))
		next =
		    extent_e_next(decode_extent(extent, DE_MUST_BE_ATTACHED));
	else
		next = extent_e_first(decode_buffer_or_string(extent));

	if (!next)
		return Qnil;
	XSETEXTENT(val, next);
	return val;
}

DEFUN("previous-e-extent", Fprevious_e_extent, 1, 1, 0,	/*
Find last extent before EXTENT using the "e" order.
If EXTENT is a buffer return the last extent in the buffer; likewise
for strings.
This function is analogous to `next-e-extent'.
*/
      (extent))
{
	Lisp_Object val;
	EXTENT prev;

	if (EXTENTP(extent))
		prev =
		    extent_e_previous(decode_extent
				      (extent, DE_MUST_BE_ATTACHED));
	else
		prev = extent_e_last(decode_buffer_or_string(extent));

	if (!prev)
		return Qnil;
	XSETEXTENT(val, prev);
	return val;
}

#endif

DEFUN("next-extent-change", Fnext_extent_change, 1, 2, 0,	/*
Return the next position after POS where an extent begins or ends.
If POS is at the end of the buffer or string, POS will be returned;
otherwise a position greater than POS will always be returned.
If OBJECT is nil, the current buffer is assumed.
*/
      (pos, object))
{
	Lisp_Object obj = decode_buffer_or_string(object);
	Bytind bpos;

	bpos =
	    get_buffer_or_string_pos_byte(obj, pos, GB_ALLOW_PAST_ACCESSIBLE);
	bpos = extent_find_end_of_run(obj, bpos, 1);
	return make_int(buffer_or_string_bytind_to_bufpos(obj, bpos));
}

DEFUN("previous-extent-change", Fprevious_extent_change, 1, 2, 0,	/*
Return the last position before POS where an extent begins or ends.
If POS is at the beginning of the buffer or string, POS will be returned;
otherwise a position less than POS will always be returned.
If OBJECT is nil, the current buffer is assumed.
*/
      (pos, object))
{
	Lisp_Object obj = decode_buffer_or_string(object);
	Bytind bpos;

	bpos =
	    get_buffer_or_string_pos_byte(obj, pos, GB_ALLOW_PAST_ACCESSIBLE);
	bpos = extent_find_beginning_of_run(obj, bpos, 1);
	return make_int(buffer_or_string_bytind_to_bufpos(obj, bpos));
}

/************************************************************************/
/*			parent and children stuff			*/
/************************************************************************/

DEFUN("extent-parent", Fextent_parent, 1, 1, 0,	/*
Return the parent (if any) of EXTENT.
If an extent has a parent, it derives all its properties from that extent
and has no properties of its own. (The only "properties" that the
extent keeps are the buffer/string it refers to and the start and end
points.) It is possible for an extent's parent to itself have a parent.
*/
      (extent))
/* do I win the prize for the strangest split infinitive? */
{
	EXTENT e = decode_extent(extent, 0);
	return extent_parent(e);
}

DEFUN("extent-children", Fextent_children, 1, 1, 0,	/*
Return a list of the children (if any) of EXTENT.
The children of an extent are all those extents whose parent is that extent.
This function does not recursively trace children of children.
\(To do that, use `extent-descendants'.)
*/
      (extent))
{
	EXTENT e = decode_extent(extent, 0);
	Lisp_Object children = extent_children(e);

	if (!NILP(children))
		return Fcopy_sequence(XWEAK_LIST_LIST(children));
	else
		return Qnil;
}

static void remove_extent_from_children_list(EXTENT e, Lisp_Object child)
{
	Lisp_Object children = extent_children(e);

#ifdef ERROR_CHECK_EXTENTS
	assert(!NILP(memq_no_quit(child, XWEAK_LIST_LIST(children))));
#endif
	XWEAK_LIST_LIST(children) =
	    delq_no_quit(child, XWEAK_LIST_LIST(children));
}

static void add_extent_to_children_list(EXTENT e, Lisp_Object child)
{
	Lisp_Object children = extent_children(e);

	if (NILP(children)) {
		children = make_weak_list(WEAK_LIST_SIMPLE);
		set_extent_no_chase_aux_field(e, children, children);
	}
#ifdef ERROR_CHECK_EXTENTS
	assert(NILP(memq_no_quit(child, XWEAK_LIST_LIST(children))));
#endif
	XWEAK_LIST_LIST(children) = Fcons(child, XWEAK_LIST_LIST(children));
}

DEFUN("set-extent-parent", Fset_extent_parent, 2, 2, 0,	/*
Set the parent of EXTENT to PARENT (may be nil).
See `extent-parent'.
*/
      (extent, parent))
{
	EXTENT e = decode_extent(extent, 0);
	Lisp_Object cur_parent = extent_parent(e);
	Lisp_Object rest;

	XSETEXTENT(extent, e);
	if (!NILP(parent))
		CHECK_LIVE_EXTENT(parent);
	if (EQ(parent, cur_parent))
		return Qnil;
	for (rest = parent; !NILP(rest); rest = extent_parent(XEXTENT(rest)))
		if (EQ(rest, extent))
			signal_type_error(Qinvalid_change,
					  "Circular parent chain would result",
					  extent);
	if (NILP(parent)) {
		remove_extent_from_children_list(XEXTENT(cur_parent), extent);
		set_extent_no_chase_aux_field(e, parent, Qnil);
		e->flags.has_parent = 0;
	} else {
		add_extent_to_children_list(XEXTENT(parent), extent);
		set_extent_no_chase_aux_field(e, parent, parent);
		e->flags.has_parent = 1;
	}
	/* changing the parent also changes the properties of all children. */
	{
		int old_invis = (!NILP(cur_parent) &&
				 !NILP(extent_invisible(XEXTENT(cur_parent))));
		int new_invis = (!NILP(parent) &&
				 !NILP(extent_invisible(XEXTENT(parent))));

		extent_maybe_changed_for_redisplay(e, 1,
						   new_invis != old_invis);
	}

	return Qnil;
}

/************************************************************************/
/*			basic extent mutators				*/
/************************************************************************/

/* Note:  If you track non-duplicable extents by undo, you'll get bogus
   undo records for transient extents via update-extent.
   For example, query-replace will do this.
 */

static void set_extent_endpoints_1(EXTENT extent, Memind start, Memind end)
{
#ifdef ERROR_CHECK_EXTENTS
	Lisp_Object obj = extent_object(extent);

	assert(start <= end);
	if (BUFFERP(obj)) {
		assert(valid_memind_p(XBUFFER(obj), start));
		assert(valid_memind_p(XBUFFER(obj), end));
	}
#endif

	/* Optimization: if the extent is already where we want it to be,
	   do nothing. */
	if (!extent_detached_p(extent) && extent_start(extent) == start &&
	    extent_end(extent) == end)
		return;

	if (extent_detached_p(extent)) {
		if (extent_duplicable_p(extent)) {
			Lisp_Object extent_obj;
			XSETEXTENT(extent_obj, extent);
			record_extent(extent_obj, 1);
		}
	} else
		extent_detach(extent);

	set_extent_start(extent, start);
	set_extent_end(extent, end);
	extent_attach(extent);
}

/* Set extent's endpoints to S and E, and put extent in buffer or string
   OBJECT. (If OBJECT is nil, do not change the extent's object.) */

void set_extent_endpoints(EXTENT extent, Bytind s, Bytind e, Lisp_Object object)
{
	Memind start, end;

	if (NILP(object)) {
		object = extent_object(extent);
		assert(!NILP(object));
	} else if (!EQ(object, extent_object(extent))) {
		extent_detach(extent);
		extent_object(extent) = object;
	}

	start = s < 0 ? extent_start(extent) :
	    buffer_or_string_bytind_to_memind(object, s);
	end = e < 0 ? extent_end(extent) :
	    buffer_or_string_bytind_to_memind(object, e);
	set_extent_endpoints_1(extent, start, end);
}

static void set_extent_openness(EXTENT extent, int start_open, int end_open)
{
	if (start_open != -1)
		extent_start_open_p(extent) = start_open;
	if (end_open != -1)
		extent_end_open_p(extent) = end_open;
	/* changing the open/closedness of an extent does not affect
	   redisplay. */
}

static EXTENT make_extent_internal(Lisp_Object object, Bytind from, Bytind to)
{
	EXTENT extent;

	extent = make_extent_detached(object);
	set_extent_endpoints(extent, from, to, Qnil);
	return extent;
}

static EXTENT
copy_extent(EXTENT original, Bytind from, Bytind to, Lisp_Object object)
{
	EXTENT e;

	e = make_extent_detached(object);
	if (from >= 0)
		set_extent_endpoints(e, from, to, Qnil);

	e->plist = Fcopy_sequence(original->plist);
	memcpy(&e->flags, &original->flags, sizeof(e->flags));
	if (e->flags.has_aux) {
		/* also need to copy the aux struct.  It won't work for
		   this extent to share the same aux struct as the original
		   one. */
		struct extent_auxiliary *data =
		    alloc_lcrecord_type(struct extent_auxiliary,
					&lrecord_extent_auxiliary);

		copy_lcrecord(data, XEXTENT_AUXILIARY(XCAR(original->plist)));
		XSETEXTENT_AUXILIARY(XCAR(e->plist), data);
	}

	{
		/* we may have just added another child to the parent extent. */
		Lisp_Object parent = extent_parent(e);
		if (!NILP(parent)) {
			Lisp_Object extent;
			XSETEXTENT(extent, e);
			add_extent_to_children_list(XEXTENT(parent), extent);
		}
	}

	return e;
}

static void destroy_extent(EXTENT extent)
{
	Lisp_Object rest, nextrest, children;
	Lisp_Object extent_obj;

	if (!extent_detached_p(extent))
		extent_detach(extent);
	/* disassociate the extent from its children and parent */
	children = extent_children(extent);
	if (!NILP(children)) {
		LIST_LOOP_DELETING(rest, nextrest, XWEAK_LIST_LIST(children))
		    Fset_extent_parent(XCAR(rest), Qnil);
	}
	XSETEXTENT(extent_obj, extent);
	Fset_extent_parent(extent_obj, Qnil);
	/* mark the extent as destroyed */
	extent_object(extent) = Qt;
}

DEFUN("make-extent", Fmake_extent, 2, 3, 0,	/*
Make an extent for the range [FROM, TO) in BUFFER-OR-STRING.
BUFFER-OR-STRING defaults to the current buffer.  Insertions at point
TO will be outside of the extent; insertions at FROM will be inside the
extent, causing the extent to grow. (This is the same way that markers
behave.) You can change the behavior of insertions at the endpoints
using `set-extent-property'.  The extent is initially detached if both
FROM and TO are nil, and in this case BUFFER-OR-STRING defaults to nil,
meaning the extent is in no buffer and no string.
*/
      (from, to, buffer_or_string))
{
	Lisp_Object extent_obj;
	Lisp_Object obj;

	obj = decode_buffer_or_string(buffer_or_string);
	if (NILP(from) && NILP(to)) {
		if (NILP(buffer_or_string))
			obj = Qnil;
		XSETEXTENT(extent_obj, make_extent_detached(obj));
	} else {
		Bytind start, end;

		get_buffer_or_string_range_byte(obj, from, to, &start, &end,
						GB_ALLOW_PAST_ACCESSIBLE);
		XSETEXTENT(extent_obj, make_extent_internal(obj, start, end));
	}
	return extent_obj;
}

DEFUN("copy-extent", Fcopy_extent, 1, 2, 0,	/*
Make a copy of EXTENT.  It is initially detached.
Optional argument BUFFER-OR-STRING defaults to EXTENT's buffer or string.
*/
      (extent, buffer_or_string))
{
	EXTENT ext = decode_extent(extent, 0);

	if (NILP(buffer_or_string))
		buffer_or_string = extent_object(ext);
	else
		buffer_or_string = decode_buffer_or_string(buffer_or_string);

	XSETEXTENT(extent, copy_extent(ext, -1, -1, buffer_or_string));
	return extent;
}

DEFUN("delete-extent", Fdelete_extent, 1, 1, 0,	/*
Remove EXTENT from its buffer and destroy it.
This does not modify the buffer's text, only its display properties.
The extent cannot be used thereafter.
*/
      (extent))
{
	EXTENT ext;

	/* We do not call decode_extent() here because already-destroyed
	   extents are OK. */
	CHECK_EXTENT(extent);
	ext = XEXTENT(extent);

	if (!EXTENT_LIVE_P(ext))
		return Qnil;
	destroy_extent(ext);
	return Qnil;
}

DEFUN("detach-extent", Fdetach_extent, 1, 1, 0,	/*
Remove EXTENT from its buffer in such a way that it can be re-inserted.
An extent is also detached when all of its characters are all killed by a
deletion, unless its `detachable' property has been unset.

Extents which have the `duplicable' attribute are tracked by the undo
mechanism.  Detachment via `detach-extent' and string deletion is recorded,
as is attachment via `insert-extent' and string insertion.  Extent motion,
face changes, and attachment via `make-extent' and `set-extent-endpoints'
are not recorded.  This means that extent changes which are to be undo-able
must be performed by character editing, or by insertion and detachment of
duplicable extents.
*/
      (extent))
{
	EXTENT ext = decode_extent(extent, 0);

	if (extent_detached_p(ext))
		return extent;
	if (extent_duplicable_p(ext))
		record_extent(extent, 0);
	extent_detach(ext);

	return extent;
}

DEFUN("set-extent-endpoints", Fset_extent_endpoints, 3, 4, 0,	/*
Set the endpoints of EXTENT to START, END.
If START and END are null, call detach-extent on EXTENT.
BUFFER-OR-STRING specifies the new buffer or string that the extent should
be in, and defaults to EXTENT's buffer or string. (If nil, and EXTENT
is in no buffer and no string, it defaults to the current buffer.)
See documentation on `detach-extent' for a discussion of undo recording.
*/
      (extent, start, end, buffer_or_string))
{
	EXTENT ext;
	Bytind s, e;

	ext = decode_extent(extent, 0);

	if (NILP(buffer_or_string)) {
		buffer_or_string = extent_object(ext);
		if (NILP(buffer_or_string))
			buffer_or_string = Fcurrent_buffer();
	} else
		buffer_or_string = decode_buffer_or_string(buffer_or_string);

	if (NILP(start) && NILP(end))
		return Fdetach_extent(extent);

	get_buffer_or_string_range_byte(buffer_or_string, start, end, &s, &e,
					GB_ALLOW_PAST_ACCESSIBLE);

	buffer_or_string_extent_info_force(buffer_or_string);
	set_extent_endpoints(ext, s, e, buffer_or_string);
	return extent;
}

/************************************************************************/
/*		           mapping over extents				*/
/************************************************************************/

static unsigned int decode_map_extents_flags(Lisp_Object flags)
{
	unsigned int retval = 0;
	unsigned int all_extents_specified = 0;
	unsigned int in_region_specified = 0;

	if (EQ(flags, Qt))	/* obsoleteness compatibility */
		return ME_END_CLOSED;
	if (NILP(flags))
		return 0;
	if (SYMBOLP(flags))
		flags = Fcons(flags, Qnil);
	while (!NILP(flags)) {
		Lisp_Object sym;
		CHECK_CONS(flags);
		sym = XCAR(flags);
		CHECK_SYMBOL(sym);
		if (EQ(sym, Qall_extents_closed) || EQ(sym, Qall_extents_open)
		    || EQ(sym, Qall_extents_closed_open)
		    || EQ(sym, Qall_extents_open_closed)) {
			if (all_extents_specified)
				error
				    ("Only one `all-extents-*' flag may be specified");
			all_extents_specified = 1;
		}
		if (EQ(sym, Qstart_in_region) || EQ(sym, Qend_in_region) ||
		    EQ(sym, Qstart_and_end_in_region) ||
		    EQ(sym, Qstart_or_end_in_region)) {
			if (in_region_specified)
				error
				    ("Only one `*-in-region' flag may be specified");
			in_region_specified = 1;
		}

		/* I do so love that conditional operator ... */
		retval |=
		    EQ(sym, Qend_closed) ? ME_END_CLOSED :
		    EQ(sym, Qstart_open) ? ME_START_OPEN :
		    EQ(sym, Qall_extents_closed) ? ME_ALL_EXTENTS_CLOSED :
		    EQ(sym, Qall_extents_open) ? ME_ALL_EXTENTS_OPEN :
		    EQ(sym,
		       Qall_extents_closed_open) ? ME_ALL_EXTENTS_CLOSED_OPEN :
		    EQ(sym,
		       Qall_extents_open_closed) ? ME_ALL_EXTENTS_OPEN_CLOSED :
		    EQ(sym, Qstart_in_region) ? ME_START_IN_REGION : EQ(sym,
									Qend_in_region)
		    ? ME_END_IN_REGION : EQ(sym,
					    Qstart_and_end_in_region) ?
		    ME_START_AND_END_IN_REGION : EQ(sym,
						    Qstart_or_end_in_region) ?
		    ME_START_OR_END_IN_REGION : EQ(sym,
						   Qnegate_in_region) ?
		    ME_NEGATE_IN_REGION
		    : (invalid_argument("Invalid `map-extents' flag", sym), 0);

		flags = XCDR(flags);
	}
	return retval;
}

DEFUN("extent-in-region-p", Fextent_in_region_p, 1, 4, 0,	/*
Return whether EXTENT overlaps a specified region.
This is equivalent to whether `map-extents' would visit EXTENT when called
with these args.
*/
      (extent, from, to, flags))
{
	Bytind start, end;
	EXTENT ext = decode_extent(extent, DE_MUST_BE_ATTACHED);
	Lisp_Object obj = extent_object(ext);

	get_buffer_or_string_range_byte(obj, from, to, &start, &end,
					GB_ALLOW_NIL |
					GB_ALLOW_PAST_ACCESSIBLE);

	return extent_in_region_p(ext, start, end,
				  decode_map_extents_flags(flags)) ? Qt : Qnil;
}

struct slow_map_extents_arg {
	Lisp_Object map_arg;
	Lisp_Object map_routine;
	Lisp_Object result;
	Lisp_Object property;
	Lisp_Object value;
};

static int slow_map_extents_function(EXTENT extent, void *arg)
{
	/* This function can GC */
	struct slow_map_extents_arg *closure =
	    (struct slow_map_extents_arg *)arg;
	Lisp_Object extent_obj;

	XSETEXTENT(extent_obj, extent);

	/* make sure this extent qualifies according to the PROPERTY
	   and VALUE args */

	if (!NILP(closure->property)) {
		Lisp_Object value =
		    Fextent_property(extent_obj, closure->property,
				     Qnil);
		if ((NILP(closure->value) && NILP(value)) ||
		    (!NILP(closure->value) && !EQ(value, closure->value)))
			return 0;
	}

	closure->result = call2(closure->map_routine, extent_obj,
				closure->map_arg);
	return !NILP(closure->result);
}

DEFUN("map-extents", Fmap_extents, 1, 8, 0,	/*
Map FUNCTION over the extents which overlap a region in OBJECT.
OBJECT is normally a buffer or string but could be an extent (see below).
The region is normally bounded by [FROM, TO) (i.e. the beginning of the
region is closed and the end of the region is open), but this can be
changed with the FLAGS argument (see below for a complete discussion).

FUNCTION is called with the arguments (extent, MAPARG).  The arguments
OBJECT, FROM, TO, MAPARG, and FLAGS are all optional and default to
the current buffer, the beginning of OBJECT, the end of OBJECT, nil,
and nil, respectively.  `map-extents' returns the first non-nil result
produced by FUNCTION, and no more calls to FUNCTION are made after it
returns non-nil.

If OBJECT is an extent, FROM and TO default to the extent's endpoints,
and the mapping omits that extent and its predecessors.  This feature
supports restarting a loop based on `map-extents'.  Note: OBJECT must
be attached to a buffer or string, and the mapping is done over that
buffer or string.

An extent overlaps the region if there is any point in the extent that is
also in the region. (For the purpose of overlap, zero-length extents and
regions are treated as closed on both ends regardless of their endpoints'
specified open/closedness.) Note that the endpoints of an extent or region
are considered to be in that extent or region if and only if the
corresponding end is closed.  For example, the extent [5,7] overlaps the
region [2,5] because 5 is in both the extent and the region.  However, (5,7]
does not overlap [2,5] because 5 is not in the extent, and neither [5,7] nor
\(5,7] overlaps the region [2,5) because 5 is not in the region.

The optional FLAGS can be a symbol or a list of one or more symbols,
modifying the behavior of `map-extents'.  Allowed symbols are:

end-closed              The region's end is closed.

start-open              The region's start is open.

all-extents-closed      Treat all extents as closed on both ends for the
purpose of determining whether they overlap the
region, irrespective of their actual open- or
closedness.
all-extents-open        Treat all extents as open on both ends.
all-extents-closed-open Treat all extents as start-closed, end-open.
all-extents-open-closed Treat all extents as start-open, end-closed.

start-in-region         In addition to the above conditions for extent
overlap, the extent's start position must lie within
the specified region.  Note that, for this
condition, open start positions are treated as if
0.5 was added to the endpoint's value, and open
end positions are treated as if 0.5 was subtracted
from the endpoint's value.
end-in-region           The extent's end position must lie within the
region.
start-and-end-in-region Both the extent's start and end positions must lie
within the region.
start-or-end-in-region  Either the extent's start or end position must lie
within the region.

negate-in-region        The condition specified by a `*-in-region' flag
must NOT hold for the extent to be considered.

At most one of `all-extents-closed', `all-extents-open',
`all-extents-closed-open', and `all-extents-open-closed' may be specified.

At most one of `start-in-region', `end-in-region',
`start-and-end-in-region', and `start-or-end-in-region' may be specified.

If optional arg PROPERTY is non-nil, only extents with that property set
on them will be visited.  If optional arg VALUE is non-nil, only extents
whose value for that property is `eq' to VALUE will be visited.
*/
      (function, object, from, to, maparg, flags, property, value))
{
	/* This function can GC */
	struct slow_map_extents_arg closure;
	unsigned int me_flags;
	Bytind start, end;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
	EXTENT after = 0;

	if (EXTENTP(object)) {
		after = decode_extent(object, DE_MUST_BE_ATTACHED);
		if (NILP(from))
			from = Fextent_start_position(object);
		if (NILP(to))
			to = Fextent_end_position(object);
		object = extent_object(after);
	} else
		object = decode_buffer_or_string(object);

	get_buffer_or_string_range_byte(object, from, to, &start, &end,
					GB_ALLOW_NIL |
					GB_ALLOW_PAST_ACCESSIBLE);

	me_flags = decode_map_extents_flags(flags);

	if (!NILP(property)) {
		if (!NILP(value))
			value = canonicalize_extent_property(property, value);
	}

	GCPRO5(function, maparg, object, property, value);

	closure.map_arg = maparg;
	closure.map_routine = function;
	closure.result = Qnil;
	closure.property = property;
	closure.value = value;

	map_extents_bytind(start, end, slow_map_extents_function,
			   (void *)&closure, object, after,
			   /* You never know what the user might do ... */
			   me_flags | ME_MIGHT_CALL_ELISP);

	UNGCPRO;
	return closure.result;
}

/************************************************************************/
/*		mapping over extents -- other functions			*/
/************************************************************************/

/* ------------------------------- */
/*      map-extent-children        */
/* ------------------------------- */

struct slow_map_extent_children_arg {
	Lisp_Object map_arg;
	Lisp_Object map_routine;
	Lisp_Object result;
	Lisp_Object property;
	Lisp_Object value;
	Bytind start_min;
	Bytind prev_start;
	Bytind prev_end;
};

static int slow_map_extent_children_function(EXTENT extent, void *arg)
{
	/* This function can GC */
	struct slow_map_extent_children_arg *closure =
	    (struct slow_map_extent_children_arg *)arg;
	Lisp_Object extent_obj;
	Bytind start = extent_endpoint_bytind(extent, 0);
	Bytind end = extent_endpoint_bytind(extent, 1);
	/* Make sure the extent starts inside the region of interest,
	   rather than just overlaps it.
	 */
	if (start < closure->start_min)
		return 0;
	/* Make sure the extent is not a child of a previous visited one.
	   We know already, because of extent ordering,
	   that start >= prev_start, and that if
	   start == prev_start, then end <= prev_end.
	 */
	if (start == closure->prev_start) {
		if (end < closure->prev_end)
			return 0;
	} else {		/* start > prev_start */

		if (start < closure->prev_end)
			return 0;
		/* corner case:  prev_end can be -1 if there is no prev */
	}
	XSETEXTENT(extent_obj, extent);

	/* make sure this extent qualifies according to the PROPERTY
	   and VALUE args */

	if (!NILP(closure->property)) {
		Lisp_Object value =
		    Fextent_property(extent_obj, closure->property,
				     Qnil);
		if ((NILP(closure->value) && NILP(value)) ||
		    (!NILP(closure->value) && !EQ(value, closure->value)))
			return 0;
	}

	closure->result = call2(closure->map_routine, extent_obj,
				closure->map_arg);

	/* Since the callback may change the buffer, compute all stored
	   buffer positions here.
	 */
	closure->start_min = -1;	/* no need for this any more */
	closure->prev_start = extent_endpoint_bytind(extent, 0);
	closure->prev_end = extent_endpoint_bytind(extent, 1);

	return !NILP(closure->result);
}

DEFUN("map-extent-children", Fmap_extent_children, 1, 8, 0,	/*
Map FUNCTION over the extents in the region from FROM to TO.
FUNCTION is called with arguments (extent, MAPARG).  See `map-extents'
for a full discussion of the arguments FROM, TO, and FLAGS.

The arguments are the same as for `map-extents', but this function differs
in that it only visits extents which start in the given region, and also
in that, after visiting an extent E, it skips all other extents which start
inside E but end before E's end.

Thus, this function may be used to walk a tree of extents in a buffer:
(defun walk-extents (buffer &optional ignore)
(map-extent-children 'walk-extents buffer))
*/
      (function, object, from, to, maparg, flags, property, value))
{
	/* This function can GC */
	struct slow_map_extent_children_arg closure;
	unsigned int me_flags;
	Bytind start, end;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
	EXTENT after = 0;

	if (EXTENTP(object)) {
		after = decode_extent(object, DE_MUST_BE_ATTACHED);
		if (NILP(from))
			from = Fextent_start_position(object);
		if (NILP(to))
			to = Fextent_end_position(object);
		object = extent_object(after);
	} else
		object = decode_buffer_or_string(object);

	get_buffer_or_string_range_byte(object, from, to, &start, &end,
					GB_ALLOW_NIL |
					GB_ALLOW_PAST_ACCESSIBLE);

	me_flags = decode_map_extents_flags(flags);

	if (!NILP(property)) {
		if (!NILP(value))
			value = canonicalize_extent_property(property, value);
	}

	GCPRO5(function, maparg, object, property, value);

	closure.map_arg = maparg;
	closure.map_routine = function;
	closure.result = Qnil;
	closure.property = property;
	closure.value = value;
	closure.start_min = start;
	closure.prev_start = -1;
	closure.prev_end = -1;
	map_extents_bytind(start, end, slow_map_extent_children_function,
			   (void *)&closure, object, after,
			   /* You never know what the user might do ... */
			   me_flags | ME_MIGHT_CALL_ELISP);

	UNGCPRO;
	return closure.result;
}

/* ------------------------------- */
/*             extent-at           */
/* ------------------------------- */

/* find "smallest" matching extent containing pos -- (flag == 0) means
   all extents match, else (EXTENT_FLAGS (extent) & flag) must be true;
   for more than one matching extent with precisely the same endpoints,
   we choose the last extent in the extents_list.
   The search stops just before "before", if that is non-null.
   */

struct extent_at_arg {
	Lisp_Object best_match;	/* or list of extents */
	Memind best_start;
	Memind best_end;
	Lisp_Object prop;
	EXTENT before;
	int all_extents;
};

enum extent_at_flag {
	EXTENT_AT_AFTER,
	EXTENT_AT_BEFORE,
	EXTENT_AT_AT
};

static enum extent_at_flag decode_extent_at_flag(Lisp_Object at_flag)
{
	if (NILP(at_flag))
		return EXTENT_AT_AFTER;

	CHECK_SYMBOL(at_flag);
	if (EQ(at_flag, Qafter))
		return EXTENT_AT_AFTER;
	if (EQ(at_flag, Qbefore))
		return EXTENT_AT_BEFORE;
	if (EQ(at_flag, Qat))
		return EXTENT_AT_AT;

	invalid_argument("Invalid AT-FLAG in `extent-at'", at_flag);
	return EXTENT_AT_AFTER;	/* unreached */
}

static int extent_at_mapper(EXTENT e, void *arg)
{
	struct extent_at_arg *closure = (struct extent_at_arg *)arg;

	if (e == closure->before)
		return 1;

	/* If closure->prop is non-nil, then the extent is only acceptable
	   if it has a non-nil value for that property. */
	if (!NILP(closure->prop)) {
		Lisp_Object extent;
		XSETEXTENT(extent, e);
		if (NILP(Fextent_property(extent, closure->prop, Qnil)))
			return 0;
	}

	if (!closure->all_extents) {
		EXTENT current;

		if (NILP(closure->best_match))
			goto accept;
		current = XEXTENT(closure->best_match);
		/* redundant but quick test */
		if (extent_start(current) > extent_start(e))
			return 0;

		/* we return the "last" best fit, instead of the first --
		   this is because then the glyph closest to two equivalent
		   extents corresponds to the "extent-at" the text just past
		   that same glyph */
		else if (!EXTENT_LESS_VALS(e, closure->best_start,
					   closure->best_end))
			goto accept;
		else
			return 0;
	      accept:
		XSETEXTENT(closure->best_match, e);
		closure->best_start = extent_start(e);
		closure->best_end = extent_end(e);
	} else {
		Lisp_Object extent;

		XSETEXTENT(extent, e);
		closure->best_match = Fcons(extent, closure->best_match);
	}

	return 0;
}

static Lisp_Object
extent_at_bytind(Bytind position, Lisp_Object object, Lisp_Object property,
		 EXTENT before, enum extent_at_flag at_flag, int all_extents)
{
	struct extent_at_arg closure;
	struct gcpro gcpro1;

	/* it might be argued that invalid positions should cause
	   errors, but the principle of least surprise dictates that
	   nil should be returned (extent-at is often used in
	   response to a mouse event, and in many cases previous events
	   have changed the buffer contents).

	   Also, the openness stuff in the text-property code currently
	   does not check its limits and might go off the end. */
	if ((at_flag == EXTENT_AT_BEFORE
	     ? position <= buffer_or_string_absolute_begin_byte(object)
	     : position < buffer_or_string_absolute_begin_byte(object))
	    || (at_flag == EXTENT_AT_AFTER
		? position >= buffer_or_string_absolute_end_byte(object)
		: position > buffer_or_string_absolute_end_byte(object)))
		return Qnil;

	closure.best_match = Qnil;
	closure.prop = property;
	closure.before = before;
	closure.all_extents = all_extents;

	GCPRO1(closure.best_match);
	map_extents_bytind(at_flag ==
			   EXTENT_AT_BEFORE ? position - 1 : position,
			   at_flag == EXTENT_AT_AFTER ? position + 1 : position,
			   extent_at_mapper, (void *)&closure, object, 0,
			   ME_START_OPEN | ME_ALL_EXTENTS_CLOSED);
	if (all_extents)
		closure.best_match = Fnreverse(closure.best_match);
	UNGCPRO;

	return closure.best_match;
}

DEFUN("extent-at", Fextent_at, 1, 5, 0,	/*
Find "smallest" extent at POS in OBJECT having PROPERTY set.
Normally, an extent is "at" POS if it overlaps the region (POS, POS+1);
i.e. if it covers the character after POS. (However, see the definition
of AT-FLAG.) "Smallest" means the extent that comes last in the display
order; this normally means the extent whose start position is closest to
POS.  See `next-extent' for more information.
OBJECT specifies a buffer or string and defaults to the current buffer.
PROPERTY defaults to nil, meaning that any extent will do.
Properties are attached to extents with `set-extent-property', which see.
Returns nil if POS is invalid or there is no matching extent at POS.
If the fourth argument BEFORE is not nil, it must be an extent; any returned
extent will precede that extent.  This feature allows `extent-at' to be
used by a loop over extents.
AT-FLAG controls how end cases are handled, and should be one of:

nil or `after'              An extent is at POS if it covers the character
after POS.  This is consistent with the way
that text properties work.
`before'            An extent is at POS if it covers the character
before POS.
`at'                        An extent is at POS if it overlaps or abuts POS.
This includes all zero-length extents at POS.

Note that in all cases, the start-openness and end-openness of the extents
considered is ignored.  If you want to pay attention to those properties,
you should use `map-extents', which gives you more control.
*/
      (pos, object, property, before, at_flag))
{
	Bytind position;
	EXTENT before_extent;
	enum extent_at_flag fl;

	object = decode_buffer_or_string(object);
	position =
	    get_buffer_or_string_pos_byte(object, pos, GB_NO_ERROR_IF_BAD);
	if (NILP(before))
		before_extent = 0;
	else
		before_extent = decode_extent(before, DE_MUST_BE_ATTACHED);
	if (before_extent && !EQ(object, extent_object(before_extent)))
		invalid_argument("extent not in specified buffer or string",
				 object);
	fl = decode_extent_at_flag(at_flag);

	return extent_at_bytind(position, object, property, before_extent, fl,
				0);
}

DEFUN("extents-at", Fextents_at, 1, 5, 0,	/*
Find all extents at POS in OBJECT having PROPERTY set.
Normally, an extent is "at" POS if it overlaps the region (POS, POS+1);
i.e. if it covers the character after POS. (However, see the definition
of AT-FLAG.)
This provides similar functionality to `extent-list', but does so in a way
that is compatible with `extent-at'. (For example, errors due to POS out of
range are ignored; this makes it safer to use this function in response to
a mouse event, because in many cases previous events have changed the buffer
contents.)
OBJECT specifies a buffer or string and defaults to the current buffer.
PROPERTY defaults to nil, meaning that any extent will do.
Properties are attached to extents with `set-extent-property', which see.
Returns nil if POS is invalid or there is no matching extent at POS.
If the fourth argument BEFORE is not nil, it must be an extent; any returned
extent will precede that extent.  This feature allows `extents-at' to be
used by a loop over extents.
AT-FLAG controls how end cases are handled, and should be one of:

nil or `after'            An extent is at POS if it covers the character
after POS.  This is consistent with the way
that text properties work.
`before'          An extent is at POS if it covers the character
before POS.
`at'                      An extent is at POS if it overlaps or abuts POS.
This includes all zero-length extents at POS.

Note that in all cases, the start-openness and end-openness of the extents
considered is ignored.  If you want to pay attention to those properties,
you should use `map-extents', which gives you more control.
*/
      (pos, object, property, before, at_flag))
{
	Bytind position;
	EXTENT before_extent;
	enum extent_at_flag fl;

	object = decode_buffer_or_string(object);
	position =
	    get_buffer_or_string_pos_byte(object, pos, GB_NO_ERROR_IF_BAD);
	if (NILP(before))
		before_extent = 0;
	else
		before_extent = decode_extent(before, DE_MUST_BE_ATTACHED);
	if (before_extent && !EQ(object, extent_object(before_extent)))
		invalid_argument("extent not in specified buffer or string",
				 object);
	fl = decode_extent_at_flag(at_flag);

	return extent_at_bytind(position, object, property, before_extent, fl,
				1);
}

/* ------------------------------- */
/*   verify_extent_modification()  */
/* ------------------------------- */

/* verify_extent_modification() is called when a buffer or string is
   modified to check whether the modification is occuring inside a
   read-only extent.
 */

struct verify_extents_arg {
	Lisp_Object object;
	Memind start;
	Memind end;
	Lisp_Object iro;	/* value of inhibit-read-only */
};

static int verify_extent_mapper(EXTENT extent, void *arg)
{
	struct verify_extents_arg *closure = (struct verify_extents_arg *)arg;
	Lisp_Object prop = extent_read_only(extent);

	if (NILP(prop))
		return 0;

	if (CONSP(closure->iro) && !NILP(Fmemq(prop, closure->iro)))
		return 0;

#if 0				/* Nobody seems to care for this any more -sb */
	/* Allow deletion if the extent is completely contained in
	   the region being deleted.
	   This is important for supporting tokens which are internally
	   write-protected, but which can be killed and yanked as a whole.
	   Ignore open/closed distinctions at this point.
	   -- Rose
	 */
	if (closure->start != closure->end &&
	    extent_start(extent) >= closure->start &&
	    extent_end(extent) <= closure->end)
		return 0;
#endif

	while (1)
		Fsignal(Qbuffer_read_only, (list1(closure->object)));

	RETURN_NOT_REACHED(0)
}

/* Value of Vinhibit_read_only is precomputed and passed in for
   efficiency */

void
verify_extent_modification(Lisp_Object object, Bytind from, Bytind to,
			   Lisp_Object inhibit_read_only_value)
{
	int closed;
	struct verify_extents_arg closure;

	/* If insertion, visit closed-endpoint extents touching the insertion
	   point because the text would go inside those extents.  If deletion,
	   treat the range as open on both ends so that touching extents are not
	   visited.  Note that we assume that an insertion is occurring if the
	   changed range has zero length, and a deletion otherwise.  This
	   fails if a change (i.e. non-insertion, non-deletion) is happening.
	   As far as I know, this doesn't currently occur in XEmacs. --ben */
	closed = (from == to);
	closure.object = object;
	closure.start = buffer_or_string_bytind_to_memind(object, from);
	closure.end = buffer_or_string_bytind_to_memind(object, to);
	closure.iro = inhibit_read_only_value;

	map_extents_bytind(from, to, verify_extent_mapper, (void *)&closure,
			   object, 0, closed ? ME_END_CLOSED : ME_START_OPEN);
}

/* ------------------------------------ */
/*    process_extents_for_insertion()   */
/* ------------------------------------ */

struct process_extents_for_insertion_arg {
	Bytind opoint;
	int length;
	Lisp_Object object;
};

/*   A region of length LENGTH was just inserted at OPOINT.  Modify all
     of the extents as required for the insertion, based on their
     start-open/end-open properties.
 */

static int process_extents_for_insertion_mapper(EXTENT extent, void *arg)
{
	struct process_extents_for_insertion_arg *closure =
	    (struct process_extents_for_insertion_arg *)arg;
	Memind indice = buffer_or_string_bytind_to_memind(closure->object,
							  closure->opoint);

	/* When this function is called, one end of the newly-inserted text should
	   be adjacent to some endpoint of the extent, or disjoint from it.  If
	   the insertion overlaps any existing extent, something is wrong.
	 */
#ifdef ERROR_CHECK_EXTENTS
	if (extent_start(extent) > indice &&
	    extent_start(extent) < indice + closure->length)
		abort();
	if (extent_end(extent) > indice &&
	    extent_end(extent) < indice + closure->length)
		abort();
#endif

	/* The extent-adjustment code adjusted the extent's endpoints as if
	   all extents were closed-open -- endpoints at the insertion point
	   remain unchanged.  We need to fix the other kinds of extents:

	   1. Start position of start-open extents needs to be moved.

	   2. End position of end-closed extents needs to be moved.

	   Note that both conditions hold for zero-length (] extents at the
	   insertion point.  But under these rules, zero-length () extents
	   would get adjusted such that their start is greater than their
	   end; instead of allowing that, we treat them as [) extents by
	   modifying condition #1 to not fire nothing when dealing with a
	   zero-length open-open extent.

	   Existence of zero-length open-open extents is unfortunately an
	   inelegant part of the extent model, but there is no way around
	   it. */

	{
		Memind new_start = extent_start(extent);
		Memind new_end = extent_end(extent);

		if (indice == extent_start(extent)
		    && extent_start_open_p(extent)
		    /* zero-length () extents are exempt; see comment above. */
		    && !(new_start == new_end && extent_end_open_p(extent))
		    )
			new_start += closure->length;
		if (indice == extent_end(extent) && !extent_end_open_p(extent))
			new_end += closure->length;

		set_extent_endpoints_1(extent, new_start, new_end);
	}

	return 0;
}

void
process_extents_for_insertion(Lisp_Object object, Bytind opoint,
			      Bytecount length)
{
	struct process_extents_for_insertion_arg closure;

	closure.opoint = opoint;
	closure.length = length;
	closure.object = object;

	map_extents_bytind(opoint, opoint + length,
			   process_extents_for_insertion_mapper,
			   (void *)&closure, object, 0,
			   ME_END_CLOSED | ME_MIGHT_MODIFY_EXTENTS |
			   ME_INCLUDE_INTERNAL);
}

/* ------------------------------------ */
/*    process_extents_for_deletion()    */
/* ------------------------------------ */

struct process_extents_for_deletion_arg {
	Memind start, end;
	int destroy_included_extents;
};

/* This function is called when we're about to delete the range [from, to].
   Detach all of the extents that are completely inside the range [from, to],
   if they're detachable or open-open. */

static int process_extents_for_deletion_mapper(EXTENT extent, void *arg)
{
	struct process_extents_for_deletion_arg *closure =
	    (struct process_extents_for_deletion_arg *)arg;

	/* If the extent lies completely within the range that
	   is being deleted, then nuke the extent if it's detachable
	   (otherwise, it will become a zero-length extent). */

	if (closure->start <= extent_start(extent) &&
	    extent_end(extent) <= closure->end) {
		if (extent_detachable_p(extent)) {
			if (closure->destroy_included_extents)
				destroy_extent(extent);
			else
				extent_detach(extent);
		}
	}

	return 0;
}

/* DESTROY_THEM means destroy the extents instead of just deleting them.
   It is unused currently, but perhaps might be used (there used to
   be a function process_extents_for_destruction(), #if 0'd out,
   that did the equivalent). */
void
process_extents_for_deletion(Lisp_Object object, Bytind from,
			     Bytind to, int destroy_them)
{
	struct process_extents_for_deletion_arg closure;

	closure.start = buffer_or_string_bytind_to_memind(object, from);
	closure.end = buffer_or_string_bytind_to_memind(object, to);
	closure.destroy_included_extents = destroy_them;

	map_extents_bytind(from, to, process_extents_for_deletion_mapper,
			   (void *)&closure, object, 0,
			   ME_END_CLOSED | ME_MIGHT_MODIFY_EXTENTS);
}

/* ------------------------------- */
/*   report_extent_modification()  */
/* ------------------------------- */
struct report_extent_modification_closure {
	Lisp_Object buffer;
	Bufpos start, end;
	int afterp;
	int speccount;
};

static Lisp_Object report_extent_modification_restore(Lisp_Object buffer)
{
	if (current_buffer != XBUFFER(buffer))
		Fset_buffer(buffer);
	return Qnil;
}

static int report_extent_modification_mapper(EXTENT extent, void *arg)
{
	struct report_extent_modification_closure *closure =
	    (struct report_extent_modification_closure *)arg;
	Lisp_Object exobj, startobj, endobj;
	Lisp_Object hook = (closure->afterp
			    ? extent_after_change_functions(extent)
			    : extent_before_change_functions(extent));
	if (NILP(hook))
		return 0;

	XSETEXTENT(exobj, extent);
	XSETINT(startobj, closure->start);
	XSETINT(endobj, closure->end);

	/* Now that we are sure to call elisp, set up an unwind-protect so
	   inside_change_hook gets restored in case we throw.  Also record
	   the current buffer, in case we change it.  Do the recording only
	   once.

	   One confusing thing here is that our caller never actually calls
	   unbind_to (closure.speccount, Qnil).  This is because
	   map_extents_bytind() unbinds before, and with a smaller
	   speccount.  The additional unbind_to() in
	   report_extent_modification() would cause XEmacs to abort.  */
	if (closure->speccount == -1) {
		closure->speccount = specpdl_depth();
		record_unwind_protect(report_extent_modification_restore,
				      Fcurrent_buffer());
	}

	/* The functions will expect closure->buffer to be the current
	   buffer, so change it if it isn't.  */
	if (current_buffer != XBUFFER(closure->buffer))
		Fset_buffer(closure->buffer);

	/* #### It's a shame that we can't use any of the existing run_hook*
	   functions here.  This is so because all of them work with
	   symbols, to be able to retrieve default values of local hooks.
	   <sigh>

	   #### Idea: we could set up a dummy symbol, and call the hook
	   functions on *that*.  */

	if (!CONSP(hook) || EQ(XCAR(hook), Qlambda))
		call3(hook, exobj, startobj, endobj);
	else {
		Lisp_Object tail;
		EXTERNAL_LIST_LOOP(tail, hook)
		    /* #### Shouldn't this perform the same Fset_buffer() check as
		       above?  */
		    call3(XCAR(tail), exobj, startobj, endobj);
	}
	return 0;
}

void
report_extent_modification(Lisp_Object buffer, Bufpos start, Bufpos end,
			   int afterp)
{
	struct report_extent_modification_closure closure;

	closure.buffer = buffer;
	closure.start = start;
	closure.end = end;
	closure.afterp = afterp;
	closure.speccount = -1;

	map_extents(start, end, report_extent_modification_mapper,
		    (void *)&closure, buffer, NULL, ME_MIGHT_CALL_ELISP);
}

/************************************************************************/
/*			extent properties				*/
/************************************************************************/

static void set_extent_invisible(EXTENT extent, Lisp_Object value)
{
	if (!EQ(extent_invisible(extent), value)) {
		set_extent_invisible_1(extent, value);
		extent_changed_for_redisplay(extent, 1, 1);
	}
}

/* This function does "memoization" -- similar to the interning
   that happens with symbols.  Given a list of faces, an equivalent
   list is returned such that if this function is called twice with
   input that is `equal', the resulting outputs will be `eq'.

   Note that the inputs and outputs are in general *not* `equal' --
   faces in symbol form become actual face objects in the output.
   This is necessary so that temporary faces stay around. */

static Lisp_Object memoize_extent_face_internal(Lisp_Object list)
{
	int len;
	int thelen;
	Lisp_Object cons, thecons;
	Lisp_Object oldtail, tail;
	struct gcpro gcpro1;

	if (NILP(list))
		return Qnil;
	if (!CONSP(list))
		return Fget_face(list);

	/* To do the memoization, we use a hash table mapping from
	   external lists to internal lists.  We do `equal' comparisons
	   on the keys so the memoization works correctly.

	   Note that we canonicalize things so that the keys in the
	   hash table (the external lists) always contain symbols and
	   the values (the internal lists) always contain face objects.

	   We also maintain a "reverse" table that maps from the internal
	   lists to the external equivalents.  The idea here is twofold:

	   1) `extent-face' wants to return a list containing face symbols
	   rather than face objects.
	   2) We don't want things to get quite so messed up if the user
	   maliciously side-effects the returned lists.
	 */

	len = XINT(Flength(list));
	thelen = XINT(Flength(Vextent_face_reusable_list));
	oldtail = Qnil;
	tail = Qnil;
	GCPRO1(oldtail);

	/* We canonicalize the given list into another list.
	   We try to avoid consing except when necessary, so we have
	   a reusable list.
	 */

	if (thelen < len) {
		cons = Vextent_face_reusable_list;
		while (!NILP(XCDR(cons)))
			cons = XCDR(cons);
		XCDR(cons) = Fmake_list(make_int(len - thelen), Qnil);
	} else if (thelen > len) {
		int i;

		/* Truncate the list temporarily so it's the right length;
		   remember the old tail. */
		cons = Vextent_face_reusable_list;
		for (i = 0; i < len - 1; i++)
			cons = XCDR(cons);
		tail = cons;
		oldtail = XCDR(cons);
		XCDR(cons) = Qnil;
	}

	thecons = Vextent_face_reusable_list;
	EXTERNAL_LIST_LOOP(cons, list) {
		Lisp_Object face = Fget_face(XCAR(cons));

		XCAR(thecons) = Fface_name(face);
		thecons = XCDR(thecons);
	}

	list =
	    Fgethash(Vextent_face_reusable_list,
		     Vextent_face_memoize_hash_table, Qnil);
	if (NILP(list)) {
		Lisp_Object symlist =
		    Fcopy_sequence(Vextent_face_reusable_list);
		Lisp_Object facelist =
		    Fcopy_sequence(Vextent_face_reusable_list);

		LIST_LOOP(cons, facelist) {
			XCAR(cons) = Fget_face(XCAR(cons));
		}
		Fputhash(symlist, facelist, Vextent_face_memoize_hash_table);
		Fputhash(facelist, symlist,
			 Vextent_face_reverse_memoize_hash_table);
		list = facelist;
	}

	/* Now restore the truncated tail of the reusable list, if necessary. */
	if (!NILP(tail))
		XCDR(tail) = oldtail;

	UNGCPRO;
	return list;
}

static Lisp_Object external_of_internal_memoized_face(Lisp_Object face)
{
	if (NILP(face))
		return Qnil;
	else if (!CONSP(face))
		return XFACE(face)->name;
	else {
		face = Fgethash(face, Vextent_face_reverse_memoize_hash_table,
				Qunbound);
		assert(!UNBOUNDP(face));
		return face;
	}
}

static Lisp_Object
canonicalize_extent_property(Lisp_Object prop, Lisp_Object value)
{
	if (EQ(prop, Qface) || EQ(prop, Qmouse_face))
		value = (external_of_internal_memoized_face
			 (memoize_extent_face_internal(value)));
	return value;
}

/* Do we need a lisp-level function ? */
DEFUN("set-extent-initial-redisplay-function", Fset_extent_initial_redisplay_function, 2, 2, 0,	/*
Note: This feature is experimental!

Set initial-redisplay-function of EXTENT to the function
FUNCTION.

The first time the EXTENT is (re)displayed, an eval event will be
dispatched calling FUNCTION with EXTENT as its only argument.
*/
      (extent, function))
{
	EXTENT e = decode_extent(extent, DE_MUST_BE_ATTACHED);

	e = extent_ancestor(e);	/* Is this needed? Macro also does chasing! */
	set_extent_initial_redisplay_function(e, function);
	extent_in_red_event_p(e) = 0;	/* If the function changed we can spawn
					   new events */
	extent_changed_for_redisplay(e, 1, 0);	/* Do we need to mark children too ? */

	return function;
}

DEFUN("extent-face", Fextent_face, 1, 1, 0,	/*
Return the name of the face in which EXTENT is displayed, or nil
if the extent's face is unspecified.  This might also return a list
of face names.
*/
      (extent))
{
	Lisp_Object face;

	CHECK_EXTENT(extent);
	face = extent_face(XEXTENT(extent));

	return external_of_internal_memoized_face(face);
}

DEFUN("set-extent-face", Fset_extent_face, 2, 2, 0,	/*
Make the given EXTENT have the graphic attributes specified by FACE.
FACE can also be a list of faces, and all faces listed will apply,
with faces earlier in the list taking priority over those later in the
list.
*/
      (extent, face))
{
	EXTENT e = decode_extent(extent, 0);
	Lisp_Object orig_face = face;

	/* retrieve the ancestor for efficiency and proper redisplay noting. */
	e = extent_ancestor(e);

	face = memoize_extent_face_internal(face);

	extent_face(e) = face;
	extent_changed_for_redisplay(e, 1, 0);

	return orig_face;
}

DEFUN("extent-mouse-face", Fextent_mouse_face, 1, 1, 0,	/*
Return the face used to highlight EXTENT when the mouse passes over it.
The return value will be a face name, a list of face names, or nil
if the extent's mouse face is unspecified.
*/
      (extent))
{
	Lisp_Object face;

	CHECK_EXTENT(extent);
	face = extent_mouse_face(XEXTENT(extent));

	return external_of_internal_memoized_face(face);
}

DEFUN("set-extent-mouse-face", Fset_extent_mouse_face, 2, 2, 0,	/*
Set the face used to highlight EXTENT when the mouse passes over it.
FACE can also be a list of faces, and all faces listed will apply,
with faces earlier in the list taking priority over those later in the
list.
*/
      (extent, face))
{
	EXTENT e;
	Lisp_Object orig_face = face;

	CHECK_EXTENT(extent);
	e = XEXTENT(extent);
	/* retrieve the ancestor for efficiency and proper redisplay noting. */
	e = extent_ancestor(e);

	face = memoize_extent_face_internal(face);

	set_extent_mouse_face(e, face);
	extent_changed_for_redisplay(e, 1, 0);

	return orig_face;
}

void
set_extent_glyph(EXTENT extent, Lisp_Object glyph, int endp,
		 glyph_layout layout)
{
	extent = extent_ancestor(extent);

	if (!endp) {
		set_extent_begin_glyph(extent, glyph);
		extent_begin_glyph_layout(extent) = layout;
	} else {
		set_extent_end_glyph(extent, glyph);
		extent_end_glyph_layout(extent) = layout;
	}

	extent_changed_for_redisplay(extent, 1, 0);
}

static Lisp_Object glyph_layout_to_symbol(glyph_layout layout)
{
	switch (layout) {
	case GL_TEXT:
		return Qtext;
	case GL_OUTSIDE_MARGIN:
		return Qoutside_margin;
	case GL_INSIDE_MARGIN:
		return Qinside_margin;
	case GL_WHITESPACE:
		return Qwhitespace;
	default:
		abort();
		return Qnil;	/* unreached */
	}
}

static glyph_layout symbol_to_glyph_layout(Lisp_Object layout_obj)
{
	if (NILP(layout_obj))
		return GL_TEXT;

	CHECK_SYMBOL(layout_obj);
	if (EQ(layout_obj, Qoutside_margin))
		return GL_OUTSIDE_MARGIN;
	if (EQ(layout_obj, Qinside_margin))
		return GL_INSIDE_MARGIN;
	if (EQ(layout_obj, Qwhitespace))
		return GL_WHITESPACE;
	if (EQ(layout_obj, Qtext))
		return GL_TEXT;

	invalid_argument("Unknown glyph layout type", layout_obj);
	return GL_TEXT;		/* unreached */
}

static Lisp_Object
set_extent_glyph_1(Lisp_Object extent_obj, Lisp_Object glyph, int endp,
		   Lisp_Object layout_obj)
{
	EXTENT extent = decode_extent(extent_obj, 0);
	glyph_layout layout = symbol_to_glyph_layout(layout_obj);

	/* Make sure we've actually been given a valid glyph or it's nil
	   (meaning we're deleting a glyph from an extent). */
	if (!NILP(glyph))
		CHECK_BUFFER_GLYPH(glyph);

	set_extent_glyph(extent, glyph, endp, layout);
	return glyph;
}

DEFUN("set-extent-begin-glyph", Fset_extent_begin_glyph, 2, 3, 0,	/*
Display a bitmap, subwindow or string at the beginning of EXTENT.
BEGIN-GLYPH must be a glyph object.  The layout policy defaults to `text'.
*/
      (extent, begin_glyph, layout))
{
	return set_extent_glyph_1(extent, begin_glyph, 0, layout);
}

DEFUN("set-extent-end-glyph", Fset_extent_end_glyph, 2, 3, 0,	/*
Display a bitmap, subwindow or string at the end of EXTENT.
END-GLYPH must be a glyph object.  The layout policy defaults to `text'.
*/
      (extent, end_glyph, layout))
{
	return set_extent_glyph_1(extent, end_glyph, 1, layout);
}

DEFUN("extent-begin-glyph", Fextent_begin_glyph, 1, 1, 0,	/*
Return the glyph object displayed at the beginning of EXTENT.
If there is none, nil is returned.
*/
      (extent))
{
	return extent_begin_glyph(decode_extent(extent, 0));
}

DEFUN("extent-end-glyph", Fextent_end_glyph, 1, 1, 0,	/*
Return the glyph object displayed at the end of EXTENT.
If there is none, nil is returned.
*/
      (extent))
{
	return extent_end_glyph(decode_extent(extent, 0));
}

DEFUN("set-extent-begin-glyph-layout", Fset_extent_begin_glyph_layout, 2, 2, 0,	/*
Set the layout policy of EXTENT's begin glyph.
Access this using the `extent-begin-glyph-layout' function.
*/
      (extent, layout))
{
	EXTENT e = decode_extent(extent, 0);
	e = extent_ancestor(e);
	extent_begin_glyph_layout(e) = symbol_to_glyph_layout(layout);
	extent_maybe_changed_for_redisplay(e, 1, 0);
	return layout;
}

DEFUN("set-extent-end-glyph-layout", Fset_extent_end_glyph_layout, 2, 2, 0,	/*
Set the layout policy of EXTENT's end glyph.
Access this using the `extent-end-glyph-layout' function.
*/
      (extent, layout))
{
	EXTENT e = decode_extent(extent, 0);
	e = extent_ancestor(e);
	extent_end_glyph_layout(e) = symbol_to_glyph_layout(layout);
	extent_maybe_changed_for_redisplay(e, 1, 0);
	return layout;
}

DEFUN("extent-begin-glyph-layout", Fextent_begin_glyph_layout, 1, 1, 0,	/*
Return the layout policy associated with EXTENT's begin glyph.
Set this using the `set-extent-begin-glyph-layout' function.
*/
      (extent))
{
	EXTENT e = decode_extent(extent, 0);
	return glyph_layout_to_symbol((glyph_layout)
				      extent_begin_glyph_layout(e));
}

DEFUN("extent-end-glyph-layout", Fextent_end_glyph_layout, 1, 1, 0,	/*
Return the layout policy associated with EXTENT's end glyph.
Set this using the `set-extent-end-glyph-layout' function.
*/
      (extent))
{
	EXTENT e = decode_extent(extent, 0);
	return glyph_layout_to_symbol((glyph_layout)
				      extent_end_glyph_layout(e));
}

DEFUN("set-extent-priority", Fset_extent_priority, 2, 2, 0,	/*
Set the display priority of EXTENT to PRIORITY (an integer).
When the extent attributes are being merged for display, the priority
is used to determine which extent takes precedence in the event of a
conflict (two extents whose faces both specify font, for example: the
font of the extent with the higher priority will be used).
Extents are created with priority 0; priorities may be negative.
*/
      (extent, priority))
{
	EXTENT e = decode_extent(extent, 0);

	CHECK_INT(priority);
	e = extent_ancestor(e);
	set_extent_priority(e, XINT(priority));
	extent_maybe_changed_for_redisplay(e, 1, 0);
	return priority;
}

DEFUN("extent-priority", Fextent_priority, 1, 1, 0,	/*
Return the display priority of EXTENT; see `set-extent-priority'.
*/
      (extent))
{
	EXTENT e = decode_extent(extent, 0);
	return make_int(extent_priority(e));
}

DEFUN("set-extent-property", Fset_extent_property, 3, 3, 0,	/*
Change a property of an extent.
PROPERTY may be any symbol; the value stored may be accessed with
the `extent-property' function.
The following symbols have predefined meanings:

detached           Removes the extent from its buffer; setting this is
the same as calling `detach-extent'.

destroyed          Removes the extent from its buffer, and makes it
unusable in the future; this is the same calling
`delete-extent'.

priority           Change redisplay priority; same as `set-extent-priority'.

start-open         Whether the set of characters within the extent is
treated being open on the left, that is, whether
the start position is an exclusive, rather than
inclusive, boundary.  If true, then characters
inserted exactly at the beginning of the extent
will remain outside of the extent; otherwise they
will go into the extent, extending it.

end-open           Whether the set of characters within the extent is
treated being open on the right, that is, whether
the end position is an exclusive, rather than
inclusive, boundary.  If true, then characters
inserted exactly at the end of the extent will
remain outside of the extent; otherwise they will
go into the extent, extending it.

By default, extents have the `end-open' but not the
`start-open' property set.

read-only          Text within this extent will be unmodifiable.

initial-redisplay-function (EXPERIMENTAL)
function to be called the first time (part of) the extent
is redisplayed. It will be called with the extent as its
first argument.
Note: The function will not be called immediately
during redisplay, an eval event will be dispatched.

detachable         Whether the extent gets detached (as with
`detach-extent') when all the text within the
extent is deleted.  This is true by default.  If
this property is not set, the extent becomes a
zero-length extent when its text is deleted. (In
such a case, the `start-open' property is
automatically removed if both the `start-open' and
`end-open' properties are set, since zero-length
extents open on both ends are not allowed.)

face               The face in which to display the text.  Setting
this is the same as calling `set-extent-face'.

mouse-face          If non-nil, the extent will be highlighted in this
face when the mouse moves over it.

pointer            If non-nil, and a valid pointer glyph, this specifies
the shape of the mouse pointer while over the extent.

highlight          Obsolete: Setting this property is equivalent to
setting a `mouse-face' property of `highlight'.
Reading this property returns non-nil if
the extent has a non-nil `mouse-face' property.

duplicable         Whether this extent should be copied into strings,
so that kill, yank, and undo commands will restore
or copy it.  `duplicable' extents are copied from
an extent into a string when `buffer-substring' or
a similar function creates a string.  The extents
in a string are copied into other strings created
from the string using `concat' or `substring'.
When `insert' or a similar function inserts the
string into a buffer, the extents are copied back
into the buffer.

unique             Meaningful only in conjunction with `duplicable'.
When this is set, there may be only one instance
of this extent attached at a time: if it is copied
to the kill ring and then yanked, the extent is
not copied.  If, however, it is killed (removed
from the buffer) and then yanked, it will be
re-attached at the new position.

invisible          If the value is non-nil, text under this extent
may be treated as not present for the purpose of
redisplay, or may be displayed using an ellipsis
or other marker; see `buffer-invisibility-spec'
and `invisible-text-glyph'.  In all cases,
however, the text is still visible to other
functions that examine a buffer's text.

keymap             This keymap is consulted for mouse clicks on this
extent, or keypresses made while point is within the
extent.

copy-function      This is a hook that is run when a duplicable extent
is about to be copied from a buffer to a string (or
the kill ring).  It is called with three arguments,
the extent, and the buffer-positions within it
which are being copied.  If this function returns
nil, then the extent will not be copied; otherwise
it will.

paste-function     This is a hook that is run when a duplicable extent is
about to be copied from a string (or the kill ring)
into a buffer.  It is called with three arguments,
the original extent, and the buffer positions which
the copied extent will occupy.  (This hook is run
after the corresponding text has already been
inserted into the buffer.)  Note that the extent
argument may be detached when this function is run.
If this function returns nil, no extent will be
inserted.  Otherwise, there will be an extent
covering the range in question.

If the original extent is not attached to a buffer,
then it will be re-attached at this range.
Otherwise, a copy will be made, and that copy
attached here.

The copy-function and paste-function are meaningful
only for extents with the `duplicable' flag set,
and if they are not specified, behave as if `t' was
the returned value.  When these hooks are invoked,
the current buffer is the buffer which the extent
is being copied from/to, respectively.

begin-glyph        A glyph to be displayed at the beginning of the extent,
or nil.

end-glyph          A glyph to be displayed at the end of the extent,
or nil.

begin-glyph-layout The layout policy (one of `text', `whitespace',
`inside-margin', or `outside-margin') of the extent's
begin glyph.

end-glyph-layout   The layout policy of the extent's end glyph.

syntax-table       A cons or a syntax table object.  If a cons, the car must
be an integer (interpreted as a syntax code, applicable to
all characters in the extent).  Otherwise, syntax of
characters in the extent is looked up in the syntax table.
You should use the text property API to manipulate this
property.  (This may be required in the future.)
*/
      (extent, property, value))
{
	/* This function can GC if property is `keymap' */
	EXTENT e = decode_extent(extent, 0);

	if (EQ(property, Qread_only))
		set_extent_read_only(e, value);
	else if (EQ(property, Qunique))
		extent_unique_p(e) = !NILP(value);
	else if (EQ(property, Qduplicable))
		extent_duplicable_p(e) = !NILP(value);
	else if (EQ(property, Qinvisible))
		set_extent_invisible(e, value);
	else if (EQ(property, Qdetachable))
		extent_detachable_p(e) = !NILP(value);

	else if (EQ(property, Qdetached)) {
		if (NILP(value))
			error("can only set `detached' to t");
		Fdetach_extent(extent);
	} else if (EQ(property, Qdestroyed)) {
		if (NILP(value))
			error("can only set `destroyed' to t");
		Fdelete_extent(extent);
	} else if (EQ(property, Qpriority))
		Fset_extent_priority(extent, value);
	else if (EQ(property, Qface))
		Fset_extent_face(extent, value);
	else if (EQ(property, Qinitial_redisplay_function))
		Fset_extent_initial_redisplay_function(extent, value);
	else if (EQ(property, Qbefore_change_functions))
		set_extent_before_change_functions(e, value);
	else if (EQ(property, Qafter_change_functions))
		set_extent_after_change_functions(e, value);
	else if (EQ(property, Qmouse_face))
		Fset_extent_mouse_face(extent, value);
	/* Obsolete: */
	else if (EQ(property, Qhighlight))
		Fset_extent_mouse_face(extent, Qhighlight);
	else if (EQ(property, Qbegin_glyph_layout))
		Fset_extent_begin_glyph_layout(extent, value);
	else if (EQ(property, Qend_glyph_layout))
		Fset_extent_end_glyph_layout(extent, value);
	/* For backwards compatibility.  We use begin glyph because it is by
	   far the more used of the two. */
	else if (EQ(property, Qglyph_layout))
		Fset_extent_begin_glyph_layout(extent, value);
	else if (EQ(property, Qbegin_glyph))
		Fset_extent_begin_glyph(extent, value, Qnil);
	else if (EQ(property, Qend_glyph))
		Fset_extent_end_glyph(extent, value, Qnil);
	else if (EQ(property, Qstart_open))
		set_extent_openness(e, !NILP(value), -1);
	else if (EQ(property, Qend_open))
		set_extent_openness(e, -1, !NILP(value));
	/* Support (but don't document...) the obvious *_closed antonyms. */
	else if (EQ(property, Qstart_closed))
		set_extent_openness(e, NILP(value), -1);
	else if (EQ(property, Qend_closed))
		set_extent_openness(e, -1, NILP(value));
	else {
		if (EQ(property, Qkeymap))
			while (!NILP(value) && NILP(Fkeymapp(value)))
				value = wrong_type_argument(Qkeymapp, value);

		external_plist_put(extent_plist_addr(e), property, value, 0,
				   ERROR_ME);
	}

	return value;
}

DEFUN("set-extent-properties", Fset_extent_properties, 2, 2, 0,	/*
Change some properties of EXTENT.
PLIST is a property list.
For a list of built-in properties, see `set-extent-property'.
*/
      (extent, plist))
{
	/* This function can GC, if one of the properties is `keymap' */
	Lisp_Object property, value;
	struct gcpro gcpro1;
	GCPRO1(plist);

	plist = Fcopy_sequence(plist);
	Fcanonicalize_plist(plist, Qnil);

	while (!NILP(plist)) {
		property = Fcar(plist);
		plist = Fcdr(plist);
		value = Fcar(plist);
		plist = Fcdr(plist);
		Fset_extent_property(extent, property, value);
	}
	UNGCPRO;
	return Qnil;
}

DEFUN("extent-property", Fextent_property, 2, 3, 0,	/*
Return EXTENT's value for property PROPERTY.
If no such property exists, DEFAULT is returned.
See `set-extent-property' for the built-in property names.
*/
      (extent, property, default_))
{
	EXTENT e = decode_extent(extent, 0);

	if (EQ(property, Qdetached))
		return extent_detached_p(e) ? Qt : Qnil;
	else if (EQ(property, Qdestroyed))
		return !EXTENT_LIVE_P(e) ? Qt : Qnil;
	else if (EQ(property, Qstart_open))
		return extent_normal_field(e, start_open) ? Qt : Qnil;
	else if (EQ(property, Qend_open))
		return extent_normal_field(e, end_open) ? Qt : Qnil;
	else if (EQ(property, Qunique))
		return extent_normal_field(e, unique) ? Qt : Qnil;
	else if (EQ(property, Qduplicable))
		return extent_normal_field(e, duplicable) ? Qt : Qnil;
	else if (EQ(property, Qdetachable))
		return extent_normal_field(e, detachable) ? Qt : Qnil;
	/* Support (but don't document...) the obvious *_closed antonyms. */
	else if (EQ(property, Qstart_closed))
		return extent_start_open_p(e) ? Qnil : Qt;
	else if (EQ(property, Qend_closed))
		return extent_end_open_p(e) ? Qnil : Qt;
	else if (EQ(property, Qpriority))
		return make_int(extent_priority(e));
	else if (EQ(property, Qread_only))
		return extent_read_only(e);
	else if (EQ(property, Qinvisible))
		return extent_invisible(e);
	else if (EQ(property, Qface))
		return Fextent_face(extent);
	else if (EQ(property, Qinitial_redisplay_function))
		return extent_initial_redisplay_function(e);
	else if (EQ(property, Qbefore_change_functions))
		return extent_before_change_functions(e);
	else if (EQ(property, Qafter_change_functions))
		return extent_after_change_functions(e);
	else if (EQ(property, Qmouse_face))
		return Fextent_mouse_face(extent);
	/* Obsolete: */
	else if (EQ(property, Qhighlight))
		return !NILP(Fextent_mouse_face(extent)) ? Qt : Qnil;
	else if (EQ(property, Qbegin_glyph_layout))
		return Fextent_begin_glyph_layout(extent);
	else if (EQ(property, Qend_glyph_layout))
		return Fextent_end_glyph_layout(extent);
	/* For backwards compatibility.  We use begin glyph because it is by
	   far the more used of the two. */
	else if (EQ(property, Qglyph_layout))
		return Fextent_begin_glyph_layout(extent);
	else if (EQ(property, Qbegin_glyph))
		return extent_begin_glyph(e);
	else if (EQ(property, Qend_glyph))
		return extent_end_glyph(e);
	else {
		Lisp_Object value = external_plist_get(extent_plist_addr(e),
						       property, 0, ERROR_ME);
		return UNBOUNDP(value) ? default_ : value;
	}
}

DEFUN("extent-properties", Fextent_properties, 1, 1, 0,	/*
Return a property list of the attributes of EXTENT.
Do not modify this list; use `set-extent-property' instead.
*/
      (extent))
{
	EXTENT e, anc;
	Lisp_Object result, face, anc_obj;
	glyph_layout layout;

	CHECK_EXTENT(extent);
	e = XEXTENT(extent);
	if (!EXTENT_LIVE_P(e))
		return cons3(Qdestroyed, Qt, Qnil);

	anc = extent_ancestor(e);
	XSETEXTENT(anc_obj, anc);

	/* For efficiency, use the ancestor for all properties except detached */

	result = extent_plist_slot(anc);

	if (!NILP(face = Fextent_face(anc_obj)))
		result = cons3(Qface, face, result);

	if (!NILP(face = Fextent_mouse_face(anc_obj)))
		result = cons3(Qmouse_face, face, result);

	if ((layout = (glyph_layout) extent_begin_glyph_layout(anc)) != GL_TEXT) {
		Lisp_Object sym = glyph_layout_to_symbol(layout);
		result = cons3(Qglyph_layout, sym, result);	/* compatibility */
		result = cons3(Qbegin_glyph_layout, sym, result);
	}

	if ((layout = (glyph_layout) extent_end_glyph_layout(anc)) != GL_TEXT)
		result =
		    cons3(Qend_glyph_layout, glyph_layout_to_symbol(layout),
			  result);

	if (!NILP(extent_end_glyph(anc)))
		result = cons3(Qend_glyph, extent_end_glyph(anc), result);

	if (!NILP(extent_begin_glyph(anc)))
		result = cons3(Qbegin_glyph, extent_begin_glyph(anc), result);

	if (extent_priority(anc) != 0)
		result =
		    cons3(Qpriority, make_int(extent_priority(anc)), result);

	if (!NILP(extent_initial_redisplay_function(anc)))
		result = cons3(Qinitial_redisplay_function,
			       extent_initial_redisplay_function(anc), result);

	if (!NILP(extent_before_change_functions(anc)))
		result = cons3(Qbefore_change_functions,
			       extent_before_change_functions(anc), result);

	if (!NILP(extent_after_change_functions(anc)))
		result = cons3(Qafter_change_functions,
			       extent_after_change_functions(anc), result);

	if (!NILP(extent_invisible(anc)))
		result = cons3(Qinvisible, extent_invisible(anc), result);

	if (!NILP(extent_read_only(anc)))
		result = cons3(Qread_only, extent_read_only(anc), result);

	if (extent_normal_field(anc, end_open))
		result = cons3(Qend_open, Qt, result);

	if (extent_normal_field(anc, start_open))
		result = cons3(Qstart_open, Qt, result);

	if (extent_normal_field(anc, detachable))
		result = cons3(Qdetachable, Qt, result);

	if (extent_normal_field(anc, duplicable))
		result = cons3(Qduplicable, Qt, result);

	if (extent_normal_field(anc, unique))
		result = cons3(Qunique, Qt, result);

	/* detached is not an inherited property */
	if (extent_detached_p(e))
		result = cons3(Qdetached, Qt, result);

	return result;
}

/************************************************************************/
/*			     highlighting				*/
/************************************************************************/

/* The display code looks into the Vlast_highlighted_extent variable to
   correctly display highlighted extents.  This updates that variable,
   and marks the appropriate buffers as needing some redisplay.
 */
static void do_highlight(Lisp_Object extent_obj, int highlight_p)
{
	if ((highlight_p && (EQ(Vlast_highlighted_extent, extent_obj))) ||
	    (!highlight_p && (EQ(Vlast_highlighted_extent, Qnil))))
		return;
	if (EXTENTP(Vlast_highlighted_extent) &&
	    EXTENT_LIVE_P(XEXTENT(Vlast_highlighted_extent))) {
		/* do not recurse on descendants.  Only one extent is highlighted
		   at a time. */
		extent_changed_for_redisplay(XEXTENT(Vlast_highlighted_extent),
					     0, 0);
	}
	Vlast_highlighted_extent = Qnil;
	if (!NILP(extent_obj)
	    && BUFFERP(extent_object(XEXTENT(extent_obj)))
	    && highlight_p) {
		extent_changed_for_redisplay(XEXTENT(extent_obj), 0, 0);
		Vlast_highlighted_extent = extent_obj;
	}
}

DEFUN("force-highlight-extent", Fforce_highlight_extent, 1, 2, 0,	/*
Highlight or unhighlight the given extent.
If the second arg is non-nil, it will be highlighted, else dehighlighted.
This is the same as `highlight-extent', except that it will work even
on extents without the `mouse-face' property.
*/
      (extent, highlight_p))
{
	if (NILP(extent))
		highlight_p = Qnil;
	else
		XSETEXTENT(extent, decode_extent(extent, DE_MUST_BE_ATTACHED));
	do_highlight(extent, !NILP(highlight_p));
	return Qnil;
}

DEFUN("highlight-extent", Fhighlight_extent, 1, 2, 0,	/*
Highlight EXTENT, if it is highlightable.
\(that is, if it has the `mouse-face' property).
If the second arg is non-nil, it will be highlighted, else dehighlighted.
Highlighted extents are displayed as if they were merged with the face
or faces specified by the `mouse-face' property.
*/
      (extent, highlight_p))
{
	if (EXTENTP(extent) && NILP(extent_mouse_face(XEXTENT(extent))))
		return Qnil;
	else
		return Fforce_highlight_extent(extent, highlight_p);
}

/************************************************************************/
/*			   strings and extents				*/
/************************************************************************/

/* copy/paste hooks */

static int
run_extent_copy_paste_internal(EXTENT e, Bufpos from, Bufpos to,
			       Lisp_Object object, Lisp_Object prop)
{
	/* This function can GC */
	Lisp_Object extent;
	Lisp_Object copy_fn;
	XSETEXTENT(extent, e);
	copy_fn = Fextent_property(extent, prop, Qnil);
	if (!NILP(copy_fn)) {
		Lisp_Object flag;
		struct gcpro gcpro1, gcpro2, gcpro3;
		GCPRO3(extent, copy_fn, object);
		if (BUFFERP(object))
			flag = call3_in_buffer(XBUFFER(object), copy_fn, extent,
					       make_int(from), make_int(to));
		else
			flag =
			    call3(copy_fn, extent, make_int(from),
				  make_int(to));
		UNGCPRO;
		if (NILP(flag) || !EXTENT_LIVE_P(XEXTENT(extent)))
			return 0;
	}
	return 1;
}

static int run_extent_copy_function(EXTENT e, Bytind from, Bytind to)
{
	Lisp_Object object = extent_object(e);
	/* This function can GC */
	return run_extent_copy_paste_internal
	    (e, buffer_or_string_bytind_to_bufpos(object, from),
	     buffer_or_string_bytind_to_bufpos(object, to), object,
	     Qcopy_function);
}

static int
run_extent_paste_function(EXTENT e, Bytind from, Bytind to, Lisp_Object object)
{
	/* This function can GC */
	return run_extent_copy_paste_internal
	    (e, buffer_or_string_bytind_to_bufpos(object, from),
	     buffer_or_string_bytind_to_bufpos(object, to), object,
	     Qpaste_function);
}

static void update_extent(EXTENT extent, Bytind from, Bytind to)
{
	set_extent_endpoints(extent, from, to, Qnil);
}

/* Insert an extent, usually from the dup_list of a string which
   has just been inserted.
   This code does not handle the case of undo.
   */
static Lisp_Object
insert_extent(EXTENT extent, Bytind new_start, Bytind new_end,
	      Lisp_Object object, int run_hooks)
{
	/* This function can GC */
	Lisp_Object tmp;

	if (!EQ(extent_object(extent), object))
		goto copy_it;

	if (extent_detached_p(extent)) {
		if (run_hooks &&
		    !run_extent_paste_function(extent, new_start, new_end,
					       object))
			/* The paste-function said don't re-attach this extent here. */
			return Qnil;
		else
			update_extent(extent, new_start, new_end);
	} else {
		Bytind exstart = extent_endpoint_bytind(extent, 0);
		Bytind exend = extent_endpoint_bytind(extent, 1);

		if (exend < new_start || exstart > new_end)
			goto copy_it;
		else {
			new_start = min(exstart, new_start);
			new_end = max(exend, new_end);
			if (exstart != new_start || exend != new_end)
				update_extent(extent, new_start, new_end);
		}
	}

	XSETEXTENT(tmp, extent);
	return tmp;

      copy_it:
	if (run_hooks &&
	    !run_extent_paste_function(extent, new_start, new_end, object))
		/* The paste-function said don't attach a copy of the extent here. */
		return Qnil;
	else {
		XSETEXTENT(tmp,
			   copy_extent(extent, new_start, new_end, object));
		return tmp;
	}
}

DEFUN("insert-extent", Finsert_extent, 1, 5, 0,	/*
Insert EXTENT from START to END in BUFFER-OR-STRING.
BUFFER-OR-STRING defaults to the current buffer if omitted.
This operation does not insert any characters,
but otherwise acts as if there were a replicating extent whose
parent is EXTENT in some string that was just inserted.
Returns the newly-inserted extent.
The fourth arg, NO-HOOKS, can be used to inhibit the running of the
extent's `paste-function' property if it has one.
See documentation on `detach-extent' for a discussion of undo recording.
*/
      (extent, start, end, no_hooks, buffer_or_string))
{
	EXTENT ext = decode_extent(extent, 0);
	Lisp_Object copy;
	Bytind s, e;

	buffer_or_string = decode_buffer_or_string(buffer_or_string);
	get_buffer_or_string_range_byte(buffer_or_string, start, end, &s, &e,
					GB_ALLOW_PAST_ACCESSIBLE);

	copy = insert_extent(ext, s, e, buffer_or_string, NILP(no_hooks));
	if (EXTENTP(copy)) {
		if (extent_duplicable_p(XEXTENT(copy)))
			record_extent(copy, 1);
	}
	return copy;
}

/* adding buffer extents to a string */

struct add_string_extents_arg {
	Bytind from;
	Bytecount length;
	Lisp_Object string;
};

static int add_string_extents_mapper(EXTENT extent, void *arg)
{
	/* This function can GC */
	struct add_string_extents_arg *closure =
	    (struct add_string_extents_arg *)arg;
	Bytecount start = extent_endpoint_bytind(extent, 0) - closure->from;
	Bytecount end = extent_endpoint_bytind(extent, 1) - closure->from;

	if (extent_duplicable_p(extent)) {
		start = max(start, 0);
		end = min(end, closure->length);

		/* Run the copy-function to give an extent the option of
		   not being copied into the string (or kill ring).
		 */
		if (extent_duplicable_p(extent) &&
		    !run_extent_copy_function(extent, start + closure->from,
					      end + closure->from))
			return 0;
		copy_extent(extent, start, end, closure->string);
	}

	return 0;
}

/* Add the extents in buffer BUF from OPOINT to OPOINT+LENGTH to
   the string STRING. */
void
add_string_extents(Lisp_Object string, struct buffer *buf, Bytind opoint,
		   Bytecount length)
{
	/* This function can GC */
	struct add_string_extents_arg closure;
	struct gcpro gcpro1, gcpro2;
	Lisp_Object buffer;

	closure.from = opoint;
	closure.length = length;
	closure.string = string;
	buffer = make_buffer(buf);
	GCPRO2(buffer, string);
	map_extents_bytind(opoint, opoint + length, add_string_extents_mapper,
			   (void *)&closure, buffer, 0,
			   /* ignore extents that just abut the region */
			   ME_END_CLOSED | ME_ALL_EXTENTS_OPEN |
			   /* we are calling E-Lisp (the extent's copy function)
			      so anything might happen */
			   ME_MIGHT_CALL_ELISP);
	UNGCPRO;
}

struct splice_in_string_extents_arg {
	Bytecount pos;
	Bytecount length;
	Bytind opoint;
	Lisp_Object buffer;
};

static int splice_in_string_extents_mapper(EXTENT extent, void *arg)
{
	/* This function can GC */
	struct splice_in_string_extents_arg *closure =
	    (struct splice_in_string_extents_arg *)arg;
	/* BASE_START and BASE_END are the limits in the buffer of the string
	   that was just inserted.

	   NEW_START and NEW_END are the prospective buffer positions of the
	   extent that is going into the buffer. */
	Bytind base_start = closure->opoint;
	Bytind base_end = base_start + closure->length;
	Bytind new_start = (base_start + extent_endpoint_bytind(extent, 0) -
			    closure->pos);
	Bytind new_end = (base_start + extent_endpoint_bytind(extent, 1) -
			  closure->pos);

	if (new_start < base_start)
		new_start = base_start;
	if (new_end > base_end)
		new_end = base_end;
	if (new_end <= new_start)
		return 0;

	if (!extent_duplicable_p(extent))
		return 0;

	if (!inside_undo &&
	    !run_extent_paste_function(extent, new_start, new_end,
				       closure->buffer))
		return 0;
	copy_extent(extent, new_start, new_end, closure->buffer);

	return 0;
}

/* We have just inserted a section of STRING (starting at POS, of
   length LENGTH) into buffer BUF at OPOINT.  Do whatever is necessary
   to get the string's extents into the buffer. */

void
splice_in_string_extents(Lisp_Object string, struct buffer *buf,
			 Bytind opoint, Bytecount length, Bytecount pos)
{
	struct splice_in_string_extents_arg closure;
	struct gcpro gcpro1, gcpro2;
	Lisp_Object buffer;

	buffer = make_buffer(buf);
	closure.opoint = opoint;
	closure.pos = pos;
	closure.length = length;
	closure.buffer = buffer;
	GCPRO2(buffer, string);
	map_extents_bytind(pos, pos + length,
			   splice_in_string_extents_mapper,
			   (void *)&closure, string, 0,
			   /* ignore extents that just abut the region */
			   ME_END_CLOSED | ME_ALL_EXTENTS_OPEN |
			   /* we are calling E-Lisp (the extent's copy function)
			      so anything might happen */
			   ME_MIGHT_CALL_ELISP);
	UNGCPRO;
}

struct copy_string_extents_arg {
	Bytecount new_pos;
	Bytecount old_pos;
	Bytecount length;
	Lisp_Object new_string;
};

struct copy_string_extents_1_arg {
	Lisp_Object parent_in_question;
	EXTENT found_extent;
};

static int copy_string_extents_mapper(EXTENT extent, void *arg)
{
	struct copy_string_extents_arg *closure =
	    (struct copy_string_extents_arg *)arg;
	Bytecount old_start, old_end, new_start, new_end;

	old_start = extent_endpoint_bytind(extent, 0);
	old_end = extent_endpoint_bytind(extent, 1);

	old_start = max(closure->old_pos, old_start);
	old_end = min(closure->old_pos + closure->length, old_end);

	if (old_start >= old_end)
		return 0;

	new_start = old_start + closure->new_pos - closure->old_pos;
	new_end = old_end + closure->new_pos - closure->old_pos;

	copy_extent(extent, new_start, new_end, closure->new_string);
	return 0;
}

/* The string NEW_STRING was partially constructed from OLD_STRING.
   In particular, the section of length LEN starting at NEW_POS in
   NEW_STRING came from the section of the same length starting at
   OLD_POS in OLD_STRING.  Copy the extents as appropriate. */

void
copy_string_extents(Lisp_Object new_string, Lisp_Object old_string,
		    Bytecount new_pos, Bytecount old_pos, Bytecount length)
{
	struct copy_string_extents_arg closure;
	struct gcpro gcpro1, gcpro2;

	closure.new_pos = new_pos;
	closure.old_pos = old_pos;
	closure.new_string = new_string;
	closure.length = length;
	GCPRO2(new_string, old_string);
	map_extents_bytind(old_pos, old_pos + length,
			   copy_string_extents_mapper,
			   (void *)&closure, old_string, 0,
			   /* ignore extents that just abut the region */
			   ME_END_CLOSED | ME_ALL_EXTENTS_OPEN |
			   /* we are calling E-Lisp (the extent's copy function)
			      so anything might happen */
			   ME_MIGHT_CALL_ELISP);
	UNGCPRO;
}

/* Checklist for sanity checking:
   - {kill, yank, copy} at {open, closed} {start, end} of {writable, read-only} extent
   - {kill, copy} & yank {once, repeatedly} duplicable extent in {same, different} buffer
 */

/************************************************************************/
/*				text properties				*/
/************************************************************************/

/* Text properties
   Originally this stuff was implemented in lisp (all of the functionality
   exists to make that possible) but speed was a problem.
 */

Lisp_Object Qtext_prop;
Lisp_Object Qtext_prop_extent_paste_function;

static Lisp_Object
get_text_property_bytind(Bytind position, Lisp_Object prop,
			 Lisp_Object object, enum extent_at_flag fl,
			 int text_props_only)
{
	Lisp_Object extent;

	/* text_props_only specifies whether we only consider text-property
	   extents (those with the 'text-prop property set) or all extents. */
	if (!text_props_only)
		extent = extent_at_bytind(position, object, prop, 0, fl, 0);
	else {
		EXTENT prior = 0;
		while (1) {
			extent =
			    extent_at_bytind(position, object, Qtext_prop,
					     prior, fl, 0);
			if (NILP(extent))
				return Qnil;
			if (EQ
			    (prop, Fextent_property(extent, Qtext_prop, Qnil)))
				break;
			prior = XEXTENT(extent);
		}
	}

	if (!NILP(extent))
		return Fextent_property(extent, prop, Qnil);
	if (!NILP(Vdefault_text_properties))
		return Fplist_get(Vdefault_text_properties, prop, Qnil);
	return Qnil;
}

static Lisp_Object
get_text_property_1(Lisp_Object pos, Lisp_Object prop, Lisp_Object object,
		    Lisp_Object at_flag, int text_props_only)
{
	Bytind position;
	int invert = 0;

	object = decode_buffer_or_string(object);
	position =
	    get_buffer_or_string_pos_byte(object, pos, GB_NO_ERROR_IF_BAD);

	/* We canonicalize the start/end-open/closed properties to the
	   non-default version -- "adding" the default property really
	   needs to remove the non-default one.  See below for more
	   on this. */
	if (EQ(prop, Qstart_closed)) {
		prop = Qstart_open;
		invert = 1;
	}

	if (EQ(prop, Qend_open)) {
		prop = Qend_closed;
		invert = 1;
	}

	{
		Lisp_Object val =
		    get_text_property_bytind(position, prop, object,
					     decode_extent_at_flag(at_flag),
					     text_props_only);
		if (invert)
			val = NILP(val) ? Qt : Qnil;
		return val;
	}
}

DEFUN("get-text-property", Fget_text_property, 2, 4, 0,	/*
Return the value of the PROP property at the given position.
Optional arg OBJECT specifies the buffer or string to look in, and
defaults to the current buffer.
Optional arg AT-FLAG controls what it means for a property to be "at"
a position, and has the same meaning as in `extent-at'.
This examines only those properties added with `put-text-property'.
See also `get-char-property'.
*/
      (pos, prop, object, at_flag))
{
	return get_text_property_1(pos, prop, object, at_flag, 1);
}

DEFUN("get-char-property", Fget_char_property, 2, 4, 0,	/*
Return the value of the PROP property at the given position.
Optional arg OBJECT specifies the buffer or string to look in, and
defaults to the current buffer.
Optional arg AT-FLAG controls what it means for a property to be "at"
a position, and has the same meaning as in `extent-at'.
This examines properties on all extents.
See also `get-text-property'.
*/
      (pos, prop, object, at_flag))
{
	return get_text_property_1(pos, prop, object, at_flag, 0);
}

/* About start/end-open/closed:

   These properties have to be handled specially because of their
   strange behavior.  If I put the "start-open" property on a region,
   then *all* text-property extents in the region have to have their
   start be open.  This is unlike all other properties, which don't
   affect the extents of text properties other than their own.

   So:

   1) We have to map start-closed to (not start-open) and end-open
      to (not end-closed) -- i.e. adding the default is really the
      same as remove the non-default property.  It won't work, for
      example, to have both "start-open" and "start-closed" on
      the same region.
   2) Whenever we add one of these properties, we go through all
      text-property extents in the region and set the appropriate
      open/closedness on them.
   3) Whenever we change a text-property extent for a property,
      we have to make sure we set the open/closedness properly.

      (2) and (3) together rely on, and maintain, the invariant
      that the open/closedness of text-property extents is correct
      at the beginning and end of each operation.
   */

struct put_text_prop_arg {
	Lisp_Object prop, value;	/* The property and value we are storing */
	Bytind start, end;	/* The region into which we are storing it */
	Lisp_Object object;
	Lisp_Object the_extent;	/* Our chosen extent; this is used for
				   communication between subsequent passes. */
	int changed_p;		/* Output: whether we have modified anything */
};

static int put_text_prop_mapper(EXTENT e, void *arg)
{
	struct put_text_prop_arg *closure = (struct put_text_prop_arg *)arg;

	Lisp_Object object = closure->object;
	Lisp_Object value = closure->value;
	Bytind e_start, e_end;
	Bytind start = closure->start;
	Bytind end = closure->end;
	Lisp_Object extent, e_val;
	int is_eq;

	XSETEXTENT(extent, e);

	/* Note: in some cases when the property itself is 'start-open
	   or 'end-closed, the checks to set the openness may do a bit
	   of extra work; but it won't hurt because we then fix up the
	   openness later on in put_text_prop_openness_mapper(). */
	if (!EQ(Fextent_property(extent, Qtext_prop, Qnil), closure->prop))
		/* It's not for this property; do nothing. */
		return 0;

	e_start = extent_endpoint_bytind(e, 0);
	e_end = extent_endpoint_bytind(e, 1);
	e_val = Fextent_property(extent, closure->prop, Qnil);
	is_eq = EQ(value, e_val);

	if (!NILP(value) && NILP(closure->the_extent) && is_eq) {
		/* We want there to be an extent here at the end, and we haven't picked
		   one yet, so use this one.  Extend it as necessary.  We only reuse an
		   extent which has an EQ value for the prop in question to avoid
		   side-effecting the kill ring (that is, we never change the property
		   on an extent after it has been created.)
		 */
		if (e_start != start || e_end != end) {
			Bytind new_start = min(e_start, start);
			Bytind new_end = max(e_end, end);
			set_extent_endpoints(e, new_start, new_end, Qnil);
			/* If we changed the endpoint, then we need to set its
			   openness. */
			set_extent_openness(e, new_start != e_start
					    ? !NILP(get_text_property_bytind
						    (start, Qstart_open, object,
						     EXTENT_AT_AFTER, 1)) : -1,
					    new_end != e_end
					    ? NILP(get_text_property_bytind
						   (end - 1, Qend_closed,
						    object, EXTENT_AT_AFTER, 1))
					    : -1);
			closure->changed_p = 1;
		}
		closure->the_extent = extent;
	}

	/* Even if we're adding a prop, at this point, we want all other extents of
	   this prop to go away (as now they overlap).  So the theory here is that,
	   when we are adding a prop to a region that has multiple (disjoint)
	   occurrences of that prop in it already, we pick one of those and extend
	   it, and remove the others.
	 */

	else if (EQ(extent, closure->the_extent)) {
		/* just in case map-extents hits it again (does that happen?) */
		;
	} else if (e_start >= start && e_end <= end) {
		/* Extent is contained in region; remove it.  Don't destroy or modify
		   it, because we don't want to change the attributes pointed to by the
		   duplicates in the kill ring.
		 */
		extent_detach(e);
		closure->changed_p = 1;
	} else if (!NILP(closure->the_extent) &&
		   is_eq && e_start <= end && e_end >= start) {
		EXTENT te = XEXTENT(closure->the_extent);
		/* This extent overlaps, and has the same prop/value as the extent we've
		   decided to reuse, so we can remove this existing extent as well (the
		   whole thing, even the part outside of the region) and extend
		   the-extent to cover it, resulting in the minimum number of extents in
		   the buffer.
		 */
		Bytind the_start = extent_endpoint_bytind(te, 0);
		Bytind the_end = extent_endpoint_bytind(te, 1);
		if (e_start != the_start &&	/* note AND not OR -- hmm, why is this
						   the case? I think it's because the
						   assumption that the text-property
						   extents don't overlap makes it
						   OK; changing it to an OR would
						   result in changed_p sometimes getting
						   falsely marked.  Is this bad? */
		    e_end != the_end) {
			Bytind new_start = min(e_start, the_start);
			Bytind new_end = max(e_end, the_end);
			set_extent_endpoints(te, new_start, new_end, Qnil);
			/* If we changed the endpoint, then we need to set its
			   openness.  We are setting the endpoint to be the same as
			   that of the extent we're about to remove, and we assume
			   (the invariant mentioned above) that extent has the
			   proper endpoint setting, so we just use it. */
			set_extent_openness(te, new_start != e_start ?
					    (int)extent_start_open_p(e) : -1,
					    new_end != e_end ?
					    (int)extent_end_open_p(e) : -1);
			closure->changed_p = 1;
		}
		extent_detach(e);
	} else if (e_end <= end) {
		/* Extent begins before start but ends before end, so we can just
		   decrease its end position.
		 */
		if (e_end != start) {
			set_extent_endpoints(e, e_start, start, Qnil);
			set_extent_openness(e, -1, NILP(get_text_property_bytind
							(start - 1, Qend_closed,
							 object,
							 EXTENT_AT_AFTER, 1)));
			closure->changed_p = 1;
		}
	} else if (e_start >= start) {
		/* Extent ends after end but begins after start, so we can just
		   increase its start position.
		 */
		if (e_start != end) {
			set_extent_endpoints(e, end, e_end, Qnil);
			set_extent_openness(e, !NILP(get_text_property_bytind
						     (end, Qstart_open, object,
						      EXTENT_AT_AFTER, 1)), -1);
			closure->changed_p = 1;
		}
	} else {
		/* Otherwise, `extent' straddles the region.  We need to split it.
		 */
		set_extent_endpoints(e, e_start, start, Qnil);
		set_extent_openness(e, -1, NILP(get_text_property_bytind
						(start - 1, Qend_closed, object,
						 EXTENT_AT_AFTER, 1)));
		set_extent_openness(copy_extent
				    (e, end, e_end, extent_object(e)),
				    !NILP(get_text_property_bytind
					  (end, Qstart_open, object,
					   EXTENT_AT_AFTER, 1)), -1);
		closure->changed_p = 1;
	}

	return 0;		/* to continue mapping. */
}

static int put_text_prop_openness_mapper(EXTENT e, void *arg)
{
	struct put_text_prop_arg *closure = (struct put_text_prop_arg *)arg;
	Bytind e_start, e_end;
	Bytind start = closure->start;
	Bytind end = closure->end;
	Lisp_Object extent;
	XSETEXTENT(extent, e);
	e_start = extent_endpoint_bytind(e, 0);
	e_end = extent_endpoint_bytind(e, 1);

	if (NILP(Fextent_property(extent, Qtext_prop, Qnil))) {
		/* It's not a text-property extent; do nothing. */
		;
	}
	/* Note end conditions and NILP/!NILP's carefully. */
	else if (EQ(closure->prop, Qstart_open)
		 && e_start >= start && e_start < end)
		set_extent_openness(e, !NILP(closure->value), -1);
	else if (EQ(closure->prop, Qend_closed)
		 && e_end > start && e_end <= end)
		set_extent_openness(e, -1, NILP(closure->value));

	return 0;		/* to continue mapping. */
}

static int
put_text_prop(Bytind start, Bytind end, Lisp_Object object,
	      Lisp_Object prop, Lisp_Object value, int duplicable_p)
{
	/* This function can GC */
	struct put_text_prop_arg closure;

	if (start == end)	/* There are no characters in the region. */
		return 0;

	/* convert to the non-default versions, since a nil property is
	   the same as it not being present. */
	if (EQ(prop, Qstart_closed)) {
		prop = Qstart_open;
		value = NILP(value) ? Qt : Qnil;
	} else if (EQ(prop, Qend_open)) {
		prop = Qend_closed;
		value = NILP(value) ? Qt : Qnil;
	}

	value = canonicalize_extent_property(prop, value);

	closure.prop = prop;
	closure.value = value;
	closure.start = start;
	closure.end = end;
	closure.object = object;
	closure.changed_p = 0;
	closure.the_extent = Qnil;

	map_extents_bytind(start, end,
			   put_text_prop_mapper, (void *)&closure, object, 0,
			   /* get all extents that abut the region */
			   ME_ALL_EXTENTS_CLOSED | ME_END_CLOSED |
			   /* it might QUIT or error if the user has
			      fucked with the extent plist. */
			   /* #### dmoore - I think this should include
			      ME_MIGHT_MOVE_SOE, since the callback function
			      might recurse back into map_extents_bytind. */
			   ME_MIGHT_THROW | ME_MIGHT_MODIFY_EXTENTS);

	/* If we made it through the loop without reusing an extent
	   (and we want there to be one) make it now.
	 */
	if (!NILP(value) && NILP(closure.the_extent)) {
		Lisp_Object extent;

		XSETEXTENT(extent, make_extent_internal(object, start, end));
		closure.changed_p = 1;
		Fset_extent_property(extent, Qtext_prop, prop);
		Fset_extent_property(extent, prop, value);
		if (duplicable_p) {
			extent_duplicable_p(XEXTENT(extent)) = 1;
			Fset_extent_property(extent, Qpaste_function,
					     Qtext_prop_extent_paste_function);
		}
		set_extent_openness(XEXTENT(extent),
				    !NILP(get_text_property_bytind
					  (start, Qstart_open, object,
					   EXTENT_AT_AFTER, 1)),
				    NILP(get_text_property_bytind
					 (end - 1, Qend_closed, object,
					  EXTENT_AT_AFTER, 1)));
	}

	if (EQ(prop, Qstart_open) || EQ(prop, Qend_closed)) {
		map_extents_bytind(start, end,
				   put_text_prop_openness_mapper,
				   (void *)&closure, object, 0,
				   /* get all extents that abut the region */
				   ME_ALL_EXTENTS_CLOSED | ME_END_CLOSED |
				   ME_MIGHT_MODIFY_EXTENTS);
	}

	return closure.changed_p;
}

DEFUN("put-text-property", Fput_text_property, 4, 5, 0,	/*
Adds the given property/value to all characters in the specified region.
The property is conceptually attached to the characters rather than the
region.  The properties are copied when the characters are copied/pasted.
Fifth argument OBJECT is the buffer or string containing the text, and
defaults to the current buffer.
*/
      (start, end, prop, value, object))
{
	/* This function can GC */
	Bytind s, e;

	object = decode_buffer_or_string(object);
	get_buffer_or_string_range_byte(object, start, end, &s, &e, 0);
	put_text_prop(s, e, object, prop, value, 1);
	return prop;
}

DEFUN("put-nonduplicable-text-property", Fput_nonduplicable_text_property, 4, 5, 0,	/*
Adds the given property/value to all characters in the specified region.
The property is conceptually attached to the characters rather than the
region, however the properties will not be copied when the characters
are copied.
Fifth argument OBJECT is the buffer or string containing the text, and
defaults to the current buffer.
*/
      (start, end, prop, value, object))
{
	/* This function can GC */
	Bytind s, e;

	object = decode_buffer_or_string(object);
	get_buffer_or_string_range_byte(object, start, end, &s, &e, 0);
	put_text_prop(s, e, object, prop, value, 0);
	return prop;
}

DEFUN("add-text-properties", Fadd_text_properties, 3, 4, 0,	/*
Add properties to the characters from START to END.
The third argument PROPS is a property list specifying the property values
to add.  The optional fourth argument, OBJECT, is the buffer or string
containing the text and defaults to the current buffer.  Returns t if
any property was changed, nil otherwise.
*/
      (start, end, props, object))
{
	/* This function can GC */
	int changed = 0;
	Bytind s, e;

	object = decode_buffer_or_string(object);
	get_buffer_or_string_range_byte(object, start, end, &s, &e, 0);
	CHECK_LIST(props);
	for (; !NILP(props); props = Fcdr(Fcdr(props))) {
		Lisp_Object prop = XCAR(props);
		Lisp_Object value = Fcar(XCDR(props));
		changed |= put_text_prop(s, e, object, prop, value, 1);
	}
	return changed ? Qt : Qnil;
}

DEFUN("add-nonduplicable-text-properties", Fadd_nonduplicable_text_properties, 3, 4, 0,	/*
Add nonduplicable properties to the characters from START to END.
\(The properties will not be copied when the characters are copied.)
The third argument PROPS is a property list specifying the property values
to add.  The optional fourth argument, OBJECT, is the buffer or string
containing the text and defaults to the current buffer.  Returns t if
any property was changed, nil otherwise.
*/
      (start, end, props, object))
{
	/* This function can GC */
	int changed = 0;
	Bytind s, e;

	object = decode_buffer_or_string(object);
	get_buffer_or_string_range_byte(object, start, end, &s, &e, 0);
	CHECK_LIST(props);
	for (; !NILP(props); props = Fcdr(Fcdr(props))) {
		Lisp_Object prop = XCAR(props);
		Lisp_Object value = Fcar(XCDR(props));
		changed |= put_text_prop(s, e, object, prop, value, 0);
	}
	return changed ? Qt : Qnil;
}

DEFUN("remove-text-properties", Fremove_text_properties, 3, 4, 0,	/*
Remove the given properties from all characters in the specified region.
PROPS should be a plist, but the values in that plist are ignored (treated
as nil).  Returns t if any property was changed, nil otherwise.
Fourth argument OBJECT is the buffer or string containing the text, and
defaults to the current buffer.
*/
      (start, end, props, object))
{
	/* This function can GC */
	int changed = 0;
	Bytind s, e;

	object = decode_buffer_or_string(object);
	get_buffer_or_string_range_byte(object, start, end, &s, &e, 0);
	CHECK_LIST(props);
	for (; !NILP(props); props = Fcdr(Fcdr(props))) {
		Lisp_Object prop = XCAR(props);
		changed |= put_text_prop(s, e, object, prop, Qnil, 1);
	}
	return changed ? Qt : Qnil;
}

/* Whenever a text-prop extent is pasted into a buffer (via `yank' or `insert'
   or whatever) we attach the properties to the buffer by calling
   `put-text-property' instead of by simply allowing the extent to be copied or
   re-attached.  Then we return nil, telling the extents code not to attach it
   again.  By handing the insertion hackery in this way, we make kill/yank
   behave consistently with put-text-property and not fragment the extents
   (since text-prop extents must partition, not overlap).

   The lisp implementation of this was probably fast enough, but since I moved
   the rest of the put-text-prop code here, I moved this as well for
   completeness.
 */
DEFUN("text-prop-extent-paste-function", Ftext_prop_extent_paste_function, 3, 3, 0,	/*
Used as the `paste-function' property of `text-prop' extents.
*/
      (extent, from, to))
{
	/* This function can GC */
	Lisp_Object prop, val;

	prop = Fextent_property(extent, Qtext_prop, Qnil);
	if (NILP(prop))
		signal_type_error(Qinternal_error,
				  "Internal error: no text-prop", extent);
	val = Fextent_property(extent, prop, Qnil);
#if 0
	/* removed by bill perry, 2/9/97
	 ** This little bit of code would not allow you to have a text property
	 ** with a value of Qnil.  This is bad bad bad.
	 */
	if (NILP(val))
		signal_type_error_2(Qinternal_error,
				    "Internal error: no text-prop",
				    extent, prop);
#endif
	Fput_text_property(from, to, prop, val, Qnil);
	return Qnil;		/* important! */
}

/* This function could easily be written in Lisp but the C code wants
   to use it in connection with invisible extents (at least currently).
   If this changes, consider moving this back into Lisp. */

DEFUN("next-single-property-change", Fnext_single_property_change, 2, 4, 0,	/*
Return the position of next property change for a specific property.
Scans characters forward from POS till it finds a change in the PROP
property, then returns the position of the change.  The optional third
argument OBJECT is the buffer or string to scan (defaults to the current
buffer).
The property values are compared with `eq'.
Return nil if the property is constant all the way to the end of OBJECT.
If the value is non-nil, it is a position greater than POS, never equal.

If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.
If two or more extents with conflicting non-nil values for PROP overlap
a particular character, it is undefined which value is considered to be
the value of PROP. (Note that this situation will not happen if you always
use the text-property primitives.)
*/
      (pos, prop, object, limit))
{
	Bufpos bpos;
	Bufpos blim;
	Lisp_Object extent, value;
	int limit_was_nil;

	object = decode_buffer_or_string(object);
	bpos = get_buffer_or_string_pos_char(object, pos, 0);
	if (NILP(limit)) {
		blim = buffer_or_string_accessible_end_char(object);
		limit_was_nil = 1;
	} else {
		blim = get_buffer_or_string_pos_char(object, limit, 0);
		limit_was_nil = 0;
	}

	extent = Fextent_at(make_int(bpos), object, prop, Qnil, Qnil);
	if (!NILP(extent))
		value = Fextent_property(extent, prop, Qnil);
	else
		value = Qnil;

	while (1) {
		bpos = XINT(Fnext_extent_change(make_int(bpos), object));
		if (bpos >= blim)
			break;	/* property is the same all the way to the end */
		extent = Fextent_at(make_int(bpos), object, prop, Qnil, Qnil);
		if ((NILP(extent) && !NILP(value)) ||
		    (!NILP(extent) && !EQ(value,
					  Fextent_property(extent, prop,
							   Qnil))))
			return make_int(bpos);
	}

	/* I think it's more sensible for this function to return nil always
	   in this situation and it used to do it this way, but it's been changed
	   for FSF compatibility. */
	if (limit_was_nil)
		return Qnil;
	else
		return make_int(blim);
}

/* See comment on previous function about why this is written in C. */

DEFUN("previous-single-property-change", Fprevious_single_property_change, 2, 4, 0,	/*
Return the position of next property change for a specific property.
Scans characters backward from POS till it finds a change in the PROP
property, then returns the position of the change.  The optional third
argument OBJECT is the buffer or string to scan (defaults to the current
buffer).
The property values are compared with `eq'.
Return nil if the property is constant all the way to the start of OBJECT.
If the value is non-nil, it is a position less than POS, never equal.

If the optional fourth argument LIMIT is non-nil, don't search back
past position LIMIT; return LIMIT if nothing is found until LIMIT.
If two or more extents with conflicting non-nil values for PROP overlap
a particular character, it is undefined which value is considered to be
the value of PROP. (Note that this situation will not happen if you always
use the text-property primitives.)
*/
      (pos, prop, object, limit))
{
	Bufpos bpos;
	Bufpos blim;
	Lisp_Object extent, value;
	int limit_was_nil;

	object = decode_buffer_or_string(object);
	bpos = get_buffer_or_string_pos_char(object, pos, 0);
	if (NILP(limit)) {
		blim = buffer_or_string_accessible_begin_char(object);
		limit_was_nil = 1;
	} else {
		blim = get_buffer_or_string_pos_char(object, limit, 0);
		limit_was_nil = 0;
	}

	/* extent-at refers to the character AFTER bpos, but we want the
	   character before bpos.  Thus the - 1.  extent-at simply
	   returns nil on bogus positions, so not to worry. */
	extent = Fextent_at(make_int(bpos - 1), object, prop, Qnil, Qnil);
	if (!NILP(extent))
		value = Fextent_property(extent, prop, Qnil);
	else
		value = Qnil;

	while (1) {
		bpos = XINT(Fprevious_extent_change(make_int(bpos), object));
		if (bpos <= blim)
			break;	/* property is the same all the way to the beginning */
		extent =
		    Fextent_at(make_int(bpos - 1), object, prop, Qnil, Qnil);
		if ((NILP(extent) && !NILP(value))
		    || (!NILP(extent)
			&& !EQ(value, Fextent_property(extent, prop, Qnil))))
			return make_int(bpos);
	}

	/* I think it's more sensible for this function to return nil always
	   in this situation and it used to do it this way, but it's been changed
	   for FSF compatibility. */
	if (limit_was_nil)
		return Qnil;
	else
		return make_int(blim);
}

#ifdef MEMORY_USAGE_STATS

int
compute_buffer_extent_usage(struct buffer *b, struct overhead_stats *ovstats)
{
	/* #### not yet written */
	return 0;
}

#endif				/* MEMORY_USAGE_STATS */

/************************************************************************/
/*				initialization				*/
/************************************************************************/

void syms_of_extents(void)
{
	INIT_LRECORD_IMPLEMENTATION(extent);
	INIT_LRECORD_IMPLEMENTATION(extent_info);
	INIT_LRECORD_IMPLEMENTATION(extent_auxiliary);

	defsymbol(&Qextentp, "extentp");
	defsymbol(&Qextent_live_p, "extent-live-p");

	defsymbol(&Qall_extents_closed, "all-extents-closed");
	defsymbol(&Qall_extents_open, "all-extents-open");
	defsymbol(&Qall_extents_closed_open, "all-extents-closed-open");
	defsymbol(&Qall_extents_open_closed, "all-extents-open-closed");
	defsymbol(&Qstart_in_region, "start-in-region");
	defsymbol(&Qend_in_region, "end-in-region");
	defsymbol(&Qstart_and_end_in_region, "start-and-end-in-region");
	defsymbol(&Qstart_or_end_in_region, "start-or-end-in-region");
	defsymbol(&Qnegate_in_region, "negate-in-region");

	defsymbol(&Qdetached, "detached");
	defsymbol(&Qdestroyed, "destroyed");
	defsymbol(&Qbegin_glyph, "begin-glyph");
	defsymbol(&Qend_glyph, "end-glyph");
	defsymbol(&Qstart_open, "start-open");
	defsymbol(&Qend_open, "end-open");
	defsymbol(&Qstart_closed, "start-closed");
	defsymbol(&Qend_closed, "end-closed");
	defsymbol(&Qread_only, "read-only");
	/* defsymbol (&Qhighlight, "highlight"); in faces.c */
	defsymbol(&Qunique, "unique");
	defsymbol(&Qduplicable, "duplicable");
	defsymbol(&Qdetachable, "detachable");
	defsymbol(&Qpriority, "priority");
	defsymbol(&Qmouse_face, "mouse-face");
	defsymbol(&Qinitial_redisplay_function, "initial-redisplay-function");

	defsymbol(&Qglyph_layout, "glyph-layout");	/* backwards compatibility */
	defsymbol(&Qbegin_glyph_layout, "begin-glyph-layout");
	defsymbol(&Qend_glyph_layout, "end-glyph-layout");
	defsymbol(&Qoutside_margin, "outside-margin");
	defsymbol(&Qinside_margin, "inside-margin");
	defsymbol(&Qwhitespace, "whitespace");
	/* Qtext defined in general.c */

	defsymbol(&Qpaste_function, "paste-function");
	defsymbol(&Qcopy_function, "copy-function");

	defsymbol(&Qtext_prop, "text-prop");
	defsymbol(&Qtext_prop_extent_paste_function,
		  "text-prop-extent-paste-function");

	DEFSUBR(Fextentp);
	DEFSUBR(Fextent_live_p);
	DEFSUBR(Fextent_detached_p);
	DEFSUBR(Fextent_start_position);
	DEFSUBR(Fextent_end_position);
	DEFSUBR(Fextent_object);
	DEFSUBR(Fextent_length);

	DEFSUBR(Fmake_extent);
	DEFSUBR(Fcopy_extent);
	DEFSUBR(Fdelete_extent);
	DEFSUBR(Fdetach_extent);
	DEFSUBR(Fset_extent_endpoints);
	DEFSUBR(Fnext_extent);
	DEFSUBR(Fprevious_extent);
#if DEBUG_SXEMACS
	DEFSUBR(Fnext_e_extent);
	DEFSUBR(Fprevious_e_extent);
#endif
	DEFSUBR(Fnext_extent_change);
	DEFSUBR(Fprevious_extent_change);

	DEFSUBR(Fextent_parent);
	DEFSUBR(Fextent_children);
	DEFSUBR(Fset_extent_parent);

	DEFSUBR(Fextent_in_region_p);
	DEFSUBR(Fmap_extents);
	DEFSUBR(Fmap_extent_children);
	DEFSUBR(Fextent_at);
	DEFSUBR(Fextents_at);

	DEFSUBR(Fset_extent_initial_redisplay_function);
	DEFSUBR(Fextent_face);
	DEFSUBR(Fset_extent_face);
	DEFSUBR(Fextent_mouse_face);
	DEFSUBR(Fset_extent_mouse_face);
	DEFSUBR(Fset_extent_begin_glyph);
	DEFSUBR(Fset_extent_end_glyph);
	DEFSUBR(Fextent_begin_glyph);
	DEFSUBR(Fextent_end_glyph);
	DEFSUBR(Fset_extent_begin_glyph_layout);
	DEFSUBR(Fset_extent_end_glyph_layout);
	DEFSUBR(Fextent_begin_glyph_layout);
	DEFSUBR(Fextent_end_glyph_layout);
	DEFSUBR(Fset_extent_priority);
	DEFSUBR(Fextent_priority);
	DEFSUBR(Fset_extent_property);
	DEFSUBR(Fset_extent_properties);
	DEFSUBR(Fextent_property);
	DEFSUBR(Fextent_properties);

	DEFSUBR(Fhighlight_extent);
	DEFSUBR(Fforce_highlight_extent);

	DEFSUBR(Finsert_extent);

	DEFSUBR(Fget_text_property);
	DEFSUBR(Fget_char_property);
	DEFSUBR(Fput_text_property);
	DEFSUBR(Fput_nonduplicable_text_property);
	DEFSUBR(Fadd_text_properties);
	DEFSUBR(Fadd_nonduplicable_text_properties);
	DEFSUBR(Fremove_text_properties);
	DEFSUBR(Ftext_prop_extent_paste_function);
	DEFSUBR(Fnext_single_property_change);
	DEFSUBR(Fprevious_single_property_change);
}

void reinit_vars_of_extents(void)
{
	extent_auxiliary_defaults.begin_glyph = Qnil;
	extent_auxiliary_defaults.end_glyph = Qnil;
	extent_auxiliary_defaults.parent = Qnil;
	extent_auxiliary_defaults.children = Qnil;
	extent_auxiliary_defaults.priority = 0;
	extent_auxiliary_defaults.invisible = Qnil;
	extent_auxiliary_defaults.read_only = Qnil;
	extent_auxiliary_defaults.mouse_face = Qnil;
	extent_auxiliary_defaults.initial_redisplay_function = Qnil;
	extent_auxiliary_defaults.before_change_functions = Qnil;
	extent_auxiliary_defaults.after_change_functions = Qnil;
}

void vars_of_extents(void)
{
	reinit_vars_of_extents();

	DEFVAR_INT("mouse-highlight-priority", &mouse_highlight_priority	/*
The priority to use for the mouse-highlighting pseudo-extent
that is used to highlight extents with the `mouse-face' attribute set.
See `set-extent-priority'.
										 */ );
	/* Set mouse-highlight-priority (which ends up being used both for the
	   mouse-highlighting pseudo-extent and the primary selection extent)
	   to a very high value because very few extents should override it.
	   1000 gives lots of room below it for different-prioritized extents.
	   10 doesn't. ediff, for example, likes to use priorities around 100.
	   --ben */
	mouse_highlight_priority = /* 10 */ 1000;

	DEFVAR_LISP("default-text-properties", &Vdefault_text_properties	/*
Property list giving default values for text properties.
Whenever a character does not specify a value for a property, the value
stored in this list is used instead.  This only applies when the
functions `get-text-property' or `get-char-property' are called.
										 */ );
	Vdefault_text_properties = Qnil;

	staticpro(&Vlast_highlighted_extent);
	Vlast_highlighted_extent = Qnil;

	Vextent_face_reusable_list = Fcons(Qnil, Qnil);
	staticpro(&Vextent_face_reusable_list);
}

void complex_vars_of_extents(void)
{
	staticpro(&Vextent_face_memoize_hash_table);
	/* The memoize hash table maps from lists of symbols to lists of
	   faces.  It needs to be `equal' to implement the memoization.
	   The reverse table maps in the other direction and just needs
	   to do `eq' comparison because the lists of faces are already
	   memoized. */
	Vextent_face_memoize_hash_table =
	    make_lisp_hash_table(100, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQUAL);
	staticpro(&Vextent_face_reverse_memoize_hash_table);
	Vextent_face_reverse_memoize_hash_table =
	    make_lisp_hash_table(100, HASH_TABLE_KEY_WEAK, HASH_TABLE_EQ);
}
