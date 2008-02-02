/* Line number cache.
   Copyright (C) 1997 Free Software Foundation, Inc.

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

/* To calculate the line numbers, redisplay must count the newlines
   from a known position.  This used to be BUF_BEGV, but this made the
   line numbering extremely slow for large buffers, because Emacs had
   to rescan the whole buffer at each redisplay.

   To make line numbering efficient, we maintain a buffer-local cache
   of recently used positions and their line numbers.  The cache is
   implemented as a small ring of cache positions.  A cache position
   is either nil or a cons of a buffer position (marker) and the
   corresponding line number.

   When calculating the line numbers, this cache is consulted if it
   would otherwise take too much time to count the newlines in the
   buffer (see the comment to buffer_line_number().)

   Insertion and deletions that contain/delete newlines invalidate the
   cached positions after the insertion point.  This guarantees
   relatively fast line numbers caching (even in buffers where point
   moves a lot), and low memory usage.  All of this is done only in
   the buffers where the cache is actually initialized -- i.e. where
   line-numbering is on, and you move the point farther than
   LINE_NUMBER_FAR from the beginning of buffer.  In this sense, the
   cache is lazy -- if you don't use it, you don't pay for it.

   NOTE: line-number cache should not be confused with line-start
   cache.  Line-start cache (a part of redisplay) works with the
   display lines, whereas this works with the buffer lines (literally
   counting the newlines).  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"

#include "line-number.h"

/* #### The following three values could stand more exploration for
   best performance.  */

/* Size of the ring.  The current code expects this to be a small
   number.  If you make it larger, you should probably optimize the
   code below to keep it sorted. */
#define LINE_NUMBER_RING_SIZE 8

/* How much traversal has to be exceeded for two points to be
   considered "far" from each other.  When two points are far, cache
   will be used.  */
#define LINE_NUMBER_FAR 16384

/* How large a string has to be to give up searching it for newlines,
   before change. */
#define LINE_NUMBER_LARGE_STRING 256

/* To be used only when you *know* the cache has been allocated!  */
#define LINE_NUMBER_RING(b) (XCAR ((b)->text->line_number_cache))
#define LINE_NUMBER_BEGV(b) (XCDR ((b)->text->line_number_cache))

/* Initialize the cache.  Cache is (in pseudo-BNF):

   CACHE		= nil | INITIALIZED-CACHE
   INITIALIZED-CACHE	= cons (RING, BEGV-LINE)
   RING			= vector (*RING-ELEMENT)
   RING-ELEMENT		= nil | RING-PAIR
   RING-PAIR		= cons (marker, integer)
   BEGV-LINE		= integer

   Line number cache should never, ever, be visible to Lisp (because
   destructively modifying its elements can cause crashes.)  Debug it
   using debug_print (current_buffer->text->last_number_cache).  */
static void allocate_line_number_cache(struct buffer *b)
{
	b->text->line_number_cache =
	    Fcons(make_vector(LINE_NUMBER_RING_SIZE, Qnil), Qzero);
	narrow_line_number_cache(b);
}

/* Flag LINE_NUMBER_BEGV (b) as dirty.  Do it only if the line number
   cache is already initialized.  */
void narrow_line_number_cache(struct buffer *b)
{
	if (NILP(b->text->line_number_cache))
		return;

	if (BUF_BEG(b) == BUF_BEGV(b))
		/* The is the case Fwiden and save_restriction_restore.  Since we
		   know the correct value, we can update it now.  */
		LINE_NUMBER_BEGV(b) = Qzero;
	else
		/* Calculating the line number of BUF_BEGV here is a bad idea,
		   because there is absolutely no reason to do it before the next
		   redisplay.  We simply mark it as dirty instead.  */
		LINE_NUMBER_BEGV(b) = make_int(-1);
}

/* Invalidate the line number cache positions that lie after POS. */
static void invalidate_line_number_cache(struct buffer *b, Bufpos pos)
{
	EMACS_INT i, j;
	Lisp_Object *ring = XVECTOR_DATA(LINE_NUMBER_RING(b));

	for (i = 0; i < LINE_NUMBER_RING_SIZE; i++) {
		if (!CONSP(ring[i]))
			break;
		/* As the marker stays behind the insertions, this check might
		   as well be `>'.  However, Finsert_before_markers can advance
		   the marker anyway, which bites in shell buffers.

		   #### This forces recreation of the cached marker (and
		   recalculation of newlines) every time a newline is inserted
		   at point, which is way losing.  Isn't there a way to make a
		   marker impervious to Finsert_before_markers()??  Maybe I
		   should convert the code to use extents.  */
		if (marker_position(XCAR(ring[i])) >= pos) {
			/* Get the marker out of the way.  */
			Fset_marker(XCAR(ring[i]), Qnil, Qnil);
			/* ...and shift the ring elements, up to the first nil.  */
			for (j = i;
			     !NILP(ring[j]) && j < LINE_NUMBER_RING_SIZE - 1;
			     j++)
				ring[j] = ring[j + 1];
			ring[j] = Qnil;
			/* Must recheck position i. */
			i--;
		}
	}
}

/* Invalidate the cache positions after POS, if the string to be
   inserted contains a newline.  If the string is too large (larger
   than LINE_NUMBER_LARGE_STRING), invalidate the cache positions
   after POS without prior search.

   This will do nothing if the cache is uninitialized.  */
void
insert_invalidate_line_number_cache(struct buffer *b, Bufpos pos,
				    const Bufbyte * nonreloc, Bytecount length)
{
	if (NILP(b->text->line_number_cache))
		return;

	if (length > LINE_NUMBER_LARGE_STRING ||
	    /* We could also count how many newlines there are in the string
	       and update the cache accordingly, but it would be too much
	       work for too little gain. */
	    memchr((const void *)nonreloc, '\n', (size_t) length))
		invalidate_line_number_cache(b, pos);
}

/* Invalidate the cache positions after FROM, if the region to be
   deleted contains a newline.  If the region-to-be-deleted is larger
   than LINE_NUMBER_LARGE_STRING, invalidate the cache positions after
   FROM without unconditionally.

   This will do nothing if the cache is uninitialized.  */
void
delete_invalidate_line_number_cache(struct buffer *b, Bufpos from, Bufpos to)
{
	if (NILP(b->text->line_number_cache))
		return;

	if ((to - from) > LINE_NUMBER_LARGE_STRING)
		invalidate_line_number_cache(b, from);
	else {
		EMACS_INT shortage;
		scan_buffer(b, '\n', from, to, 1, &shortage, 0);
		if (!shortage)
			invalidate_line_number_cache(b, from);
	}
}

/* Get the nearest known position we know the line number of
   (i.e. BUF_BEGV, and cached positions).  The return position will be
   either closer than BEG, or BEG.  The line of this known position
   will be stored in LINE.

   *LINE should be initialized to the line number of BEG (normally,
   BEG will be BUF_BEGV, and *LINE will be XINT (LINE_NUMBER_BEGV).
   This will initialize the cache, if necessary.  */
static void
get_nearest_line_number(struct buffer *b, Bufpos * beg, Bufpos pos,
			EMACS_INT * line)
{
	EMACS_INT i;
	Lisp_Object *ring = XVECTOR_DATA(LINE_NUMBER_RING(b));
	Charcount length = pos - *beg;

	if (length < 0)
		length = -length;

	/* Find the ring entry closest to POS, if it is closer than BEG. */
	for (i = 0; i < LINE_NUMBER_RING_SIZE && CONSP(ring[i]); i++) {
		Bufpos newpos = marker_position(XCAR(ring[i]));
		Charcount howfar = newpos - pos;
		if (howfar < 0)
			howfar = -howfar;
		if (howfar < length) {
			length = howfar;
			*beg = newpos;
			*line = XINT(XCDR(ring[i]));
		}
	}
}

/* Add a (POS . LINE) pair to the ring, and rotate it. */
static void add_position_to_cache(struct buffer *b, Bufpos pos, EMACS_INT line)
{
	Lisp_Object *ring = XVECTOR_DATA(LINE_NUMBER_RING(b));
	int i = LINE_NUMBER_RING_SIZE - 1;

	/* Set the last marker in the ring to point nowhere. */
	if (CONSP(ring[i]))
		Fset_marker(XCAR(ring[i]), Qnil, Qnil);

	/* Rotate the ring... */
	for (; i > 0; i--)
		ring[i] = ring[i - 1];

	/* ...and update it. */
	ring[0] = Fcons(Fset_marker(Fmake_marker(), make_int(pos),
				    make_buffer(b)), make_int(line));
}

/* Calculate the line number in buffer B at position POS.  If CACHEP
   is non-zero, initialize and facilitate the line-number cache.  The
   line number of the first line is 0.  If narrowing is in effect,
   count the lines are counted from the beginning of the visible
   portion of the buffer.

   The cache works as follows: To calculate the line number, we need
   two positions: position of point (POS) and the position from which
   to count newlines (BEG).  We start by setting BEG to BUF_BEGV.  If
   this would require too much searching (i.e. pos - BUF_BEGV >
   LINE_NUMBER_FAR), try to find a closer position in the ring.  If it
   is found, use that position for BEG, and increment the line number
   appropriately.

   If the calculation (with or without the cache lookup) required more
   than LINE_NUMBER_FAR characters of traversal, update the cache.  */
EMACS_INT buffer_line_number(struct buffer *b, Bufpos pos, int cachep)
{
	Bufpos beg = BUF_BEGV(b);
	EMACS_INT cached_lines = 0;
	EMACS_INT shortage, line;

	if ((pos > beg ? pos - beg : beg - pos) <= LINE_NUMBER_FAR)
		cachep = 0;

	if (cachep) {
		if (NILP(b->text->line_number_cache))
			allocate_line_number_cache(b);
		/* If we don't know the line number of BUF_BEGV, calculate it now.  */
		if (XINT(LINE_NUMBER_BEGV(b)) == -1) {
			LINE_NUMBER_BEGV(b) = Qzero;
			/* #### This has a side-effect of changing the cache.  */
			LINE_NUMBER_BEGV(b) =
			    make_int(buffer_line_number(b, BUF_BEGV(b), 1));
		}
		cached_lines = XINT(LINE_NUMBER_BEGV(b));
		get_nearest_line_number(b, &beg, pos, &cached_lines);
	}

	scan_buffer(b, '\n', beg, pos,
		    pos > beg ? EMACS_INT_MAX : -EMACS_INT_MAX, &shortage, 0);

	line = EMACS_INT_MAX - shortage;
	if (beg > pos)
		line = -line;
	line += cached_lines;

	if (cachep) {
		/* If too far, update the cache. */
		if ((pos > beg ? pos - beg : beg - pos) > LINE_NUMBER_FAR)
			add_position_to_cache(b, pos, line);
		/* Account for narrowing.  If cache is not used, this is
		   unnecessary, because we counted from BUF_BEGV anyway.  */
		line -= XINT(LINE_NUMBER_BEGV(b));
	}

	return line;
}
