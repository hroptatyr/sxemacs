/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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

/* Emacs uses a lot of different display attributes; for example, assume
   that only four fonts are in use (normal, bold, italic, and bold-italic).
   Then assume that one stipple or background is used for text selections,
   and another is used for highlighting mousable regions.  That makes 16
   GCs already.  Add in the fact that another GC may be needed to display
   the text cursor in any of those regions, and you've got 32.  Add in
   more fonts, and it keeps increasing exponentially.

   We used to keep these GCs in a cache of merged (fully qualified) faces.
   However, a lot of other code in xterm.c used XChangeGC of existing GCs,
   which is kind of slow and kind of random.  Also, managing the face cache
   was tricky because it was hard to know when a face was no longer visible
   on the frame -- we had to mark all frames as garbaged whenever a face
   was changed, which caused an unpleasant amount of flicker (since faces are
   created/destroyed (= changed) whenever a frame is created/destroyed.

   So this code maintains a cache at the GC level instead of at the face
   level.  There is an upper limit on the size of the cache, after which we
   will stop creating GCs and start reusing them (reusing the least-recently-
   used ones first).  So if faces get changed, their GCs will eventually be
   recycled.  Also more sharing of GCs is possible.

   This code uses hash tables.  It could be that, if the cache size is small
   enough, a linear search might be faster; but I doubt it, since we need
   `equal' comparisons, not `eq', and I expect that the optimal cache size
   will be ~100.

   Written by jwz, 14 jun 93
 */

#include <config.h>
#include <X11/Xlib.h>
#include "xgccache.h"

#define GC_CACHE_SIZE 100

#define GCCACHE_HASH

#ifdef GCCACHE_HASH
#include "lisp.h"
#include "hash.h"
#endif

struct gcv_and_mask {
	XGCValues gcv;
	unsigned long mask;
};

struct gc_cache_cell {
	GC gc;
	struct gcv_and_mask gcvm;
	struct gc_cache_cell *prev, *next;
};

struct gc_cache {
	Display *dpy;		/* used only as arg to XCreateGC/XFreeGC */
	Window window;		/* used only as arg to XCreateGC */
	int size;
	struct gc_cache_cell *head;
	struct gc_cache_cell *tail;
#ifdef GCCACHE_HASH
	struct hash_table *table;
#endif

	int create_count;
	int delete_count;
};

#ifdef GCCACHE_HASH
static long unsigned int
gc_cache_hash(const void *arg)
{
	const struct gcv_and_mask *gcvm = (const struct gcv_and_mask *)arg;
	const long unsigned int *longs = (const long unsigned int*)&gcvm->gcv;
	long unsigned int hash = gcvm->mask;
	size_t i;
	/* This could look at the mask and only use the used slots in the
	   hash code.  That would win in that we wouldn't have to initialize
	   every slot of the gcv when calling gc_cache_lookup.  But we need
	   the hash function to be as fast as possible; some timings should
	   be done. */
	for (i = 0; i < (sizeof(XGCValues) / sizeof(long unsigned int)); i++) {
		hash = (hash << 1) ^ *longs++;
	}
	return hash;
}

#endif				/* GCCACHE_HASH */

static int gc_cache_eql(const void *arg1, const void *arg2)
{
	/* See comment in gc_cache_hash */
	return !memcmp(arg1, arg2, sizeof(struct gcv_and_mask));
}

struct gc_cache *make_gc_cache(Display * dpy, Window window)
{
	struct gc_cache *cache = xnew(struct gc_cache);
	cache->dpy = dpy;
	cache->window = window;
	cache->size = 0;
	cache->head = cache->tail = 0;
	cache->create_count = cache->delete_count = 0;
#ifdef GCCACHE_HASH
	cache->table =
	    make_general_hash_table(GC_CACHE_SIZE, gc_cache_hash, gc_cache_eql);
#endif
	return cache;
}

void free_gc_cache(struct gc_cache *volatile cache)
{
	struct gc_cache_cell *volatile rest, *volatile next;
	rest = cache->head;
	while (rest) {
		XFreeGC(cache->dpy, rest->gc);
		next = rest->next;
		xfree(rest);
		rest = next;
	}
#ifdef GCCACHE_HASH
	free_hash_table(cache->table);
#endif
	xfree(cache);
}

GC gc_cache_lookup(struct gc_cache *cache, XGCValues * gcv, unsigned long mask)
{
	struct gc_cache_cell *cell = NULL, *next = NULL, *prev = NULL;
	struct gcv_and_mask gcvm;

	if (cache == NULL)
		abort();
	else if ((!!cache->head) != (!!cache->tail))
		abort();
	else if (cache->head && cache->tail && (cache->head->prev || cache->tail->next))
		abort();
	else {
		gcvm.mask = mask;
		gcvm.gcv = *gcv;	/* this copies... */

#ifdef GCCACHE_HASH

		if (gethash(&gcvm, cache->table,
			    (const void **)((void*)&cell)))
#else				/* !GCCACHE_HASH */

		/* start at the end (most recently used) */
		cell = cache->tail;
		while (cell) {
			if (gc_cache_eql(&gcvm, &cell->gcvm))
				break;
			else
				cell = cell->prev;
		}

		/* #### This whole file needs some serious overhauling. */
		if (!(mask | GCTile) && cell->gc->values.tile)
			cell = NULL;
		else if (!(mask | GCStipple) && cell->gc->values.stipple)
			cell = NULL;
#endif				/* !GCCACHE_HASH */

		{
			/* Found a cell.  Move this cell to the end of
			   the list, so that it will be less likely to
			   be collected than a cell that was accessed
			   less recently.
			*/

			if (!cell) {
				abort();
				return NULL;
			} else {
				if (cell == cache->tail)
					return cell->gc;
				next = cell->next;
				prev = cell->prev;
				if (prev)
					prev->next = next;
				if (next)
					next->prev = prev;
				if (cache->head == cell)
					cache->head = next;
				cell->next = NULL;
				cell->prev = cache->tail;
				if (cache->tail)
					cache->tail->next = cell;
				else
					abort();
				cache->tail = cell;
				if (cache->head == cell)
					abort();
				else if (cell->next)
					abort();
				else if (cache->head != NULL && cache->head->prev)
					abort();
				else if (cache->tail != NULL && cache->tail->next)
					abort();
				return cell->gc;
			}
		}

		/* else, cache miss. */
		if (cache == NULL)
			abort();
		else if (cache->size == GC_CACHE_SIZE)
			/* Reuse the first cell on the list
			   (least-recently-used).  Remove it from the
			   list, and unhash it from the table.
			*/
		{
			cell = cache->head;
			if (cache->head != NULL) {
				cache->head = cell->next;
				cache->head->prev = 0;
			}
			if (cache->tail == cell)
				cache->tail = 0;	/* only one */
			XFreeGC(cache->dpy, cell->gc);
			cache->delete_count++;
#ifdef GCCACHE_HASH
			remhash(&cell->gcvm, cache->table);
#endif
		} else if (cache->size > GC_CACHE_SIZE)
			abort();
		else {
			/* Allocate a new cell (don't put it in the
			   list or table yet). */
			cell = xnew(struct gc_cache_cell);
			cache->size++;
		}
		if (cell != NULL) {
			/* Now we've got a cell (new or reused).  Fill
			   it in. */
			memcpy(&cell->gcvm.gcv, gcv, sizeof(XGCValues));
			cell->gcvm.mask = mask;

			/* Put the cell on the end of the list. */
			cell->next = 0;
			cell->prev = cache->tail;
			if (cache->tail)
				cache->tail->next = cell;
			cache->tail = cell;
			if (!cache->head)
				cache->head = cell;
			cache->create_count++;
#ifdef GCCACHE_HASH
			/* Hash it in the table */
			puthash(&cell->gcvm, cell, cache->table);
#endif
			/* Now make and return the GC. */
			cell->gc = XCreateGC(cache->dpy, cache->window,
					     mask, gcv);
			/* debug */
			assert(cell->gc == gc_cache_lookup(cache, gcv, mask));
			return cell->gc;
		}
	}
	return NULL; /* No cell determined */
}

#ifdef DEBUG_SXEMACS

void describe_gc_cache(struct gc_cache *cache);
void describe_gc_cache(struct gc_cache *cache)
{
	int count = 0;
	struct gc_cache_cell *cell = cache->head;
	stderr_out("\nsize:    %d", cache->size);
	stderr_out("\ncreated: %d", cache->create_count);
	stderr_out("\ndeleted: %d", cache->delete_count);
	while (cell) {
		struct gc_cache_cell *cell2;
		int i = 0;
		stderr_out("\n%d:\t0x%lx  GC: 0x%08lx  hash: 0x%08lx\n",
			   count, (long)cell, (long)cell->gc,
			   gc_cache_hash(&cell->gcvm));
		for (cell2 = cache->head; cell2; cell2 = cell2->next, i++)
			if (count != i &&
			    gc_cache_hash(&cell->gcvm) ==
			    gc_cache_hash(&cell2->gcvm))
				stderr_out("\tHASH COLLISION with cell %d\n",
					   i);
		stderr_out("\tmask:       %8lx\n", cell->gcvm.mask);

#define FROB(field) do {						\
  if ((int)cell->gcvm.gcv.field != (~0))				\
    stderr_out ("\t%-12s%8x\n", #field ":", (int)cell->gcvm.gcv.field);	\
} while (0)
		FROB(function);
		FROB(plane_mask);
		FROB(foreground);
		FROB(background);
		FROB(line_width);
		FROB(line_style);
		FROB(cap_style);
		FROB(join_style);
		FROB(fill_style);
		FROB(fill_rule);
		FROB(arc_mode);
		FROB(tile);
		FROB(stipple);
		FROB(ts_x_origin);
		FROB(ts_y_origin);
		FROB(font);
		FROB(subwindow_mode);
		FROB(graphics_exposures);
		FROB(clip_x_origin);
		FROB(clip_y_origin);
		FROB(clip_mask);
		FROB(dash_offset);
#undef FROB

		count++;
		if (cell->next && cell == cache->tail)
			stderr_out("\nERROR!  tail is here!\n\n");
		else if (!cell->next && cell != cache->tail)
			stderr_out("\nERROR!  tail is not at the end\n\n");
		cell = cell->next;
	}
	if (count != cache->size)
		stderr_out("\nERROR!  count should be %d\n\n", cache->size);
}

#endif				/* DEBUG_SXEMACS */
