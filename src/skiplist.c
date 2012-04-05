/*** skiplist.c -- Pugh's Skiplists
 *
 * Copyright (C) 2006, 2007, 2008 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <hroptatyr@sxemacs.org>
 *
 * This file is part of SXEmacs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ***/

/* Synched up with: Not in FSF. */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"

/* for __ase_ffs() */
#include "ent/ent.h"
#include "skiplist.h"

#define __SKIPLIST_DEBUG__(args...)	fprintf(stderr, "SKIPLIST " args)
#ifndef SKIPLIST_DEBUG_FLAG
#define SL_DEBUG(args...)
#else
#define SL_DEBUG(args...)		__SKIPLIST_DEBUG__(args)
#endif
#define SL_CRITICAL(args...)		__SKIPLIST_DEBUG__("CRITICAL: " args)
#define SL_DEBUG_LEVEL(args...)		SL_DEBUG("[level]: " args)
#define SL_DEBUG_NODE(args...)		SL_DEBUG("[node]: " args)
#define SL_DEBUG_DATA(args...)		SL_DEBUG("[data]: " args)
#define SL_DEBUG_PATH(args...)		SL_DEBUG("[path]: " args)

#define ALIGNED(n)	__attribute__((aligned(n), packed))

Lisp_Object Qskiplistp;


struct skiplist_data_s {
	hcode_t hash;
	Lisp_Object key;
	Lisp_Object value;
};

struct skiplist_level_s {
	skiplist_level_t nextnode;  /* pointer to neighbour node */
	skiplist_level_t nextlevel; /* pointer to above level */
	skiplist_node_t node;	/* parent */
};

struct skiplist_node_s {
	skiplist_level_t head;	/* pointer to first level */
	skiplist_level_t foot;	/* pointer to lowest level */
	size_t nlevels;		/* number of levels in this node */

	struct skiplist_data_s data;	/* pointer to node's data cell */
};

/* paths stuff, just internal */
#define skiplist_path_nil		(skiplist_level_t)NULL

/* inlines */
static inline size_t
skiplist_size(const skiplist_t sl)
	__attribute__((always_inline));
static inline size_t
skiplist_size(const skiplist_t sl)
{
	/* what a fooking name, no? */
	return (size_t)skiplist_nnodes(sl);
}

/* static bindings */
/* low level bindings */
static skiplist_level_t make_skiplist_levels(skiplist_node_t, size_t);

static inline int
skiplist_find_key_path(skiplist_t, Lisp_Object, skiplist_level_t[])
	__attribute__((always_inline));
static inline int
skiplist_find_hash_path(skiplist_t, hcode_t, skiplist_level_t[])
	__attribute__((always_inline));
static inline skiplist_level_t
skiplist_find_hash_return_level(skiplist_t, hcode_t)
	__attribute__((always_inline));

/* high level bindings */

/* low level bindings */
#define node_head_level(node)	(node)->head
#define node_foot_level(node)	(node)->foot
#define node_data(node)		(node)->data
#define node_nlevels(node)	(node)->nlevels
#define node_data_hash(node)	(node)->data.hash
#define node_data_key(node)	(node)->data.key
#define node_data_value(node)	(node)->data.value

#define next_node(level)	(level)->nextnode
#define next_level(level)	(level)->nextlevel
#define parent_node(level)	(level)->node

#define next_hash(level)	node_data_hash(parent_node(next_node(level)))
#define next_key(level)		node_data_key(parent_node(next_node(level)))
#define next_value(level)	node_data_value(parent_node(next_node(level)))

#define level_hash(l)							\
	(l ? (l->node) ? (l->node->data.hash) : 0 : 0)
#define level_key(l)							\
	(l ? (l->node) ? (l->node->data.key) : 0 : 0)

/* static hcode_t skiplist_hash(Lisp_Object); */
#define skiplist_hash(obj)	internal_hash((obj), 0)
#define skiplist_keyeq(o1, o2)	internal_equal((o1), (o2), 0)

/* this is p^n - 1 */
/* here: p=2 n=2 */
#define SL_PROBABILITY_MASK	3


static inline skiplist_level_t
make_skiplist_levels(skiplist_node_t node, size_t nlevels)
	__attribute__((always_inline));
static inline skiplist_level_t
make_skiplist_levels(skiplist_node_t node, size_t nlevels)
{
/* creates an array of NLEVEL levels and returns a pointer */
	skiplist_level_t levels =
		xnew_array(struct skiplist_level_s, nlevels+1);

	/* initialise the parent, it's at least one level we hope */
	parent_node(levels) = node;
	next_level(levels) = NULL;
	next_node(levels) = NULL;
	while (nlevels > 0) {
		levels[nlevels].node = node;
		levels[nlevels].nextnode = NULL;
		levels[nlevels].nextlevel = &levels[nlevels-1];
		--nlevels;
	}
	return levels;
}

static inline void
__fill_skiplist_node(skiplist_node_t n, hcode_t h,
		     Lisp_Object key, Lisp_Object value)
	__attribute__((always_inline));
static inline void
__fill_skiplist_node(skiplist_node_t n, hcode_t h,
		     Lisp_Object key, Lisp_Object value)
{
	/* initialise an empty node */
	node_data_hash(n) = h;
	node_data_key(n) = key;
	node_data_value(n) = value;
	return;
}

static inline skiplist_node_t
_make_skiplist_node(hcode_t h, Lisp_Object key, Lisp_Object val, size_t nl)
	__attribute__((always_inline));
static inline skiplist_node_t
_make_skiplist_node(hcode_t h, Lisp_Object key, Lisp_Object val, size_t nl)
{
	skiplist_node_t node = xnew_and_zero(struct skiplist_node_s);
	skiplist_level_t levs = make_skiplist_levels(node, nl);

	/* fill in the values */
	__fill_skiplist_node(node, h, key, val);

	node->nlevels = nl;
	node->foot = levs;
	node->head = &(levs[nl]);
	return node;
}

static inline skiplist_node_t
make_skiplist_node(Lisp_Object key, Lisp_Object value, size_t nlevels)
	__attribute__((always_inline));
static inline skiplist_node_t
make_skiplist_node(Lisp_Object key, Lisp_Object value, size_t nlevels)
{
	return _make_skiplist_node(skiplist_hash(key), key, value, nlevels);
}

static inline skiplist_level_t
raise_head_level(skiplist_t sl)
{
	skiplist_nlevels(sl)++;
	return skiplist_head(sl);
}

static inline skiplist_level_t
lower_head_level(skiplist_t sl)
{
	if (skiplist_nlevels(sl)-- > 0) {
		skiplist_nlevels(sl) = 0;
	}
	return skiplist_head(sl);
}

static inline void
reconcile_levelling(skiplist_t sl)
{
	for (; skiplist_nlevels(sl) > 0; skiplist_nlevels(sl)--) {
		if (next_node(skiplist_head(sl)) != NULL) {
			return;
		}
	}
	return;
}

static inline void
free_skiplist_levels(skiplist_level_t level_array)
	__attribute__((always_inline));
static inline void
free_skiplist_levels(skiplist_level_t level_array)
{
	xfree(level_array);
	return;
}

static inline void
free_skiplist_node(skiplist_node_t node)
	__attribute__((always_inline));
static inline void
free_skiplist_node(skiplist_node_t node)
{
	/* free the level array */
	SL_DEBUG_LEVEL("freeing level array 0x%p", node->foot);
#if defined LET_GCC_BUGS_BITE_US
	/* must be the inline code plus write-combining :( */
	free_skiplist_levels(node->foot);
#else
	xfree(node->foot);
#endif

	xfree(node);
	return;
}

static inline void
add_level_neighbour(skiplist_level_t level, skiplist_level_t neighbour)
	__attribute__((always_inline));
static inline void
add_level_neighbour(skiplist_level_t level, skiplist_level_t neighbour)
{
	next_node(neighbour) = next_node(level);
	next_node(level) = neighbour;
	return;
}

/* higher level bindings */
static inline int
skiplist_find_key_path(skiplist_t slist, Lisp_Object key, skiplist_level_t p[])
{
	return skiplist_find_hash_path(slist, skiplist_hash(key), p);
}

static inline skiplist_level_t
skiplist_find_level(skiplist_t slist, Lisp_Object key)
{
	return skiplist_find_hash_return_level(slist, skiplist_hash(key));
}

static inline skiplist_level_t
pop_node_level(skiplist_node_t node)
{
	skiplist_level_t tmp;

	tmp = node->head;
	if (!tmp)
		return tmp;

	node->head = tmp->nextlevel;
	tmp->nextlevel = NULL;	/* does not make sense for isolated levels */
	node->nlevels--;

	if (node->head == NULL) {
		node->foot = NULL;
	}

	SL_DEBUG_LEVEL("popped level 0x%lx\n", (long unsigned int)tmp);
	return tmp;
}

static inline int
skiplist_find_hash_path(skiplist_t slist, hcode_t hash, skiplist_level_t p[])
{
	skiplist_level_t tmp = NULL;
	hcode_t tmphash;
	/* result is the index of the last element */
	int result = 0;

	if (!(tmp = skiplist_head(slist))) {
		p[0] = NULL;
		return 0;
	}

	p[result] = tmp; /* just push the skiplist head */
	for (; tmp; tmp = next_level(tmp)) {
		for (; next_node(tmp) &&
			     (tmphash = next_hash(tmp)) &&
			     (tmphash < hash);
		     tmp = next_node(tmp)) { }
		p[++result] = tmp;
	}
	return result;
}

static inline skiplist_level_t
skiplist_find_hash_return_level(skiplist_t slist, hcode_t hash)
{
	skiplist_level_t result, tmp;
	hcode_t tmphash;

	if (UNLIKELY((result = tmp = skiplist_head(slist)) == NULL)) {
		return NULL;
	}

	for (; tmp; tmp = next_level(tmp)) {
		for (; next_node(tmp) &&
			     (tmphash = next_hash(tmp)) &&
			     (tmphash < hash);
		     tmp = next_node(tmp) ) {}
		result = tmp;
	}

	return result;
}

/* debugging only ... what's the global #define for it? */
#ifdef SKIPLIST_DEBUG_FLAG
static void
list_skiplist(skiplist_t sl)
{
	skiplist_level_t head, tmp;

	__SKIPLIST_DEBUG__("*** SXEmacs: internal skiplist structure\n");
	head = skiplist_head(sl);
	while (head) {
		tmp = head;
		while (tmp) {
			__SKIPLIST_DEBUG__("%lu->", level_hash(tmp));
			tmp = next_node(tmp);
		}
		__SKIPLIST_DEBUG__("0\n");
		head = next_level(head);
	}
}

/* debugging only */
DEFUN("list-skiplist", Flist_skiplist, 1, 1, 0, /*
Do not use me!
*/
      (skiplist))
{
	list_skiplist(XSKIPLIST(skiplist));

	return Qt;
}
#endif


/* lisp bindings */
static Lisp_Object
mark_skiplist(Lisp_Object obj)
{
	/* traverse the skiplist, we simply use the lowest level since
	 * that should be a single-linked list */
	for (skiplist_level_t tmp = next_node(XSKIPLIST_FOOT(obj));
	     tmp; tmp = next_node(tmp)) {
		mark_object(node_data_key(parent_node(tmp)));
		mark_object(node_data_value(parent_node(tmp)));
	}

	mark_object(XSKIPLIST_PLIST(obj));
	return XSKIPLIST_PLIST(obj);
}

static void
print_skiplist(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	write_fmt_str(printcharfun, "#<skiplist :size %lu  :levels %lu >",
		      (long unsigned int)XSKIPLIST_NNODES(obj),
		      (long unsigned int)XSKIPLIST_NLEVELS(obj));
}

static void
finalise_skiplist(void *header, int SXE_UNUSED(for_disksave))
{
	skiplist_t sl = header;

	SL_DEBUG("*** SXEmacs: skiplist finalisation 0x%lx\n",
		 (long unsigned int)sl);

	/* traverse the skiplist and free all node and data cells */
	for (skiplist_level_t tmp = next_node(skiplist_foot(sl)); tmp; ) {
		volatile skiplist_level_t nex = next_node(tmp);
		SL_DEBUG_NODE("freeing 0x%lx\n",
			      (long unsigned int)tmp->node);
		free_skiplist_node(tmp->node);
		tmp = nex;
	}

	/* free skiplist head levels */
	xfree(sl->headlevs);

	/* and finally commit suicide */
	return;
}

static Lisp_Object
skiplist_getprop(Lisp_Object obj, Lisp_Object property)
{
	return external_plist_get(&XSKIPLIST_PLIST(obj), property, 0, ERROR_ME);
}

static int
skiplist_putprop(Lisp_Object obj, Lisp_Object property, Lisp_Object value)
{
	external_plist_put(&XSKIPLIST_PLIST(obj), property, value, 0, ERROR_ME);
	return 1;
}

static int
skiplist_remprop(Lisp_Object obj, Lisp_Object property)
{
	return external_remprop(&XSKIPLIST_PLIST(obj), property, 0, ERROR_ME);
}

DEFUN("skiplist-plist", Fskiplist_plist, 1, 1, 0, /*
Return the property list of SKIPLIST.
*/
      (skiplist))
{
	CHECK_SKIPLIST(skiplist);
	return XSKIPLIST_PLIST(skiplist);
}

static const struct lrecord_description skiplist_description[] = {
	{XD_OPAQUE_PTR, offsetof(struct skiplist_s, headlevs)},
	{XD_INT, offsetof(struct skiplist_s, nnodes)},
	{XD_INT, offsetof(struct skiplist_s, nlevels)},
	{XD_LISP_OBJECT, offsetof(struct skiplist_s, plist)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("skiplist", skiplist,
					 mark_skiplist, print_skiplist,
					 finalise_skiplist,
					 NULL, NULL,
					 skiplist_description,
					 skiplist_getprop,
					 skiplist_putprop,
					 skiplist_remprop,
					 Fskiplist_plist,
					 struct skiplist_s);

static inline skiplist_t
allocate_skiplist(void)
	__attribute__((always_inline));
static inline skiplist_t
allocate_skiplist(void)
{
	skiplist_t skiplist =
		alloc_lcrecord_type(struct skiplist_s, &lrecord_skiplist);
	return skiplist;
}

Lisp_Object
make_skiplist(void)
{
	skiplist_t sl = allocate_skiplist();
	Lisp_Object result;

	/* the categories are actually seq and dict, but use the per-type
	   implementation for a start */
	sl->lheader.lheader.morphisms = (1<<cat_mk_lc);

	sl->headlevs = make_skiplist_levels(NULL, MAX_SKIPLIST_HEIGHT);
	skiplist_nnodes(sl) = 0;
	skiplist_nlevels(sl) = 0;	/* means 1 actually */
	skiplist_plist(sl) = Qnil;

	XSETSKIPLIST(result, sl);
	return result;
}

/* constructor */
DEFUN("make-skiplist", Fmake_skiplist, 0, 0, 0, /*
Return a new empty skiplist object.
*/
      ())
{
	/* gotta seed our oracle; this is a stupid seed value though */
	return make_skiplist();
}


/* predicate */
DEFUN("skiplistp", Fskiplistp, 1, 1, 0, /*
Return non-nil if OBJECT is a skiplist.
*/
      (object))
{
	if (SKIPLISTP(object)) {
		return Qt;
	} else {
		return Qnil;
	}
}

DEFUN("skiplist-empty-p", Fskiplist_empty_p, 1, 1, 0, /*
Return non-nil if SKIPLIST is empty.
*/
      (skiplist))
{
	CHECK_SKIPLIST(skiplist);

	if (XSKIPLIST_NNODES(skiplist) == 0) {
		return Qt;
	} else {
		return Qnil;
	}
}

/* modifiers and accessors */
static inline void
_put_skiplist(skiplist_t sl, skiplist_level_t *path, size_t psz,
	      hcode_t h, Lisp_Object key, Lisp_Object value)
{
	/* entirely new data, build a node for it */
	/* determine the number of levels to add, this is a log distribution
	 * so we use ffs(3) of a random number */
	size_t nlevels = __ase_ffsl(random());
	size_t cnt;
	skiplist_level_t levels, last = path[psz--];
	skiplist_node_t node;

	node = _make_skiplist_node(h, key, value, nlevels);
	sl->nnodes++;
	levels = node->foot;

	/* and add them */
	add_level_neighbour(last, node->foot /* level[0] */);

	if (LIKELY(nlevels <= psz)) {
		cnt = nlevels;
	} else {
		cnt = psz;
	}
	for (size_t i = 1; i <= cnt; i++) {
		skiplist_level_t level = &levels[i];

		SL_DEBUG_LEVEL("created level 0x%lx\n",
			       (long unsigned int)level);

		last = /* skiplist_path_pop(path) */ path[psz--];
		SL_DEBUG("last 0x%lx  "
			 "level 0x%lx\n",
			 (long unsigned int)last,
			 (long unsigned int)level);
		add_level_neighbour(last, level);
	}
	for (size_t i = cnt+1; i <= nlevels; i++) {
		skiplist_level_t newhlevel = raise_head_level(sl);
		skiplist_level_t level = &levels[i];

		SL_DEBUG("head level 0x%lx  "
			 "level 0x%lx\n",
			 (long unsigned int)newhlevel,
			 (long unsigned int)level);
		add_level_neighbour(newhlevel, level);
	}
	return;
}

void
put_skiplist(skiplist_t sl, Lisp_Object key, Lisp_Object value)
{
	skiplist_level_t last;
	/* C99 we need you */
	skiplist_level_t path[skiplist_nlevels(sl)+2];
	hcode_t hkey = skiplist_hash(key);
	int lastidx;
	hcode_t h;

	last = path[lastidx = skiplist_find_hash_path(sl, hkey, path)];

	if (UNLIKELY(/* skiplist_path_size(path) == 0 */ lastidx == 0 ||
		     /* skiplist_pop(path) == NULL */ last == NULL)) {
		return;
	}

	/* hash this prick */
	h = skiplist_hash(key);

	/* now either we have to insert, or replace */
	/* for that we check if the element right of left is by chance
	 * the thing we look for */
	if (UNLIKELY(h == level_hash(next_node(last)))) {
		skiplist_level_t level = next_node(last);

		if (UNLIKELY(!skiplist_keyeq(key, level_key(level)))) {
			SL_CRITICAL("*** SXEmacs CRITICAL: "
				    "non trivial skiplist collision :(\n");
		}

		/* oh, we have to replace, we just nuke the old
		 * data cell and replace it with the new one
		 * created above */
		SL_DEBUG("*** SXEmacs: skiplist collision, replace\n");
		node_data_key(parent_node(level)) = key;
		node_data_value(parent_node(level)) = value;
		return;
	} else {
		_put_skiplist(sl, path, lastidx, h, key, value);
	}
	return;
}

DEFUN("put-skiplist", Fput_skiplist, 3, 3, 0, /*
Add KEY to the SKIPLIST and assign VALUE.
*/
      (skiplist, key, value))
{
	CHECK_SKIPLIST(skiplist);

	put_skiplist(XSKIPLIST(skiplist), key, value);

	return skiplist;
}

Lisp_Object
get_skiplist(skiplist_t sl, Lisp_Object key, Lisp_Object default_)
{
	skiplist_node_t node;
	skiplist_level_t level;

	if (UNLIKELY((level = skiplist_find_level(sl, key)) == NULL)) {
		return default_;
	}

	/* level points to rightmost and footmost level to the left of key */
	if (next_node(level)) {
		level = next_node(level);
	}

	if (!(node = level->node)) {
		return default_;
	}

	if (!(skiplist_keyeq(node_data_key(node), key))) {
		return default_;
	}

	return node_data_value(node);
}

DEFUN("get-skiplist", Fget_skiplist, 2, 3, 0, /*
Return the value of KEY in SKIPLIST.
If KEY is not an element, return `nil' instead or --
if specified -- DEFAULT.
*/
      (skiplist, key, default_))
{
	CHECK_SKIPLIST(skiplist);

	return get_skiplist(XSKIPLIST(skiplist), key, default_);
}

void
remove_skiplist(skiplist_t sl, Lisp_Object key)
/* remove KEY from SKIPLIST (pathless approach) */
{
	skiplist_node_t node;
	skiplist_level_t level, last;
	/* C99 we need you! */
	skiplist_level_t path[skiplist_nlevels(sl)+2];
	hcode_t hkey = skiplist_hash(key);
	int lastidx;

	lastidx = skiplist_find_hash_path(sl, hkey, path);
	last = path[lastidx];

	if (/* skiplist_path_size(path) == 0 */ lastidx == 0 ||
	    /* skiplist_last(path) == NULL */ path[0] == NULL) {
		return;
	}

	if (hkey == level_hash(next_node(last)) &&
	    skiplist_keyeq(key, level_key(next_node(last)))) {
		node = parent_node(next_node(last));

		/* traverse (bottom-up) the level structure
		 * and free any occurring level pointers */
		while (lastidx > 0 /* because we `popped' the head */ &&
		       (last =
			/* skiplist_path_pop(path) */
			path[lastidx--]) != skiplist_path_nil &&
		       next_node(last) &&
		       parent_node(next_node(last)) == node) {
			level = next_node(last);
			next_node(last) = next_node(level);
		}

		/* free node (kill data cell and levels with it) */
		free_skiplist_node(node);
		/* decrement skiplist size */
		skiplist_nnodes(sl)--;

		/* now, the skiplist head might have many nil pointers
		 * we reduce the overall levelling in that case */
		reconcile_levelling(sl);
	}
	return;
}

DEFUN("remove-skiplist", Fremove_skiplist, 2, 2, 0, /*
Remove the element specified by KEY from SKIPLIST.
If KEY is not an element, this is a no-op.
*/
      (skiplist, key))
{
	CHECK_SKIPLIST(skiplist);

	remove_skiplist(XSKIPLIST(skiplist), key);

	return skiplist;
}

/* C99 where are you? */
bool
skiplist_owns_p(skiplist_t skiplist, Lisp_Object key)
/* return !0 iff SKIPLIST has a node for KEY */
{
	skiplist_node_t node;
	skiplist_level_t level;
	hcode_t hkey = skiplist_hash(key);

	level = skiplist_find_hash_return_level(skiplist, hkey);

	if (level == skiplist_path_nil) {
		return false;
	}

	/* level points to rightmost and footmost level to the left of key */
	if (next_node(level))
		level = next_node(level);

	if ((node = level->node) == NULL) {
		return false;
	}

	if (!(skiplist_keyeq(node_data_key(node), key))) {
		return false;
	}

	return true;
}

DEFUN("skiplist-owns-p", Fskiplist_owns_p, 2, 2, 0, /*
Return non-nil if KEY is associated with a value in SKIPLIST.
*/
      (skiplist, key))
{
	CHECK_SKIPLIST(skiplist);

	return (skiplist_owns_p(XSKIPLIST(skiplist), key) ? Qt : Qnil);
}


/* informational cruft */
DEFUN("skiplist-size", Fskiplist_size, 1, 1, 0, /*
Return the size of SKIPLIST, that is the number of elements.
*/
      (skiplist))
{
	CHECK_SKIPLIST(skiplist);
	return make_int((int32_t)XSKIPLIST_NNODES(skiplist));
}


Lisp_Object
copy_skiplist(skiplist_t skiplist)
{
	Lisp_Object result = make_skiplist();
	skiplist_t sl_copy = XSKIPLIST(result);
	skiplist_level_t tmp;
	Lisp_Object key, val;

	/* traverse the skiplist */
	tmp = next_node(skiplist_foot(skiplist));
	while (tmp) {
		key = node_data_key(parent_node(tmp));
		val = node_data_value(parent_node(tmp));
		put_skiplist(sl_copy, key, val);
		tmp = next_node(tmp);
	}

	return result;
}
DEFUN("copy-skiplist", Fcopy_skiplist, 1, 1, 0,	/*
Return a copy of skiplist SKIPLIST.
The elements of SKIPLIST are not copied; they are shared
with the original.
*/
      (skiplist))
{

	CHECK_SKIPLIST(skiplist);

	return copy_skiplist(XSKIPLIST(skiplist));
}

void unite_skiplist(skiplist_t target, skiplist_t source)
{
	/* unite target and source and store result in target */
	Lisp_Object key, value;
	skiplist_level_t lev;

	lev = next_node(skiplist_foot(source)); /* start at the bottom */
	while (lev) {
		key = node_data_key(parent_node(lev));
		value = node_data_value(parent_node(lev));
		put_skiplist(target, key, value);
		lev = next_node(lev);
	}
}

DEFUN("skiplist-union", Fskiplist_union, 0, MANY, 0, /*
Return the union skiplist of SKIPLISTS.
Args are &rest SKIPLIST.

The union is a skiplist containing all key-value-pairs which are
in at least one of the SKIPLISTS.

Note: Key-value-pairs with equal keys and distinct values are
processed from left to right, that is the final union for such pairs
contains the value of the rightmost skiplist in @var{skiplists}.
*/
      (int nargs, Lisp_Object *args))
{
	int i;
	Lisp_Object result;

	for (i=0; i<nargs; i++)
		CHECK_SKIPLIST(args[i]);

	result = make_skiplist();
	for (i=0; i<nargs; i++) {
		unite_skiplist(XSKIPLIST(result), XSKIPLIST(args[i]));
	}
	return result;
}

void intersect_skiplist(skiplist_t target, skiplist_t source)
{
	/* intersect target and source and store result in target */
	Lisp_Object key;
	skiplist_level_t lev;

	lev = next_node(skiplist_foot(target)); /* start at the bottom */
	while (lev) {
		key = node_data_key(parent_node(lev));
		lev = next_node(lev);
		if (!skiplist_owns_p(source, key)) {
			remove_skiplist(target, key);
		} else {
			lev = next_node(lev);
		}
	}
}

DEFUN("skiplist-intersection", Fskiplist_intersection, 0, MANY, 0, /*
Return the intersection skiplist of SKIPLISTS.
Args are &rest SKIPLIST.

The intersection is a skiplist containing all key-value-pairs
which are in all skiplists of SKIPLISTS.

Note: Key-value-pairs with equal keys and distinct values are
processed from right to left, that is the final intersection for such
pairs contains the value of the leftmost skiplist in SKIPLISTS.
*/
      (int nargs, Lisp_Object *args))
{
	int i;
	Lisp_Object result;

	if (nargs == 0)
		return make_skiplist();

	for (i=0; i<nargs; i++)
		CHECK_SKIPLIST(args[i]);

	result = copy_skiplist(XSKIPLIST(args[0]));
	for (i=1; i<nargs; i++) {
		intersect_skiplist(XSKIPLIST(result), XSKIPLIST(args[i]));
	}
	return result;
}

void
map_skiplist(skiplist_t sl, skiplist_map_f mapf)
{
	skiplist_level_t lev;

	lev = next_node(skiplist_foot(sl)); /* start at the bottom */
	while (lev) {
		Lisp_Object k, v;
		k = node_data_key(parent_node(lev));
		v = node_data_value(parent_node(lev));
		/* apply */
		mapf(k, v);
		lev = next_node(lev);
	}
	return;
}

void
map2_skiplist(skiplist_t sl, skiplist_map2_f mapf, void *ptr)
{
	skiplist_level_t lev;

	lev = next_node(skiplist_foot(sl)); /* start at the bottom */
	while (lev) {
		Lisp_Object k, v;
		k = node_data_key(parent_node(lev));
		v = node_data_value(parent_node(lev));
		/* apply */
		mapf(k, v, ptr);
		lev = next_node(lev);
	}
	return;
}

DEFUN("map-skiplist", Fmap_skiplist, 2, 2, 0,	/*
Map FUNCTION over entries in SKIPLIST, calling it with two args,
each key and value in SKIPLIST.

FUNCTION may not modify SKIPLIST, with the one exception that FUNCTION
may remove or reput the entry currently being processed by FUNCTION.
*/
      (function, skiplist))
{
	skiplist_t sl;
	Lisp_Object args[3];
	skiplist_level_t lev;
	struct gcpro gcpro1, gcpro2;

	CHECK_SKIPLIST(skiplist);

	GCPRO2(function, skiplist);
	sl = XSKIPLIST(skiplist);
	lev = next_node(skiplist_foot(sl)); /* start at the bottom */
	while (lev) {
		args[0] = function;
		args[1] = node_data_key(parent_node(lev));
		args[2] = node_data_value(parent_node(lev));
		/* apply */
		Ffuncall(countof(args), args);
		lev = next_node(lev);
	}

	UNGCPRO;
	return skiplist;
}


/* converters */
DEFUN("skiplist-to-alist", Fskiplist_to_alist, 1, 1, 0, /*
Return the ordinary association list induced by SKIPLIST.
*/
      (skiplist))
{
	Lisp_Object result = Qnil;
	skiplist_level_t tmp;
	Lisp_Object key, val;

	CHECK_SKIPLIST(skiplist);

	/* traverse the skiplist */
	tmp = next_node(XSKIPLIST_FOOT(skiplist));
	while (tmp) {
		key = node_data_key(parent_node(tmp));
		val = node_data_value(parent_node(tmp));
		result = Fcons(Fcons(key, val), result);
		tmp = next_node(tmp);
	}

	return result;
}

DEFUN("skiplist-to-plist", Fskiplist_to_plist, 1, 1, 0, /*
Return the ordinary association list induced by SKIPLIST.
*/
      (skiplist))
{
	Lisp_Object result = Qnil;
	skiplist_level_t tmp;
	Lisp_Object key, val;

	CHECK_SKIPLIST(skiplist);

	/* traverse the skiplist */
	tmp = next_node(XSKIPLIST_FOOT(skiplist));
	while (tmp) {
		key = node_data_key(parent_node(tmp));
		val = node_data_value(parent_node(tmp));
		result = Fcons(val, result);
		result = Fcons(key, result);
		tmp = next_node(tmp);
	}

	return result;
}

DEFUN("alist-to-skiplist", Falist_to_skiplist, 1, 1, 0, /*
Return a skiplist from ALIST with equal key space and image.
*/
      (alist))
{
	Lisp_Object result = make_skiplist();
	skiplist_t sl = XSKIPLIST(result);
	Lisp_Object tmp, key, val;

	CHECK_LIST(alist);

	/* traverse the alist */
	tmp = alist;
	while (!NILP(tmp)) {
		key = XCAR(XCAR(tmp));
		val = XCDR(XCAR(tmp));

		put_skiplist(sl, key, val);

		tmp = Fcdr(tmp);
	}

	return result;
}

DEFUN("plist-to-skiplist", Fplist_to_skiplist, 1, 1, 0, /*
Return a skiplist from PLIST with equal key space and image.
*/
      (plist))
{
	Lisp_Object result = make_skiplist();
	skiplist_t sl = XSKIPLIST(result);
	Lisp_Object tmp, key, val;

	CHECK_LIST(plist);

	/* traverse the plist */
	tmp = plist;
	while (!NILP(tmp)) {
		key = XCAR(tmp);
		val = XCAR(XCDR(tmp));

		put_skiplist(sl, key, val);

		tmp = Fcdr(Fcdr(tmp));
	}

	return result;
}


/* iterator crap, only needed for dict so make it static */
static void
skiplist_iter_init(dict_t d, dict_iter_t di)
{
	di->dict = d;
	/* go to the bottommost level */
	di->data = next_node(skiplist_foot((skiplist_t)d));
	return;
}

static void
skiplist_iter_fini(dict_iter_t di)
{
	di->dict = di->data = NULL;
	return;
}

/* the next one is for dicts only */
static void
skiplist_diter_next(dict_iter_t di, Lisp_Object *key, Lisp_Object *val)
{
	skiplist_level_t sll = di->data;

	if (LIKELY(sll != NULL)) {
		*key = node_data_key(parent_node(sll));
		*val = node_data_value(parent_node(sll));
		di->data = next_node(sll);
	} else {
		*key = *val = Qnull_pointer;
	}
	return;
}

/* and the one for seqs */
static void
skiplist_siter_next(seq_iter_t di, void **elm)
{
	skiplist_level_t sll = di->data;

	if (LIKELY(sll != NULL)) {
		*elm = (void*)node_data_key(parent_node(sll));
		di->data = next_node(sll);
	} else {
		*elm = Qnull_pointer;
	}
	return;
}

static void
skiplist_iter_reset(seq_iter_t si)
{
	/* go to the bottommost level */
	si->data = next_node(skiplist_foot((skiplist_t)si->seq));
	return;
}

static size_t
skiplist_explode(void *restrict tgt[], size_t ntgt, const seq_t s)
{
	volatile size_t i = 0;
	volatile skiplist_level_t n = next_node(skiplist_foot((skiplist_t)s));

	while (n != NULL && i < ntgt) {
		tgt[i++] = (void*)node_data_key(parent_node(n));
		n = next_node(n);
	}
	return i;
}


/*
 * Initialisation stuff
 */
static struct dict_impl_s __dskiplist = {
	.size_f = (dict_size_f)skiplist_size,
	.put_f = (dict_put_f)put_skiplist,
	.get_f = (dict_get_f)get_skiplist,
	.remove_f = (dict_remove_f)remove_skiplist,
	.iter_init_f = skiplist_iter_init,
	.iter_next_f = skiplist_diter_next,
	.iter_fini_f = skiplist_iter_fini,
};

static struct seq_impl_s __sskiplist = {
	.length_f = (seq_length_f)skiplist_size,
	.iter_init_f = (seq_iter_init_f)skiplist_iter_init,
	.iter_next_f = skiplist_siter_next,
	.iter_fini_f = (seq_iter_fini_f)skiplist_iter_fini,
	.iter_reset_f = skiplist_iter_reset,
	.explode_f = skiplist_explode,
};

/* deal with dict interface */
const dict_impl_t dict_skiplist = &__dskiplist;
/* deal with the seq interface (actually a set interface) */
const seq_impl_t seq_skiplist = &__sskiplist;

void syms_of_skiplist(void)
{
	INIT_LRECORD_IMPLEMENTATION(skiplist);

	defsymbol(&Qskiplistp, "skiplistp");

	DEFSUBR(Fmake_skiplist);
	DEFSUBR(Fskiplist_plist);

	DEFSUBR(Fskiplistp);
	DEFSUBR(Fskiplist_empty_p);

	DEFSUBR(Fput_skiplist);
	DEFSUBR(Fget_skiplist);
	DEFSUBR(Fremove_skiplist);
	DEFSUBR(Fskiplist_owns_p);

	DEFSUBR(Fskiplist_size);

	DEFSUBR(Fcopy_skiplist);
	DEFSUBR(Fskiplist_union);
	DEFSUBR(Fskiplist_intersection);
	DEFSUBR(Fmap_skiplist);

#ifdef SKIPLIST_DEBUG_FLAG
	DEFSUBR(Flist_skiplist);
#endif

	DEFSUBR(Fskiplist_to_alist);
	DEFSUBR(Fskiplist_to_plist);
	DEFSUBR(Falist_to_skiplist);
	DEFSUBR(Fplist_to_skiplist);
}

void
skiplist_reinit(void)
{
	morphisms[lrecord_type_skiplist].seq_impl = seq_skiplist;
	morphisms[lrecord_type_skiplist].aset_impl = dict_skiplist;
	return;
}

void vars_of_skiplist(void)
{
	Fprovide(intern("skiplist"));
}

/* skiplist.c ends here*/
