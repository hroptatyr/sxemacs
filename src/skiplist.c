/*
  skiplist.c -- Pugh's Skiplists
  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

/* Synched up with: Not in FSF. */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"

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

Lisp_Object Qskiplistp;


struct skiplist_level_s {
	skiplist_level_t nextnode;  /* pointer to neighbour node */
	skiplist_level_t nextlevel; /* pointer to above level */
	skiplist_node_t node;	/* parent */
	long int level;
};

struct skiplist_node_s {
	skiplist_level_t head;	/* pointer to first level */
	skiplist_level_t foot;	/* pointer to lowest level */
	long int levels;	/* number of levels in this node */

	skiplist_data_t data;	/* pointer to node's data cell */
};

struct skiplist_path_s {
	skiplist_level_t *path;
	size_t palloc_sz;
	size_t head;
	size_t tail;
};

/* paths stuff, just internal */
#define skiplist_path_nil		(skiplist_level_t)NULL
#if 0
typedef Lisp_Dllist *skiplist_path_t; /* type for skiplist search paths */
#define new_skiplist_path()		noseeum_make_dllist()
#define free_skiplist_path(sl)		noseeum_free_dllist(sl)
#define skiplist_path_size(sl)		dllist_size(sl)
#define skiplist_path_first(sl)		(skiplist_level_t)dllist_car(sl)
#define skiplist_path_last(sl)		(skiplist_level_t)dllist_rac(sl)
#define skiplist_path_push(sp, sl)	dllist_append(sp, sl)
#define skiplist_path_pop(sp)		(skiplist_level_t)dllist_pop_rac(sp)
#define skiplist_path_pophead(sp)	(skiplist_level_t)dllist_pop_car(sp)
#endif

/* array based */
extern inline skiplist_path_t
make_skiplist_path(skiplist_t sl)
{
	skiplist_path_t p = xnew(struct skiplist_path_s);

	if ((p->palloc_sz = skiplist_levels(sl)*skiplist_nodes(sl)) < 8)
		p->palloc_sz = 8;
	p->path = xnew_array(skiplist_level_t, p->palloc_sz);
	p->head = 0;
	p->tail = 0;
	SL_DEBUG_PATH("created path 0x%lx of size %ld\n",
		      (long unsigned int)p, (long int)p->palloc_sz);
	return p;
}

extern inline void
free_skiplist_path(skiplist_path_t slp)
{
	SL_DEBUG_PATH("freeing path 0x%lx of size %ld\n",
		      (long unsigned int)slp, (long int)slp->palloc_sz);
	xfree(slp->path);
	xfree(slp);
}

extern inline size_t
skiplist_path_size(skiplist_path_t slp)
{
	return slp->tail - slp->head;
}

extern inline skiplist_level_t
skiplist_path_first(skiplist_path_t slp)
{
	if (skiplist_path_size(slp)) {
		return slp->path[slp->head];
	} else {
		return skiplist_path_nil;
	}
}

extern inline skiplist_level_t
skiplist_path_last(skiplist_path_t slp)
{
	if (skiplist_path_size(slp)) {
		return slp->path[slp->tail-1];
	} else {
		return skiplist_path_nil;
	}
}

extern inline void
skiplist_path_push(skiplist_path_t slp, skiplist_level_t l)
{
	SL_DEBUG_PATH("path 0x%lx, pushing 0x%lx\n",
		      (long unsigned int)slp, (long unsigned int)l);
	slp->path[slp->tail++] = l;
	return;
}

extern inline skiplist_level_t
skiplist_path_pop(skiplist_path_t slp)
{
	if (skiplist_path_size(slp)) {
		SL_DEBUG_PATH("path 0x%lx, popping 0x%lx\n",
			      (long unsigned int)slp,
			      (long unsigned int)slp->path[slp->tail-1]);
		return slp->path[--slp->tail];
	} else {
		SL_DEBUG_PATH("path 0x%lx, popping NULL\n",
			      (long unsigned int)slp);
		slp->head = 0;
		slp->tail = 0;
		return skiplist_path_nil;
	}
}

extern inline skiplist_level_t
skiplist_path_pophead(skiplist_path_t slp)
{
	if (skiplist_path_size(slp)) {
		SL_DEBUG_PATH("path 0x%lx, popping 0x%lx\n",
			      (long unsigned int)slp,
			      (long unsigned int)slp->path[slp->head]);
		return slp->path[slp->head++];
	} else {
		SL_DEBUG_PATH("path 0x%lx, popping NULL\n",
			      (long unsigned int)slp);
		slp->head = 0;
		slp->tail = 0;
		return skiplist_path_nil;
	}
}

/* static bindings */
/* low level bindings */
static skiplist_data_t make_skiplist_data(Lisp_Object, Lisp_Object);
/* static void free_skiplist_data(skiplist_data_t); */

static skiplist_node_t make_skiplist_node(skiplist_data_t);
/* static void free_skiplist_node(skiplist_node_t*); */

static skiplist_level_t make_skiplist_level(skiplist_node_t);
/* static void free_skiplist_level(skiplist_level_t*); */

static skiplist_level_t add_node_level(skiplist_node_t node);

#ifdef BRAIN_MUD
static skiplist_path_t skiplist_find_key(skiplist_t, Lisp_Object);
#endif
static skiplist_path_t skiplist_find_hash(skiplist_t, hashcode_t);

extern int get_random(void);

/* high level bindings */

/* low level bindings */
#define data_hash(data)		(data)->hash
#define data_key(data)		(data)->key
#define data_value(data)	(data)->value

#define node_head_level(node)	(node)->head
#define node_foot_level(node)	(node)->foot
#define node_data(node)		(node)->data
#define node_levels(node)	(node)->levels

#define next_node(level)	(level)->nextnode
#define next_level(level)	(level)->nextlevel
#define parent_node(level)	(level)->node
#define parent_data(level)	node_data(parent_node(level))
#define level_number(l)		(l)->level

#define next_hash(level)	data_hash(parent_data(next_node(level)))
#define next_key(level)		data_key(parent_data(next_node(level)))
#define next_value(level)	data_value(parent_data(next_node(level)))

#define level_hash(l)							\
	(l ? (l->node) ? (l->node->data) ? (l->node->data->hash) : 0 : 0 : 0)
#define level_key(l)							\
	(l ? (l->node) ? (l->node->data) ? (l->node->data->key) : 0 : 0 : 0)

/* static hashcode_t skiplist_hash(Lisp_Object); */
#define skiplist_hash(obj)	internal_hash((obj), 0)
#define skiplist_keyeq(o1, o2)	internal_equal((o1), (o2), 0)

/* this is p^n - 1 */
/* here: p=2 n=1 */
#define SL_PROBABILITY_MASK	1


static inline skiplist_data_t
make_skiplist_data(Lisp_Object key, Lisp_Object value)
{
	/* create the node's data cell, drop key and value there */
	skiplist_data_t data = xnew(struct skiplist_data_s);

	data_hash(data) = skiplist_hash(key);
	data_key(data) = key;
	data_value(data) = value;

	return data;
}

static inline skiplist_node_t
make_skiplist_node(skiplist_data_t data)
{
	skiplist_node_t node = xnew(struct skiplist_node_s);

	/* initialise an empty node */
	node->head = NULL;
	node->foot = NULL;
	node->levels = 0;
	node_data(node) = data;

	return node;
}

static inline skiplist_level_t
make_skiplist_level(skiplist_node_t node)
{
	skiplist_level_t level = xnew(struct skiplist_level_s);

	/* initialise an empty node */
	next_node(level) = NULL;
	next_level(level) = NULL;
	parent_node(level) = node;
	level_number(level) = 0;

	return level;
}

#define free_skiplist_data(data)	xfree(data)
#define free_skiplist_node(node)	xfree(node)
#define free_skiplist_level(level)	xfree(level)

static inline void
add_level_neighbour(skiplist_level_t level, skiplist_level_t neighbour)
{
	neighbour->nextnode = level->nextnode;
	level->nextnode = neighbour;
}

extern inline int
descend_level_p(void)
{
	/* randomly return yes or no */
	return ((random() & SL_PROBABILITY_MASK) == 0);
}

/* higher level bindings */
#ifdef BRAIN_MUD
static skiplist_path_t
skiplist_find_key(skiplist_t slist, Lisp_Object key)
{
	return skiplist_find_hash(slist, skiplist_hash(key));
}
#else
#define skiplist_find_key(sl, k)	skiplist_find_hash(sl, skiplist_hash(k))
#endif

static skiplist_level_t
add_node_level(skiplist_node_t node)
{
	skiplist_level_t level = make_skiplist_level(node);

	level->nextlevel = node->head;
	node->head = level;

	level->node = node;
	node->levels++;
	level_number(level) = node->levels;

	if (!node->foot)
		node->foot = level;

	SL_DEBUG_LEVEL("created level 0x%lx\n", (long unsigned int)level);
	return level;
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
	node->levels--;

	if (node->head == NULL)
		node->foot = NULL;

	SL_DEBUG_LEVEL("popped level 0x%lx\n", (long unsigned int)tmp);
	return tmp;
}

static skiplist_path_t
skiplist_find_hash(skiplist_t slist, hashcode_t hash)
{
	skiplist_level_t tmp = NULL;
	hashcode_t tmphash;
	/* result is the search path in a dllist */
	skiplist_path_t result = make_skiplist_path(slist);

	if (!(tmp = skiplist_head(slist)))
		return result;

	skiplist_path_push(result, tmp); /* just push the skiplist head */
	while (tmp) {
		while (next_node(tmp) &&
		       (tmphash = next_hash(tmp)) &&
		       (tmphash < hash)) {
			tmp = next_node(tmp);
		}
		skiplist_path_push(result, tmp);
		tmp = next_level(tmp);
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
			__SKIPLIST_DEBUG__("%lu(%ld)->",
					   level_hash(tmp), level_number(tmp));
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
	skiplist_level_t tmp;
	skiplist_data_t data;

	/* traverse the skiplist, we simply use the lowest level since
	 * that should be a single-linked list */
	tmp = XSKIPLIST_HEAD(obj)->node->foot; /* jump to lowest level */
	while (tmp) {
		data = tmp->node->data;
		if (data) {
			mark_object(data->key);
			mark_object(data->value);
		}
		tmp = tmp->nextnode;
	}

        mark_object(XSKIPLIST_PLIST(obj));
	return XSKIPLIST_PLIST(obj);
}

static void
print_skiplist(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char num[16];

	write_c_string("#<skiplist :size ", printcharfun);
	snprintf(num, 15, "%ld", XSKIPLIST_NODES(obj));
	write_c_string(num, printcharfun);
	write_c_string(" :levels ", printcharfun);
	snprintf(num, 15, "%ld", XSKIPLIST_LEVELS(obj));
	write_c_string(num, printcharfun);
	write_c_string(">", printcharfun);
}

static void
finalise_skiplist(void *header, int for_disksave)
{
	skiplist_t skiplist = header;
	skiplist_level_t tmp, lev, head;

	head = skiplist_head(skiplist);
	SL_DEBUG("*** SXEmacs: skiplist finalisation 0x%lx\n",
		 (long unsigned int)head);

	/* traverse the skiplist and free all node and data cells */
	tmp = head->node->foot;
	while (tmp) {
		if (tmp->node && tmp->node->data) {
			SL_DEBUG_DATA("freeing 0x%lx\n",
				      (long unsigned int)tmp->node->data);
			free_skiplist_data(tmp->node->data);
		}
		if (tmp->node) {
			SL_DEBUG_NODE("freeing 0x%lx\n",
				      (long unsigned int)tmp->node);
			free_skiplist_node(tmp->node);
		}
		tmp = next_node(tmp);
	}

	/* now traverse all levels */
	while (head) {
		lev = head;
		head = next_level(head);
		while (lev) {
			tmp = next_node(lev);
			SL_DEBUG_LEVEL("freeing 0x%lx",
				       (long unsigned int)lev);
			free_skiplist_level(lev);
			lev = tmp;
		}
	}

	skiplist_nodes(skiplist) = 0;
	skiplist_levels(skiplist) = 0;
	skiplist_plist(skiplist) = Qnil;

	/* avoid some warning */
	if (for_disksave);
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
	{XD_OPAQUE_PTR, offsetof(struct skiplist_s, head)},
	{XD_INT, offsetof(struct skiplist_s, nodes)},
	{XD_INT, offsetof(struct skiplist_s, levels)},
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

Lisp_Object make_skiplist(void)
{
	skiplist_t skiplist = allocate_skiplist();
	Lisp_Object result;

	skiplist_head(skiplist) = add_node_level(make_skiplist_node(NULL));
	skiplist_nodes(skiplist) = 0;
	skiplist_levels(skiplist) = 1;
	skiplist_plist(skiplist) = Qnil;

	XSETSKIPLIST(result, skiplist);
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
Return non-`nil' if OBJECT is a skiplist, `nil' otherwise.
					*/
      (object))
{
	if (SKIPLISTP(object))
		return Qt;
	else
		return Qnil;
}

DEFUN("skiplist-empty-p", Fskiplist_empty_p, 1, 1, 0, /*
Return non-`nil' if SKIPLIST is empty, `nil' otherwise.
						      */
      (skiplist))
{
	CHECK_SKIPLIST(skiplist);

	if (XSKIPLIST_NODES(skiplist) == 0)
		return Qt;
	else
		return Qnil;
}

/* modifiers and accessors */
void put_skiplist(skiplist_t skiplist, Lisp_Object key, Lisp_Object value)
{
	skiplist_path_t path;
	skiplist_data_t data;
	skiplist_node_t node, headnode;
	skiplist_level_t level, last, headlevel;

	path = skiplist_find_key(skiplist, key);

	if (!skiplist_path_size(path) ||
	    !(last = skiplist_path_pop(path))) {
		free_skiplist_path(path);
		return;
	}

	headlevel = skiplist_path_pophead(path);
	headnode = parent_node(headlevel);

	data = make_skiplist_data(key, value);

	/* now either we have to insert, or replace */
	/* for that we check if the element right of left is by chance
	 * the thing we look for */
	if (data_hash(data) == level_hash(next_node(last))) {
		level = next_node(last);
		if (skiplist_keyeq(data_key(data), level_key(level))) {
			/* oh, we have to replace, we just nuke the old
			 * data cell and replace it with the new one
			 * created above */
			SL_DEBUG("*** SXEmacs: skiplist collision, replace\n");
			free_skiplist_data(parent_data(level));
			parent_data(level) = data;
		} else {
			SL_CRITICAL("*** SXEmacs CRITICAL: "
				    "non trivial skiplist collision :(\n");
		}
		free_skiplist_path(path);
		return;
	} else {
		/* entirely new data, build a node for it */
		node = make_skiplist_node(data);
		level = make_skiplist_level(node);

		level = add_node_level(node);

		add_level_neighbour(last, level);

		skiplist->nodes++;
	}

	/* shall we add another level? */
	while (descend_level_p()) {
		level = add_node_level(node);
		if ((last = skiplist_path_pop(path)) != skiplist_path_nil) {
			SL_DEBUG("last 0x%lx, ln(last): %ld  "
				 "level 0x%lx, ln(level) %ld\n",
				 (long unsigned int)last, level_number(last),
				 (long unsigned int)level, level_number(level));
			if (level_number(last) == level_number(level)) {
				add_level_neighbour(last, level);
			} else {
				SL_CRITICAL("*** SXEmacs CRITICAL: "
					    "severe skiplist "
					    "structure error!\n");
			}
		} else {
			skiplist_level_t newhlevel =
				add_node_level(headnode);
			skiplist_head(skiplist) = newhlevel;
			add_level_neighbour(newhlevel, level);
			skiplist->levels = headnode->levels;
		}
	}
	free_skiplist_path(path);
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
get_skiplist(skiplist_t skiplist, Lisp_Object key, Lisp_Object default_)
{
	skiplist_path_t path;
	skiplist_data_t data;
	skiplist_node_t node;
	skiplist_level_t level;

	path = skiplist_find_key(skiplist, key);

	if (!(level = skiplist_path_last(path))) {
		free_skiplist_path(path);
		return default_;
	}

	/* level points to rightmost and footmost level to the left of key */
	if (next_node(level))
		level = next_node(level);

	if (!(node = level->node)) {
		free_skiplist_path(path);
		return default_;
	}

	if (!(data = node->data)) {
		free_skiplist_path(path);
		return default_;
	}

	if (!(skiplist_keyeq(data_key(data), key))) {
		free_skiplist_path(path);
		return default_;
	}

	free_skiplist_path(path);
	return data_value(data);
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

void remove_skiplist(skiplist_t skiplist, Lisp_Object key)
{
	skiplist_path_t path;
	skiplist_node_t node, headnode;
	skiplist_level_t level, last, headlevel;

	path = skiplist_find_key(skiplist, key);

	if (!skiplist_path_size(path) ||
	    !(last = skiplist_path_last(path))) {
		free_skiplist_path(path);
		return;
	}

	headlevel = skiplist_path_pophead(path);
	headnode = parent_node(headlevel);

	/* now either we have to insert, or replace */
	/* for that we check if the element right of left is by chance
	 * the thing we look for */
	if (skiplist_hash(key) == level_hash(next_node(last)) &&
	    skiplist_keyeq(key, level_key(next_node(last)))) {
		node = parent_node(next_node(last));

		/* traverse (bottom-up) the level structure
		 * and free any occurring level pointers */
		while ((last = skiplist_path_pop(path)) != skiplist_path_nil &&
		       next_node(last) &&
		       parent_node(next_node(last)) == node) {
			level = next_node(last);
			next_node(last) = next_node(level);
			free_skiplist_level(level);
		}

		/* free node and data cell */
		free_skiplist_data(node_data(node));
		free_skiplist_node(node);
		/* decrement skiplist size */
		skiplist_nodes(skiplist)--;

		/* now, the skiplist head might have many nil pointers
		 * we reduce the overall levelling in that case */
		while (next_node(headlevel) != NULL &&
		       (level = next_level(headlevel))) {
			skiplist_level_t hl = headlevel;
			headlevel = node_head_level(headnode) = level;
			free_skiplist_level(hl);
			node_levels(headnode)--;
			skiplist_levels(skiplist)--;
		}
		/* ready, now we have to tune the head level pointer
		 * of the entire skiplist */
		skiplist_head(skiplist) = headlevel;
	}
	free_skiplist_path(path);
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

int skiplist_owns_p(skiplist_t skiplist, Lisp_Object key)
/* return !0 iff SKIPLIST has a node for KEY */
{
	skiplist_path_t path;
	skiplist_data_t data;
	skiplist_node_t node;
	skiplist_level_t level;

	path = skiplist_find_key(skiplist, key);

	if (!(level = skiplist_path_last(path))) {
		free_skiplist_path(path);
		return 0;
	}

	/* level points to rightmost and footmost level to the left of key */
	if (next_node(level))
		level = next_node(level);

	if (!(node = level->node)) {
		free_skiplist_path(path);
		return 0;
	}

	if (!(data = node->data)) {
		free_skiplist_path(path);
		return 0;
	}

	if (!(skiplist_keyeq(data_key(data), key))) {
		free_skiplist_path(path);
		return 0;
	}

	free_skiplist_path(path);
	return !0;
}
DEFUN("skiplist-owns-p", Fskiplist_owns_p, 2, 2, 0, /*
Return non-`nil' if KEY is associated with a value in SKIPLIST,
`nil' otherwise.
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
	return make_int((int32_t)XSKIPLIST_NODES(skiplist));
}


Lisp_Object
copy_skiplist(skiplist_t skiplist)
{
	Lisp_Object result = make_skiplist();
	skiplist_t sl_copy = XSKIPLIST(result);
	skiplist_level_t tmp;
	Lisp_Object key, val;

	/* traverse the skiplist */
	tmp = skiplist_head(skiplist)->node->foot;
	while (tmp) {
		if (tmp->node && tmp->node->data) {
			key = tmp->node->data->key;
			val = tmp->node->data->value;
			put_skiplist(sl_copy, key, val);
		}
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

	lev = skiplist_head(source)->node->foot; /* start at the bottom */
	while (lev) {
		if (lev->node && lev->node->data) {
			key = lev->node->data->key;
			value = lev->node->data->value;
			put_skiplist(target, key, value);
		}
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

	lev = skiplist_head(target)->node->foot; /* start at the bottom */
	while (lev) {
		if (lev->node && lev->node->data) {
			key = lev->node->data->key;
			lev = next_node(lev);
			if (!skiplist_owns_p(source, key))
				remove_skiplist(target, key);
		} else
			lev = next_node(lev);
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

	lev = skiplist_head(sl)->node->foot; /* start at the bottom */
	while (lev) {
		if (lev->node && lev->node->data) {
			Lisp_Object k, v;
			k = lev->node->data->key;
			v = lev->node->data->value;

			mapf(k, v);
		}
		lev = next_node(lev);
	}
	return;
}

void
map2_skiplist(skiplist_t sl, skiplist_map2_f mapf, void *ptr)
{
	skiplist_level_t lev;

	lev = skiplist_head(sl)->node->foot; /* start at the bottom */
	while (lev) {
		if (lev->node && lev->node->data) {
			Lisp_Object k, v;
			k = lev->node->data->key;
			v = lev->node->data->value;

			mapf(k, v, ptr);
		}
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
        
        CHECK_SKIPLIST(skiplist);

        sl = XSKIPLIST(skiplist);
	lev = skiplist_head(sl)->node->foot; /* start at the bottom */
	while (lev) {
		if (lev->node && lev->node->data) {
			args[0] = function;
			args[1] = lev->node->data->key;
			args[2] = lev->node->data->value;

			Ffuncall(countof(args), args);
		}
		lev = next_node(lev);
	}

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
	tmp = skiplist_head(XSKIPLIST(skiplist))->node->foot;
	while (tmp) {
		if (tmp->node && tmp->node->data) {
			key = tmp->node->data->key;
			val = tmp->node->data->value;
			result = Fcons(Fcons(key, val), result);
		}
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
	tmp = skiplist_head(XSKIPLIST(skiplist))->node->foot;
	while (tmp) {
		if (tmp->node && tmp->node->data) {
			key = tmp->node->data->key;
			val = tmp->node->data->value;
			result = Fcons(val, result);
			result = Fcons(key, result);
		}
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


/*
 * Initialisation stuff
 */
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

void vars_of_skiplist(void)
{
	Fprovide(intern("skiplist"));
}

