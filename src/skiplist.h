/*
  skiplist.c -- Pugh's Skiplist's
  Copyright (C) 2006 Sebastian Freundt

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

#ifndef INCLUDED_skiplist_h_
#define INCLUDED_skiplist_h_

#include "elhash.h"

/* Our implementation of skiplists:

   +------+          +------+          +------+
   | data |   nil    | data |   nil    | data |   nil
   +------+    ^     +------+    ^     +------+    ^
          |\+--|--+          \+--|--+         |\+--|--+
          | |ptr0-|---------->|ptr0-|---------|>|ptr0-|---->nil
          | +--^--+           +--^--+         | +--^--+
          |    |                 |            |    |
           \+--|--+             head           \+--|--+
            |ptr1-|---------------------------->|ptr1-|---->nil
            +--^--+                             +--^--+
               |                                   |
+--------+     |                                  head
|Skiplist+--->head
+--------+

*/


typedef struct skiplist_data_s skiplist_data_t;
typedef struct skiplist_level_s skiplist_level_t;
typedef struct skiplist_node_s skiplist_node_t;

struct skiplist_data_s {
	hashcode_t hash;
	Lisp_Object key;
	Lisp_Object value;
};

struct skiplist_level_s {
	skiplist_level_t *nextnode;  /* pointer to neighbour node */
	skiplist_level_t *nextlevel; /* pointer to above level */
	skiplist_node_t *node;	/* parent */
	uint32_t level;
};

struct skiplist_node_s {
	skiplist_level_t *head;	/* pointer to first level */
	skiplist_level_t *foot;	/* pointer to lowest level */
	uint32_t levels;	/* number of levels in this node */

	skiplist_data_t *data;	/* pointer to node's data cell */
};

struct Lisp_Skiplist {
	struct lcrecord_header lheader;

	skiplist_level_t *head;	/* pointer to leftmost highest level */
	uint32_t nodes;		/* number of nodes in this skiplist */
	uint32_t levels;	/* maximum over all numbers of levels */

	Lisp_Object plist;	/* property list */
};
typedef struct Lisp_Skiplist Lisp_Skiplist;

extern Lisp_Object Qskiplistp;

DECLARE_LRECORD(skiplist, Lisp_Skiplist);
#define XSKIPLIST(x) XRECORD(x, skiplist, Lisp_Skiplist)
#define XSETSKIPLIST(x, p) XSETRECORD(x, p, skiplist)
#define wrap_skiplist(p) wrap_object(p)
#define SKIPLISTP(x) RECORDP(x, skiplist)
#define CHECK_SKIPLIST(x) CHECK_RECORD(x, skiplist)
#define CONCHECK_SKIPLIST(x) CONCHECK_RECORD(x, skiplist)

#define skiplist_head(ms) (ms)->head
#define skiplist_nodes(ms) (ms)->nodes
#define skiplist_levels(ms) (ms)->levels
#define skiplist_plist(ms) (ms)->plist
#define XSKIPLIST_HEAD(x) skiplist_head(XSKIPLIST(x))
#define XSKIPLIST_NODES(x) skiplist_nodes(XSKIPLIST(x))
#define XSKIPLIST_LEVELS(x) skiplist_levels(XSKIPLIST(x))
#define XSKIPLIST_PLIST(x) skiplist_plist(XSKIPLIST(x))


extern Lisp_Object make_skiplist(void);
extern void put_skiplist(Lisp_Skiplist*, Lisp_Object, Lisp_Object);
extern Lisp_Object get_skiplist(Lisp_Skiplist*, Lisp_Object, Lisp_Object);
extern void remove_skiplist(Lisp_Skiplist*, Lisp_Object);
extern int skiplist_empty_p(Lisp_Skiplist*);
extern int skiplist_owns_p(Lisp_Skiplist*, Lisp_Object);
extern Lisp_Object copy_skiplist(Lisp_Skiplist*);
extern void unite_skiplist(Lisp_Skiplist*, Lisp_Skiplist*);
extern void intersect_skiplist(Lisp_Skiplist*, Lisp_Skiplist*);

EXFUN(Fmake_skiplist, 0);



/* some API conveniences */
typedef Lisp_Dllist skiplist_path_t; /* type for skiplist search paths */
#define new_skiplist_path()		noseeum_make_dllist()
#define free_skiplist_path(sl)		noseeum_free_dllist(sl)
#define skiplist_path_size(sl)		dllist_size(sl)
#define skiplist_path_first(sl)		(skiplist_level_t*)dllist_car(sl)
#define skiplist_path_last(sl)		(skiplist_level_t*)dllist_rac(sl)
#define skiplist_path_push(sp, sl)	dllist_append(sp, sl)
#define skiplist_path_pop(sp)		(skiplist_level_t*)dllist_pop_rac(sp)
#define skiplist_path_pophead(sp)	(skiplist_level_t*)dllist_pop_car(sp)

#define skiplist_path_nil		(skiplist_level_t*)NULL

#endif	/* INCLUDED_skiplist_h_ */
