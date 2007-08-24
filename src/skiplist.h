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


typedef struct skiplist_data_s *skiplist_data_t;
typedef struct skiplist_level_s *skiplist_level_t;
typedef struct skiplist_node_s *skiplist_node_t;

typedef Lisp_Object(*skiplist_map_f)(Lisp_Object key, Lisp_Object value);
typedef Lisp_Object(*skiplist_map2_f)(Lisp_Object key, Lisp_Object val, void*);

struct skiplist_data_s {
	hashcode_t hash;
	Lisp_Object key;
	Lisp_Object value;
};

struct skiplist_s {
	struct lcrecord_header lheader;

	skiplist_level_t head;	/* pointer to leftmost highest level */
	long int nodes;		/* number of nodes in this skiplist */
	long int levels;	/* maximum over all numbers of levels */

	Lisp_Object plist;	/* property list */
};
typedef struct skiplist_s *Lisp_Skiplist;
typedef struct skiplist_s *skiplist_t;
typedef struct skiplist_path_s *skiplist_path_t;

extern Lisp_Object Qskiplistp;

DECLARE_LRECORD(skiplist, struct skiplist_s);
#define XSKIPLIST(x) XRECORD(x, skiplist, struct skiplist_s)
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
extern void put_skiplist(skiplist_t, Lisp_Object, Lisp_Object);
extern Lisp_Object get_skiplist(skiplist_t, Lisp_Object, Lisp_Object);
extern void remove_skiplist(skiplist_t, Lisp_Object);
extern int skiplist_empty_p(skiplist_t);
extern int skiplist_owns_p(skiplist_t, Lisp_Object);
extern Lisp_Object copy_skiplist(Lisp_Skiplist);
extern void unite_skiplist(skiplist_t, skiplist_t);
extern void intersect_skiplist(skiplist_t, skiplist_t);

extern void map_skiplist(skiplist_t, skiplist_map_f);
extern void map2_skiplist(skiplist_t, skiplist_map2_f, void*);
#define skiplist_empty_p(sl)	(skiplist_nodes(sl) == 0 ? -1 : 0)

EXFUN(Fmake_skiplist, 0);

extern inline skiplist_path_t make_skiplist_path(skiplist_t);
extern inline void free_skiplist_path(skiplist_path_t);
extern inline size_t skiplist_path_size(skiplist_path_t);
extern inline skiplist_level_t skiplist_path_first(skiplist_path_t);
extern inline skiplist_level_t skiplist_path_last(skiplist_path_t);
extern inline void skiplist_path_push(skiplist_path_t, skiplist_level_t);
extern inline skiplist_level_t skiplist_path_pop(skiplist_path_t);
extern inline skiplist_level_t skiplist_path_pophead(skiplist_path_t);
extern inline int descend_level_p(void);


#endif	/* INCLUDED_skiplist_h_ */
