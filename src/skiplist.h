/*** skiplist.h -- Pugh's Skiplists
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

#ifndef INCLUDED_skiplist_h_
#define INCLUDED_skiplist_h_

#include "elhash.h"
#include <stdbool.h>
#include "seq.h"
#include "dict.h"

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

extern Lisp_Object Qskiplistp;
extern void skiplist_reinit(void);

typedef struct skiplist_s *skiplist_t;
typedef Lisp_Object(*skiplist_map_f)(Lisp_Object key, Lisp_Object value);
typedef Lisp_Object(*skiplist_map2_f)(Lisp_Object key, Lisp_Object val, void*);
/* hidden structs */
typedef struct skiplist_data_s *skiplist_data_t;
typedef struct skiplist_level_s *skiplist_level_t;
typedef struct skiplist_node_s *skiplist_node_t;
typedef struct skiplist_path_s *skiplist_path_t;

#define MAX_SKIPLIST_HEIGHT	63

struct skiplist_s {
	struct lcrecord_header lheader;

	/* the sequence category */
	void *si;
	/* the dict (aset) category */
	void *di;

	/* the head levels are just a fixed size array */
	skiplist_level_t headlevs;
	size_t nnodes;		/* number of nodes in this skiplist */
	size_t nlevels;		/* maximum over all numbers of levels */

	Lisp_Object plist;	/* property list */
};

DECLARE_LRECORD(skiplist, struct skiplist_s);
#define XSKIPLIST(x) XRECORD(x, skiplist, struct skiplist_s)
#define XSETSKIPLIST(x, p) XSETRECORD(x, p, skiplist)
#define wrap_skiplist(p) wrap_object(p)
#define SKIPLISTP(x) RECORDP(x, skiplist)
#define CHECK_SKIPLIST(x) CHECK_RECORD(x, skiplist)
#define CONCHECK_SKIPLIST(x) CONCHECK_RECORD(x, skiplist)

#define skiplist_nnodes(ms) (ms)->nnodes
#define skiplist_nlevels(ms) (ms)->nlevels
#define skiplist_head(ms) (&((ms)->headlevs[skiplist_nlevels(ms)]))
#define skiplist_foot(ms) (&((ms)->headlevs[0]))
#define skiplist_plist(ms) (ms)->plist
#define XSKIPLIST_HEAD(x) skiplist_head(XSKIPLIST(x))
#define XSKIPLIST_FOOT(x) skiplist_foot(XSKIPLIST(x))
#define XSKIPLIST_NNODES(x) skiplist_nnodes(XSKIPLIST(x))
#define XSKIPLIST_NLEVELS(x) skiplist_nlevels(XSKIPLIST(x))
#define XSKIPLIST_PLIST(x) skiplist_plist(XSKIPLIST(x))


extern Lisp_Object make_skiplist(void);
extern void put_skiplist(skiplist_t, Lisp_Object, Lisp_Object);
extern Lisp_Object get_skiplist(skiplist_t, Lisp_Object, Lisp_Object);
extern void remove_skiplist(skiplist_t, Lisp_Object);
extern_inline bool skiplist_empty_p(skiplist_t);
extern bool skiplist_owns_p(skiplist_t, Lisp_Object);
extern Lisp_Object copy_skiplist(skiplist_t);
extern void unite_skiplist(skiplist_t, skiplist_t);
extern void intersect_skiplist(skiplist_t, skiplist_t);

extern void map_skiplist(skiplist_t, skiplist_map_f);
extern void map2_skiplist(skiplist_t, skiplist_map2_f, void*);

extern_inline bool
skiplist_empty_p(skiplist_t sl)
{
	return (skiplist_nnodes(sl) == 0 ? true : false);
}

EXFUN(Fmake_skiplist, 0);
EXFUN(Fget_skiplist,3);
EXFUN(Fremove_skiplist,2);

#if 0
extern_inline skiplist_path_t make_skiplist_path(skiplist_t);
extern_inline void free_skiplist_path(skiplist_path_t);
extern_inline size_t skiplist_path_size(skiplist_path_t);
extern_inline skiplist_level_t skiplist_path_first(skiplist_path_t);
extern_inline skiplist_level_t skiplist_path_last(skiplist_path_t);
extern_inline void skiplist_path_push(skiplist_path_t, skiplist_level_t);
extern_inline skiplist_level_t skiplist_path_pop(skiplist_path_t);
extern_inline skiplist_level_t skiplist_path_pophead(skiplist_path_t);
extern_inline int descend_level_p(void);
#endif

#endif	/* INCLUDED_skiplist_h_ */
