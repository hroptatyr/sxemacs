/*
  bloom.c -- Bloom filters
  Copyright (C) 2006 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

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

#ifndef INCLUDED_bloom_h_
#define INCLUDED_bloom_h_


#define BLOOM_DEPTH_TYPE uint8_t /* which type to use for each slot */

typedef BLOOM_DEPTH_TYPE* bloom_vector;

struct Lisp_Bloom {
	struct lcrecord_header lheader;

	bloom_vector vector;
	uint32_t order;
	uint32_t degree;
	uint32_t size;
	/* a scratch vector for the hash fun,
	 * avoids dozens of xmalloc/xfree calls */
	uint32_t *scratch;
};
typedef struct Lisp_Bloom Lisp_Bloom;

extern Lisp_Object Qbloomp;

DECLARE_LRECORD(bloom, Lisp_Bloom);
#define XBLOOM(x) XRECORD(x, bloom, Lisp_Bloom)
#define XSETBLOOM(x, p) XSETRECORD(x, p, bloom)
#define wrap_bloom(p) wrap_object(p)
#define BLOOMP(x) RECORDP(x, bloom)
#define CHECK_BLOOM(x) CHECK_RECORD(x, bloom)
#define CONCHECK_BLOOM(x) CONCHECK_RECORD(x, bloom)

#define bloom_vector(ms) (ms)->vector
#define bloom_order(ms) (ms)->order
#define bloom_degree(ms) (ms)->degree
#define bloom_size(ms) (ms)->size
#define bloom_scratch(ms) (ms)->scratch
#define XBLOOM_VECTOR(x) bloom_vector(XBLOOM(x))
#define XBLOOM_ORDER(x) bloom_order(XBLOOM(x))
#define XBLOOM_DEGREE(x) bloom_degree(XBLOOM(x))
#define XBLOOM_SIZE(x) bloom_size(XBLOOM(x))
#define XBLOOM_SCRATCH(x) bloom_scratch(XBLOOM(x))


extern Lisp_Object make_bloom(uint32_t, uint32_t);
extern Lisp_Object make_bloom_universe(uint32_t, uint32_t);
extern int bloom_owns_p(Lisp_Bloom*, Lisp_Object);
extern void bloom_add(Lisp_Bloom*, Lisp_Object);
extern void bloom_remove(Lisp_Bloom*, Lisp_Object);
extern void bloom_union(Lisp_Bloom*, Lisp_Bloom*);
extern void bloom_intersection(Lisp_Bloom*, Lisp_Bloom*);

#endif	/* INCLUDED_bloom_h_ */
