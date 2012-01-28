/*** seq.h -- generic sequence service
 *
 * Copyright (C) 2008 Sebastian Freundt
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
 * Commentary:
 * A sequence is defined like in maths: A mapping of an somewhat ordered
 * index set into a target space.  The target space hereby is usually the
 * universe of lisp objects, the index set hereby is usually undefined,
 * i.e. varying from sequence to sequence, moreover it's possibly infinite
 * and all we use is the `somewhat ordered' property, so it makes sense to
 * talk about the `next element' of a sequence.
 *
 * Sequences have the following operations:
 * - iter_init  -  initialise an iterator through the sequence
 * - iter_fini  -  finalise an iterator
 * - iter_next  -  return the next element of the sequence
 *
 * Finite sequences have the following operations:
 * - length     -  return the length of a sequence
 * - iter_init  -  initialise an iterator through the sequence
 * - iter_fini  -  finalise an iterator
 * - iter_next  -  return the next element of the sequence
 * - iter_reset -  reset the iterator, next iter_next() call will give the
 *                 first element again
 * - explode    -  given a pointer to an object array offload some elements
 *                 of the sequence there
 *
 ***/

#ifndef INCLUDED_seq_h_
#define INCLUDED_seq_h_

#include "lrecord.h"
#include "category.h"
#include <stdbool.h>

#ifdef ALL_DEBUG_FLAGS
#undef SEQ_DEBUG_FLAG
#define SEQ_DEBUG_FLAG
#endif

#define __SEQ_DEBUG__(args...)		fprintf(stderr, "SEQ " args)
#ifndef SEQ_DEBUG_FLAG
#define SEQ_DEBUG(args...)
#else
#define SEQ_DEBUG(args...)		__SEQ_DEBUG__(args)
#endif
#define SEQ_CRITICAL(args...)		__SEQ_DEBUG__("CRITICAL: " args)

typedef struct seq_s *seq_t;
typedef struct seq_iter_s *seq_iter_t;
typedef struct seq_impl_s *seq_impl_t;

typedef size_t(*seq_length_f)(const seq_t);
typedef void(*seq_iter_init_f)(seq_t, seq_iter_t);
typedef void(*seq_iter_next_f)(seq_iter_t, void**);
typedef void(*seq_iter_fini_f)(seq_iter_t);
typedef void(*seq_iter_reset_f)(seq_iter_t);
typedef size_t(*seq_explode_f)(void*restrict[], size_t, const seq_t);

/* return the sequence implementation of an object */
extern_inline seq_impl_t seq_impl(const void*);
/* ... the same, but throw an error if the object has none */
extern_inline seq_impl_t seq_impl_err(const void*);

extern_inline size_t seq_length(const seq_t);
extern_inline bool seq_infinite_p(const seq_t);
extern_inline bool seq_empty_p(const seq_t);

extern_inline void seq_iter_init(seq_t, seq_iter_t);
extern_inline void seq_iter_fini(seq_iter_t);
extern_inline void seq_iter_next(seq_iter_t, void**);
extern_inline void seq_iter_reset(seq_iter_t);
extern_inline size_t seq_explode(void*restrict[], size_t, const seq_t);

extern seq_impl_t seq_unsupp;


struct seq_impl_s {
	seq_length_f length_f;
	seq_iter_init_f iter_init_f;
	seq_iter_fini_f iter_fini_f;
	seq_iter_next_f iter_next_f;
	seq_iter_reset_f iter_reset_f;
	seq_explode_f explode_f;
};

struct seq_s {
	struct lcrecord_header lheader;
	seq_impl_t impl;
	/* the dual approach */
	void *seq;
};

struct seq_iter_s {
	seq_t seq;
	void *data;
};

extern seq_impl_t seq_unsupported, seq_empty;


/* inlines */
extern_inline seq_impl_t
seq_impl(const void *obj)
{
	if (UNLIKELY(INTP((Lisp_Object)obj) || CHARP((Lisp_Object)obj))) {
		return NULL;
	} else if (UNLIKELY(NILP((Lisp_Object)obj))) {
		return seq_empty;
	}
	return cat_morphism(obj, cat_mk_seq);
}

extern_inline seq_impl_t
seq_impl_err(const void *obj)
{
	if (UNLIKELY(INTP((Lisp_Object)obj) || CHARP((Lisp_Object)obj))) {
		(void)dead_wrong_type_argument(Qsequencep, (Lisp_Object)obj);
		return NULL;
	} else if (UNLIKELY(NILP((Lisp_Object)obj))) {
		return seq_empty;
	}
	return cat_morphism(obj, cat_mk_seq);
}

extern_inline size_t
seq_length(const seq_t d)
{
/* return the length of a finite sequence
 * may be applied to any sequence and in case of an infinite one this
 * returns (size_t)-1 */
	seq_impl_t si = seq_impl(d);

	if (si != NULL) {
		return si->length_f(d);
	} else {
		/* is this right here? */
		return wrong_type_argument(Qsequencep, (Lisp_Object)d);
	}
}

extern_inline bool
seq_empty_p(const seq_t d)
{
	return seq_length(d) == 0;
}

extern_inline bool
seq_infinite_p(const seq_t d)
{
	return seq_length(d) == (size_t)-1;
}

extern_inline void
seq_iter_init(seq_t d, seq_iter_t di)
{
	seq_impl_t si = seq_impl(d);

	if (si != NULL) {
		si->iter_init_f(d, di);
	} else {
		/* is this right here? */
		(void)wrong_type_argument(Qsequencep, (Lisp_Object)d);
	}
	return;
}

extern_inline void
seq_iter_next(seq_iter_t di, void **elt)
{
	seq_impl(di->seq)->iter_next_f(di, elt);
	return;
}

extern_inline void
seq_iter_fini(seq_iter_t di)
{
	seq_impl(di->seq)->iter_fini_f(di);
	return;
}

extern_inline void
seq_iter_reset(seq_iter_t di)
{
	seq_impl(di->seq)->iter_reset_f(di);
	return;
}

extern_inline size_t
seq_explode(void *restrict tgt[], size_t ntgt, const seq_t s)
{
/* put up to NTGT values of S into TGT, return the number of put elements */
	seq_impl_t si = seq_impl(s);

	if (si != NULL) {
		return si->explode_f(tgt, ntgt, s);
	} else {
		(void)wrong_type_argument(Qsequencep, (const Lisp_Object)s);
		return (size_t)-1;
	}
}


extern void seq_LTX_init(void);
extern void seq_LTX_reinit(void);
extern void seq_LTX_deinit(void);

#endif	/* INCLUDED_seq_h_ */
