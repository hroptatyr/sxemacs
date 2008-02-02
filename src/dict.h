/*** dict.c -- generic dict (single valued mapping) service
 *
 * Copyright (C) 2007, 2008 Sebastian Freundt
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
 * See dict.c
 *
 ***/

#ifndef INCLUDED_dict_h_
#define INCLUDED_dict_h_

#include "lisp.h"
#include <stdbool.h>

#ifdef ALL_DEBUG_FLAGS
#undef DICT_DEBUG_FLAG
#define DICT_DEBUG_FLAG
#endif

#define __DICT_DEBUG__(args...)		fprintf(stderr, "DICT " args)
#ifndef DICT_DEBUG_FLAG
#define DICT_DEBUG(args...)
#else
#define DICT_DEBUG(args...)		__DICT_DEBUG__(args)
#endif
#define DICT_CRITICAL(args...)		__DICT_DEBUG__("CRITICAL: " args)

typedef struct dict_s *dict_t;
typedef struct dict_iter_s *dict_iter_t;
typedef struct dict_impl_s *dict_impl_t;

typedef size_t(*dict_size_f)(dict_t);
typedef Lisp_Object(*dict_put_f)(dict_t, Lisp_Object, Lisp_Object);
typedef Lisp_Object(*dict_get_f)(dict_t, Lisp_Object, Lisp_Object);
typedef Lisp_Object(*dict_remove_f)(dict_t, Lisp_Object);
typedef void(*dict_iter_init_f)(dict_t, dict_iter_t);
typedef void(*dict_iter_next_f)(dict_iter_t, Lisp_Object*, Lisp_Object*);
typedef void(*dict_iter_fini_f)(dict_iter_t);

extern size_t dict_size(const dict_t);
extern bool dict_empty_p(const dict_t);
extern dict_t make_dict(void*);
extern Lisp_Object put_dict(dict_t, Lisp_Object key, Lisp_Object val);
extern Lisp_Object get_dict(dict_t, Lisp_Object key, Lisp_Object _default);
extern Lisp_Object remove_dict(dict_t, Lisp_Object key);

extern void dict_iter_init(dict_t, dict_iter_t);
extern void dict_iter_next(dict_iter_t, Lisp_Object*, Lisp_Object*);
extern void dict_iter_fini(dict_iter_t);

struct dict_impl_s {
	dict_size_f size_f;
	dict_put_f put_f;
	dict_get_f get_f;
	dict_remove_f remove_f;
	dict_iter_init_f iter_init_f;
	dict_iter_next_f iter_next_f;
	dict_iter_fini_f iter_fini_f;
};

struct dict_s {
	struct lcrecord_header lheader;
	dict_impl_t impl;
	/* the dual approach */
	void *dict;
};

struct dict_iter_s {
	dict_t dict;
	void *data;
};


/* inlines */
extern inline size_t
dict_size(const dict_t d)
{
	return d->impl->size_f(d);
}

extern inline bool
dict_empty_p(dict_t d)
{
	return dict_size(d) == 0;
}

extern inline dict_t
make_dict(void *foo)
{
	return NULL;
}

extern inline Lisp_Object
put_dict(dict_t d, Lisp_Object key, Lisp_Object val)
{
	return d->impl->put_f(d, key, val);
}

extern inline Lisp_Object
get_dict(dict_t d, Lisp_Object key, Lisp_Object _default)
{
	return d->impl->get_f(d, key, _default);
}

extern inline Lisp_Object
remove_dict(dict_t d, Lisp_Object key)
{
	return d->impl->remove_f(d, key);
}

extern inline void
dict_iter_init(dict_t d, dict_iter_t di)
{
	d->impl->iter_init_f(d, di);
	return;
}

extern inline void
dict_iter_next(dict_iter_t di, Lisp_Object *key, Lisp_Object *val)
{
	di->dict->impl->iter_next_f(di, key, val);
	return;
}

extern inline void
dict_iter_fini(dict_iter_t di)
{
	di->dict->impl->iter_fini_f(di);
	return;
}

#endif	/* INCLUDED_dict_h_ */
