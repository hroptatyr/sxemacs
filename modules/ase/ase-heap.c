/*
  ase-heap.c -- Heaps
  Copyright (C) 2007 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

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
  */

/* Synched up with: Not in FSF. */

#include "config.h"
#include "sxemacs.h"
#include "ent/ent.h"
#include "ase-heap.h"
#include "opaque.h"

#ifdef ALL_DEBUG_FLAGS
#undef EMOD_ASE_DEBUG_FLAG
#define EMOD_ASE_DEBUG_FLAG
#endif

#define EMOD_ASE_DEBUG_HEAP(args...)	EMOD_ASE_DEBUG("[HEAP]: " args)

PROVIDE(ase_heap);

Lisp_Object Qase_heap, Qase_heapp;
Lisp_Object Qase_yheap, Qase_yheapp;
Lisp_Object Qase_dheap, Qase_dheapp;
Lisp_Object Qase_wheap, Qase_wheapp;
Lisp_Object Qase_heap_default_kind;

Lisp_Object Qweak, Qdense, Qdynamic, Q_kind, Q_relation, Q_coloured;

#define ASE_HEAP_MIN_SIZE	4096	/* 65536 */
#define ALIGNED(n)	__attribute__((aligned(n), packed))

static inline ase_yheap_t _ase_make_yheap(ase_heap_options_t opts);
static inline Lisp_Object _ase_wrap_yheap(ase_yheap_t);
static inline ase_dheap_t _ase_make_dheap(ase_heap_options_t opts);
static inline Lisp_Object _ase_wrap_dheap(ase_dheap_t);
static inline ase_wheap_t _ase_make_wheap(ase_heap_options_t opts);
static inline Lisp_Object _ase_wrap_wheap(ase_wheap_t);

/* the op tables */
struct ase_heap_ops_s ase_heap_ops[NUMBER_OF_ASE_HEAP_KINDS] =
{{(ase_heap_constr_f)_ase_make_yheap, (ase_heap_wrap_f)_ase_wrap_yheap,
  (ase_heap_add_f)ase_add_yheap, (ase_heap_pop_f)ase_pop_yheap},
 {(ase_heap_constr_f)_ase_make_dheap, (ase_heap_wrap_f)_ase_wrap_dheap,
  (ase_heap_add_f)ase_add_dheap, (ase_heap_pop_f)ase_pop_dheap},
 {(ase_heap_constr_f)_ase_make_wheap, (ase_heap_wrap_f)_ase_wrap_wheap,
  (ase_heap_add_f)ase_add_wheap, (ase_heap_pop_f)ase_pop_wheap}};

/* hidden structs */
struct ase_yheap_cell_s {
	Lisp_Object data;
	Lisp_Object colour;

	ase_yheap_cell_t left;
	ase_yheap_cell_t right;

	ase_yheap_cell_t mother;
	ase_yheap_t father;

	ase_yheap_cell_t prev;
	ase_yheap_cell_t next;
};


/* auxiliary stuff */
static inline void
__ase_array_swap(Lisp_Object *d, int idx1, int idx2)
	__attribute__((always_inline));
static inline void
__ase_array_swap(Lisp_Object *d, int idx1, int idx2)
{
	Lisp_Object tmp = d[idx1];
	d[idx1] = d[idx2];
	d[idx2] = tmp;
	return;
}

static inline void
_ase_dheap_swap(ase_dheap_t h, int idx1, int idx2)
	__attribute__((always_inline));
static inline void
_ase_dheap_swap(ase_dheap_t h, int idx1, int idx2)
{
	Lisp_Object *d;

	/* swap priority data */
	d = ase_dheap_cells(h);
	__ase_array_swap(d, idx1, idx2);

	if (!ase_heap_opts_coloured(h))
		return;

	/* swap colours too */
	d = ase_dheap_colours(h);
	__ase_array_swap(d, idx1, idx2);
	return;
}

static inline void
_ase_wheap_swap(ase_wheap_t h, int idx1, int idx2)
	__attribute__((always_inline));
static inline void
_ase_wheap_swap(ase_wheap_t h, int idx1, int idx2)
{
	Lisp_Object *d;

	/* swap priority data */
	d = ase_wheap_cells(h);
	__ase_array_swap(d, idx1, idx2);

	if (!ase_heap_opts_coloured(h))
		return;

	/* swap colours too */
	d = ase_wheap_colours(h);
	__ase_array_swap(d, idx1, idx2);
	return;
}

static inline void
_ase_yheap_cell_swap_data(ase_yheap_cell_t c1, ase_yheap_cell_t c2)
	__attribute__((always_inline));
static inline void
_ase_yheap_cell_swap_data(ase_yheap_cell_t c1, ase_yheap_cell_t c2)
{
	Lisp_Object l1 = ase_yheap_cell_data(c1);
	Lisp_Object l2 = ase_yheap_cell_data(c2);

	ase_yheap_cell_data(c1) = l2;
	ase_yheap_cell_data(c2) = l1;

	/* for coloured heaps */
	l1 = ase_yheap_cell_colour(c1);
	l2 = ase_yheap_cell_colour(c2);
	ase_yheap_cell_colour(c1) = l2;
	ase_yheap_cell_colour(c2) = l1;
	return;
}

/* stuff for the dynacat, printers */
static inline void
_ase_yheap_prnt_cell(ase_yheap_cell_t c, Lisp_Object pcf)
{
	write_c_string(" ", pcf);
	print_internal(ase_yheap_cell_data(c), pcf, 0);
}

static inline void
_ase_yheap_prnt(ase_yheap_t a, Lisp_Object pcf)
{
	ase_yheap_cell_t c = ase_yheap_root(a);

	if (c == NULL) {
		write_c_string(" empty", pcf);
		return;
	}

	while (c && ase_yheap_cell_data(c) != Qnull_pointer) {
		_ase_yheap_prnt_cell(c, pcf);
		c = ase_yheap_cell_next(c);
	}
	return;
}

static void
ase_yheap_prnt(Lisp_Object obj, Lisp_Object pcf, int SXE_UNUSED(foo))
{
	ase_yheap_t h = XASE_YHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("h:0x%08lx@0x%08lx shall be printed...\n",
			    (long unsigned int)h, (long unsigned int)obj);
	write_c_string("#<ase:heap :dynamic", pcf);

	write_c_string(" :size ", pcf);
	write_fmt_str(pcf, "%u", (unsigned int)ase_yheap_size(h));

	if (ase_yheap_root(h) != NULL &&
	    ase_yheap_cell_data(ase_yheap_root(h)) != Qnull_pointer) {
		write_c_string(" :elements", pcf);
		_ase_yheap_prnt(h, pcf);
	} else {
		write_c_string(" :empty>", pcf);
		return;
	}
	write_c_string(">", pcf);
}

static inline void
_ase_dheap_prnt(ase_dheap_t h, Lisp_Object pcf)
{
	size_t size = ase_dheap_size(h);
	Lisp_Object *d = ase_dheap_cells(h);
	unsigned int i;

	for (i = 0; i < size; i++) {
		write_c_string(" ", pcf);
		print_internal(d[i], pcf, 0);
	}
	return;
}

static void
ase_dheap_prnt(Lisp_Object obj, Lisp_Object pcf, int SXE_UNUSED(foo))
{
	ase_dheap_t h = XASE_DHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("d:0x%08lx@0x%08lx shall be printed...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	write_fmt_str(pcf, "#<ase:heap :dense :size %u",
		      (unsigned int)ase_dheap_size(h));

	if (ase_heap_opts_coloured(h)) {
		write_c_string(" :coloured", pcf);
	}

	if (ase_dheap_size(h)) {
		write_c_string(" :elements", pcf);
		_ase_dheap_prnt(h, pcf);
	} else {
		write_c_string(" :empty>", pcf);
		return;
	}
	write_c_string(">", pcf);
}

static inline void
_ase_wheap_prnt(ase_wheap_t h, Lisp_Object pcf)
{
	size_t size = ase_wheap_size(h);
	Lisp_Object *d = ase_wheap_cells(h);
	unsigned int i;

	for (i = 0; i < size; i++) {
		write_c_string(" ", pcf);
		print_internal(d[i], pcf, 0);
	}
	return;
}

static void
ase_wheap_prnt(Lisp_Object obj, Lisp_Object pcf, int SXE_UNUSED(foo))
{
	ase_wheap_t h = XASE_WHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("w:0x%08lx@0x%08lx shall be printed...\n",
			    (long unsigned int)h, (long unsigned int)obj);
	write_fmt_string(pcf, "#<ase:heap :weak :size %u",
			 (unsigned int)ase_wheap_size(h));

	if (ase_heap_opts_coloured(h)) {
		write_c_string(" :coloured", pcf);
	}

	if (ase_wheap_size(h)) {
		write_c_string(" :elements", pcf);
		_ase_wheap_prnt(h, pcf);
	} else {
		write_c_string(" :empty>", pcf);
		return;
	}
	write_c_string(">", pcf);
}

static inline void
_ase_yheap_cell_fini(ase_yheap_cell_t c)
	__attribute__((always_inline));
static inline void
_ase_yheap_cell_fini(ase_yheap_cell_t c)
{
	EMOD_ASE_DEBUG_HEAP("c:0x%08lx shall be freed...\n",
			    (long unsigned int)c);

	memset(c, 0, sizeof(struct ase_yheap_cell_s));
	xfree(c);
	return;
}

static inline void
_ase_yheap_fini(ase_yheap_t h)
	__attribute__((always_inline));
static inline void
_ase_yheap_fini(ase_yheap_t h)
{
	ase_yheap_cell_t c;
	ase_heap_lock(h);
	EMOD_ASE_DEBUG_HEAP("h:0x%08lx freeing used/free cells...\n",
			    (long unsigned int)h);
	c = ase_yheap_root(h);
	while (c) {
		ase_yheap_cell_t tmp = ase_yheap_cell_next(c);
		_ase_yheap_cell_fini(c);
		c = tmp;
	}

	ase_heap_unlock(h);
	ase_heap_fini_mutex(h);
	xfree(ase_heap_options(h));
	return;
}

static void
ase_yheap_fini(Lisp_Object obj, int SXE_UNUSED(foo))
{
	ase_yheap_t h = XASE_YHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("h:0x%08lx@0x%08lx shall be freed...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	_ase_yheap_fini(h);
	memset(h, 0, sizeof(struct ase_yheap_s));
	xfree(h);
	return;
}

static inline void
_ase_dheap_fini(ase_dheap_t h)
	__attribute__((always_inline));
static inline void
_ase_dheap_fini(ase_dheap_t h)
{
	ase_heap_lock(h);
	xfree(ase_dheap_cells(h));
	if (ase_dheap_colours(h)) {
		xfree(ase_dheap_colours(h));
	}
	ase_heap_unlock(h);
	ase_heap_fini_mutex(h);
	xfree(ase_heap_options(h));
	return;
}

static void
ase_dheap_fini(Lisp_Object obj, int SXE_UNUSED(foo))
{
	ase_dheap_t h = XASE_DHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("d:0x%08lx@0x%08lx shall be freed...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	_ase_dheap_fini(h);
	memset(h, 0, sizeof(struct ase_dheap_s));
	xfree(h);
	return;
}

static inline void
_ase_wheap_fini(ase_wheap_t h)
	__attribute__((always_inline));
static inline void
_ase_wheap_fini(ase_wheap_t h)
{
	ase_heap_lock(h);
	xfree(ase_wheap_cells(h));
	xfree(ase_wheap_rbits(h));
	if (ase_dheap_colours(h)) {
		xfree(ase_wheap_colours(h));
	}
	ase_heap_unlock(h);
	ase_heap_fini_mutex(h);
	xfree(ase_heap_options(h));
	return;
}

static void
ase_wheap_fini(Lisp_Object obj, int SXE_UNUSED(foo))
{
	ase_wheap_t h = XASE_WHEAP(obj);

	EMOD_ASE_DEBUG_HEAP("w:0x%08lx@0x%08lx shall be freed...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	_ase_wheap_fini(h);
	memset(h, 0, sizeof(struct ase_wheap_s));
	xfree(h);
	return;
}

static inline void
_ase_yheap_mark_cell(ase_yheap_cell_t c)
{
	if (c == NULL || ase_yheap_cell_data(c) == Qnull_pointer)
		return;
	mark_object(ase_yheap_cell_data(c));
	mark_object(ase_yheap_cell_colour(c));
}

static void
ase_yheap_mark(Lisp_Object obj)
{
	ase_yheap_t h = XASE_YHEAP(obj);
	ase_yheap_cell_t c = ase_yheap_root(h);

	EMOD_ASE_DEBUG_HEAP("h:0x%08lx@0x%08lx shall be marked...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	ase_heap_lock(h);
	while (c && ase_yheap_cell_data(c) != Qnull_pointer) {
		_ase_yheap_mark_cell(c);
		c = ase_yheap_cell_next(c);
	}
	ase_heap_unlock(h);
	return;
}

static void
ase_dheap_mark(Lisp_Object obj)
{
	ase_dheap_t h = XASE_DHEAP(obj);
	Lisp_Object *d, *c;
	size_t size;
	unsigned int i;

	EMOD_ASE_DEBUG_HEAP("d:0x%08lx@0x%08lx shall be marked...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	ase_heap_lock(h);
	d = ase_dheap_cells(h);
	c = ase_dheap_colours(h);
	size = ase_dheap_size(h);
	for (i = 0; i < size; i++) {
		mark_object(d[i]);
	}
	if (c) {
		for (i = 0; i < size; i++) {
			mark_object(c[i]);
		}
	}
	ase_heap_unlock(h);
	return;
}

static void
ase_wheap_mark(Lisp_Object obj)
{
	ase_wheap_t h = XASE_WHEAP(obj);
	Lisp_Object *d, *c;
	size_t size;
	unsigned int i;

	EMOD_ASE_DEBUG_HEAP("w:0x%08lx@0x%08lx shall be marked...\n",
			    (long unsigned int)h, (long unsigned int)obj);

	ase_heap_lock(h);
	d = ase_wheap_cells(h);
	c = ase_wheap_colours(h);
	size = ase_wheap_size(h);
	for (i = 0; i < size; i++) {
		mark_object(d[i]);
	}
	if (c) {
		for (i = 0; i < size; i++) {
			mark_object(c[i]);
		}
	}
	ase_heap_unlock(h);
	return;
}


static inline ase_yheap_cell_t
_ase_make_initial_heap_cell(void)
	__attribute__((always_inline));
static inline ase_yheap_cell_t
_ase_make_initial_heap_cell(void)
{
	ase_yheap_cell_t c = xnew(struct ase_yheap_cell_s);

	ase_yheap_cell_data(c) = Qnull_pointer;
	ase_yheap_cell_colour(c) = Qnil;
	ase_yheap_cell_left(c) = NULL;
	ase_yheap_cell_right(c) = NULL;
	ase_yheap_cell_mother(c) = NULL;
	ase_yheap_cell_father(c) = NULL;
	ase_yheap_cell_prev(c) = NULL;
	ase_yheap_cell_next(c) = NULL;

	EMOD_ASE_DEBUG_HEAP("c:0x%08lx shall be created...\n",
			    (long unsigned int)c);
	return c;
}

static inline ase_yheap_cell_t
_ase_make_heap_cell(ase_yheap_cell_t mother)
	__attribute__((always_inline));
static inline ase_yheap_cell_t
_ase_make_heap_cell(ase_yheap_cell_t mother)
{
	ase_yheap_cell_t c = xnew(struct ase_yheap_cell_s);

	ase_yheap_cell_data(c) = Qnull_pointer;
	ase_yheap_cell_colour(c) = Qnil;
	ase_yheap_cell_left(c) = NULL;
	ase_yheap_cell_right(c) = NULL;
	ase_yheap_cell_mother(c) = mother;
	ase_yheap_cell_father(c) = ase_yheap_cell_father(mother);
	ase_yheap_cell_prev(c) = NULL;
	ase_yheap_cell_next(c) = NULL;

	EMOD_ASE_DEBUG_HEAP("c:0x%08lx shall be created...\n",
			    (long unsigned int)c);
	return c;
}

static inline Lisp_Object
_ase_wrap_yheap(ase_yheap_t h)
{
	Lisp_Object result;

	result = make_dynacat(h);
	XDYNACAT(result)->type = Qase_yheap;

	set_dynacat_printer(result, ase_yheap_prnt);
	set_dynacat_marker(result, ase_yheap_mark);
	set_dynacat_finaliser(result, ase_yheap_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_yheap_prnt);

	EMOD_ASE_DEBUG_HEAP("h:0x%08lx shall be wrapped to 0x%08lx...\n",
			    (long unsigned int)h, (long unsigned int)result);

	return result;
}

static inline Lisp_Object
_ase_wrap_dheap(ase_dheap_t h)
{
	Lisp_Object result;

	result = make_dynacat(h);
	XDYNACAT(result)->type = Qase_dheap;

	set_dynacat_printer(result, ase_dheap_prnt);
	set_dynacat_marker(result, ase_dheap_mark);
	set_dynacat_finaliser(result, ase_dheap_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_dheap_prnt);

	EMOD_ASE_DEBUG_HEAP("d:0x%08lx shall be wrapped to 0x%08lx...\n",
			    (long unsigned int)h, (long unsigned int)result);

	return result;
}

static inline ase_dheap_t
_ase_make_dheap(ase_heap_options_t opts)
{
	ase_dheap_t h = xnew(struct ase_dheap_s);
	size_t all = ase_heap_options_min_size(opts);
	Lisp_Object *d;

	/* status so far */
	ase_dheap_size(h) = 0;
	ase_heap_init_mutex(h);
	ase_dheap_heapp(h) = 1;

	/* options */
	ase_heap_options(h) = opts;

	d = xnew_array_and_zero(Lisp_Object, all);
	ase_dheap_cells(h) = d;
	ase_dheap_colours(h) = NULL;
	ase_dheap_alloc(h) = all;

	if (ase_heap_options_coloured(opts))
		ase_dheap_colours(h) = xnew_array_and_zero(Lisp_Object, all);

	EMOD_ASE_DEBUG_HEAP("d:0x%08lx shall be created...\n",
			    (long unsigned int)h);

	return h;
}

Lisp_Object ase_make_dheap(ase_heap_options_t opts)
{
	ase_dheap_t h = NULL;

	h = _ase_make_dheap(opts);
	return _ase_wrap_dheap(h);
}

static inline Lisp_Object
_ase_wrap_wheap(ase_wheap_t h)
{
	Lisp_Object result;

	result = make_dynacat(h);
	XDYNACAT(result)->type = Qase_wheap;

	set_dynacat_printer(result, ase_wheap_prnt);
	set_dynacat_marker(result, ase_wheap_mark);
	set_dynacat_finaliser(result, ase_wheap_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_wheap_prnt);

	EMOD_ASE_DEBUG_HEAP("w:0x%08lx shall be wrapped to 0x%08lx...\n",
			    (long unsigned int)h, (long unsigned int)result);

	return result;
}

static inline ase_wheap_t
_ase_make_wheap(ase_heap_options_t opts)
{
	ase_wheap_t h = xnew(struct ase_wheap_s);
	size_t all = ase_heap_options_min_size(opts);
	Lisp_Object *d;
	int *r;

	/* status so far */
	ase_wheap_size(h) = 0;
	ase_heap_init_mutex(h);
	ase_wheap_heapp(h) = 1;

	/* options */
	ase_heap_options(h) = opts;

	d = xnew_array_and_zero(Lisp_Object, all);
	r = xnew_array_and_zero(int, all / sizeof(int) / 8);
	ase_wheap_cells(h) = d;
	ase_wheap_rbits(h) = r;
	ase_wheap_colours(h) = NULL;
	ase_wheap_alloc(h) = all;

	if (ase_heap_options_coloured(opts))
		ase_dheap_colours(h) = xnew_array_and_zero(Lisp_Object, all);

	EMOD_ASE_DEBUG_HEAP("w:0x%08lx shall be created...\n",
			    (long unsigned int)h);

	return h;
}

Lisp_Object ase_make_wheap(ase_heap_options_t opts)
{
	ase_wheap_t h = NULL;

	h = _ase_make_wheap(opts);
	return _ase_wrap_wheap(h);
}

static inline ase_yheap_t
_ase_make_yheap(ase_heap_options_t opts)
{
	ase_yheap_t h = xnew(struct ase_yheap_s);
	ase_yheap_cell_t c;

	/* status so far */
	ase_heap_init_mutex(h);
	ase_yheap_heapp(h) = 1;

	/* options */
	ase_heap_options(h) = opts;

	/* create one empty cell */
	c = _ase_make_initial_heap_cell();
	ase_yheap_cell_father(c) = h;
	ase_yheap_root(h) = NULL;
	ase_yheap_first_free(h) = ase_yheap_last_free(h) = c;
	ase_yheap_size(h) = 0;
	ase_yheap_alloc(h) = 1;

	EMOD_ASE_DEBUG_HEAP("h:0x%08lx shall be created...\n",
			    (long unsigned int)h);
	return h;
}

Lisp_Object ase_make_yheap(ase_heap_options_t opts)
{
	ase_yheap_t h = NULL;

	h = _ase_make_yheap(opts);
	return _ase_wrap_yheap(h);
}


static inline void
_ase_fixup_heap_cell(ase_yheap_t h, ase_yheap_cell_t c)
	__attribute__((always_inline));
static inline void
_ase_fixup_heap_cell(ase_yheap_t h, ase_yheap_cell_t c)
{
	/* create new free cells */
	if (ase_yheap_cell_left(c) == NULL &&
	    ase_yheap_cell_right(c) == NULL) {
		ase_yheap_cell_t l = ase_yheap_last_free(h);
		ase_yheap_cell_t n = _ase_make_heap_cell(c);

		ase_yheap_cell_left(c) = n;
		/* append c to l */
		ase_yheap_cell_prev(n) = l;
		ase_yheap_cell_next(l) = n;
		l = ase_yheap_last_free(h) = n;
		ase_yheap_alloc(h)++;

		n = _ase_make_heap_cell(c);
		ase_yheap_cell_right(c) = n;
		/* append c to l */
		ase_yheap_cell_prev(n) = l;
		ase_yheap_cell_next(l) = n;
		ase_yheap_last_free(h) = n;
		ase_yheap_alloc(h)++;
	}
}

static inline void
_ase_dheap_realloc(ase_dheap_t h, size_t new_alloc)
	__attribute__((always_inline));
static inline void
_ase_dheap_realloc(ase_dheap_t h, size_t new_alloc)
{
	Lisp_Object *oldd = ase_dheap_cells(h);
	Lisp_Object *newd = NULL;
	size_t s = ase_dheap_size(h);

	newd = xnew_array_and_zero(Lisp_Object, new_alloc);
	memcpy(newd, oldd, sizeof(Lisp_Object)*s);
	xfree(oldd);
	ase_dheap_cells(h) = newd;
	ase_dheap_alloc(h) = new_alloc;

	if (!ase_heap_opts_coloured(h))
		return;

	oldd = ase_dheap_colours(h);
	newd = xnew_array_and_zero(Lisp_Object, new_alloc);
	memcpy(newd, oldd, sizeof(Lisp_Object)*s);
	xfree(oldd);
	ase_dheap_colours(h) = newd;
	return;
}

static inline void
_ase_dheap_check_resize(ase_dheap_t h)
	__attribute__((always_inline));
static inline void
_ase_dheap_check_resize(ase_dheap_t h)
{
	size_t s = ase_dheap_size(h), all = ase_dheap_alloc(h);

	if (s < ase_heap_opts_min_size(h))
		return;

	if (s >= all) {
		EMOD_ASE_DEBUG_HEAP("d:0x%08lx upsize from %d to %d\n",
				    (long unsigned int)h,
				    (int)all, (int)(all*2));
		_ase_dheap_realloc(h, 2*all);
		return;
	}
	if (s <= all/4) {
		EMOD_ASE_DEBUG_HEAP("d:0x%08lx downsize from %d to %d\n",
				    (long unsigned int)h,
				    (int)all, (int)(all/2));
		_ase_dheap_realloc(h, all/2);
		return;
	}
}

static inline void
_ase_wheap_realloc(ase_wheap_t h, size_t new_alloc)
	__attribute__((always_inline));
static inline void
_ase_wheap_realloc(ase_wheap_t h, size_t new_alloc)
{
	Lisp_Object *oldd = ase_wheap_cells(h);
	Lisp_Object *newd = NULL;
	int *oldr = ase_wheap_rbits(h);
	int *newr = NULL;
	size_t s = ase_wheap_size(h);

	newd = xnew_array_and_zero(Lisp_Object, new_alloc);
	newr = xnew_array_and_zero(int, new_alloc/sizeof(int)/8);
	memcpy(newd, oldd, sizeof(Lisp_Object)*s);
	memcpy(newr, oldr, s/8);
	xfree(oldd);
	xfree(oldr);
	ase_wheap_cells(h) = newd;
	ase_wheap_rbits(h) = newr;
	ase_wheap_alloc(h) = new_alloc;

	if (!ase_heap_opts_coloured(h))
		return;

	oldd = ase_wheap_colours(h);
	newd = xnew_array_and_zero(Lisp_Object, new_alloc);
	memcpy(newd, oldd, sizeof(Lisp_Object)*s);
	xfree(oldd);
	ase_wheap_colours(h) = newd;
	return;
}

static inline void
_ase_wheap_check_resize(ase_wheap_t h)
	__attribute__((always_inline));
static inline void
_ase_wheap_check_resize(ase_wheap_t h)
{
	size_t s = ase_wheap_size(h), all = ase_wheap_alloc(h);

	if (s < ase_heap_opts_min_size(h))
		return;

	if (s >= all) {
		EMOD_ASE_DEBUG_HEAP("w:0x%08lx upsize from %d to %d\n",
				    (long unsigned int)h,
				    (int)all, (int)(all*2));
		_ase_wheap_realloc(h, 2*all);
		return;
	}
	if (s <= all/4) {
		EMOD_ASE_DEBUG_HEAP("w:0x%08lx downsize from %d to %d\n",
				    (long unsigned int)h,
				    (int)all, (int)(all/2));
		_ase_wheap_realloc(h, all/2);
		return;
	}
}

/* dense heap navigation */
static inline int
ase_dheap_cell_mother(int c)
	__attribute__((always_inline));
static inline int
ase_dheap_cell_mother(int c)
{
	return (c-1) >> 1;
}

static inline int
ase_dheap_cell_left(int c)
	__attribute__((always_inline));
static inline int
ase_dheap_cell_left(int c)
{
	return 2*c+1;
}

static inline int
ase_dheap_cell_right(int c)
	__attribute__((always_inline));
static inline int
ase_dheap_cell_right(int c)
{
	return 2*c+2;
}

/* weak heap navigation */
static inline int
ase_wheap_cell_rbit(ase_wheap_t h, int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_rbit(ase_wheap_t h, int c)
{
	int *r = ase_wheap_rbits(h);
	int w = sizeof(int) * 8;
	int cell = c / w, bit = c % w;
	int bit2 = 1 << bit;

	if (r[cell] & bit2)
		return 1;
	else
		return 0;
}

static inline int
ase_wheap_cell_rbit_neg(ase_wheap_t h, int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_rbit_neg(ase_wheap_t h, int c)
{
	int *r = ase_wheap_rbits(h);
	int w = sizeof(int) * 8;
	int cell = c / w, bit = c % w;
	int bit2 = 1 << bit;

	return r[cell] ^= bit2;
}

static inline int
ase_wheap_cell_mother(ase_wheap_t SXE_UNUSED(h), int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_mother(ase_wheap_t SXE_UNUSED(h), int c)
{
	return (c >> 1);
}

static inline int
ase_wheap_cell_nana(ase_wheap_t h, int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_nana(ase_wheap_t h, int c)
{
	/* used to use odd(c), but simpler is odd(c) := c & 1 */
	while ((c&1) == ase_wheap_cell_rbit(h, (c>>1)))
		c = ase_wheap_cell_mother(h, c);
	return ase_wheap_cell_mother(h, c);
}

static inline int
ase_wheap_cell_left(ase_wheap_t h, int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_left(ase_wheap_t h, int c)
{
	return 2*c + ase_wheap_cell_rbit(h, c);
}

static inline int
ase_wheap_cell_right(ase_wheap_t h, int c)
	__attribute__((always_inline));
static inline int
ase_wheap_cell_right(ase_wheap_t h, int c)
{
	return 2*c + 1 - ase_wheap_cell_rbit(h, c);
}


static void
_ase_yheapify_sink(ase_yheap_cell_t c)
{
	/* iterative approach */
	Lisp_Object cdata = ase_yheap_cell_data(c);
	ase_yheap_cell_t l, r;
	ase_binary_relation_t rel = ase_heap_opts_po(ase_yheap_cell_father(c));

	if (cdata == Qnull_pointer) {
		return;
	}
	while ((l = ase_yheap_cell_left(c))) {
		Lisp_Object ldata = 0, rdata = 0;
		ase_yheap_cell_t chosen = l;

		if (l == NULL ||
		    (ldata = ase_yheap_cell_data(l)) == Qnull_pointer) {
			return;
		}

		if ((r = ase_yheap_cell_right(c)) &&
		    (rdata = ase_yheap_cell_data(r)) &&
		    ent_binrel(rel, rdata, ldata)) {
			chosen = r;
			ldata = rdata;
		}

		if (ent_binrel(rel, ldata, cdata)) {
			_ase_yheap_cell_swap_data(c, chosen);
			c = chosen;
		} else {
			return;
		}
	}
	return;
}

static inline void
_ase_dheapify_sink(ase_dheap_t h, int c)
	__attribute__((always_inline));
static inline void
_ase_dheapify_sink(ase_dheap_t h, int c)
{
	/* iterative approach */
	size_t size = ase_dheap_size(h);
	Lisp_Object *d = ase_dheap_cells(h);
	Lisp_Object cdata = d[c];
	ase_binary_relation_t rel = ase_heap_opts_po(h);
	unsigned int l, r;

	if (cdata == Qnull_pointer) {
		return;
	}
	while ((l = ase_dheap_cell_left(c)) && l < size && d[l]) {
		int chosen = l;

		if ((r = l+1) && r < size && d[r] &&
		    ent_binrel(rel, d[r], d[l])) {
			chosen = r;
		}

		if (ent_binrel(rel, d[chosen], cdata)) {
			_ase_dheap_swap(h, c, chosen);
			c = chosen;
			continue;
		}
		return;
	}
	return;
}

static void
_ase_yheapify(ase_yheap_cell_t c)
{
	Lisp_Object cdata = ase_yheap_cell_data(c);
	ase_yheap_cell_t l = ase_yheap_cell_left(c);
	ase_yheap_cell_t r = ase_yheap_cell_right(c);

	if (cdata == Qnull_pointer || (l == NULL && r == NULL))
		return;

	if (l) {
		_ase_yheapify(l);
	}
	if (r) {
		_ase_yheapify(r);
	}

	_ase_yheapify_sink(c);
}

void
ase_yheapify(ase_yheap_t h)
{
	_ase_yheapify(ase_yheap_root(h));
	ase_yheap_heapp(h) = 1;
	return;
}

static inline void
_ase_dheap_sift(ase_dheap_t h, int start, int count)
	__attribute__((always_inline));
static inline void
_ase_dheap_sift(ase_dheap_t h, int start, int count)
{
	int root = start, child;
	ase_binary_relation_t po = ase_heap_opts_po(h);
	Lisp_Object *d = ase_dheap_cells(h);

	while ((child = ase_dheap_cell_left(root)) < count) {
		if (child < count-1 &&
		    ent_binrel(po, d[child], d[child+1])) {
			child++; /* choose the right child */
		}
		if (ent_binrel(po, d[root], d[child])) {
			_ase_dheap_swap(h, root, child);
			root = child;
		} else {
			return;
		}
	}
	return;
}

static inline int
__ase_wheapify_sink(ase_wheap_t h, int i, int j)
	__attribute__((always_inline));
static inline int
__ase_wheapify_sink(ase_wheap_t h, int i, int j)
{
/* aka Merge(i, j) in Edelkamp/Wegener's paper */
	Lisp_Object *d = ase_wheap_cells(h);
	ase_binary_relation_t po = ase_heap_opts_po(h);
	int res = 0;

	EMOD_ASE_DEBUG_HEAP("Merge(%d, %d)\n", i, j);
	if ((res = ent_binrel(po, d[j], d[i]))) {
		/* swap(nana(idx), idx) */
		_ase_wheap_swap(h, i, j);
		/* update bit field */
		ase_wheap_cell_rbit_neg(h, j);
	}
	return res;
}

static inline void
_ase_wheapify_sink(ase_wheap_t h, int idx)
	__attribute__((always_inline));
static inline void
_ase_wheapify_sink(ase_wheap_t h, int m)
{
/* aka MergeForest(m) in Edelkamp/Wegener's paper */
	int l, x = 1;

	EMOD_ASE_DEBUG_HEAP("MergeForest(%d)\n", m);
	if (m <= 1)
		return;

	while ((l = ase_wheap_cell_left(h, x)) < m) {
		x = l;
	}
	while (x > 0) {
		/* merge(m,x) */
		__ase_wheapify_sink(h, m, x);
		/* move on to mother cell */
		x = ase_wheap_cell_mother(h, x);
	}
	return;
}

void
_ase_wheapify(ase_wheap_t h)
{
	int i;
	for (i = ase_wheap_size(h)-1; i >= 1; i--) {
		__ase_wheapify_sink(h, ase_wheap_cell_nana(h, i), i);
	}
}

void
ase_wheap_sort(ase_wheap_t h)
{
	int s = ase_wheap_size(h), i, j;

	/* normally WeakHeapify is called first
	 * howbeit, our wheaps always suffice the weak property */
	for (--s; s >= 2; s--) {
		_ase_wheapify_sink(h, s);
	}
	/* now the i-th most extreme value is at index ase_wheap_size-i */
	s = (ase_wheap_size(h)+1)>>1;
	for (i = 1, j = ase_wheap_size(h)-1; i < s; i++, j--) {
		_ase_wheap_swap(h, i, j);
	}
}

void
ase_dheap_sort(ase_dheap_t h)
{
	size_t size = ase_dheap_size(h);
	int start = size/2 - 1, end = size-1;

	while (start >= 0) {
		_ase_dheap_sift(h, start, size);
		start--;
	}
	while (end > 0) {
		_ase_dheap_swap(h, end, 0);
		_ase_dheap_sift(h, 0, end);
		end--;
	}
	return;
}

#if defined __GNUC__
static Lisp_Object
ase_add_yheap_unwind_protect(Lisp_Object unwind_obj)
	__attribute__((unused));
#endif
static Lisp_Object
ase_add_yheap_unwind_protect(Lisp_Object unwind_obj)
{
	ase_yheap_t h = get_opaque_ptr(unwind_obj);
	ase_heap_unlock(h);
	free_opaque_ptr(unwind_obj);
	return Qnil;
}

void
ase_add_yheap(ase_yheap_t h, Lisp_Object o, Lisp_Object colour)
{
	ase_yheap_cell_t c, mother;
#if 0
	int speccount = specpdl_depth();
#endif

	EMOD_ASE_DEBUG_HEAP("c:0x%08lx shall be populated...\n",
			    (long unsigned int)c);

#if 0
	record_unwind_protect(ase_add_yheap_unwind_protect,
			      make_opaque_ptr(h));
#endif

	ase_heap_lock(h);
	c = ase_yheap_first_free(h);
	if (c == NULL) {
		EMOD_ASE_CRITICAL("broken heap 0x%08lx\n",
				  (long unsigned int)h);
		ase_heap_unlock(h);
		return;
	}

	ase_yheap_cell_data(c) = o;
	ase_yheap_cell_colour(c) = colour;

	/* it may violate the heap property now */
	ase_yheap_heapp(h) = 0;

	if (ase_yheap_cell_left(c) == NULL) {
		_ase_fixup_heap_cell(h, c);
	}
	ase_yheap_first_free(h) = ase_yheap_cell_next(c);
	ase_yheap_size(h)++;

	if (ase_yheap_root(h) == NULL) {
		ase_yheap_root(h) = c;
	}

	/* bottom-up heapify now */
	mother = c;
	while ((mother = ase_yheap_cell_mother(mother))) {
		_ase_yheapify_sink(mother);
	}
	ase_heap_unlock(h);
	return;
}

#if defined __GNUC__
static Lisp_Object
ase_add_dheap_unwind_protect(Lisp_Object unwind_obj)
	__attribute__((unused));
#endif
static Lisp_Object
ase_add_dheap_unwind_protect(Lisp_Object unwind_obj)
{
	ase_dheap_t h = get_opaque_ptr(unwind_obj);
	ase_heap_unlock(h);
	free_opaque_ptr(unwind_obj);
	return Qnil;
}

void
ase_add_dheap(ase_dheap_t h, Lisp_Object o, Lisp_Object colour)
{
	int idx, mother;
#if 0
	int speccount = specpdl_depth();
#endif
	Lisp_Object *d, *c;

	EMOD_ASE_DEBUG_HEAP("c:%d shall be populated...\n", idx);

#if 0
	record_unwind_protect(ase_add_dheap_unwind_protect,
			      make_opaque_ptr(h));
#endif

	/* lock the heap */
	ase_heap_lock(h);
	mother = idx = ase_dheap_size(h)++;
	d = ase_dheap_cells(h);
	c = ase_dheap_colours(h);

	d[idx] = o;
	if (c) {
		c[idx] = colour;
	}

	/* it may violate the heap property now */
	ase_dheap_heapp(h) = 0;

	/* bottom-up heapify now */
	mother = idx;
	while ((mother = ase_dheap_cell_mother(mother)) != -1) {
		_ase_dheapify_sink(h, mother);
	}

	_ase_dheap_check_resize(h);
	ase_heap_unlock(h);
	return;
}

#if defined __GNUC__
static Lisp_Object
ase_add_wheap_unwind_protect(Lisp_Object unwind_obj)
	__attribute__((unused));
#endif
static Lisp_Object
ase_add_wheap_unwind_protect(Lisp_Object unwind_obj)
{
	ase_wheap_t h = get_opaque_ptr(unwind_obj);
	ase_heap_unlock(h);
	free_opaque_ptr(unwind_obj);
	return Qnil;
}

void
ase_add_wheap(ase_wheap_t h, Lisp_Object o, Lisp_Object colour)
{
	int idx;
#if 0
	int speccount = specpdl_depth();
#endif
	Lisp_Object *d;

	EMOD_ASE_DEBUG_HEAP("c:%d shall be populated...\n", idx);

#if 0
	record_unwind_protect(ase_add_wheap_unwind_protect,
			      make_opaque_ptr(h));
#endif

	/* lock the heap */
	ase_heap_lock(h);
	idx = ase_wheap_size(h)++;
	d = ase_wheap_cells(h);

	d[idx] = o;
	if (ase_heap_opts_coloured(h)) {
		ase_wheap_colours(h)[idx] = colour;
	}

	/* it may violate the heap property now */
	ase_wheap_heapp(h) = 0;

	/* bottom-up wheapify now */
	while (idx) {
		int nana = ase_wheap_cell_nana(h, idx);
		if (!__ase_wheapify_sink(h, nana, idx))
			break;
		idx = nana;
	}

	_ase_wheap_check_resize(h);
	ase_heap_unlock(h);
	return;
}

/* popping (dequeue operation) */
Lisp_Object
ase_pop_yheap(ase_yheap_t h)
{
	ase_yheap_cell_t rc, c;
	Lisp_Object result = Qnil, swap;

	/* lock the heap */
	ase_heap_lock(h);
	rc = ase_yheap_root(h);
	if (rc == NULL || ase_yheap_cell_data(rc) == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	if (!ase_heap_opts_coloured(h)) {
		result = ase_yheap_cell_data(rc);
	} else {
		result = ase_yheap_cell_colour(rc);
	}
	c = ase_yheap_cell_prev(ase_yheap_first_free(h));
	swap = ase_yheap_cell_data(c);
	ase_yheap_first_free(h) = c;
	ase_yheap_size(h)--;

	ase_yheap_cell_data(rc) = swap;
	ase_yheap_cell_data(c) = Qnull_pointer;
	ase_yheap_cell_colour(c) = Qnull_pointer;
	_ase_yheapify_sink(rc);

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_pop_dheap(ase_dheap_t h)
{
	Lisp_Object *d, result;
	int end_idx;

	/* lock */
	ase_heap_lock(h);
	d = ase_dheap_cells(h);

	if (d[0] == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	/* pop off the most extreme element */
	if (!ase_heap_opts_coloured(h)) {
		result = d[0];
	} else {
		result = ase_dheap_colours(h)[0];
		ase_dheap_colours(h)[0] = Qnull_pointer;
	}

	end_idx = --ase_dheap_size(h);
	_ase_dheap_swap(h, 0, end_idx);
	d[end_idx] = Qnull_pointer;

	/* reestablish heap property */
	_ase_dheapify_sink(h, 0);
	_ase_dheap_check_resize(h);

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_pop_wheap(ase_wheap_t h)
{
	Lisp_Object *d, *c, result;
	int end_idx;

	/* lock */
	ase_heap_lock(h);
	d = ase_wheap_cells(h);
	c = ase_wheap_colours(h);

	if (d[0] == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	/* pop off the most extreme element */
	if (!ase_heap_opts_coloured(h)) {
		result = d[0];
	} else {
		result = c[0];
	}

	/* MergeForest(end_idx) */
	end_idx = --ase_wheap_size(h);
	_ase_wheapify_sink(h, end_idx);

	d[0] = d[end_idx];
	d[end_idx] = Qnull_pointer;

	if (ase_heap_opts_coloured(h)) {
		c[0] = c[end_idx];
		c[end_idx] = Qnull_pointer;
	}

	/* maybe resize? */
	_ase_wheap_check_resize(h);

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_yheap_top(ase_yheap_t h)
{
	ase_yheap_cell_t rc;
	Lisp_Object result = Qnil;

	/* lock the heap */
	ase_heap_lock(h);
	rc = ase_yheap_root(h);
	if (rc == NULL || ase_yheap_cell_data(rc) == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	/* grab the most extreme element */
	if (!ase_heap_opts_coloured(h)) {
		result = ase_yheap_cell_data(rc);
	} else {
		result = ase_yheap_cell_colour(rc);
	}

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_dheap_top(ase_dheap_t h)
{
	Lisp_Object *d, *c, result;

	/* lock */
	ase_heap_lock(h);
	d = ase_dheap_cells(h);
	c = ase_dheap_colours(h);

	if (d[0] == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	/* grab the most extreme element */
	if (!ase_heap_opts_coloured(h)) {
		result = d[0];
	} else {
		result = c[0];
	}

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_wheap_top(ase_wheap_t h)
{
	Lisp_Object *d, *c, result;

	/* lock */
	ase_heap_lock(h);
	d = ase_wheap_cells(h);
	c = ase_wheap_colours(h);

	if (d[0] == Qnull_pointer) {
		ase_heap_unlock(h);
		return Qnil;
	}

	/* grab the most extreme element */
	if (!ase_heap_opts_coloured(h)) {
		result = d[0];
	} else {
		result = c[0];
	}

	ase_heap_unlock(h);
	return result;
}

Lisp_Object
ase_yheap_top_rank(ase_yheap_t h)
{
	ase_yheap_cell_t rc;
	Lisp_Object result = Qnil;

	/* lock the heap */
	ase_heap_lock(h);
	rc = ase_yheap_root(h);

	result = ase_yheap_cell_data(rc);
	ase_heap_unlock(h);
	if (result != Qnull_pointer) {
		return result;
	}
	return Qnil;
}

Lisp_Object
ase_dheap_top_rank(ase_dheap_t h)
{
	Lisp_Object *d, result;

	/* lock */
	ase_heap_lock(h);
	d = ase_dheap_cells(h);

	result = d[0];
	ase_heap_unlock(h);

	if (result != Qnull_pointer) {
		return result;
	}
	return Qnil;
}

Lisp_Object
ase_wheap_top_rank(ase_wheap_t h)
{
	Lisp_Object *d, result;

	/* lock */
	ase_heap_lock(h);
	d = ase_wheap_cells(h);

	result = d[0];
	ase_heap_unlock(h);

	if (result != Qnull_pointer) {
		return result;
	}
	return Qnil;
}


static inline Lisp_Object
_ase_heap_to_listX(void *h, ase_heap_pop_f popfun)
	__attribute__((always_inline));
static inline Lisp_Object
_ase_heap_to_listX(void *h, ase_heap_pop_f popfun)
{
	Lisp_Object result = Qnil, tmp, trv;

	result = trv = Fcons(Qnil, Qnil);
	while (!NILP(tmp = popfun(h))) {
		trv = (XCDR(trv) = Fcons(tmp, Qnil));
	}
	return XCDR(result);
}

Lisp_Object
ase_yheap_to_listX(ase_yheap_t h)
{
	return _ase_heap_to_listX(h, (ase_heap_pop_f)ase_pop_yheap);
}

Lisp_Object
ase_dheap_to_listX(ase_dheap_t h)
{
	return _ase_heap_to_listX(h, (ase_heap_pop_f)ase_pop_dheap);
}

Lisp_Object
ase_wheap_to_listX(ase_wheap_t h)
{
	return _ase_heap_to_listX(h, (ase_heap_pop_f)ase_pop_wheap);
}

Lisp_Object
ase_yheap_to_list(ase_yheap_t h)
{
	Lisp_Object res, tmp;

	res = tmp = ase_yheap_to_listX(h);
	/* now add them all back to the heap */
	while (!NILP(tmp)) {
		ase_add_yheap(h, XCAR(tmp), Qnil);
		tmp = XCDR(tmp);
	}
	return res;
}

Lisp_Object
ase_dheap_to_list(ase_dheap_t h)
{
	size_t size = ase_yheap_size(h);
	Lisp_Object *d = ase_dheap_cells(h);
	Lisp_Object result = Qnil;
	int i;

	ase_dheap_sort(h);
	for (i = size-1; i >= 0; i--) {
		result = Fcons(d[i], result);
	}
	return result;
}

Lisp_Object
ase_wheap_to_list(ase_wheap_t h)
{
	size_t size = ase_wheap_size(h);
	Lisp_Object *d = ase_wheap_cells(h);
	Lisp_Object result = Qnil;
	int i;

	ase_wheap_sort(h);
	for (i = size-1; i >= 0; i--) {
		result = Fcons(d[i], result);
	}
	return result;
}

static inline Lisp_Object
_ase_heap_to_vectorX(void *h, ase_heap_pop_f popfun, size_t size)
	__attribute__((always_inline));
static inline Lisp_Object
_ase_heap_to_vectorX(void *h, ase_heap_pop_f popfun, size_t size)
{
	size_t i = 0;
	Lisp_Object result = make_vector(size, Qnil), tmp;

	while (!NILP(tmp = popfun(h))) {
		XVECTOR_DATA(result)[i++] = tmp;
	}
	return result;
}

Lisp_Object
ase_yheap_to_vectorX(ase_yheap_t h)
{
	size_t s = ase_yheap_size(h);
	return _ase_heap_to_vectorX(h, (ase_heap_pop_f)ase_pop_yheap, s);
}

Lisp_Object
ase_dheap_to_vectorX(ase_dheap_t h)
{
	size_t s = ase_dheap_size(h);
	return _ase_heap_to_vectorX(h, (ase_heap_pop_f)ase_pop_dheap, s);
}

Lisp_Object
ase_wheap_to_vectorX(ase_wheap_t h)
{
	size_t s = ase_wheap_size(h);
	return _ase_heap_to_vectorX(h, (ase_heap_pop_f)ase_pop_wheap, s);
}

Lisp_Object
ase_yheap_to_vector(ase_yheap_t h)
{
	Lisp_Object res;
	int i;

	res = ase_yheap_to_vectorX(h);
	/* now add them all back to the heap */
	i = XVECTOR_LENGTH(res);
	while (--i >= 0) {
		ase_add_yheap(h, XVECTOR_DATA(res)[i], Qnil);
	}
	return res;
}

Lisp_Object
ase_dheap_to_vector(ase_dheap_t h)
{
	size_t size = ase_dheap_size(h);
	Lisp_Object *d = ase_dheap_cells(h);
	Lisp_Object result = make_vector(size, Qnil);
	size_t i = 0;

	ase_dheap_sort(h);
	for (i = 0; i < size; i++) {
		XVECTOR_DATA(result)[i] = d[i];
	}
	return result;
}

Lisp_Object
ase_wheap_to_vector(ase_wheap_t h)
{
	size_t size = ase_wheap_size(h);
	Lisp_Object *d = ase_wheap_cells(h);
	Lisp_Object result = make_vector(size, Qnil);
	size_t i = 0;

	ase_wheap_sort(h);
	for (i = 0; i < size; i++) {
		XVECTOR_DATA(result)[i] = d[i];
	}
	return result;
}

static inline Lisp_Object
_ase_heap_to_dllistX(void *h, ase_heap_pop_f popfun)
	__attribute__((always_inline));
static inline Lisp_Object
_ase_heap_to_dllistX(void *h, ase_heap_pop_f popfun)
{
	dllist_t resdll = make_dllist();
	Lisp_Object result = Qnil, tmp;

	while (!NILP(tmp = popfun(h))) {
		dllist_append(resdll, (void*)tmp);
	}

	XSETDLLIST(result, resdll);
	return result;
}

Lisp_Object
ase_yheap_to_dllistX(ase_yheap_t h)
{
	return _ase_heap_to_dllistX(h, (ase_heap_pop_f)ase_pop_yheap);
}

Lisp_Object
ase_dheap_to_dllistX(ase_dheap_t h)
{
	return _ase_heap_to_dllistX(h, (ase_heap_pop_f)ase_pop_dheap);
}

Lisp_Object
ase_wheap_to_dllistX(ase_wheap_t h)
{
	return _ase_heap_to_dllistX(h, (ase_heap_pop_f)ase_pop_wheap);
}

Lisp_Object
ase_yheap_to_dllist(ase_yheap_t h)
{
	Lisp_Object res;

	res = ase_yheap_to_dllistX(h);
	/* now add them all back to the heap */
	WITH_DLLIST_TRAVERSE(
		XDLLIST(res),
		ase_add_yheap(h, (Lisp_Object)dllist_item, Qnil));
	return res;
}

Lisp_Object
ase_dheap_to_dllist(ase_dheap_t h)
{
	size_t size = ase_dheap_size(h);
	Lisp_Object *d = ase_dheap_cells(h);
	Lisp_Object result = Qnil;
	dllist_t resdll = make_dllist();
	size_t i;

	ase_dheap_sort(h);
	for (i = 0; i < size; i++) {
		dllist_append(resdll, (void*)d[i]);
	}

	XSETDLLIST(result, resdll);
	return result;
}

Lisp_Object
ase_wheap_to_dllist(ase_wheap_t h)
{
	size_t size = ase_wheap_size(h);
	Lisp_Object *d = ase_wheap_cells(h);
	Lisp_Object result = Qnil;
	dllist_t resdll = make_dllist();
	size_t i;

	ase_wheap_sort(h);
	for (i = 0; i < size; i++) {
		dllist_append(resdll, (void*)d[i]);
	}

	XSETDLLIST(result, resdll);
	return result;
}

static inline ase_heap_kind_t
ase_determine_heap_kind(Lisp_Object o)
	__attribute__((always_inline));
static inline ase_heap_kind_t
ase_determine_heap_kind(Lisp_Object o)
{
	if (EQ(o, Qweak)) {
		return ASE_HEAP_WEAK;
	} else if (EQ(o, Qdense)) {
		return ASE_HEAP_DENSE;
	} else if (EQ(o, Qdynamic)) {
		return ASE_HEAP_DYNAMIC;
	}

	return ASE_HEAP_WEAK;
}


/* ###autoload */
DEFUN("ase-heap", Fase_heap, 0, MANY, 0, /*
Return a new heap object.

Arguments: &rest keys
:kind  kind of the heap, can be one of 'weak (default), 'dense, or 'dynamic
:relation
*/
      (int nargs, Lisp_Object *args))
{
	ase_heap_options_t opts = xnew(struct ase_heap_options_s);
	ase_heap_constr_f constrf;
	ase_heap_wrap_f wrapf;
	ase_heap_kind_t kind;
	int i;

	/* standard options */
	ase_heap_options_po(opts) = ASE_BINARY_REL_GREATERP;
	ase_heap_options_pof(opts) = NULL;
	kind = ase_heap_options_kind(opts) =
		ase_determine_heap_kind(Qase_heap_default_kind);
	ase_heap_options_min_size(opts) = ASE_HEAP_MIN_SIZE;
	ase_heap_options_coloured(opts) = 0;

	for (i = 0; i < nargs-1; i++) {
		if (EQ(args[i], Q_kind)) {
			kind = ase_heap_options_kind(opts) =
				ase_determine_heap_kind(args[++i]);
		}
		if (EQ(args[i], Q_relation)) {
		}
		if (EQ(args[i], Q_coloured)) {
			if (!NILP(args[++i]))
				ase_heap_options_coloured(opts) = 1;
		}
	}

	constrf = ase_heap_ops[kind].constrf;
	wrapf = ase_heap_ops[kind].wrapf;
	return wrapf(constrf(opts));
}

DEFUN("ase-add-heap", Fase_add_heap, 2, 3, 0, /*
Add OBJECT to HEAP and (optionally) COLOUR it.
*/
      (heap, object, colour))
{
	void *h = NULL;
	ase_heap_add_f addfun = NULL;

	CHECK_ASE_HEAP(heap);

	if (ASE_WHEAPP(heap)) {
		addfun = ase_heap_ops[ASE_HEAP_WEAK].addf;
		h = XASE_WHEAP(heap);
	} else if (ASE_DHEAPP(heap)) {
		addfun = ase_heap_ops[ASE_HEAP_DENSE].addf;
		h = XASE_DHEAP(heap);
	} else if (ASE_YHEAPP(heap)) {
		addfun = ase_heap_ops[ASE_HEAP_DYNAMIC].addf;
		h = XASE_YHEAP(heap);
	}
	addfun(h, object, colour);
	return heap;
}

DEFUN("ase-pop-heap", Fase_pop_heap, 1, 1, 0, /*
Pop off and return the most extreme element of HEAP.
*/
      (heap))
{
	void *h = NULL;
	ase_heap_pop_f popfun = NULL;

	CHECK_ASE_HEAP(heap);

	if (ASE_WHEAPP(heap)) {
		popfun = ase_heap_ops[ASE_HEAP_WEAK].popf;
		h = XASE_WHEAP(heap);
	} else if (ASE_DHEAPP(heap)) {
		popfun = ase_heap_ops[ASE_HEAP_DENSE].popf;
		h = XASE_DHEAP(heap);
	} else if (ASE_YHEAPP(heap)) {
		popfun = ase_heap_ops[ASE_HEAP_DYNAMIC].popf;
		h = XASE_YHEAP(heap);
	}
	return popfun(h);
}


/* convenience funs */
DEFUN("ase-heap-size", Fase_heap_size, 1, 1, 0, /*
Return the number of elements inside HEAP.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);

	if (ASE_WHEAPP(heap)) {
		return make_int(ase_wheap_size(XASE_WHEAP(heap)));
	} else if (ASE_DHEAPP(heap)) {
		return make_int(ase_dheap_size(XASE_DHEAP(heap)));
	} else if (ASE_YHEAPP(heap)) {
		return make_int(ase_yheap_size(XASE_YHEAP(heap)));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-top", Fase_heap_top, 1, 1, 0, /*
Return the topmost element of HEAP.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);

	if (ASE_WHEAPP(heap)) {
		return ase_wheap_top(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_top(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_top(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-top-rank", Fase_heap_top_rank, 1, 1, 0, /*
Return the rank (priority) of the topmost element of HEAP.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);

	if (ASE_WHEAPP(heap)) {
		return ase_wheap_top_rank(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_top_rank(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_top_rank(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-list", Fase_heap_to_list, 1, 1, 0, /*
Return a (sorted) list with the elements of HEAP.
HEAP is kept alive.  See also `ase-heap-to-list*'
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_list(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_list(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_list(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-list*", Fase_heap_to_listX, 1, 1, 0, /*
Return a (sorted) list with the elements of HEAP.
HEAP is destroyed by side-effect, each element from HEAP is
popped off and consed to the result list.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_listX(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_listX(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_listX(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-vector", Fase_heap_to_vector, 1, 1, 0, /*
Return a (sorted) vector with the elements of HEAP.
HEAP is kept alive hereby.  See also `ase-heap-to-vector*'.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_vector(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_vector(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_vector(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-vector*", Fase_heap_to_vectorX, 1, 1, 0, /*
Return a (sorted) vector with the elements of HEAP.
HEAP is destroyed by side-effect, each element from HEAP is
popped off and written into the result vector.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_vectorX(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_vectorX(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_vectorX(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-dllist", Fase_heap_to_dllist, 1, 1, 0, /*
Return a (sorted) list with the elements of HEAP.
HEAP is kept intact.  See also `ase-heap-to-dllist*'.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_dllist(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_dllist(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_dllist(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}

DEFUN("ase-heap-to-dllist*", Fase_heap_to_dllistX, 1, 1, 0, /*
Return a (sorted) list with the elements of HEAP.
HEAP is destroyed by side-effect, each element from HEAP is
popped off and appended to the result dllist.
*/
      (heap))
{
	CHECK_ASE_HEAP(heap);
	if (ASE_WHEAPP(heap)) {
		return ase_wheap_to_dllistX(XASE_WHEAP(heap));
	} else if (ASE_DHEAPP(heap)) {
		return ase_dheap_to_dllistX(XASE_DHEAP(heap));
	} else if (ASE_YHEAPP(heap)) {
		return ase_yheap_to_dllistX(XASE_YHEAP(heap));
	}
	return Qnull_pointer;
}


/* initialiser code */
#define EMODNAME	ase_heap

void
EMOD_PUBINIT(void)
{
	DEFSUBR(Fase_heap);
	DEFSUBR(Fase_add_heap);
	DEFSUBR(Fase_pop_heap);

	DEFSUBR(Fase_heap_to_list);
	DEFSUBR(Fase_heap_to_listX);
	DEFSUBR(Fase_heap_to_vector);
	DEFSUBR(Fase_heap_to_vectorX);
	DEFSUBR(Fase_heap_to_dllist);
	DEFSUBR(Fase_heap_to_dllistX);

	DEFSUBR(Fase_heap_size);
	DEFSUBR(Fase_heap_top);
	DEFSUBR(Fase_heap_top_rank);

	DEFASETYPE_WITH_OPS(Qase_heap, "ase:heap");
	defsymbol(&Qase_heapp, "ase:heapp");
	defsymbol(&Qase_yheap, "ase:yheap");
	defsymbol(&Qase_yheapp, "ase:yheapp");
	defsymbol(&Qase_dheap, "ase:dheap");
	defsymbol(&Qase_dheapp, "ase:dheapp");
	defsymbol(&Qase_wheap, "ase:wheap");
	defsymbol(&Qase_wheapp, "ase:wheapp");

	DEFSYMBOL(Qweak);
	DEFSYMBOL(Qdense);
	DEFSYMBOL(Qdynamic);

	DEFKEYWORD(Q_kind);
	DEFKEYWORD(Q_relation);
	DEFKEYWORD(Q_coloured);

	Fprovide(intern("ase-heap"));

	DEFVAR_LISP("ase:heap-default-kind", &Qase_heap_default_kind /*
*Default kind of newly created heaps.

Default: 'weak
								     */);
	Qase_heap_default_kind = Qweak;
}

void
EMOD_PUBREINIT(void)
{
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-heap"));
}


/* ase-heap.c ends here */
