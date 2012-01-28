/*** ase-interval.c -- Interval Sorcery
 *
 * Copyright (C) 2006-2008 Sebastian Freundt
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

#include "config.h"
#include "sxemacs.h"
#include "ent/ent.h"
#include "ase.h"
#include "ase-interval.h"

#define EMODNAME	ase_interval
PROVIDE(ase_interval);
REQUIRE(ase_interval, "ase", "ase-cartesian");

Lisp_Object Q_open, Q_closed, Q_less, Q_greater, Q_eql, Q_unknown;
Lisp_Object Q_disjoint, Q_connected;
Lisp_Object Qase_interval, Qase_intervalp;
Lisp_Object Qase_interval_union, Qase_interval_union_p;
Lisp_Object Qase_empty_interval, Qase_universe_interval;

static struct ase_category_s __interval_cat = {
	.setoid_p = true,
	.magma_p = false,
	.algebra_p = false,
	.mapping_p = false,
	.relation_p = false,
	.orderable_p = true,
};
const ase_category_t ase_interval_cat = (const ase_category_t)&__interval_cat;
typedef enum ase_interval_type_e ase_interval_type_t;

static inline int _ase_interval_less_p(ase_interval_t, ase_interval_t);
static inline int _ase_interval_equal_p(ase_interval_t, ase_interval_t);
static inline int ase_interval_less_p(Lisp_Object, Lisp_Object);
static inline int ase_interval_equal_p(Lisp_Object, Lisp_Object);

static DOESNT_RETURN ase_interval_embedding_error(Lisp_Object, Lisp_Object);
static ase_interval_type_t ase_interval_type(Lisp_Object o);
static int _ase_normalise_union_intr(ase_interval_union_item_t);

static inline Lisp_Object ase_intersect_intv_intv(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_intv_union(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_intr_intr(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_intr_union(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_union_intv(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_union_intr(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_intersect_union_union(Lisp_Object, Lisp_Object);

static inline Lisp_Object ase_subtract_intv_intv(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_intv_union(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_intr_intr(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_intr_union(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_union_intv(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_union_intr(Lisp_Object, Lisp_Object);
static inline Lisp_Object ase_subtract_union_union(Lisp_Object, Lisp_Object);


enum ase_interval_type_e {
	ASE_ITYPE_OBJECT,
	ASE_ITYPE_INTERVAL,
	ASE_ITYPE_INTERIOR,
	ASE_ITYPE_UNION,
	NUMBER_OF_ASE_ITYPES,
};

/* the superset relation is a generalised version #'= */
static ase_element_relation_f
ase_optable_superset[NUMBER_OF_ASE_ITYPES][NUMBER_OF_ASE_ITYPES] = {
	/* OBJECT */
	{(ase_element_relation_f)ase_interval_embedding_error,
	 (ase_element_relation_f)ase_interval_embedding_error,
	 (ase_element_relation_f)ase_interval_embedding_error,
	 (ase_element_relation_f)ase_interval_embedding_error},
	/* INTERVAL */
	{ase_interval_contains_obj_p,
	 ase_interval_contains_intv_p,
	 (ase_element_relation_f)ase_interval_embedding_error,
	 ase_interval_contains_union_p},
	/* INTERIOR */
	{ase_interval_interior_contains_obj_p,
	 (ase_element_relation_f)ase_interval_embedding_error,
	 ase_interval_interior_contains_intr_p,
	 ase_interval_interior_contains_union_p},
	/* UNION */
	{ase_interval_union_contains_obj_p,
	 ase_interval_union_contains_intv_p,
	 ase_interval_union_contains_intr_p,
	 ase_interval_union_contains_union_p}};

/* the disjoint relation is a generalised version of #'/= */
static ase_st_relation_f
ase_optable_disjoint[NUMBER_OF_ASE_ITYPES][NUMBER_OF_ASE_ITYPES] = {
	/* OBJECT */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error},
	/* INTERVAL */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_disjoint_p,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_disjoint_union_p},
	/* INTERIOR */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_interior_disjoint_p,
	 ase_interval_interior_disjoint_union_p},
	/* UNION */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_union_disjoint_intv_p,
	 ase_interval_union_disjoint_intr_p,
	 ase_interval_union_disjoint_p}};

/* the disjoint relation is a generalised version of #'/= */
static ase_st_relation_f
ase_optable_connected[NUMBER_OF_ASE_ITYPES][NUMBER_OF_ASE_ITYPES] = {
	/* OBJECT */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error},
	/* INTERVAL */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_connected_p,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_connected_union_p},
	/* INTERIOR */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 (ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_interior_connected_p,
	 ase_interval_interior_connected_union_p},
	/* UNION */
	{(ase_st_relation_f)ase_interval_embedding_error,
	 ase_interval_union_connected_intv_p,
	 ase_interval_union_connected_intr_p,
	 ase_interval_union_connected_p}};

/* the intersection operation */
static ase_binary_operation_f
ase_optable_intersect[NUMBER_OF_ASE_ITYPES][NUMBER_OF_ASE_ITYPES] = {
	/* OBJECT */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error},
	/* INTERVAL */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 ase_intersect_intv_intv,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 ase_intersect_intv_union},
	/* INTERIOR */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 ase_intersect_intr_intr,
	 ase_intersect_intr_union},
	/* UNION */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 ase_intersect_union_intv,
	 ase_intersect_union_intr,
	 ase_intersect_union_union}};

/* the difference operation */
static ase_binary_operation_f
ase_optable_subtract[NUMBER_OF_ASE_ITYPES][NUMBER_OF_ASE_ITYPES] = {
	/* OBJECT */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error},
	/* INTERVAL */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 ase_subtract_intv_intv,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 ase_subtract_intv_union},
	/* INTERIOR */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 (ase_binary_operation_f)ase_interval_embedding_error,
	 ase_subtract_intr_intr,
	 ase_subtract_intr_union},
	/* UNION */
	{(ase_binary_operation_f)ase_interval_embedding_error,
	 ase_subtract_union_intv,
	 ase_subtract_union_intr,
	 ase_subtract_union_union}};


/* stuff for the dynacat, printers */
static void
_ase_interval_prnt(ase_interval_t a, Lisp_Object pcf)
{
	if (a == NULL) {
		write_c_string("( )", pcf);
		return;
	}

	if (a->lower_eq_upper_p) {
		write_c_string("[", pcf);
		print_internal(a->lower, pcf, 0);
		write_c_string("]", pcf);
		return;
	}

	if (a->lower_open_p)
		write_c_string("(", pcf);
	else
		write_c_string("[", pcf);
	print_internal(a->lower, pcf, 0);
	write_c_string(" ", pcf);
	print_internal(a->upper, pcf, 0);
	if (a->upper_open_p)
		write_c_string(")", pcf);
	else
		write_c_string("]", pcf);
}

static void
_ase_interval_union_item_prnt(ase_interval_union_item_t u, Lisp_Object pcf)
{
	dynacat_intprinter_f prfun = NULL;
	Lisp_Object o = u->current;

	if ((prfun = get_dynacat_intprinter(o)) == NULL)
		return;

	prfun(get_dynacat(o), pcf);
	if (u->next)
		write_c_string(" u ", pcf);
	return;
}

static void
_ase_interval_union_prnt(ase_interval_union_t i, Lisp_Object pcf)
{
	ase_interval_union_item_t u = ase_interval_union(i);
	while (u) {
		_ase_interval_union_item_prnt(u, pcf);
		u = u->next;
	}
	return;
}

static void
ase_interval_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_INTV("i:0x%08x@0x%08x (rc:%d)\n",
			    (unsigned int)(XASE_INTERVAL(obj)),
			    (unsigned int)obj,
			    (XASE_INTERVAL(obj) ?
			     XASE_INTERVAL_REFVAL(obj) : 1));
	write_c_string("#<ase:interval ", pcf);
	_ase_interval_prnt(XASE_INTERVAL(obj), pcf);
	write_c_string(">", pcf);
}

static void
ase_interval_union_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_INTV("u:0x%08x@0x%08x (rc:%d)\n",
			    (unsigned int)(XASE_INTERVAL_UNION(obj)),
			    (unsigned int)obj,
			    (XASE_INTERVAL_UNION(obj) ?
			     XASE_INTERVAL_UNION_REFVAL(obj) : 1));
	write_c_string("#<ase:interval-union ", pcf);
	_ase_interval_union_prnt(XASE_INTERVAL_UNION(obj), pcf);
	write_c_string(">", pcf);
	return;
}

/* stuff for the dynacat, finalisers */
static void
_ase_interval_union_item_fini(ase_interval_union_item_t u)
{
	EMOD_ASE_DEBUG_GC("uitem:0x%08x refcnt vanished, freeing\n",
			  (unsigned int)u);
	if (!u)
		return;
	if (u->current &&
	    ASE_INTERVALP(u->current) &&
	    !ASE_INTERVAL_EMPTY_P(u->current))
		(void)XASE_INTERVAL_DECREF(u->current);
	xfree(u);
	return;
}

static void
_ase_interval_union_fini(ase_interval_union_item_t u)
{
	ase_interval_union_item_t tmp;
	while (u) {
		u = (tmp = u)->next;
		_ase_interval_union_item_fini(tmp);
	}
	return;
}

static void
_ase_interval_fini(ase_interval_t a)
{
	EMOD_ASE_DEBUG_GC("i:0x%08x (rc:%d) shall be freed...\n",
			  (unsigned int)(a), ase_interval_refval(a));

	if (ase_interval_decref(a) <= 0) {
		ase_interval_fini_refcnt(a);
		xfree(a);
	} else {
		EMOD_ASE_DEBUG_GC("VETO! References exist\n");
	}
	return;
}

static void
ase_interval_fini(Lisp_Object obj, int unused)
{
	ase_interval_t a = XASE_INTERVAL(obj);

	if (ase_interval_empty_p(a))
		return;

	_ase_interval_fini(a);
	return;
}

static void
ase_interval_union_fini(Lisp_Object obj, int unused)
{
	ase_interval_union_t i = XASE_INTERVAL_UNION(obj);

	if (i == NULL)
		return;

	EMOD_ASE_DEBUG_GC("u:0x%08x@0x%08x (rc:%d) shall be freed...\n",
			  (unsigned int)(i), (unsigned int)obj,
			  ase_interval_union_refval(i));

	if (ase_interval_union_decref(i) <= 0) {
		_ase_interval_union_fini(ase_interval_union(i));
		ase_interval_union_fini_refcnt(i);
		xfree(i);
	} else {
		EMOD_ASE_DEBUG_GC("VETO! References exist\n");
	}
	return;
}

/* stuff for the dynacat, markers */
static void
_ase_interval_mark(ase_interval_t a)
{
	if (a == NULL)
		return;

	mark_object(a->lower);
	mark_object(a->upper);
	mark_object(a->lebesgue_measure);
	mark_object(a->rational_measure);
	mark_object(a->colour);
	return;
}

static void
_ase_interval_union_item_mark(ase_interval_union_item_t u)
{
	mark_object(u->current);
}

static void
_ase_interval_union_mark(ase_interval_union_t i)
{
	ase_interval_union_item_t u = ase_interval_union(i);

	mark_object(i->lebesgue_measure);
	mark_object(i->rational_measure);
	mark_object(i->colour);

	while (u) {
		_ase_interval_union_item_mark(u);
		u = u->next;
	}
	return;
}

static void
ase_interval_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_INTV("i:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			    (unsigned int)(XASE_INTERVAL(obj)),
			    (unsigned int)obj,
			    (XASE_INTERVAL(obj) ?
			     XASE_INTERVAL_REFVAL(obj) : 1));
	_ase_interval_mark(XASE_INTERVAL(obj));
	return;
}

static void
ase_interval_union_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_INTV("u:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			    (unsigned int)(XASE_INTERVAL_UNION(obj)),
			    (unsigned int)obj,
			    (XASE_INTERVAL_UNION(obj) ?
			     XASE_INTERVAL_UNION_REFVAL(obj) : 1));
	_ase_interval_union_mark(XASE_INTERVAL_UNION(obj));
	return;
}


Lisp_Object
_ase_wrap_interval(ase_interval_t a)
{
	Lisp_Object result;

	result = make_dynacat(a);
	XDYNACAT(result)->type = Qase_interval;

	if (a)
		ase_interval_incref(a);

	set_dynacat_printer(result, ase_interval_prnt);
	set_dynacat_marker(result, ase_interval_mark);
	set_dynacat_finaliser(result, ase_interval_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_interval_prnt);

	EMOD_ASE_DEBUG_INTV("i:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			    (unsigned int)a,
			    (a ? ase_interval_refval(a) : 1),
			    (unsigned int)result);

	return result;
}

Lisp_Object
_ase_wrap_interval_union(ase_interval_union_t iu)
{
	Lisp_Object result;

	result = make_dynacat(iu);
	XDYNACAT(result)->type = Qase_interval_union;

	if (iu)
		ase_interval_union_incref(iu);

	set_dynacat_printer(result, ase_interval_union_prnt);
	set_dynacat_marker(result, ase_interval_union_mark);
	set_dynacat_finaliser(result, ase_interval_union_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_interval_union_prnt);

	EMOD_ASE_DEBUG_INTV("u:0x%016lx (rc:%d) "
			    "shall be wrapped to 0x%016lx...\n",
			    (long unsigned int)iu,
			    (iu ? ase_interval_union_refval(iu) : 1),
			    (long unsigned int)result);

	return result;
}

ase_interval_t
_ase_make_interval(Lisp_Object lower, Lisp_Object upper,
		   int lower_open_p, int upper_open_p)
{
	ase_interval_t a = NULL;
	int lequ_p;

	if ((lequ_p = _ase_equal_p(lower, upper)) &&
	    (lower_open_p || upper_open_p)) {
		return NULL;
	}

	a = xnew(struct ase_interval_s);

	a->obj.category = ase_interval_cat;

	a->lower = lower;
	a->upper = upper;
	a->lower_eq_upper_p = lequ_p;
	if (!INFINITYP(lower))
		a->lower_open_p = lower_open_p;
	else
		a->lower_open_p = 1;
	if (!INFINITYP(upper))
		a->upper_open_p = upper_open_p;
	else
		a->upper_open_p = 1;
	a->lebesgue_measure = Qnil;
	a->rational_measure = Qnil;
	a->colour = Qnil;

	ase_interval_init_refcnt(a);

	EMOD_ASE_DEBUG_INTV("i:0x%08x (rc:0) shall be created...\n",
			    (unsigned int)a);
	return a;
}

static ase_interval_union_item_t
_ase_make_interval_union_item(Lisp_Object intv)
{
	ase_interval_union_item_t u = xnew(struct ase_interval_union_item_s);

	u->next = NULL;
	u->current = intv;
	if (ASE_INTERVALP(intv) && !ASE_INTERVAL_EMPTY_P(intv))
		XASE_INTERVAL_INCREF(intv);

	EMOD_ASE_DEBUG_INTV("uitem:0x%08x shall be created...\n",
			    (unsigned int)u);
	return u;
}

static ase_interval_union_t
_ase_make_interval_union(ase_interval_union_item_t ui)
{
	ase_interval_union_t i = xnew(struct ase_interval_union_s);

	i->union_ser = ui;
	i->lebesgue_measure = Qnil;
	i->rational_measure = Qnil;
	i->colour = Qnil;

	i->no_intv = 1;
	ase_interval_union_init_refcnt(i);

	EMOD_ASE_DEBUG_INTV("u:0x%08x (rc:0) shall be created...\n",
			    (unsigned int)i);
	return i;
}


Lisp_Object ase_empty_interval(void)
{
	Lisp_Object result = Qnil;

	XSETASE_INTERVAL(result, NULL);

	return result;
}

Lisp_Object ase_empty_interval_union(void)
{
	Lisp_Object result = Qnil;
	ase_interval_union_item_t u = NULL;
	ase_interval_union_t i = NULL;

	u = _ase_make_interval_union_item(Qase_empty_interval);
	i = _ase_make_interval_union(u);

	XSETASE_INTERVAL_UNION(result, i);

	return result;
}

Lisp_Object ase_universe_interval(void)
{
	ase_interval_t a = xnew(struct ase_interval_s);

	a->lower = Vninfinity;
	a->upper = Vpinfinity;
	a->lower_eq_upper_p = 0;
	a->lower_open_p = 1;
	a->upper_open_p = 1;
	a->lebesgue_measure = Qnil;
	a->rational_measure = Qnil;
	a->colour = Qnil;

	ase_interval_init_refcnt(a);
	return _ase_wrap_interval(a);
}

Lisp_Object ase_make_interval(Lisp_Object lower, Lisp_Object upper,
			      int l_open_p, int u_open_p)
{
	ase_interval_t a = NULL;
	Lisp_Object result = Qnil;

	a = _ase_make_interval(lower, upper, l_open_p, u_open_p);
	XSETASE_INTERVAL(result, a);

	return result;
}


static DOESNT_RETURN
ase_interval_embedding_error(Lisp_Object o1, Lisp_Object o2)
{
	ase_cartesian_embedding_error(o1, o2);
	return;
}

/* we have 3 different arithmetics:
 * - comparison and ordering of lower bounds
 * - comparison and ordering of upper bounds
 * - comparison and ordering of an upper bound with a lower bound
 */
bool		/* inline this? */
_ase_interval_contains_obj_p(ase_interval_t a, Lisp_Object obj)
{
	if (UNLIKELY(a == NULL)) {
		return false;
	}

	if ((a->lower_open_p
	     ? _ase_less_p(a->lower, obj)
	     : _ase_lessequal_p(a->lower, obj)) &&
	    (a->upper_open_p
	     ? _ase_greater_p(a->upper, obj)
	     : _ase_greaterequal_p(a->upper, obj))) {
		return true;
	} else {
		return false;
	}
}

int		/* inline this? */
_ase_interval_contains_intv_p(ase_interval_t a1, ase_interval_t a2)
{
	int result = 1;

	if (UNLIKELY(a1 == NULL))
		return 0;
	if (UNLIKELY(a2 == NULL))
		return -1;

	if (LIKELY(a2->lower_open_p)) {
		result &= (_ase_interval_contains_obj_p(a1, a2->lower) ||
			    _ase_equal_p(a1->lower, a2->lower));
	} else {
		result &= _ase_interval_contains_obj_p(a1, a2->lower);
	}

	if (LIKELY(a2->upper_open_p)) {
		result &= (_ase_interval_contains_obj_p(a1, a2->upper) ||
			    _ase_equal_p(a1->upper, a2->upper));
	} else {
		result &= _ase_interval_contains_obj_p(a1, a2->upper);
	}

	return result;
}

static int
_ase_interval_contains_union_p(ase_interval_t a, ase_interval_union_t i)
{
	/* true iff a \supset j \forall j in i */
	ase_interval_union_item_t u = ase_interval_union(i);
	while (u) {
		if (!_ase_interval_contains_intv_p(
			    a, XASE_INTERVAL(u->current)))
			return 0;
		u = u->next;
	}
	return -1;
}

static int
_ase_interval_less_p(ase_interval_t a1, ase_interval_t a2)
{
	if (a1 == NULL)
		return 0;
	if (a2 == NULL)
		return 1;

	/* should suffice to compare the lower bounds */
	return (_ase_less_p(a1->lower, a2->lower) ||
		(!a1->lower_open_p && a2->lower_open_p &&
		 _ase_equal_p(a1->lower, a2->lower)));
}
static int
_ase_interval_equal_p(ase_interval_t a1, ase_interval_t a2)
{
	/* trivial case */
	if (!a1 && !a2)
		return 1;
	else if (!a1)
		return 0;
	else if (!a2)
		return 0;
	else if (a1->lower_eq_upper_p && a2->lower_eq_upper_p)
		return _ase_equal_p(a1->lower, a2->lower);
	else if (a1->lower_eq_upper_p)
		return 0;
	else if (a2->lower_eq_upper_p)
		return 0;

	return (_ase_interval_contains_intv_p(a1, a2) &&
		_ase_interval_contains_intv_p(a2, a1));
}

static int
ase_interval_less_p(Lisp_Object a1, Lisp_Object a2)
{
	if (ASE_INTERVALP(a1) && ASE_INTERVALP(a2)) {
		return _ase_interval_less_p(
			XASE_INTERVAL(a1), XASE_INTERVAL(a2));
	}
	return 0;
}

static int
ase_interval_equal_p(Lisp_Object a1, Lisp_Object a2)
{
	if (ASE_INTERVALP(a1) && ASE_INTERVALP(a2)) {
		return _ase_interval_equal_p(
			XASE_INTERVAL(a1), XASE_INTERVAL(a2));
	}
	return 0;
}

static int
ase_interval_or_union_less_p(Lisp_Object a1, Lisp_Object a2)
{
	Lisp_Object na1, na2;
	if (ASE_INTERVAL_UNION_P(a1))
		na1 = XASE_INTERVAL_UNION_FIRST(a1);
	else
		na1 = a1;
	if (ASE_INTERVAL_UNION_P(a2))
		na2 = XASE_INTERVAL_UNION_FIRST(a2);
	else
		na2 = a2;
	return ase_interval_less_p(na1, na2);
}

static inline bool
_ase_interval_bounds_connected_p(ase_interval_t a1, ase_interval_t a2)
{
/* only compares upper with lower bound, assumes numerical equality */
	if (a1->upper_open_p && a2->lower_open_p) {
		return false;
	} else {
		return true;
	}
}

static inline int
_ase_interval_bounds_disjoint_p(ase_interval_t a1, ase_interval_t a2)
{
/* only compares upper with lower bound, assumes numerical equality */
	if (!a1->upper_open_p && !a2->lower_open_p) {
		return false;
	} else {
		return true;
	}
}

static Lisp_Object
_ase_interval_interior_contains_obj_p(
	ase_cartesian_t iip1, ase_cartesian_t iip2)
{
	return ase_cartesian_pointwise_erel_p(
		iip1, iip2, ase_interval_contains_obj_p);
}

static Lisp_Object
_ase_interval_interior_contains_intr_p(
	ase_cartesian_t iip1, ase_cartesian_t iip2)
{
	return ase_cartesian_pointwise_erel_p(
		iip1, iip2, ase_interval_contains_intv_p);
}

static Lisp_Object
_ase_interval_interior_contains_union_p(
	ase_cartesian_t iip1, ase_interval_union_t iu)
{
	/* true iff a \supset j \forall j in i */
	ase_interval_union_item_t u = ase_interval_union(iu);
	while (u) {
		if (!_ase_interval_interior_contains_intr_p(
			    iip1, XASE_CARTESIAN(u->current)))
			return Qnil;
		u = u->next;
	}
	return Qt;
}

static Lisp_Object
_ase_interval_union_contains_obj_p(ase_interval_union_t iu, Lisp_Object obj)
{
	ase_interval_union_item_t u = ase_interval_union(iu);
	Lisp_Object atmp = 0;

	while (u) {
		atmp = u->current;
		if (ASE_INTERVALP(atmp)) {
			if (_ase_interval_contains_obj_p(
				    XASE_INTERVAL(atmp), obj))
				return atmp;
		} else if (ASE_INTERVAL_INTERIOR_P(atmp)) {
			if (!NILP(_ase_interval_interior_contains_obj_p(
					  XASE_CARTESIAN(atmp),
					  XASE_CARTESIAN(obj))))
				return atmp;
		}
		u = u->next;
	}
	return Qnil;
}

static Lisp_Object
_ase_interval_union_contains_intv_p(ase_interval_union_t iu, ase_interval_t a)
{
	ase_interval_union_item_t u = ase_interval_union(iu);
	Lisp_Object atmp = 0;

	while (u) {
		atmp = u->current;
		if (_ase_interval_contains_intv_p(XASE_INTERVAL(atmp), a))
			return atmp;
		u = u->next;
	}
	return Qnil;
}

static Lisp_Object
_ase_interval_union_contains_intr_p(
	ase_interval_union_t iu, ase_cartesian_t iip)
{
	ase_interval_union_item_t u = ase_interval_union(iu);
	Lisp_Object atmp = 0;

	while (u) {
		atmp = u->current;
		if (_ase_interval_interior_contains_intr_p(
			    XASE_CARTESIAN(atmp), iip))
			return atmp;
		u = u->next;
	}
	return Qnil;
}

static Lisp_Object
_ase_interval_union_contains_union_p(
	ase_interval_union_t iu1, ase_interval_union_t iu2)
{
	/* true iff \forall a \in iu2 \exists b \in iu1 : b \supset a */
	ase_interval_union_item_t u1, u2;

	u1 = ase_interval_union(iu1);
	u2 = ase_interval_union(iu2);

	while (u2 && u1) {
		Lisp_Object o1 = u1->current, o2 = u2->current;
		if (ASE_INTERVALP(o1)) {
			ase_interval_t a1 = XASE_INTERVAL(o1);
			ase_interval_t a2 = XASE_INTERVAL(o2);
			if (_ase_interval_contains_intv_p(a1, a2))
				u2 = u2->next;
			else
				u1 = u1->next;
		} else if (ASE_INTERVAL_INTERIOR_P(o1)) {
			ase_cartesian_t c1 = XASE_CARTESIAN(o1);
			ase_cartesian_t c2 = XASE_CARTESIAN(o2);
			if (_ase_interval_interior_contains_intr_p(c1, c2))
				u2 = u2->next;
			else
				u1 = u1->next;
		}
	}
	if (u2 == NULL)
		return Qt;
	return Qnil;
}

static int
_ase_interval_connected_p(ase_interval_t a1, ase_interval_t a2)
{
	if (a1 == NULL || a2 == NULL)
		return 1;

	if (_ase_equal_p(a1->upper, a2->lower)) {
		return (_ase_interval_bounds_connected_p(a1, a2));
	} else if (_ase_equal_p(a1->lower, a2->upper)) {
		return (_ase_interval_bounds_connected_p(a2, a1) << 1);
	} else if (_ase_interval_contains_obj_p(a1, a2->lower) ||
		   _ase_interval_contains_obj_p(a2, a1->upper)) {
		return 1;
	} else if (_ase_interval_contains_obj_p(a1, a2->upper) ||
		   _ase_interval_contains_obj_p(a2, a1->lower)) {
		return 2;
	} else
		return 0;
}

static int
_ase_interval_interior_connected_p(
	ase_cartesian_t iip1, ase_cartesian_t iip2)
{
	/* true iff componentwise connected */
	return ase_cartesian_pointwise_rel_p(
		iip1, iip2, ase_interval_connected_p);
}

static int
_ase_interval_union_intv_connected_p(
	ase_interval_union_t iu, ase_interval_t i)
{
	/* true iff \forall j \in iu : j u i is connected */
	ase_interval_union_item_t u = ase_interval_union(iu);

	while (u) {
		ase_interval_t a = XASE_INTERVAL(u->current);
		if (!_ase_interval_connected_p(a, i))
			return 0;
		u = u->next;
	}
	return 1;
}

static int
_ase_interval_union_intr_connected_p(
	ase_interval_union_t iu, ase_cartesian_t c)
{
	/* true iff \forall j \in iu : j u i is connected */
	ase_interval_union_item_t u = ase_interval_union(iu);

	while (u) {
		ase_cartesian_t t = XASE_CARTESIAN(u->current);
		if (!_ase_interval_interior_connected_p(t, c))
			return 0;
		u = u->next;
	}
	return 1;
}

static int
_ase_interval_union_connected_p(
	ase_interval_union_t iu1, ase_interval_union_t iu2)
{
	/* true iff iu1 u iu2 is connected, i.e.
	 * iff \forall i \in iu1 : i u iu2 is connected */
	ase_interval_union_item_t u1 = ase_interval_union(iu1);

	while (u1) {
		if (ASE_INTERVALP(u1->current)) {
			if (!_ase_interval_union_intv_connected_p(
				    iu2, XASE_INTERVAL(u1->current)))
				return 0;
		} else if (ASE_INTERVAL_INTERIOR_P(u1->current)) {
			if (!_ase_interval_union_intr_connected_p(
				    iu2, XASE_CARTESIAN(u1->current)))
				return 0;
		}
		u1 = u1->next;
	}
	return 1;
}

static int
_ase_interval_disjoint_p(ase_interval_t a1, ase_interval_t a2)
{
	if (a1 == NULL || a2 == NULL)
		return 1;

	if (_ase_equal_p(a1->upper, a2->lower)) {
		return _ase_interval_bounds_disjoint_p(a1, a2);
	} else if (_ase_equal_p(a1->lower, a2->upper)) {
		return _ase_interval_bounds_disjoint_p(a2, a1);
	} else {
		return !((_ase_interval_contains_obj_p(a1, a2->lower)) ||
			 (_ase_interval_contains_obj_p(a1, a2->upper)) ||
			 (_ase_interval_contains_obj_p(a2, a1->lower)) ||
			 (_ase_interval_contains_obj_p(a2, a1->upper)));
	}
}

static int
_ase_interval_interior_disjoint_p(
	ase_cartesian_t iip1, ase_cartesian_t iip2)
{
	/* true iff iip1 n iip2 = ( ), i.e.
	 * component-intervals are disjoint in at least one dimension */
	return ase_cartesian_antipointwise_rel_p(
		iip1, iip2, ase_interval_disjoint_p);
}

static int
_ase_interval_union_disjoint_intv_p(
	ase_interval_union_t iu1, ase_interval_t i2)
{
	/* true iff \forall i \in iu1 : i n i2 = ( ) */
	ase_interval_union_item_t u = ase_interval_union(iu1);

	while (u) {
		ase_interval_t a1 = XASE_INTERVAL(u->current);
		if (!_ase_interval_disjoint_p(a1, i2))
			return 0;
		u = u->next;
	}
	return -1;
}

static int
_ase_interval_union_disjoint_intr_p(
	ase_interval_union_t iu, ase_cartesian_t c)
{
	/* true iff \forall i \in iu1 : i n i2 = ( ) */
	ase_interval_union_item_t u = ase_interval_union(iu);

	while (u) {
		ase_cartesian_t t = XASE_CARTESIAN(u->current);
		if (!_ase_interval_interior_disjoint_p(t, c))
			return 0;
		u = u->next;
	}
	return -1;
}

static int
_ase_interval_union_disjoint_p(
	ase_interval_union_t iu1, ase_interval_union_t iu2)
{
	/* true iff i1 n i2 = ( ), i.e.
	 * iff \forall i \in i1 \forall j \in i2 : i n j = ( ) */
	ase_interval_union_item_t u1 = ase_interval_union(iu1);

	while (u1) {
		if (ASE_INTERVALP(u1->current)) {
			if (!_ase_interval_union_disjoint_intv_p(
				    iu2, XASE_INTERVAL(u1->current)))
				return 0;
		} else if (ASE_INTERVAL_INTERIOR_P(u1->current)) {
			if (!_ase_interval_union_disjoint_intr_p(
				    iu2, XASE_CARTESIAN(u1->current)))
				return 0;
		}
		u1 = u1->next;
	}
	return -1;
}

static inline int
_ase_interval_open_p(ase_interval_t a)
{
	return ((a == NULL) || (a->lower_open_p && a->upper_open_p));
}

static inline int
_ase_interval_closed_p(ase_interval_t a)
{
	return ((a == NULL) ||
		((!a->lower_open_p || INFINITYP(a->lower)) &&
		 (!a->upper_open_p || INFINITYP(a->upper))));
}

static int
_ase_interval_union_open_p(ase_interval_union_item_t u)
{
	while (u) {
		if (ASE_INTERVALP(u->current)) {
			if (!_ase_interval_open_p(XASE_INTERVAL(u->current)))
				return 0;
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			if (!ase_interval_interior_open_p(u->current))
				return 0;
		}
		u = u->next;
	}
	return 1;
}

static int
_ase_interval_union_closed_p(ase_interval_union_item_t u)
{
	while (u) {
		if (ASE_INTERVALP(u->current)) {
			if (!_ase_interval_closed_p(XASE_INTERVAL(u->current)))
				return 0;
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			if (!ase_interval_interior_closed_p(u->current))
				return 0;
		}
		u = u->next;
	}
	return 1;
}

Lisp_Object
ase_interval_contains_obj_p(Lisp_Object interval, Lisp_Object obj)
{
	if (_ase_interval_contains_obj_p(
		    XASE_INTERVAL(interval), obj))
		return interval;
	return Qnil;
}

Lisp_Object
ase_interval_contains_intv_p(Lisp_Object i1, Lisp_Object i2)
{
	if (_ase_interval_contains_intv_p(
		    XASE_INTERVAL(i1), XASE_INTERVAL(i2)))
		return i1;
	return Qnil;
}

Lisp_Object
ase_interval_contains_union_p(Lisp_Object i, Lisp_Object u)
{
	/* true iff i \supset j \forall j in u */
	if (_ase_interval_contains_union_p(
		    XASE_INTERVAL(i), XASE_INTERVAL_UNION(u)))
		return Qt;
	return Qnil;
}

Lisp_Object
ase_interval_union_contains_obj_p(Lisp_Object iu, Lisp_Object obj)
{
	return _ase_interval_union_contains_obj_p(
		XASE_INTERVAL_UNION(iu), obj);
}

Lisp_Object
ase_interval_union_contains_intv_p(Lisp_Object iu, Lisp_Object i)
{
	return _ase_interval_union_contains_intv_p(
		XASE_INTERVAL_UNION(iu), XASE_INTERVAL(i));
}

Lisp_Object
ase_interval_union_contains_intr_p(Lisp_Object iu, Lisp_Object iip)
{
	return _ase_interval_union_contains_intr_p(
		XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(iip));
}

Lisp_Object
ase_interval_union_contains_union_p(Lisp_Object iu1, Lisp_Object iu2)
{
	/* true iff \forall a \in iu2 \exists b \in iu1 : b \supset a */
	return _ase_interval_union_contains_union_p(
		XASE_INTERVAL_UNION(iu1), XASE_INTERVAL_UNION(iu2));
}

Lisp_Object
ase_interval_interior_contains_obj_p(Lisp_Object iip1, Lisp_Object iip2)
{
	if (!ASE_CARTESIAN_INTERIOR_P(iip2) ||
	    XASE_CARTESIAN_DIMENSION(iip1) !=
	    XASE_CARTESIAN_DIMENSION(iip2) ||
	    !EQ(XASE_CARTESIAN_INTERIOR_TYPE(iip1), Qase_interval)) {
		signal_error(Qembed_error, list2(iip1, iip2));
		return Qnil;
	}

	return _ase_interval_interior_contains_obj_p(
		XASE_CARTESIAN(iip1), XASE_CARTESIAN(iip2));
}

Lisp_Object
ase_interval_interior_contains_intr_p(Lisp_Object iip1, Lisp_Object iip2)
{
	if (XASE_CARTESIAN_DIMENSION(iip1) !=
	    XASE_CARTESIAN_DIMENSION(iip2) ||
	    !EQ(XASE_CARTESIAN_INTERIOR_TYPE(iip1), Qase_interval) ||
	    !EQ(XASE_CARTESIAN_INTERIOR_TYPE(iip2), Qase_interval)) {
		signal_error(Qembed_error, list2(iip1, iip2));
		return Qnil;
	}
	return _ase_interval_interior_contains_intr_p(
		XASE_CARTESIAN(iip1), XASE_CARTESIAN(iip2));
}

Lisp_Object
ase_interval_interior_contains_union_p(Lisp_Object iip, Lisp_Object iu)
{
	return _ase_interval_interior_contains_union_p(
		XASE_CARTESIAN(iip), XASE_INTERVAL_UNION(iu));
}

int ase_interval_connected_p(Lisp_Object i1, Lisp_Object i2)
{
	return _ase_interval_connected_p(XASE_INTERVAL(i1), XASE_INTERVAL(i2));
}

int ase_interval_connected_union_p(Lisp_Object i, Lisp_Object iu)
{
	return _ase_interval_union_intv_connected_p(
		XASE_INTERVAL_UNION(iu), XASE_INTERVAL(i));
}

int ase_interval_union_connected_intv_p(Lisp_Object iu, Lisp_Object i)
{
	return _ase_interval_union_intv_connected_p(
		XASE_INTERVAL_UNION(iu), XASE_INTERVAL(i));
}

int ase_interval_union_connected_intr_p(Lisp_Object iu, Lisp_Object c)
{
	return _ase_interval_union_intr_connected_p(
		XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(c));
}

int ase_interval_union_connected_p(Lisp_Object i1, Lisp_Object i2)
{
	return _ase_interval_union_connected_p(
		XASE_INTERVAL_UNION(i1), XASE_INTERVAL_UNION(i2));
}

int ase_interval_interior_connected_p(Lisp_Object iip1, Lisp_Object iip2)
{
	return _ase_interval_interior_connected_p(
		XASE_CARTESIAN(iip1), XASE_CARTESIAN(iip2));
}

int ase_interval_interior_connected_union_p(Lisp_Object c, Lisp_Object iu)
{
	return _ase_interval_union_intr_connected_p(
		XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(c));
}

int ase_interval_disjoint_p(Lisp_Object i1, Lisp_Object i2)
{
	return _ase_interval_disjoint_p(XASE_INTERVAL(i1), XASE_INTERVAL(i2));
}

int ase_interval_disjoint_union_p(Lisp_Object i, Lisp_Object iu)
{
	return _ase_interval_union_disjoint_intv_p(
		XASE_INTERVAL_UNION(iu), XASE_INTERVAL(i));
}

int ase_interval_interior_disjoint_p(Lisp_Object iip1, Lisp_Object iip2)
{
	return _ase_interval_interior_disjoint_p(
		XASE_CARTESIAN(iip1), XASE_CARTESIAN(iip2));
}

int ase_interval_interior_disjoint_union_p(Lisp_Object c, Lisp_Object iu)
{
	return _ase_interval_union_disjoint_intr_p(
		XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(c));
}

int ase_interval_union_disjoint_intv_p(Lisp_Object iu, Lisp_Object i)
{
	return _ase_interval_union_disjoint_intv_p(
		XASE_INTERVAL_UNION(iu), XASE_INTERVAL(i));
}

int ase_interval_union_disjoint_intr_p(Lisp_Object iu, Lisp_Object c)
{
	return _ase_interval_union_disjoint_intr_p(
		XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(c));
}

int ase_interval_union_disjoint_p(Lisp_Object i1, Lisp_Object i2)
{
	return _ase_interval_union_disjoint_p(
		XASE_INTERVAL_UNION(i1), XASE_INTERVAL_UNION(i2));
}

int ase_interval_open_p(Lisp_Object intv)
{
	return _ase_interval_open_p(XASE_INTERVAL(intv));
}

int ase_interval_closed_p(Lisp_Object intv)
{
	return _ase_interval_closed_p(XASE_INTERVAL(intv));
}

int ase_interval_union_open_p(Lisp_Object iu)
{
	return _ase_interval_union_open_p(XASE_INTERVAL_UNION_SER(iu));
}

int ase_interval_union_closed_p(Lisp_Object iu)
{
	return _ase_interval_union_closed_p(XASE_INTERVAL_UNION_SER(iu));
}

int ase_interval_interior_open_p(Lisp_Object iip)
{
	return ase_cartesian_pointwise_pred_p(
		XASE_CARTESIAN(iip), ase_interval_open_p);
}

int ase_interval_interior_closed_p(Lisp_Object iip)
{
	return ase_cartesian_pointwise_pred_p(
		XASE_CARTESIAN(iip), ase_interval_closed_p);
}


/* constructors */
static ase_interval_t
_ase_unite_intervals(ase_interval_t a1, ase_interval_t a2)
{
/* Returns a new interval item if a1 and a2 turn out not to be recyclable */
	int where = 0;

	if (a1 == NULL && a2 == NULL) {
		return NULL;
	} else if (a2 == NULL) {
		return a1;
	} else if (a1 == NULL) {
		return a2;
	} else if (_ase_interval_contains_intv_p(a1, a2)) {
		return a1;
	} else if (_ase_interval_contains_intv_p(a2, a1)) {
		return a2;
	} else if ((where = _ase_interval_connected_p(a1, a2))) {
		Lisp_Object new_lower, new_upper;
		int new_lower_open_p, new_upper_open_p;

		if (where == 1) {
			new_lower = a1->lower;
			new_lower_open_p = a1->lower_open_p;
			new_upper = a2->upper;
			new_upper_open_p = a2->upper_open_p;
		} else {
			new_lower = a2->lower;
			new_lower_open_p = a2->lower_open_p;
			new_upper = a1->upper;
			new_upper_open_p = a1->upper_open_p;
		}

		return _ase_make_interval(
			new_lower, new_upper,
			new_lower_open_p, new_upper_open_p);
	}

	return NULL;
}

static inline int
_ase_interval_interior_pointintv_p(ase_cartesian_t c)
{
	int pointintvp, i, dim = ase_cartesian_dimension(c);

	for (i = 0, pointintvp = 1; i < dim && pointintvp; i++) {
		Lisp_Object a = ase_cartesian_objects(c)[i];
		if (!XASE_INTERVAL(a)->lower_eq_upper_p)
			pointintvp = 0;
	}
	return pointintvp;
}

static ase_cartesian_t
_ase_unite_intervals_intr(ase_cartesian_t c1, ase_cartesian_t c2)
{
	int hypidx, hypplaneeqp = 0;
	int i, dim;

	if (c1 == NULL)
		return c2;
	if (c2 == NULL)
		return c1;
	if (!NILP(_ase_interval_interior_contains_intr_p(c1, c2))) {
		/* cartesians lack ref counters atm, hence we cant do: */
		return c1;
	} else if (!NILP(_ase_interval_interior_contains_intr_p(c2, c1))) {
		/* cartesians lack ref counters atm, hence we cant do: */
		return c2;
	}

	dim = ase_cartesian_dimension(c1);
	for (hypidx = 0; hypidx < dim; hypidx++) {
		/* we build the hyperplane of the interval by
		 * omitting the hypidx-th dimension in the next loop */
		for (i = 0, hypplaneeqp = 1; i < dim && hypplaneeqp; i++) {
			Lisp_Object i1 = ase_cartesian_objects(c1)[i];
			Lisp_Object i2 = ase_cartesian_objects(c2)[i];
			if (i != hypidx &&
			    !ase_interval_equal_p(i1, i2))
				hypplaneeqp = 0;
		}
		if (hypplaneeqp) {
			/* finally found a hyperplane where all
			 * intervals coincide, this means, we can merge */
			break;
		}
	}
	if (hypplaneeqp) {
		/* merge along the hypidx-th dimension */
		Lisp_Object i1 = ase_cartesian_objects(c1)[hypidx];
		Lisp_Object i2 = ase_cartesian_objects(c2)[hypidx];
		ase_interval_t a1 = XASE_INTERVAL(i1);
		ase_interval_t a2 = XASE_INTERVAL(i2);
		ase_interval_t a = _ase_unite_intervals(a1, a2);
		Lisp_Object *tmp = alloca_array(Lisp_Object, dim);

		if (a == NULL)
			return NULL;

		for (i = 0; i < dim; i++)
			tmp[i] = ase_cartesian_objects(c1)[i];
		tmp[hypidx] = _ase_wrap_interval(a);
		return _ase_make_cartesian(dim, tmp, 1);
	}

	return NULL;
}

static Lisp_Object
ase_unite_intervals_intv(Lisp_Object a1, Lisp_Object a2)
{
	ase_interval_t a =
		_ase_unite_intervals(XASE_INTERVAL(a1), XASE_INTERVAL(a2));

	if (a)
		return _ase_wrap_interval(a);
	else
		return Qnil;
}

static Lisp_Object
ase_unite_intervals_intr(Lisp_Object iip1, Lisp_Object iip2)
{
	ase_cartesian_t a = NULL;

	if (ASE_INTERVAL_EMPTY_P(iip1))
		return iip2;
	if (ASE_INTERVAL_EMPTY_P(iip2))
		return iip1;

	a = _ase_unite_intervals_intr(
		XASE_CARTESIAN(iip1), XASE_CARTESIAN(iip2));

	if (a)
		return _ase_wrap_cartesian_interior(a);
	else
		return Qnil;
}

static Lisp_Object
ase_unite_intervals(Lisp_Object a1, Lisp_Object a2)
{
	if (ASE_INTERVAL_INTERIOR_P(a1) || ASE_INTERVAL_INTERIOR_P(a2))
		return ase_unite_intervals_intr(a1, a2);
	else if (ASE_INTERVALP(a1) || ASE_INTERVALP(a2))
		return ase_unite_intervals_intv(a1, a2);
	else
		return Qnil;
}

static ase_interval_t
_ase_intersect_intv_intv(ase_interval_t a1, ase_interval_t a2)
{
/* Returns a new interval item if a1 and a2 turn out not to be recyclable */
	int where = 0;

	if (a1 == NULL || a2 == NULL) {
		return NULL;
	} else if (_ase_interval_disjoint_p(a1, a2)) {
		return NULL;
	} else if (_ase_interval_contains_intv_p(a1, a2)) {
		return a2;
	} else if (_ase_interval_contains_intv_p(a2, a1)) {
		return a1;
	} else if ((where = _ase_interval_connected_p(a1, a2))) {
		Lisp_Object new_lower, new_upper;
		int new_lower_open_p, new_upper_open_p;

		if (where == 1) {
			new_lower = a2->lower;
			new_lower_open_p = a2->lower_open_p;
			new_upper = a1->upper;
			new_upper_open_p = a1->upper_open_p;
		} else {
			new_lower = a1->lower;
			new_lower_open_p = a1->lower_open_p;
			new_upper = a2->upper;
			new_upper_open_p = a2->upper_open_p;
		}

		return _ase_make_interval(
			new_lower, new_upper,
			new_lower_open_p, new_upper_open_p);
	}

	return NULL;
}

static Lisp_Object
ase_intersect_intv_intv(Lisp_Object a1, Lisp_Object a2)
{
	ase_interval_t a =
		_ase_intersect_intv_intv(XASE_INTERVAL(a1), XASE_INTERVAL(a2));

	if (a)
		return _ase_wrap_interval(a);
	else
		return Qase_empty_interval;
}

static ase_cartesian_t
_ase_intersect_intr_intr(ase_cartesian_t c1, ase_cartesian_t c2)
{
/* Returns a new interval item if a1 and a2 turn out not to be recyclable */
	if (c1 == NULL || c2 == NULL) {
		return NULL;
	} else if (_ase_interval_interior_disjoint_p(c1, c2)) {
		return NULL;
	} else {
		int i, dim = ase_cartesian_dimension(c1);
		Lisp_Object *newos = alloca_array(Lisp_Object, dim);

		for (i = 0; i < dim; i++) {
			Lisp_Object o1 = ase_cartesian_objects(c1)[i];
			Lisp_Object o2 = ase_cartesian_objects(c2)[i];
			newos[i] = ase_intersect_intv_intv(o1, o2);
		}

		return _ase_make_cartesian(dim, newos, 1);
	}

	return NULL;
}

static Lisp_Object
ase_intersect_intr_intr(Lisp_Object c1, Lisp_Object c2)
{
	ase_cartesian_t c =
		_ase_intersect_intr_intr(
			XASE_CARTESIAN(c1), XASE_CARTESIAN(c2));

	if (c)
		return _ase_wrap_cartesian_interior(c);
	else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_intersect_union_intv(ase_interval_union_t iu, ase_interval_t a)
{
	ase_interval_union_item_t u = ase_interval_union(iu);
	struct ase_interval_union_item_s ures, *ur = &ures;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_interval_t a1 = XASE_INTERVAL(u->current);
		ase_interval_t na = _ase_intersect_intv_intv(a1, a);

		if (na)
			ur = ur->next = _ase_make_interval_union_item(
				_ase_wrap_interval(na));
		u = u->next;
	}

	return ures.next;
}

static Lisp_Object
ase_intersect_union_intv(Lisp_Object iu, Lisp_Object a)
{
	ase_interval_union_item_t nu =
		_ase_intersect_union_intv(
			XASE_INTERVAL_UNION(iu), XASE_INTERVAL(a));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static Lisp_Object
ase_intersect_intv_union(Lisp_Object a, Lisp_Object iu)
{
	return ase_intersect_union_intv(iu, a);
}

static ase_interval_union_item_t
_ase_intersect_union_intr(ase_interval_union_t iu, ase_cartesian_t c)
{
	ase_interval_union_item_t u = ase_interval_union(iu);
	struct ase_interval_union_item_s ures, *ur = &ures;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_cartesian_t c1 = XASE_CARTESIAN(u->current);
		ase_cartesian_t nc = _ase_intersect_intr_intr(c1, c);

		if (nc)
			ur = ur->next = _ase_make_interval_union_item(
				_ase_wrap_cartesian_interior(nc));
		u = u->next;
	}

	_ase_normalise_union_intr(&ures);

	return ures.next;
}

static Lisp_Object
ase_intersect_union_intr(Lisp_Object iu, Lisp_Object c)
{
	ase_interval_union_item_t nu =
		_ase_intersect_union_intr(
			XASE_INTERVAL_UNION(iu), XASE_CARTESIAN(c));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static Lisp_Object
ase_intersect_intr_union(Lisp_Object c, Lisp_Object iu)
{
	return ase_intersect_union_intr(iu, c);
}

static ase_interval_union_item_t
_ase_intersect_union_union(ase_interval_union_t iu1, ase_interval_union_t iu2)
{
	ase_interval_union_item_t u = ase_interval_union(iu1);
	struct ase_interval_union_item_s ures, *ur = &ures;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_interval_union_item_t na = NULL;

		if (ASE_INTERVALP(u->current)) {
			ase_interval_t a1 = XASE_INTERVAL(u->current);
			na = _ase_intersect_union_intv(iu2, a1);
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			ase_cartesian_t c1 = XASE_CARTESIAN(u->current);
			na = _ase_intersect_union_intr(iu2, c1);
		}

		if (na) {
			ur->next = na;
			/* forewind to the end of ur */
			while (ur->next)
				ur = ur->next;
		}
		u = u->next;
	}

	if (ures.next && ASE_INTERVAL_INTERIOR_P(ures.next->current)) {
		_ase_normalise_union_intr(&ures);
	}

	return ures.next;
}

static Lisp_Object
ase_intersect_union_union(Lisp_Object iu1, Lisp_Object iu2)
{
	ase_interval_union_item_t nu =
		_ase_intersect_union_union(
			XASE_INTERVAL_UNION(iu1), XASE_INTERVAL_UNION(iu2));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_intv_intv(ase_interval_t a1, ase_interval_t a2)
{
/* Returns a new interval item if a1 and a2 turn out not to be recyclable */
	int where = 0;

	if (a1 == NULL)
		return NULL;
	if (a2 == NULL) {
		return _ase_make_interval_union_item(
			_ase_wrap_interval(a1));
	} else if (_ase_interval_disjoint_p(a1, a2)) {
		return _ase_make_interval_union_item(
			_ase_wrap_interval(a1));
	} else if (_ase_interval_contains_intv_p(a2, a1)) {
		return NULL;
	} else if (_ase_interval_contains_intv_p(a1, a2)) {
		/* the hard case, now a1 decomposes to two interval items */
		Lisp_Object na1l, na1u, na2l, na2u;
		int na1lop, na1uop, na2lop, na2uop;
		ase_interval_union_item_t ures = NULL, u1 = NULL, u2 = NULL;

		na1l = a1->lower;
		na1lop = a1->lower_open_p;
		na1u = a2->lower;
		na1uop = !a2->lower_open_p;

		na2l = a2->upper;
		na2lop = !a2->upper_open_p;
		na2u = a1->upper;
		na2uop = a1->upper_open_p;

		a1 = _ase_make_interval(na1l, na1u, na1lop, na1uop);
		a2 = _ase_make_interval(na2l, na2u, na2lop, na2uop);

		if (a1) {
			u1 = _ase_make_interval_union_item(
				_ase_wrap_interval(a1));
		}
		if (a2) {
			u2 = _ase_make_interval_union_item(
				_ase_wrap_interval(a2));
		}

		if (u1 && u2) {
			ures = u1;
			ures->next = u2;
		} else if (u1) {
			ures = u1;
		} else if (u2) {
			ures = u2;
		}

		return ures;
	} else if ((where = _ase_interval_connected_p(a1, a2))) {
		Lisp_Object new_lower, new_upper;
		int new_lower_open_p, new_upper_open_p;

		if (where == 1) {
			new_lower = a1->lower;
			new_lower_open_p = a1->lower_open_p;
			new_upper = a2->lower;
			new_upper_open_p = !a2->lower_open_p;
		} else {
			new_lower = a2->upper;
			new_lower_open_p = !a2->upper_open_p;
			new_upper = a1->upper;
			new_upper_open_p = a1->upper_open_p;
		}

		return _ase_make_interval_union_item(
			_ase_wrap_interval(
				_ase_make_interval(
					new_lower, new_upper,
					new_lower_open_p, new_upper_open_p)));
	} else {
		EMOD_ASE_CRITICAL("Desaster!\n");
	}

	return NULL;
}

static Lisp_Object
ase_subtract_intv_intv(Lisp_Object a1, Lisp_Object a2)
{
	ase_interval_union_item_t u =
		_ase_subtract_intv_intv(XASE_INTERVAL(a1), XASE_INTERVAL(a2));

	if (u && u->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(u));
	else if (u) {
		Lisp_Object na = u->current;
		_ase_interval_union_item_fini(u);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_intr_intr(ase_cartesian_t c1, ase_cartesian_t c2)
{
	if (c1 == NULL)
		return NULL;
	if (c2 == NULL) {
		return _ase_make_interval_union_item(
			_ase_wrap_cartesian_interior(c1));
	} else if (_ase_interval_interior_disjoint_p(c1, c2)) {
		return _ase_make_interval_union_item(
			_ase_wrap_cartesian_interior(c1));
	} else if (!NILP(_ase_interval_interior_contains_intr_p(c2, c1))) {
		return NULL;
	} else if (_ase_interval_interior_connected_p(c1, c2)) {
		//!NILP(_ase_interval_interior_contains_intr_p(c1, c2)) ||
		/* the hard case, we decompose c1 into at most 2n
		 * n-dimensional interval products */
		int i, dim = ase_cartesian_dimension(c1);
		struct ase_interval_union_item_s ures, *ur = &ures;

		for (i = 0; i < dim; i++) {
			Lisp_Object o1 = ase_cartesian_objects(c1)[i];
			Lisp_Object o2 = ase_cartesian_objects(c2)[i];
			ase_interval_union_item_t dec =
				_ase_subtract_intv_intv(
					XASE_INTERVAL(o1), XASE_INTERVAL(o2));
			/* dec should now have two elements,
			 * one left of o2 in o1, one right of o2 in o1 */
			Lisp_Object *newos = alloca_array(Lisp_Object, dim);
			int j;

			/* copy the (i-1) whole intervals */
			for (j = 0; j < i; j++) {
				Lisp_Object t1 = ase_cartesian_objects(c1)[j];
				newos[j] = t1;
			}
			/* now push all the interval components of o2
			 * which lie in subspaces of index >i */
			for (j = i+1; j < dim; j++) {
				Lisp_Object t1 = ase_cartesian_objects(c1)[j];
				Lisp_Object t2 = ase_cartesian_objects(c2)[j];
				newos[j] = ase_intersect_intv_intv(t1, t2);
			}
			/* copy the interval left of o2 */
			newos[i] = dec->current;
			ur = ur->next =
				_ase_make_interval_union_item(
					ase_make_cartesian(dim, newos, 1));
			/* copy the interval right of o2, if there is one */
			if (dec->next) {
				newos[i] = dec->next->current;
				ur = ur->next =
					_ase_make_interval_union_item(
						ase_make_cartesian(
							dim, newos, 1));
			}
			_ase_interval_union_item_fini(dec);
		}

		return ures.next;
	} else if (_ase_interval_interior_connected_p(c1, c2)) {
		/* kinda hard case, we decompose c1 into 2n-1
		 * n-dimensional interval products */
		EMOD_ASE_CRITICAL("Desaster!\n");
	} else {
		EMOD_ASE_CRITICAL("Desaster!\n");
	}

	return NULL;
}

static Lisp_Object
ase_subtract_intr_intr(Lisp_Object c1, Lisp_Object c2)
{
	ase_interval_union_item_t u =
		_ase_subtract_intr_intr(XASE_CARTESIAN(c1), XASE_CARTESIAN(c2));

	if (u && u->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(u));
	else if (u) {
		Lisp_Object na = u->current;
		_ase_interval_union_item_fini(u);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_union_intv(ase_interval_union_item_t u, ase_interval_t a)
{
	/* (A u B) \ C = (A \ C u B \ C) */
	struct ase_interval_union_item_s ures, *ur = &ures;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_interval_t a1 = XASE_INTERVAL(u->current);
		ase_interval_union_item_t na;

		na = _ase_subtract_intv_intv(a1, a);

		if (na) {
			ur->next = na;
			/* forewind to the end of ur */
			while (ur->next)
				ur = ur->next;
		}
		u = u->next;
	}

	return ures.next;
}

static Lisp_Object
ase_subtract_union_intv(Lisp_Object iu, Lisp_Object a)
{
	/* (A u B) \ C = (A \ C u B \ C) */
	ase_interval_union_item_t nu =
		_ase_subtract_union_intv(
			XASE_INTERVAL_UNION_SER(iu),
			XASE_INTERVAL(a));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_union_intr(ase_interval_union_item_t u, ase_cartesian_t c)
{
	/* (A u B) \ C = (A \ C u B \ C) */
	struct ase_interval_union_item_s ures, *ur = &ures;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_cartesian_t c1 = XASE_CARTESIAN(u->current);
		ase_interval_union_item_t na;

		na = _ase_subtract_intr_intr(c1, c);

		if (na) {
			ur->next = na;
			/* forewind to the end of ur */
			while (ur->next)
				ur = ur->next;
		}
		u = u->next;
	}

	return ures.next;
}

static Lisp_Object
ase_subtract_union_intr(Lisp_Object iu, Lisp_Object c)
{
	/* (A u B) \ C = (A \ C u B \ C) */
	ase_interval_union_item_t nu =
		_ase_subtract_union_intr(
			XASE_INTERVAL_UNION_SER(iu),
			XASE_CARTESIAN(c));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_intv_union(ase_interval_t a, ase_interval_union_item_t u)
{
	/* A \ (B u C) = (A \ B) \ C */
	struct ase_interval_union_item_s ures, *na = &ures;

	na->current = _ase_wrap_interval(a);
	na->next = NULL;
	while (u) {
		ase_interval_t a2 = XASE_INTERVAL(u->current);

		na = _ase_subtract_union_intv(na, a2);

		if (!na)
			break;
		u = u->next;
	}
	if (na == &ures) {
		/* Copy the local temporary to the heap */
		na = xnew(struct ase_interval_union_item_s);
		assert(na);
		memcpy(na,&ures,sizeof(ures));
	}
	return na;
}

static Lisp_Object
ase_subtract_intv_union(Lisp_Object a, Lisp_Object iu)
{
	/* A \ (B u C) = (A \ B) \ C */
	ase_interval_union_item_t nu =
		_ase_subtract_intv_union(
			XASE_INTERVAL(a),
			XASE_INTERVAL_UNION_SER(iu));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_intr_union(ase_cartesian_t c, ase_interval_union_item_t u)
{
	/* A \ (B u C) = (A \ B) \ C */
	struct ase_interval_union_item_s ures, *na = &ures;

	na->current = _ase_wrap_cartesian_interior(c);
	na->next = NULL;
	while (u) {
		ase_cartesian_t c2 = XASE_CARTESIAN(u->current);

		na = _ase_subtract_union_intr(na, c2);

		if (!na)
			break;
		u = u->next;
	}

	if (na == &ures) {
		/* Copy the local temporary to the heap */
		na = xnew(struct ase_interval_union_item_s);
		assert(na);
		memcpy(na,&ures,sizeof(ures));
	}
	return na;
}

static Lisp_Object
ase_subtract_intr_union(Lisp_Object c, Lisp_Object iu)
{
	/* A \ (B u C) = (A \ B) \ C */
	ase_interval_union_item_t nu =
		_ase_subtract_intr_union(
			XASE_CARTESIAN(c),
			XASE_INTERVAL_UNION_SER(iu));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}

static ase_interval_union_item_t
_ase_subtract_union_union(ase_interval_union_t iu1, ase_interval_union_t iu2)
{
	/* (A u B) \ (C u D) = ((A u B) \ C) \ D */
	ase_interval_union_item_t na = ase_interval_union(iu1);
	ase_interval_union_item_t u = ase_interval_union(iu2);

	while (u) {
		if (ASE_INTERVALP(u->current)) {
			ase_interval_t a = XASE_INTERVAL(u->current);
			na = _ase_subtract_union_intv(na, a);
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			ase_cartesian_t c = XASE_CARTESIAN(u->current);
			na = _ase_subtract_union_intr(na, c);
		}

		if (!na)
			break;
		u = u->next;
	}

	return na;
}

static Lisp_Object
ase_subtract_union_union(Lisp_Object iu1, Lisp_Object iu2)
{
	/* (A u B) \ (C u D) = ((A u B) \ C) \ D */
	ase_interval_union_item_t nu =
		_ase_subtract_union_union(
			XASE_INTERVAL_UNION(iu1), XASE_INTERVAL_UNION(iu2));

	if (nu && nu->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(nu));
	else if (nu) {
		Lisp_Object na = nu->current;
		_ase_interval_union_item_fini(nu);
		return na;
	} else
		return Qase_empty_interval;
}


static Lisp_Object
_ase_copy_interval(ase_interval_t a)
{
	Lisp_Object result = Qnil;

	XSETASE_INTERVAL(result, a);
	return result;
}

Lisp_Object ase_copy_interval(Lisp_Object intv)
{
	return _ase_copy_interval(XASE_INTERVAL(intv));
}

static Lisp_Object*
_ase_interval_union_explode_array(int nargs, Lisp_Object *args, int add)
{
	ase_interval_union_item_t u;
	Lisp_Object *newargs = args;
	int j, mov = 0;

	for (j = 0; j < nargs+add; ) {
		if (ASE_INTERVAL_UNION_P(args[j])) {
			u = ase_interval_union(XASE_INTERVAL_UNION(args[j]));
			newargs[j] = u->current;
			u = u->next;
			while (u) {
				newargs[nargs+mov] = u->current;
				u = u->next;
				mov++;
			}
		}
		j++;
	}
	return newargs;
}

static int
_ase_normalise_union(ase_interval_union_item_t u)
{
	/* assumes first item of u is sorta head, we cant change that */
	ase_interval_union_item_t u1 = u->next, u2 = NULL, pu = u;
	Lisp_Object a1, a2, atmp;
	int i = 1;

	while ((u2 = u1->next)) {
		a1 = u1->current;
		a2 = u2->current;

		/* connectivity can solely occur at upper-lower */
		atmp = ase_unite_intervals(a1, a2);
		if (!NILP(atmp)) {
			ase_interval_union_item_t tmp;

			tmp = _ase_make_interval_union_item(atmp);
			tmp->next = u2->next;

			_ase_interval_union_item_fini(u1);
			_ase_interval_union_item_fini(u2);

			pu->next = u1 = tmp;
		} else {
			pu = u1;
			u1 = u2;
			i++;
		}
	}
	return i;
}

static int
_ase_normalise_union_intr(ase_interval_union_item_t u)
{
	/* assumes first item of u is sorta head, we cant change that */
	ase_interval_union_item_t u1 = u->next, u2 = NULL, pu1 = u, pu2;
	Lisp_Object a1, a2, atmp;
	int i = 1;

	while (u1) {
		u2 = u1->next;
		pu2 = u1;
		while (u2) {
			a1 = u1->current;
			a2 = u2->current;

			/* connectivity can occur everywhere! */
			atmp = ase_unite_intervals(a1, a2);
			if (!NILP(atmp)) {
				ase_interval_union_item_t tmp, u2n;

				tmp = _ase_make_interval_union_item(atmp);
				if (u1->next == u2) {
					tmp->next = u2->next;
				} else {
					tmp->next = u1->next;
				}
				u2n = u2->next;
				pu1->next = tmp;
				pu2->next = u2n;

				_ase_interval_union_item_fini(u1);
				_ase_interval_union_item_fini(u2);

				/* we start over from the very beginning
				 * there might be new merge opportunities now
				 * if speed is important, we should allow
				 * a merge depth of 1, settint u1 to tmp
				 * would be the equivalent action for this */
				u1 = u;
				break;
			} else {
				pu2 = u2;
				u2 = u2->next;
				i++;
			}
		}
		pu1 = u1;
		u1 = u1->next;
	}
	return i;
}

static ase_interval_union_item_t
_ase_interval_boundary(ase_interval_t a)
{
	Lisp_Object blo = Qnil, bup = Qnil;
	ase_interval_union_item_t ures = NULL;

	if (a == NULL || a->lower_eq_upper_p)
		return NULL;

	blo = _ase_wrap_interval(
		_ase_make_interval(a->lower, a->lower, 0, 0));
	if (!_ase_equal_p(a->lower, a->upper)) {
		bup = _ase_wrap_interval(
			_ase_make_interval(a->upper, a->upper, 0, 0));
	}

	ures = _ase_make_interval_union_item(blo);
	if (!NILP(bup))
		ures->next = _ase_make_interval_union_item(bup);

	return ures;
}

Lisp_Object ase_interval_boundary(Lisp_Object intv)
{
	ase_interval_union_item_t u =
		_ase_interval_boundary(XASE_INTERVAL(intv));

	if (!u)
		return Qase_empty_interval;

	return _ase_wrap_interval_union(
		_ase_make_interval_union(u));
}

static ase_interval_union_item_t
_ase_interval_interior_boundary(ase_cartesian_t c)
{
	struct ase_interval_union_item_s ures, *ur = &ures;
	int i, dim = ase_cartesian_dimension(c);

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	for (i = 0; i < dim; i++) {
		ase_interval_union_item_t tmp =
			_ase_interval_boundary(
				XASE_INTERVAL(ase_cartesian_objects(c)[i]));
		Lisp_Object *newos = alloca_array(Lisp_Object, dim);
		int j;

		if (!tmp)
			continue;

		for (j = 0; j < dim; j++) {
			newos[j] = ase_cartesian_objects(c)[j];
		}
		/* replace i-th component with one boundary point */
		newos[i] = tmp->current;
		/* replace with the new interior product */
		tmp->current =
			_ase_wrap_cartesian_interior(
				_ase_make_cartesian(dim, newos, 1));
		/* replace i-th component with the other boundary point */
		newos[i] = tmp->next->current;
		/* and replace again with new interior product */
		tmp->next->current =
			_ase_wrap_cartesian_interior(
				_ase_make_cartesian(dim, newos, 1));

		/* pump the stuff into ur */
		ur->next = tmp;
		ur = tmp->next;
	}

	return ures.next;
}

static ase_interval_union_item_t
_ase_interval_union_boundary(ase_interval_union_item_t u)
{
	struct ase_interval_union_item_s ures, *ur = &ures;
	Lisp_Object lastiv;

	lastiv = ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		ase_interval_union_item_t tmp = NULL;
		Lisp_Object curiv;

		if (ASE_INTERVALP(u->current)) {
			tmp = _ase_interval_boundary(
				XASE_INTERVAL(u->current));
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			tmp = _ase_interval_interior_boundary(
				XASE_CARTESIAN(u->current));
		}

		u = u->next;
		if (!tmp)
			continue;

		/* disjoint intervals may have equal boundary points */
		curiv = tmp->current;
		if (!ase_interval_equal_p(lastiv, curiv)) {
			ur->next = tmp;
		} else {
			ur->next = tmp->next;
			_ase_interval_union_item_fini(tmp);
		}
		while (ur->next)
			ur = ur->next;
		lastiv = ur->current;
	}

	if (ASE_INTERVAL_INTERIOR_P(lastiv)) {
		_ase_normalise_union_intr(&ures);
	}

	return ures.next;
}

Lisp_Object ase_interval_interior_boundary(Lisp_Object intv_intr_prod)
{
	ase_interval_union_item_t u =
		_ase_interval_interior_boundary(
			XASE_CARTESIAN(intv_intr_prod));

	if (!u)
		return Qase_empty_interval;

	return _ase_wrap_interval_union(
		_ase_make_interval_union(u));
}

Lisp_Object ase_interval_union_boundary(Lisp_Object intv_union)
{
	ase_interval_union_item_t u =
		_ase_interval_union_boundary(
			XASE_INTERVAL_UNION_SER(intv_union));

	if (!u)
		return Qase_empty_interval;

	return _ase_wrap_interval_union(
		_ase_make_interval_union(u));
}

static ase_interval_t
_ase_interval_closure(ase_interval_t a)
{
	if (a == NULL)
		return NULL;
	if (_ase_interval_closed_p(a))
		return a;

	return _ase_make_interval(a->lower, a->upper, 0, 0);
}

Lisp_Object ase_interval_closure(Lisp_Object intv)
{
	ase_interval_t u =
		_ase_interval_closure(XASE_INTERVAL(intv));

	if (!u)
		return Qase_empty_interval;

	return _ase_wrap_interval(u);
}

static ase_cartesian_t
_ase_interval_interior_closure(ase_cartesian_t c)
{
	int i, dim = ase_cartesian_dimension(c);
	Lisp_Object *os = ase_cartesian_objects(c);
	Lisp_Object *newos = alloca_array(Lisp_Object, dim);

	for (i = 0; i < dim; i++) {
		newos[i] = ase_interval_closure(os[i]);
	}

	return _ase_make_cartesian(dim, newos, 1);
}

Lisp_Object ase_interval_interior_closure(Lisp_Object intv_intr_prod)
{
	ase_cartesian_t c =
		_ase_interval_interior_closure(
			XASE_CARTESIAN(intv_intr_prod));

	if (!c)
		return Qase_empty_interval;

	return _ase_wrap_cartesian_interior(c);
}

static ase_interval_union_item_t
_ase_interval_union_closure(ase_interval_union_item_t u)
{
	struct ase_interval_union_item_s ures, *ur = &ures;

	if (_ase_interval_union_closed_p(u))
		return u;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		Lisp_Object ltmp = Qnil;
		if (ASE_INTERVALP(u->current)) {
			ase_interval_t tmp =
				_ase_interval_closure(
					XASE_INTERVAL(u->current));
			u = u->next;
			if (!tmp)
				continue;
			ltmp = _ase_wrap_interval(tmp);
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			ase_cartesian_t tmp =
				_ase_interval_interior_closure(
					XASE_CARTESIAN(u->current));
			u = u->next;
			if (!tmp)
				continue;
			ltmp = _ase_wrap_cartesian_interior(tmp);
		}
		ur = ur->next = _ase_make_interval_union_item(ltmp);
	}

	_ase_normalise_union(&ures);

	return ures.next;
}

Lisp_Object ase_interval_union_closure(Lisp_Object intv_union)
{
	ase_interval_union_item_t u =
		_ase_interval_union_closure(
			XASE_INTERVAL_UNION_SER(intv_union));

	if (!u)
		return Qase_empty_interval;

	if (u->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(u));

	return u->current;
}

static ase_interval_t
_ase_interval_interior(ase_interval_t a)
{
	if (a == NULL || _ase_equal_p(a->lower, a->upper))
		return NULL;

	if (_ase_interval_open_p(a))
		return a;

	return _ase_make_interval(a->lower, a->upper, 1, 1);
}

Lisp_Object ase_interval_interior(Lisp_Object intv)
{
	ase_interval_t u =
		_ase_interval_interior(XASE_INTERVAL(intv));

	if (!u)
		return Qase_empty_interval;

	return _ase_wrap_interval(u);
}

static ase_cartesian_t
_ase_interval_interior_interior(ase_cartesian_t c)
{
	int i, dim = ase_cartesian_dimension(c);
	Lisp_Object *os = ase_cartesian_objects(c);
	Lisp_Object *newos = alloca_array(Lisp_Object, dim);

	for (i = 0; i < dim; i++) {
		newos[i] = ase_interval_interior(os[i]);
	}

	return _ase_make_cartesian(dim, newos, 1);
}

Lisp_Object ase_interval_interior_interior(Lisp_Object intv_intr_prod)
{
	ase_cartesian_t c =
		_ase_interval_interior_interior(
			XASE_CARTESIAN(intv_intr_prod));

	if (!c)
		return Qase_empty_interval;

	return _ase_wrap_cartesian_interior(c);
}

static ase_interval_union_item_t
_ase_interval_union_interior(ase_interval_union_item_t u)
{
	struct ase_interval_union_item_s ures, *ur = &ures;

	if (_ase_interval_union_open_p(u))
		return u;

	ur->current = Qase_empty_interval;
	ur->next = NULL;
	while (u) {
		Lisp_Object ltmp = Qnil;
		if (ASE_INTERVALP(u->current)) {
			ase_interval_t tmp =
				_ase_interval_interior(
					XASE_INTERVAL(u->current));
			u = u->next;
			if (!tmp)
				continue;
			ltmp = _ase_wrap_interval(tmp);
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			ase_cartesian_t tmp =
				_ase_interval_interior_interior(
					XASE_CARTESIAN(u->current));
			u = u->next;
			if (!tmp)
				continue;
			ltmp = _ase_wrap_cartesian_interior(tmp);
		}
		ur = ur->next = _ase_make_interval_union_item(ltmp);
	}

	return ures.next;
}

Lisp_Object ase_interval_union_interior(Lisp_Object intv_union)
{
	ase_interval_union_item_t u =
		_ase_interval_union_interior(
			XASE_INTERVAL_UNION_SER(intv_union));

	if (!u)
		return Qase_empty_interval;

	if (u->next)
		return _ase_wrap_interval_union(
			_ase_make_interval_union(u));

	return u->current;
}

static ase_interval_type_t
ase_interval_type(Lisp_Object o)
{
	if (ASE_INTERVALP(o)) {
		return ASE_ITYPE_INTERVAL;
	} else if (ASE_INTERVAL_UNION_P(o)) {
		return ASE_ITYPE_UNION;
	} else if (ASE_INTERVAL_INTERIOR_P(o)) {
		return ASE_ITYPE_INTERIOR;
	} else {
		return ASE_ITYPE_OBJECT;
	}
}

static inline void
_ase_heapsort_sift(Lisp_Object *args, int start, int count,
		   ase_order_relation_f lessp)
{
	int root = start, child;

	while (2*root  + 1 < count) {
		child = 2*root + 1;

		if (child < count-1 && lessp(args[child], args[child+1]))
			child++;
		if (lessp(args[root], args[child])) {
			_ase_swap(args, root, child);
			root = child;
		} else {
			return;
		}
	}
	return;
}

static inline void
_ase_heapsort(int nargs, Lisp_Object *args, ase_order_relation_f lessp)
{
	int start = nargs/2 - 1, end = nargs-1;

	while (start >= 0) {
		_ase_heapsort_sift(args, start, nargs, lessp);
		start--;
	}
	while (end > 0) {
		_ase_swap(args, end, 0);
		_ase_heapsort_sift(args, 0, end, lessp);
		end--;
	}
	return;
}

static Lisp_Object
ase_interval_connected_p_heapify(int nargs, Lisp_Object *args)
{
	/* special case for flat intervals,
	 * uses a heapsort to ease the connectivity question */
	Lisp_Object *newargs;
	int j, add = 0;

	/* check for ASE_INTERVALs and sort empty intervals to the tail */
	for (j = 0; j < nargs; ) {
		if (ASE_INTERVAL_UNION_P(args[j])) {
			/* remember the number of additional elements we need */
			add += XASE_INTERVAL_UNION(args[j])->no_intv-1;
			j++;
		} else if (!ASE_INTERVAL_EMPTY_P(args[j])) {
			j++;
		} else {
			_ase_swap(args, nargs-1, j);
			nargs--;
		}
	}

	if (nargs == 0)
		return Qt;
	else if (nargs == 1)	/* reflexivity! */
		return (ASE_INTERVAL_UNION_P(args[0]) ? Qnil : Qt);

	if (add > 0) {
		EMOD_ASE_DEBUG_INTV("exploding %d union items\n", add);
		newargs = alloca_array(Lisp_Object, nargs+add);
		/* move the first nargs args here */
		memmove(newargs, args, nargs*sizeof(Lisp_Object));
		/* now explode the whole story */
		args = _ase_interval_union_explode_array(nargs, newargs, add);
		nargs += add;
	}

	/* sort intervals in less-p metric */
	_ase_heapsort(nargs, args, ase_interval_less_p);

	for (j = 1; j < nargs; j++) {
		Lisp_Object o1 = args[j-1], o2 = args[j];
		if (!ase_interval_connected_p(o1, o2))
			return Qnil;
	}

	return Qt;
}

static Lisp_Object
ase_interval_connected_p_nsquare(int nargs, Lisp_Object *args)
{
	int i, j;
	ase_interval_type_t t1, t2;
	ase_st_relation_f relf = NULL;

	if (nargs == 0)
		return Qt;
	else if (nargs == 1 && !ASE_INTERVAL_UNION_P(args[0]))
		return Qt;
	else if (nargs == 1 &&
		 ASE_INTERVAL_INTERIOR_P(XASE_INTERVAL_UNION_FIRST(args[0]))) {
		ase_interval_union_item_t u1, u2;
		u1 = XASE_INTERVAL_UNION_SER(args[0]);
		t1 = t2 = ASE_ITYPE_INTERIOR;
		relf = ase_optable_connected[t1][t2];
		while ((u2 = u1->next)) {
			Lisp_Object o1 = u1->current;
			Lisp_Object o2 = u2->current;
			if (!relf(o1, o2))
				return Qnil;
			u1 = u1->next;
		}
		return Qt;
	} else if (nargs == 1)
		return Qnil;

	/* the slow approach */
	/* connectivity itself is an intransitive relation,
	 * but if any two are (locally) connected then all items are
	 * globally connected */
	for (i = 0; i < nargs-1; i++) {
		Lisp_Object o1 = args[i];
		int foundp = 0;
		t1 = ase_interval_type(o1);
		for (j = i+1; j < nargs && !foundp; j++) {
			Lisp_Object o2 = args[j];
			t2 = ase_interval_type(o2);
			relf = ase_optable_connected[t1][t2];
			if (relf && relf(o1, o2))
				foundp = 1;
		}
		if (!foundp)
			return Qnil;
	}

	return Qt;
}

static Lisp_Object
ase_interval_disjoint_p_nsquare(int nargs, Lisp_Object *args)
{
	int i, j;
	ase_interval_type_t t1, t2;
	ase_st_relation_f relf = NULL;

	if (nargs == 0)
		return Qt;
	else if (nargs == 1)	/* irreflexivity! */
		return Qnil;

	/* don't think that sorting helps here, but i'll profile this one day */
	/* pairwise (local) disjunction implies global disjunction */
	for (i = 0; i < nargs-1; i++) {
		Lisp_Object o1 = args[i];
		t1 = ase_interval_type(o1);
		for (j = i+1; j < nargs; j++) {
			Lisp_Object o2 = args[j];
			t2 = ase_interval_type(o2);
			relf = ase_optable_disjoint[t1][t2];
			if (relf && !relf(o1, o2))
				return Qnil;
		}
	}

	return Qt;
}

static int
ase_interval_dimension(Lisp_Object o)
{
	switch (ase_interval_type(o)) {
	case ASE_ITYPE_INTERVAL:
		return 1;
	case ASE_ITYPE_INTERIOR:
		return XASE_CARTESIAN_DIMENSION(o);
	case ASE_ITYPE_UNION:
		return ase_interval_dimension(XASE_INTERVAL_UNION_FIRST(o));

	case ASE_ITYPE_OBJECT:
	case NUMBER_OF_ASE_ITYPES:
	default:
		return -1;
	}
}

static int
ase_interval_check_dimensions(int nargs, Lisp_Object *args)
{
	int i, predicdim = 0;

	if (nargs == 0)
		return 0;

	/* partial loop unrolling */
	for (i = 0; i < nargs; i++) {
		CHECK_ASE_UBERINTERVAL(args[i]);
		if (!ASE_INTERVAL_EMPTY_P(args[i])) {
			predicdim = ase_interval_dimension(args[i]);
			break;
		}
	}
	for (i++; i < nargs; i++) {
		CHECK_ASE_UBERINTERVAL(args[i]);
		if (!ASE_INTERVAL_EMPTY_P(args[i]) &&
		    predicdim != ase_interval_dimension(args[i]))
			return -1;
	}
	return predicdim;
}



/* Measures */
static Lisp_Object
_ase_interval_compute_lebesgue(ase_interval_t a)
{
	if (a == NULL)
		return Qzero;

	return ent_binop(ASE_BINARY_OP_DIFF, a->upper, a->lower);
}

static inline void
_ase_interval_update_lebesgue(ase_interval_t a)
{
	if (a && NILP(a->lebesgue_measure))
		a->lebesgue_measure = _ase_interval_compute_lebesgue(a);
	return;
}

static inline Lisp_Object
_ase_interval_lebesgue(ase_interval_t a)
{
	if (a)
		return a->lebesgue_measure;
	else
		return Qzero;
}

static Lisp_Object
_ase_interval_compute_rational(ase_interval_t a)
{
	Lisp_Object args[2];
	Lisp_Object result;

	if (a == NULL)
		return Qzero;

	if (a->lower == a->upper) {
		/* special case of 1 point intervals */
		if (INTEGERP(a->lower))
			return make_int(1);
		else
			return Qzero;
	}

	if (_ase_equal_p((args[0] = Ftruncate(a->upper)), a->upper))
		args[0] = Fsub1(a->upper);
	args[1] = Ftruncate(a->lower);

	/* care for alternation of the signum */
	if (!NILP(Fnonnegativep(a->upper)) &&
	    NILP(Fnonnegativep(a->lower)) &&
	    !_ase_equal_p(args[1], a->lower))
		args[1] = Fsub1(args[1]);

	result = ent_binop_many(ASE_BINARY_OP_DIFF, countof(args), args);

	if (INTEGERP(a->upper) && !a->upper_open_p)
		result = Fadd1(result);
	if (INTEGERP(a->lower) && !a->lower_open_p)
		result = Fadd1(result);

	return result;
}

static inline void
_ase_interval_update_rational(ase_interval_t a)
{
	if (a && NILP(a->rational_measure))
		a->rational_measure = _ase_interval_compute_rational(a);
	return;
}

static inline Lisp_Object
_ase_interval_rational(ase_interval_t a)
{
	if (a)
		return a->rational_measure;
	else
		return Qzero;
}

static int
__ase_interval_interior_update_lebesgue(ase_cartesian_t c)
{
	int i = 0, dim = ase_cartesian_dimension(c);
	for (i = 0; i < dim; i++) {
		_ase_interval_update_lebesgue(
			XASE_INTERVAL(ase_cartesian_objects(c)[i]));
	}
	return dim;
}

static Lisp_Object
__ase_interval_interior_lebesgue(ase_cartesian_t c)
{
	Lisp_Object *args;
	int i = 0, dim = __ase_interval_interior_update_lebesgue(c);

	if (dim == 0)
		return Qzero;

	args = alloca_array(Lisp_Object, dim);
	for (i = 0; i < dim; i++) {
		args[i] = _ase_interval_lebesgue(
			XASE_INTERVAL(ase_cartesian_objects(c)[i]));
	}
	return ent_binop_many(ASE_BINARY_OP_PROD, dim, args);
}

static int
__ase_interval_interior_update_rational(ase_cartesian_t c)
{
	int i = 0, dim = ase_cartesian_dimension(c);
	for (i = 0; i < dim; i++) {
		_ase_interval_update_rational(
			XASE_INTERVAL(ase_cartesian_objects(c)[i]));
	}
	return dim;
}

static Lisp_Object
__ase_interval_interior_rational(ase_cartesian_t c)
{
	Lisp_Object *args;
	int i = 0, dim = __ase_interval_interior_update_rational(c);

	if (dim == 0)
		return Qzero;

	args = alloca_array(Lisp_Object, dim);
	for (i = 0; i < dim; i++) {
		args[i] = _ase_interval_rational(
			XASE_INTERVAL(ase_cartesian_objects(c)[i]));
	}
	return ent_binop_many(ASE_BINARY_OP_PROD, dim, args);
}

static void
_ase_interval_interior_update_lebesgue(ase_cartesian_t c)
	__attribute__((always_inline));
static inline void
_ase_interval_interior_update_lebesgue(ase_cartesian_t c)
{
	if (NILP(c->lebesgue_measure))
		c->lebesgue_measure =
			__ase_interval_interior_lebesgue(c);
	return;
}

static Lisp_Object
_ase_interval_interior_lebesgue(ase_cartesian_t c)
{
	return c->lebesgue_measure;
}

static void
_ase_interval_interior_update_rational(ase_cartesian_t c)
{
	if (NILP(c->rational_measure))
		c->rational_measure =
			__ase_interval_interior_rational(c);
	return;
}

static inline Lisp_Object
_ase_interval_interior_rational(ase_cartesian_t c)
{
	return c->rational_measure;
}

static inline int
__ase_interval_union_update_lebesgue(ase_interval_union_item_t u)
	__attribute__((always_inline));
static inline int
__ase_interval_union_update_lebesgue(ase_interval_union_item_t u)
{
	int i = 0;
	while (u) {
		if (ASE_INTERVALP(u->current)) {
			_ase_interval_update_lebesgue(
				XASE_INTERVAL(u->current));
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			_ase_interval_interior_update_lebesgue(
				XASE_CARTESIAN(u->current));
		}
		u = u->next;
		i++;
	}
	return i;
}

static Lisp_Object
__ase_interval_union_lebesgue(ase_interval_union_item_t u)
{
	Lisp_Object *args;
	int i = 0, nargs = __ase_interval_union_update_lebesgue(u);

	if (nargs == 0)
		return Qzero;

	args = alloca_array(Lisp_Object, nargs);
	while (u) {
		if (ASE_INTERVALP(u->current)) {
			args[i] = _ase_interval_lebesgue(
				XASE_INTERVAL(u->current));
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			args[i] = _ase_interval_interior_lebesgue(
				XASE_CARTESIAN(u->current));
		}
		i++;
		u = u->next;
	}
	return ent_binop_many(ASE_BINARY_OP_SUM, nargs, args);
}

static int
__ase_interval_union_update_rational(ase_interval_union_item_t u)
{
	int i = 0;
	while (u) {
		if (ASE_INTERVALP(u->current)) {
			_ase_interval_update_rational(
				XASE_INTERVAL(u->current));
		} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
			_ase_interval_interior_update_rational(
				XASE_CARTESIAN(u->current));
		}
		u = u->next;
		i++;
	}
	return i;
}

static Lisp_Object
__ase_interval_union_rational(ase_interval_union_item_t u)
{
	int i = 0, nargs = __ase_interval_union_update_rational(u);
	if (nargs == 0)
		return Qzero;
	{
		Lisp_Object args[nargs];
		for ( i = nargs; i > 0; )
			args[--i] = Qnil;

		while (u) {
			if (ASE_INTERVALP(u->current)) {
				args[i] = _ase_interval_rational(
					XASE_INTERVAL(u->current));
			} else if (ASE_INTERVAL_INTERIOR_P(u->current)) {
				args[i] = _ase_interval_interior_rational(
					XASE_CARTESIAN(u->current));
			}
			i++;
			u = u->next;
		}
		return ent_binop_many(ASE_BINARY_OP_SUM, nargs, args);
	}
}

static inline void
_ase_interval_union_update_lebesgue(ase_interval_union_t iu)
{
	if (NILP(iu->lebesgue_measure))
		iu->lebesgue_measure =
			__ase_interval_union_lebesgue(ase_interval_union(iu));
	return;
}

static inline Lisp_Object
_ase_interval_union_lebesgue(ase_interval_union_t iu)
{
	return iu->lebesgue_measure;
}

static inline void
_ase_interval_union_update_rational(ase_interval_union_t iu)
{
	if (NILP(iu->rational_measure))
		iu->rational_measure =
			__ase_interval_union_rational(ase_interval_union(iu));
	return;
}

static inline Lisp_Object
_ase_interval_union_rational(ase_interval_union_t iu)
{
	return iu->rational_measure;
}

Lisp_Object
ase_interval_lebesgue_measure(ase_interval_t a)
{
	_ase_interval_update_lebesgue(a);
	return _ase_interval_lebesgue(a);
}

Lisp_Object
ase_interval_rational_measure(ase_interval_t a)
{
	_ase_interval_update_rational(a);
	return _ase_interval_rational(a);
}

Lisp_Object
ase_interval_interior_lebesgue_measure(ase_cartesian_t c)
{
	_ase_interval_interior_update_lebesgue(c);
	return _ase_interval_interior_lebesgue(c);
}

Lisp_Object
ase_interval_interior_rational_measure(ase_cartesian_t c)
{
	_ase_interval_interior_update_rational(c);
	return _ase_interval_interior_rational(c);
}

Lisp_Object
ase_interval_union_lebesgue_measure(ase_interval_union_t iu)
{
	_ase_interval_union_update_lebesgue(iu);
	return _ase_interval_union_lebesgue(iu);
}

Lisp_Object
ase_interval_union_rational_measure(ase_interval_union_t iu)
{
	_ase_interval_union_update_rational(iu);
	return _ase_interval_union_rational(iu);
}

/* arithmetical operations */
/* I x Q -> I : (a, b) + x -> (a+x, b+x) */
/* I x I -> I : (a, b) + (c, d) -> (a+c, b+d) */
/* U x Q -> U : (a, b) u (c, d) + x -> (a, b) + x u (c, d) + x */
/* U x I -> U : (a, b) u (c, d) + (e, f) -> (a, b) + (e, f) u (c, d) + (e, f) */
/* U x U -> U : A u B + C u D u E -> A+C u B+C u A+D u B+D u A+E u B+E */


/* lisp level */
DEFUN("ase-intervalp", Fase_intervalp, 1, 1, 0, /*
Return non-`nil' iff OBJECT is an ase interval.
*/
      (object))
{
	if (ASE_INTERVALP(object))
		return Qt;

	return Qnil;
}

DEFUN("ase-interval-union-p", Fase_interval_union_p, 1, 1, 0, /*
Return non-`nil' iff OBJECT is an ase interval or union thereof.
*/
      (object))
{
	if (ASE_INTERVAL_OR_UNION_P(object))
		return Qt;

	return Qnil;
}

DEFUN("ase-interval-empty-p", Fase_interval_empty_p, 1, 1, 0, /*
Return non-`nil' iff INTERVAL is the empty interval.
*/
      (interval))
{
	CHECK_ASE_INTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qt;

	return Qnil;
}

DEFUN("ase-interval-imprimitive-p", Fase_interval_imprimitive_p, 1, 1, 0, /*
Return non-`nil' iff INTERVAL is not a primitive interval.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVALP(interval))
		return Qnil;

	return Qt;
}

DEFUN("ase-interval-open-p", Fase_interval_open_p, 1, 1, 0, /*
Return non-`nil' iff INTERVAL (or a union thereof) is an open set
with respect to the standard topology.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVALP(interval)) {
		if (ASE_INTERVAL_EMPTY_P(interval))
			return Qt;
		if (ase_interval_open_p(interval))
			return Qt;
	} else if (ASE_INTERVAL_UNION_P(interval)) {
		if (ase_interval_union_open_p(interval))
			return Qt;
	} else if (ASE_INTERVAL_INTERIOR_P(interval)) {
		if (ase_interval_interior_open_p(interval))
			return Qt;
	}
	return Qnil;
}

DEFUN("ase-interval-closed-p", Fase_interval_closed_p, 1, 1, 0, /*
Return non-`nil' iff INTERVAL (or a union thereof) is a closed set
with respect to the standard metric.

An interval is said to be closed iff the complement is open.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVALP(interval)) {
		if (ASE_INTERVAL_EMPTY_P(interval))
			return Qt;
		if (ase_interval_closed_p(interval))
			return Qt;
	} else if (ASE_INTERVAL_UNION_P(interval)) {
		if (ase_interval_union_closed_p(interval))
			return Qt;
	} else if (ASE_INTERVAL_INTERIOR_P(interval)) {
		if (ase_interval_interior_closed_p(interval))
			return Qt;
	}
	return Qnil;
}


/* constructors */
/* ###autoload */
DEFUN("ase-empty-interval", Fase_empty_interval, 0, 0, 0, /*
Return the empty interval.
*/
      ())
{
	return Qase_empty_interval;
}

/* ###autoload */
DEFUN("ase-universe-interval", Fase_universe_interval, 0, 0, 0, /*
Return the universe interval.
*/
      ())
{
	return Qase_universe_interval;
}

/* ###autoload */
DEFUN("ase-interval", Fase_interval, 1, 4, 0, /*
Return a (primitive) interval with lower bound LOWER and upper bound UPPER.
To construct a (degenerated) one point interval, leave out the UPPER part.

ASE's definition of an interval:
With respect to a (strict) partial order, an interval is a connected
subset of a poset.

If no special partial order is given, it defaults to less-equal-p (<=).
If no special topology is given, it defaults to the po topology.
*/
      (lower, upper, lower_open_p, upper_open_p))
{
	Lisp_Object result = Qnil;
	Lisp_Object args[2] = {lower, upper};

	CHECK_COMPARABLE(lower);
	if (NILP(upper))
		args[1] = upper = lower;
	else
		CHECK_COMPARABLE(upper);

	if (_ase_less_p(lower, upper))
		result = ase_make_interval(
			lower, upper, !NILP(lower_open_p), !NILP(upper_open_p));
	else
		result = ase_make_interval(
			upper, lower, !NILP(upper_open_p), !NILP(lower_open_p));

	return result;
}

DEFUN("ase-interval-contains-p", Fase_interval_contains_p, 2, 2, 0, /*
Return non-`nil' iff INTERVAL (or a union thereof) contains OBJECT
as one of its elements.  OBJECT can also be another interval or
interval union to obtain the subset relation.
*/
      (interval, object))
{
	ase_interval_type_t sup, sub;
	ase_element_relation_f relf = NULL;

	CHECK_ASE_UBERINTERVAL(interval);

	sup = ase_interval_type(interval);
	sub = ase_interval_type(object);

	if ((relf = ase_optable_superset[sup][sub]) &&
	    (!NILP(relf(interval, object))))
		return Qt;

	return Qnil;
}

DEFUN("ase-interval-contains-where", Fase_interval_contains_where, 2, 2, 0, /*
Return non-`nil' iff INTERVAL contains OBJECT as one of its elements.
ELEMENT can also be another interval to obtain the subset relation.

The non-`nil' value returned is the primitive interval which
contained OBJECT.
*/
      (interval, object))
{
	ase_interval_type_t sup, sub;
	ase_element_relation_f relf = NULL;

	CHECK_ASE_UBERINTERVAL(interval);

	sup = ase_interval_type(interval);
	sub = ase_interval_type(object);

	if ((relf = ase_optable_superset[sup][sub]))
		return relf(interval, object);

	return Qnil;
}

DEFUN("ase-interval-connected-p", Fase_interval_connected_p, 0, MANY, 0, /*
Return non-`nil' iff INTERVALS are connected.
Arguments: &rest intervals

Zero intervals are trivially connected, as is one interval.
*/
      (int nargs, Lisp_Object *args))
{
	/* trivial cases */
	if (nargs == 0)
		return Qt;

	switch (ase_interval_check_dimensions(nargs, args)) {
	case 0:
		return Qt;
	case 1:
		return ase_interval_connected_p_heapify(nargs, args);
	case -1:
		signal_error(Qembed_error, Qnil);
		return Qnil;
	default:
		return ase_interval_connected_p_nsquare(nargs, args);
	}
}

DEFUN("ase-interval-disjoint-p", Fase_interval_disjoint_p, 0, MANY, 0, /*
Arguments: &rest intervals
Return non-`nil' iff INTERVALS are (pairwise) disjoint.

Zero intervals are trivially disjoint, while one interval is
trivially not disjoint.
*/
      (int nargs, Lisp_Object *args))
{
	/* trivial cases */
	if (nargs == 0)
		return Qt;

	switch (ase_interval_check_dimensions(nargs, args)) {
	case 0:
		return Qt;
	case -1:
		signal_error(Qembed_error, Qnil);
		return Qnil;
	default:
		return ase_interval_disjoint_p_nsquare(nargs, args);
	}
}

DEFUN("ase-interval-equal-p", Fase_interval_equal_p, 2, 2, 0, /*
Return non-`nil' if I1 and I2 are equal in some sense, equality
hereby means that I1 and I2 contain each other.

In fact, this is just a convenience function and totally equivalent
to
  (and (ase-interval-contains-p i1 i2) (ase-interval-contains-p i2 i1))
*/
      (i1, i2))
{
	Lisp_Object i1in2, i2in1;

	CHECK_ASE_UBERINTERVAL(i1);
	CHECK_ASE_UBERINTERVAL(i2);

	i1in2 = Fase_interval_contains_p(i1, i2);
	i2in1 = Fase_interval_contains_p(i2, i1);

	if (!NILP(i1in2) && !NILP(i2in1))
		return Qt;

	return Qnil;
}

/* more constructors */
static Lisp_Object
ase_interval_union_heapify(int nargs, Lisp_Object *args)
{
	Lisp_Object result = Qnil, *newargs;
	int j, add = 0;
	struct ase_interval_union_item_s _ures, *ures = &_ures, *u;
	ase_interval_union_t ires;

	/* check for ASE_INTERVALs and sort empty intervals to the tail */
	for (j = 0; j < nargs; ) {
		if (ASE_INTERVAL_UNION_P(args[j])) {
			/* remember the number of additional elements we need */
			add += XASE_INTERVAL_UNION(args[j])->no_intv-1;
			j++;
		} else if (!ASE_INTERVAL_EMPTY_P(args[j])) {
			j++;
		} else {
			_ase_swap(args, nargs-1, j);
			nargs--;
		}
	}

	if (nargs == 0)
		return Qase_empty_interval;
	if (nargs == 1)
		return args[0];

	if (add > 0) {
		EMOD_ASE_DEBUG_INTV("exploding %d union items\n", add);
		newargs = alloca_array(Lisp_Object, nargs+add);
		/* move the first nargs args here */
		memmove(newargs, args, nargs*sizeof(Lisp_Object));
		/* now explode the whole story */
		args = _ase_interval_union_explode_array(nargs, newargs, add);
		nargs += add;
	}

	/* sort intervals in less-p metric */
	_ase_heapsort(nargs, args, ase_interval_less_p);

	/* we start with the empty union and unite left-associatively from
	   the left */
	ures->current = Qase_empty_interval;
	u = ures->next = _ase_make_interval_union_item(args[0]);
	for (j = 1; j < nargs; j++) {
		u = u->next = _ase_make_interval_union_item(args[j]);
	}

	j = _ase_normalise_union(ures);
	if (j > 1) {
		/* only return a union when there _is_ a union */
		ires = _ase_make_interval_union(ures->next);
		ires->no_intv = j;

		XSETASE_INTERVAL_UNION(result, ires);
		return result;
	} else {
		/* otherwise downgrade to a primitive interval */
		result = ures->next->current;
		_ase_interval_union_item_fini(ures->next);
		return result;
	}
}

static inline Lisp_Object
ase_interval_union_nsquare(int nargs, Lisp_Object *args)
{
	int i, j = 0;
	struct ase_interval_union_item_s _ures, *ures = &_ures, *u;
	ase_interval_union_t ires;
	Lisp_Object result = Qnil;

	if (nargs == 0)
		return Qase_empty_interval;
	else if (nargs == 1)
		return args[0];

	/* the slow approach */
	/* we start with the empty union and unite left-associatively from
	   the left */
	ures->current = Qase_empty_interval;
	u = ures;
	for (i = 0; i < nargs; i++) {
		Lisp_Object tmp = args[i];
		if (ASE_INTERVAL_INTERIOR_P(tmp))
			u = u->next = _ase_make_interval_union_item(tmp);
		else if (ASE_INTERVAL_UNION_P(tmp)) {
			ase_interval_union_item_t tra =
				XASE_INTERVAL_UNION_SER(tmp);
			while (tra) {
				Lisp_Object c = tra->current;
				u = u->next = _ase_make_interval_union_item(c);
				tra = tra->next;
			}
		}
	}

	j = _ase_normalise_union_intr(ures);
	if (j > 1) {
		/* only return a union when there _is_ a union */
		ires = _ase_make_interval_union(ures->next);
		ires->no_intv = j;

		XSETASE_INTERVAL_UNION(result, ires);
		return result;
	} else {
		/* otherwise downgrade to a primitive interval */
		result = ures->next->current;
		_ase_interval_union_item_fini(ures->next);
		return result;
	}
}

DEFUN("ase-interval-union", Fase_interval_union, 0, MANY, 0, /*
Arguments: &rest intervals
Return the union of all INTERVALS.
*/
      (int nargs, Lisp_Object *args))
{
	int dim;

	/* trivial cases */
	if (nargs == 0)
		return Qase_empty_interval;

	dim = ase_interval_check_dimensions(nargs, args);
	switch (dim) {
	case 0:
		return Qase_empty_interval;
	case 1:
		return ase_interval_union_heapify(nargs, args);
	case -1:
		signal_error(Qembed_error, Qnil);
		return Qnil;
	default:
		return ase_interval_union_nsquare(nargs, args);
	}
}

static int
ase_interval_intersection_maybe_empty(int nargs, Lisp_Object *args)
{
	/* check for empty intervals, return 1 if there are some */
	int j;

	for (j = 0; j < nargs; j++) {
		if (ASE_INTERVAL_EMPTY_P(args[j])) {
			return 1;
		}
	}
	return 0;
}

static Lisp_Object
ase_interval_intersection_heapify(int nargs, Lisp_Object *args)
{
	int j;

	if (nargs == 0)
		return Qase_empty_interval;
	else if (nargs == 1)
		return args[0];
	else if (ase_interval_intersection_maybe_empty(nargs, args))
		return Qase_empty_interval;

	_ase_heapsort(nargs, args, ase_interval_or_union_less_p);

	/* we start with the universe and intersect left-associatively from
	   the left */
	for (j = 1; j < nargs; j++) {
		ase_interval_type_t t1 = ase_interval_type(args[0]);
		ase_interval_type_t t2 = ase_interval_type(args[j]);
		ase_binary_operation_f opf = ase_optable_intersect[t1][t2];

		if (opf) {
			args[0] = opf(args[0], args[j]);
		}
	}

	return args[0];
}

static Lisp_Object
ase_interval_intersection_nsquare(int nargs, Lisp_Object *args)
{
	int j;

	if (nargs == 0)
		return Qase_empty_interval;
	else if (nargs == 1)
		return args[0];
	else if (ase_interval_intersection_maybe_empty(nargs, args))
		return Qase_empty_interval;

	/* we start with the universe and intersect left-associatively from
	   the left */
	for (j = 1; j < nargs; j++) {
		ase_interval_type_t t1 = ase_interval_type(args[0]);
		ase_interval_type_t t2 = ase_interval_type(args[j]);
		ase_binary_operation_f opf = ase_optable_intersect[t1][t2];

		if (opf) {
			args[0] = opf(args[0], args[j]);
		}
	}

	return args[0];
}

DEFUN("ase-interval-intersection", Fase_interval_intersection, 0, MANY, 0, /*
Arguments: &rest intervals
Return the intersection of all INTERVALS.
*/
      (int nargs, Lisp_Object *args))
{
	/* trivial cases */
	if (nargs == 0)
		return Qase_empty_interval;
	else if (nargs == 1)
		return args[0];

	switch (ase_interval_check_dimensions(nargs, args)) {
	case 0:
		return Qase_empty_interval;
	case 1:
		return ase_interval_intersection_heapify(nargs, args);
	case -1:
		signal_error(Qembed_error, Qnil);
		return Qnil;
	default:
		return ase_interval_intersection_nsquare(nargs, args);
	}
}

static inline Lisp_Object
ase_interval_difference_nsquare(int nargs, Lisp_Object *args)
{
	int j;

	/* check for ASE_INTERVALs and sort empty intervals to the tail */
	for (j = 1; j < nargs; j++) {
		/* we can only resort empty intervals for j >= 1 */
		if (ASE_INTERVAL_EMPTY_P(args[j])) {
			_ase_swap(args, nargs-1, j);
			nargs--;
		}
	}

	if (nargs == 0)
		return Qase_empty_interval;
	if (nargs == 1)
		return args[0];

	/* we must not use heapsort here, since subtracting sets is
	 * not commutative */

	/* we start with args[0] and subtract left-associatively from
	   the left */
	for (j = 1; j < nargs; j++) {
		ase_interval_type_t t1 = ase_interval_type(args[0]);
		ase_interval_type_t t2 = ase_interval_type(args[j]);
		ase_binary_operation_f opf = ase_optable_subtract[t1][t2];

		if (opf) {
			args[0] = opf(args[0], args[j]);
		}
	}

	return args[0];
}

DEFUN("ase-interval-difference", Fase_interval_difference, 0, MANY, 0, /*
Arguments: &rest intervals
Return the difference of all INTERVALS from left to right.
*/
      (int nargs, Lisp_Object *args))
{
	/* Treat the case args[0] = ( ) specially */
	if (nargs == 0)
		return Qase_empty_interval;
	else if (nargs == 1)
		return args[0];

	switch (ase_interval_check_dimensions(nargs, args)) {
	case 0:
		return Qase_empty_interval;
	case -1:
		signal_error(Qembed_error, Qnil);
		return Qnil;
	default:
		return ase_interval_difference_nsquare(nargs, args);
	}
}

DEFUN("ase-copy-interval", Fase_copy_interval, 1, 1, 0, /*
Return a copy of INTERVAL.
*/
      (interval))
{
	CHECK_ASE_INTERVAL(interval);

	return ase_copy_interval(interval);
}

DEFUN("ase-interval-boundary", Fase_interval_boundary, 1, 1, 0, /*
Return the boundary of INTERVAL, that is the interior of INTERVAL
subtracted from the closure of INTERVAL.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qase_empty_interval;
	else if (ASE_INTERVALP(interval))
		return ase_interval_boundary(interval);
	else if (ASE_INTERVAL_INTERIOR_P(interval))
		return ase_interval_interior_boundary(interval);
	else if (ASE_INTERVAL_UNION_P(interval))
		return ase_interval_union_boundary(interval);

	return Qnil;
}

DEFUN("ase-interval-closure", Fase_interval_closure, 1, 1, 0, /*
Return the closure of INTERVAL, that is the smallest closed set
that contains INTERVAL.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qase_empty_interval;
	else if (ASE_INTERVALP(interval))
		return ase_interval_closure(interval);
	else if (ASE_INTERVAL_INTERIOR_P(interval))
		return ase_interval_interior_closure(interval);
	else if (ASE_INTERVAL_UNION_P(interval))
		return ase_interval_union_closure(interval);

	return Qnil;
}

DEFUN("ase-interval-interior", Fase_interval_interior, 1, 1, 0, /*
Return the interior of INTERVAL, that is the largest open set that
is contained in INTERVAL.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qase_empty_interval;
	else if (ASE_INTERVALP(interval))
		return ase_interval_interior(interval);
	else if (ASE_INTERVAL_INTERIOR_P(interval))
		return ase_interval_interior_interior(interval);
	else if (ASE_INTERVAL_UNION_P(interval))
		return ase_interval_union_interior(interval);

	return Qnil;
}

/* Accessors */
DEFUN("ase-interval-lower", Fase_interval_lower, 1, 1, 0, /*
Return the lower bound of INTERVAL or `nil' if empty.
Only the numerical value is returned.
*/
      (interval))
{
	CHECK_ASE_INTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qnil;

	return XASE_INTERVAL(interval)->lower;
}

DEFUN("ase-interval-upper", Fase_interval_upper, 1, 1, 0, /*
Return the upper bound of INTERVAL or `nil' if empty.
Only the numerical value is returned.
*/
      (interval))
{
	CHECK_ASE_INTERVAL(interval);

	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qnil;

	return XASE_INTERVAL(interval)->upper;
}

DEFUN("ase-interval-lower*", Fase_interval_lower_, 1, 1, 0, /*
Return the lower bound of INTERVAL or `nil' if empty
along with the boundary shape.
*/
      (interval))
{
	Lisp_Object res;

	CHECK_ASE_INTERVAL(interval);
	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qnil;

	res = XASE_INTERVAL(interval)->lower;
	if (XASE_INTERVAL(interval)->lower_open_p)
		return Fcons(Q_open, res);
	else
		return Fcons(Q_closed, res);
}

DEFUN("ase-interval-upper*", Fase_interval_upper_, 1, 1, 0, /*
Return the upper bound of INTERVAL or `nil' if empty
along with the boundary shape.
*/
      (interval))
{
	Lisp_Object res;

	CHECK_ASE_INTERVAL(interval);
	if (ASE_INTERVAL_EMPTY_P(interval))
		return Qnil;

	res = XASE_INTERVAL(interval)->upper;
	if (XASE_INTERVAL(interval)->upper_open_p)
		return Fcons(Q_open, res);
	else
		return Fcons(Q_closed, res);
}

DEFUN("ase-interval-explode-union", Fase_interval_explode_union, 1, 1, 0, /*
Return IUNION exploded into primitive intervals and listed in a dllist.
*/
      (iunion))
{
	Lisp_Object result = Qnil;
	dllist_t resdll = make_dllist();
	ase_interval_union_item_t u;

	CHECK_ASE_INTERVAL_UNION(iunion);
	u = XASE_INTERVAL_UNION_SER(iunion);
	while (u) {
		dllist_append(resdll, (void*)u->current);
		u = u->next;
	}

	XSETDLLIST(result, resdll);
	return result;
}


/* Measures */
DEFUN("ase-interval-lebesgue-measure",
      Fase_interval_lebesgue_measure, 1, 1, 0, /*
Return the Lebesgue measure of INTERVAL.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVALP(interval))
		return ase_interval_lebesgue_measure(XASE_INTERVAL(interval));
	else if (ASE_INTERVAL_INTERIOR_P(interval))
		return ase_interval_interior_lebesgue_measure(
			XASE_CARTESIAN(interval));
	else if (ASE_INTERVAL_UNION_P(interval))
		return ase_interval_union_lebesgue_measure(
			XASE_INTERVAL_UNION(interval));
	return Qnil;
}

DEFUN("ase-interval-rational-measure",
      Fase_interval_rational_measure, 1, 1, 0, /*
Return the number of rational integers in INTERVAL.
*/
      (interval))
{
	CHECK_ASE_UBERINTERVAL(interval);

	if (ASE_INTERVALP(interval))
		return ase_interval_rational_measure(XASE_INTERVAL(interval));
	else if (ASE_INTERVAL_INTERIOR_P(interval))
		return ase_interval_interior_rational_measure(
			XASE_CARTESIAN(interval));
	else if (ASE_INTERVAL_UNION_P(interval))
		return ase_interval_union_rational_measure(
			XASE_INTERVAL_UNION(interval));
	return Qnil;
}

DEFUN("ase-interval-dump", Fase_interval_dump, 1, 1, 0, /*
*/
      (interval))
{
	CHECK_ASE_INTERVAL_OR_UNION(interval);

	if (ASE_INTERVALP(interval)) {
		ase_interval_prnt(interval, Qexternal_debugging_output, 0);
		write_c_string("\n", Qexternal_debugging_output);
		return Qt;
	} else {
		ase_interval_union_prnt(
			interval, Qexternal_debugging_output, 0);
		write_c_string("\n", Qexternal_debugging_output);
		return Qt;
	}
}


static inline Lisp_Object
ase_interval_add_i_obj(Lisp_Object intv, Lisp_Object number)
{
	int lopenp = XASE_INTERVAL(intv)->lower_open_p;
	int uopenp = XASE_INTERVAL(intv)->upper_open_p;
	int lequp = XASE_INTERVAL(intv)->lower_eq_upper_p;
	Lisp_Object args[2] = {Qnil, number};
	Lisp_Object newl, newu;

	args[0] = XASE_INTERVAL(intv)->lower;
	newl = ent_binop(ASE_BINARY_OP_SUM, args[0], args[1]);
	if (!lequp) {
		args[0] = XASE_INTERVAL(intv)->upper;
		newu = ent_binop(ASE_BINARY_OP_SUM, args[0], args[1]);
		return ase_make_interval(newl, newu, lopenp, uopenp);
	} else {
		return ase_make_interval(newl, newl, lopenp, uopenp);
	}
}

static inline Lisp_Object
ase_interval_add_obj_i(Lisp_Object number, Lisp_Object intv)
{
	return ase_interval_add_i_obj(intv, number);
}


/* initialiser stuff */
static inline void
ase_interval_binary_optable_init(void)
{
	int idx = ase_optable_index_typesym(Qase_interval);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   idx, INT_T, ase_interval_add_i_obj);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, idx, ase_interval_add_obj_i);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   idx, FLOAT_T, ase_interval_add_obj_i);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, idx, ase_interval_add_obj_i);
}

void
EMOD_PUBINIT(void)
{
	/* constructors */
	DEFSUBR(Fase_empty_interval);
	DEFSUBR(Fase_universe_interval);
	DEFSUBR(Fase_interval);
	DEFSUBR(Fase_interval_union);
	DEFSUBR(Fase_interval_intersection);
	DEFSUBR(Fase_interval_difference);
	DEFSUBR(Fase_copy_interval);
	DEFSUBR(Fase_interval_boundary);
	DEFSUBR(Fase_interval_interior);
	DEFSUBR(Fase_interval_closure);
	/* predicates */
	DEFSUBR(Fase_intervalp);
	DEFSUBR(Fase_interval_union_p);
	DEFSUBR(Fase_interval_empty_p);
	DEFSUBR(Fase_interval_imprimitive_p);
	DEFSUBR(Fase_interval_open_p);
	DEFSUBR(Fase_interval_closed_p);
	DEFSUBR(Fase_interval_contains_p);
	DEFSUBR(Fase_interval_contains_where);
	DEFSUBR(Fase_interval_connected_p);
	DEFSUBR(Fase_interval_disjoint_p);
	DEFSUBR(Fase_interval_equal_p);
	/* accessors */
	DEFSUBR(Fase_interval_lower);
	DEFSUBR(Fase_interval_lower_);
	DEFSUBR(Fase_interval_upper);
	DEFSUBR(Fase_interval_upper_);
	DEFSUBR(Fase_interval_explode_union);
	/* measures */
	DEFSUBR(Fase_interval_lebesgue_measure);
	DEFSUBR(Fase_interval_rational_measure);

	DEFASETYPE_WITH_OPS(Qase_interval, "ase:interval");
	defsymbol(&Qase_intervalp, "ase:intervalp");
	DEFASETYPE_WITH_OPS(Qase_interval_union, "ase:interval-union");
	defsymbol(&Qase_interval_union_p, "ase:interval-union-p");

	defsymbol(&Q_less, ":<");
	defsymbol(&Q_greater, ":>");
	defsymbol(&Q_eql, ":=");
	DEFKEYWORD(Q_unknown);
	DEFKEYWORD(Q_open);
	DEFKEYWORD(Q_closed);
	DEFKEYWORD(Q_disjoint);
	DEFKEYWORD(Q_connected);

	/* debugging */
	DEFSUBR(Fase_interval_dump);

	ase_interval_binary_optable_init();

	EMOD_PUBREINIT();

	DEFVAR_CONST_LISP("ase-empty-interval", &Qase_empty_interval /*
The interval which contains no elements.
								     */);
	DEFVAR_CONST_LISP("ase-universe-interval", &Qase_universe_interval /*
The interval which contains all elements.
									   */);

	Fprovide(intern("ase-interval"));
	return;
}

void
EMOD_PUBREINIT(void)
{
	Qase_empty_interval = ase_empty_interval();
	Qase_universe_interval = ase_universe_interval();
	staticpro(&Qase_empty_interval);
	staticpro(&Qase_universe_interval);

	if (LIKELY(ase_empty_sets != NULL)) {
		dllist_append(ase_empty_sets, (void*)Qase_empty_interval);
	} else {
		EMOD_ASE_CRITICAL("Cannot proclaim empty elements\n");
	}
	return;
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-interval"));
	return;
}

/* ase-interval ends here */
