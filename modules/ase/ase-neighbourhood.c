/*** ase-neighbourhood.c -- Neighbourhood of ASE objects
 *
 * Copyright (C) 2006 - 2008 Sebastian Freundt
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
#include "ent.h"
#include "ase.h"
#include "ase-neighbourhood.h"

#define EMOD_ASE_DEBUG_NBH(args...)	EMOD_ASE_DEBUG("[NBH]: " args)

#define EMODNAME	ase_neighbourhood

PROVIDE(ase_neighbourhood);
REQUIRE(ase_neighbourhood, "ase", "ase-interval");

Lisp_Object Qase_neighbourhood, Qase_neighbourhoodp;


/* stuff for the dynacat */
static void
_ase_neighbourhood_prnt(ase_neighbourhood_t n, Lisp_Object pcf)
{
	write_c_string("{p : (< (d ", pcf);
	print_internal(n->point, pcf, 0);
	write_c_string(" p) ", pcf);
	print_internal(n->radius, pcf, 0);
	write_c_string("}", pcf);
}

static void
ase_neighbourhood_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_NBH("n:0x%08x@0x%08x (rc:%d)\n",
			   (unsigned int)(XASE_NEIGHBOURHOOD(obj)),
			   (unsigned int)obj,
			   (XASE_NEIGHBOURHOOD(obj) ?
			    XASE_NEIGHBOURHOOD_REFVAL(obj) : 1));
	write_c_string("#<ase:neighbourhood ", pcf);
	_ase_neighbourhood_prnt(XASE_NEIGHBOURHOOD(obj), pcf);
	write_c_string(" wrt supremum metric>", pcf);
}

static void
ase_neighbourhood_fini(Lisp_Object obj, int unused)
{
	ase_neighbourhood_t n = XASE_NEIGHBOURHOOD(obj);

	EMOD_ASE_DEBUG_GC("n:0x%08x@0x%08x (rc:%d) shall be freed...\n",
			  (unsigned int)(n), (unsigned int)obj,
			  ase_neighbourhood_refval(n));

	if (ase_neighbourhood_decref(n) <= 0) {
		if (n->data)
			xfree(n->data);
		n->data = NULL;
		ase_neighbourhood_fini_refcnt(n);
		xfree(n);
	} else {
		EMOD_ASE_DEBUG_GC("VETO! References exist\n");
	}
	return;
}

static void
_ase_neighbourhood_mark(ase_neighbourhood_t n)
{
	if (n == NULL)
		return;

	mark_object(n->point);
	mark_object(n->radius);
	mark_object(n->lebesgue_measure);
	mark_object(n->rational_measure);
	mark_object(n->colour);

	if (n->ldata) {
		mark_object(n->ldata);
	}

	return;
}

static void
ase_neighbourhood_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_NBH("n:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			   (unsigned int)(XASE_NEIGHBOURHOOD(obj)),
			   (unsigned int)obj,
			   (XASE_NEIGHBOURHOOD(obj) ?
			    XASE_NEIGHBOURHOOD_REFVAL(obj) : 1));
	_ase_neighbourhood_mark(XASE_NEIGHBOURHOOD(obj));
	return;
}


Lisp_Object
_ase_wrap_neighbourhood(ase_neighbourhood_t n)
{
	Lisp_Object result;

	result = make_dynacat(n);
	XDYNACAT(result)->type = Qase_neighbourhood;

	if (n)
		ase_neighbourhood_incref(n);

	set_dynacat_printer(result, ase_neighbourhood_prnt);
	set_dynacat_marker(result, ase_neighbourhood_mark);
	set_dynacat_finaliser(result, ase_neighbourhood_fini);

	EMOD_ASE_DEBUG_NBH("n:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			   (unsigned int)n,
			   (n ? ase_neighbourhood_refval(n) : 1),
			   (unsigned int)result);

	return result;
}

static inline Lisp_Object
__ase_make_neighbourhood_intv(Lisp_Object p, Lisp_Object r)
{
	Lisp_Object lo, up;
	Lisp_Object args[2] = {p, r};

	/* special case r == 0 */
	if (!NILP(Fzerop(r))) {
		return ase_make_interval(p, p, 0, 0);
	}

	lo = Fent_binop_diff(countof(args), args);
	up = Fent_binop_sum(countof(args), args);

	return ase_make_interval(lo, up, 1, 1);
}

static Lisp_Object
__ase_make_neighbourhood_intr(Lisp_Object p, Lisp_Object r)
{
	Lisp_Object args[2] = {Qnil, r};
	int i, dim = XASE_CARTESIAN_DIMENSION(p);
	Lisp_Object *tmp = alloca_array(Lisp_Object, dim);
	Lisp_Object *pobjs = XASE_CARTESIAN_OBJECTS(p);

	/* special case r == 0 */
	if (!NILP(Fzerop(r))) {
		for (i = 0; i < dim; i++) {
			tmp[i] = ase_make_interval(pobjs[i], pobjs[i], 0, 0);
		}
		return ase_make_cartesian(dim, tmp, 1);
	}

	for (i = 0; i < dim; i++) {
		Lisp_Object lo, up;
		args[0] = pobjs[i];
		lo = Fent_binop_diff(countof(args), args);
		up = Fent_binop_sum(countof(args), args);
		tmp[i] = ase_make_interval(lo, up, 1, 1);
	}
	return ase_make_cartesian(dim, tmp, 1);
}

static ase_neighbourhood_t
_ase_make_neighbourhood(Lisp_Object p, Lisp_Object r, void *metric)
{
	ase_neighbourhood_t n = NULL;

	n = xnew(struct ase_neighbourhood_s);

	n->open_p = 1;
	n->point = p;
	n->radius = r;

	n->lebesgue_measure = Qnil;
	n->rational_measure = Qnil;
	n->colour = Qnil;

	/* if it's the supremum metric (atm it always is) we use our
	 * fancy interval implementation */
	if (COMPARABLEP(p))
		n->ldata = __ase_make_neighbourhood_intv(p, r);
	else if (ASE_CARTESIAN_INTERIOR_P(p))
		n->ldata = __ase_make_neighbourhood_intr(p, r);
	n->data = NULL;

	/* initialise the reference counter */
	ase_neighbourhood_init_refcnt(n);

	EMOD_ASE_DEBUG_NBH("n:%p (rc:0) shall be created...\n", n);
	return n;
}

inline Lisp_Object
ase_make_neighbourhood(Lisp_Object pt, Lisp_Object rad, Lisp_Object metric)
{
	ase_neighbourhood_t a = NULL;
	Lisp_Object result = Qnil;

	a = _ase_make_neighbourhood(pt, rad, NULL);
	XSETASE_NEIGHBOURHOOD(result, a);

	return result;
}

/* accessors */
inline Lisp_Object
ase_neighbourhood_point(ase_neighbourhood_t n)
{
	return n->point;
}

inline Lisp_Object
ase_neighbourhood_radius(ase_neighbourhood_t n)
{
	return n->radius;
}

/* Measures */
static inline void
_ase_neighbourhood_update_lebesgue(ase_neighbourhood_t n)
{
	if (n && NILP(n->lebesgue_measure)) {
		Lisp_Object i = n->ldata;
		n->lebesgue_measure = Fase_interval_lebesgue_measure(i);
	}
	return;
}

static inline Lisp_Object
_ase_neighbourhood_lebesgue(ase_neighbourhood_t n)
{
	return n->lebesgue_measure;
}

inline Lisp_Object
ase_neighbourhood_lebesgue_measure(ase_neighbourhood_t n)
{
	_ase_neighbourhood_update_lebesgue(n);
	return _ase_neighbourhood_lebesgue(n);
}

static inline void
_ase_neighbourhood_update_rational(ase_neighbourhood_t n)
{
	if (n && NILP(n->rational_measure)) {
		Lisp_Object i = n->ldata;
		n->rational_measure = Fase_interval_rational_measure(i);
	}
	return;
}

static inline Lisp_Object
_ase_neighbourhood_rational(ase_neighbourhood_t n)
{
	return n->rational_measure;
}

inline Lisp_Object
ase_neighbourhood_rational_measure(ase_neighbourhood_t n)
{
	_ase_neighbourhood_update_rational(n);
	return _ase_neighbourhood_rational(n);
}


/* lisp level */
DEFUN("ase-neighbourhoodp", Fase_neighbourhoodp, 1, 1, 0, /*
Return non-`nil' iff OBJECT is an ase neighbourhood.
						*/
      (object))
{
	if (ASE_NEIGHBOURHOODP(object))
		return Qt;

	return Qnil;
}

/* ###autoload */
DEFUN("ase-neighbourhood", Fase_neighbourhood, 2, 3, 0, /*
Return a neighbourhood around with POINT of radius RADIUS
with respect to METRIC (optional).

If no special metric is given, the supremum metric is used.
							*/
      (point, radius, metric))
{
	if (!COMPARABLEP(point) &&
	    !(ASE_CARTESIAN_INTERIOR_P(point))) {
		dead_wrong_type_argument(Qase_cartesian_interior_p, point);
	}
	CHECK_COMPARABLE(radius);

	if (NILP(Fnonnegativep(radius)))
		return wrong_type_argument(Qnonnegativep, radius);

	return ase_make_neighbourhood(point, radius, metric);
}

DEFUN("ase-neighbourhood-open-p", Fase_neighbourhood_open_p, 1, 1, 0, /*
Return non-`nil' iff NEIGHBOURHOOD is open with respect to its metric.
								      */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	if (ase_neighbourhood_open_p(XASE_NEIGHBOURHOOD(neighbourhood)))
		return Qt;

	return Qnil;
}

DEFUN("ase-neighbourhood-closed-p", Fase_neighbourhood_closed_p, 1, 1, 0, /*
Return non-`nil' iff NEIGHBOURHOOD is closed with respect to its metric.
									  */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	if (ase_neighbourhood_closed_p(XASE_NEIGHBOURHOOD(neighbourhood)))
		return Qt;

	return Qnil;
}

DEFUN("ase-neighbourhood-contains-p", Fase_neighbourhood_contains_p, 2, 2, 0, /*
Return non-`nil' iff NEIGHBOURHOOD contains OBJECT.
OBJECT may also be another neighbourhood under the restriction that
both neighbourhoods must be defined over the same metric space.
									      */
      (neighbourhood, object))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	if (COMPARABLEP(object)) {
		if (ase_neighbourhood_contains_obj_p(
			    XASE_NEIGHBOURHOOD(neighbourhood), object))
			return Qt;
	} else if (ASE_NEIGHBOURHOODP(object)) {
		if (ase_neighbourhood_contains_nbh_p(
			    XASE_NEIGHBOURHOOD(neighbourhood),
			    XASE_NEIGHBOURHOOD(object)))
			return Qt;
	}

	return Qnil;
}

DEFUN("ase-neighbourhood-equal-p", Fase_neighbourhood_equal_p, 2, 2, 0, /*
Return non-`nil' if N1 and N2 are equal in some sense, equality
hereby means that N1 and N2 contain each other.

In fact, this is just a convenience function and totally equivalent
to
  (and (ase-neighbourhood-contains-p n1 n2)
       (ase-neighbourhood-contains-p n2 n1))

Both neighbourhoods must be defined over the same metric space.
									*/
      (n1, n2))
{
	Lisp_Object n1in2, n2in1;

	CHECK_ASE_NEIGHBOURHOOD(n1);
	CHECK_ASE_NEIGHBOURHOOD(n2);

	n1in2 = Fase_neighbourhood_contains_p(n1, n2);
	n2in1 = Fase_neighbourhood_contains_p(n2, n1);

	if (!NILP(n1in2) && !NILP(n2in1))
		return Qt;

	return Qnil;
}

/* just for now until we can overload <, > and = */
DEFUN("ase-neighbourhood-<", Fase_neighbourhood_lssp, 2, 2, 0, /*
Return (< n1 n2).
								*/
      (n1, n2))
{
	int cmp;

	CHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(n1);
	CHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(n2);

	if (COMPARABLEP(n1) && ASE_NEIGHBOURHOODP(n2)) {
		cmp = ase_neighbourhood_greater_obj_p(
			XASE_NEIGHBOURHOOD(n2), n1);
	} else if (COMPARABLEP(n2) && ASE_NEIGHBOURHOODP(n1)) {
		cmp = ase_neighbourhood_less_obj_p(
			XASE_NEIGHBOURHOOD(n1), n2);
	} else if (ASE_NEIGHBOURHOODP(n1) && ASE_NEIGHBOURHOODP(n2)) {
		cmp = ase_neighbourhood_less_nbh_p(
			XASE_NEIGHBOURHOOD(n1), XASE_NEIGHBOURHOOD(n2));
	} else
		return _ase_less_p(n1, n2);

	if (cmp)
		return Qt;

	return Qnil;
}

DEFUN("ase-neighbourhood->", Fase_neighbourhood_gtrp, 2, 2, 0, /*
Return (> n1 n2).
								*/
      (n1, n2))
{
	int cmp;

	CHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(n1);
	CHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(n2);

	if (COMPARABLEP(n1) && ASE_NEIGHBOURHOODP(n2)) {
		cmp = ase_neighbourhood_less_obj_p(
			XASE_NEIGHBOURHOOD(n2), n1);
	} else if (COMPARABLEP(n2) && ASE_NEIGHBOURHOODP(n1)) {
		cmp = ase_neighbourhood_greater_obj_p(
			XASE_NEIGHBOURHOOD(n1), n2);
	} else if (ASE_NEIGHBOURHOODP(n1) && ASE_NEIGHBOURHOODP(n2)) {
		cmp = ase_neighbourhood_greater_nbh_p(
			XASE_NEIGHBOURHOOD(n1), XASE_NEIGHBOURHOOD(n2));
	} else
		return _ase_less_p(n2, n1);

	if (cmp)
		return Qt;

	return Qnil;
}

/* accessors */
DEFUN("ase-neighbourhood-point", Fase_neighbourhood_point, 1, 1, 0, /*
Return the point of NEIGHBOURHOOD which defined it.
								    */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	return ase_neighbourhood_point(XASE_NEIGHBOURHOOD(neighbourhood));
}

DEFUN("ase-neighbourhood-radius", Fase_neighbourhood_radius, 1, 1, 0, /*
Return the radius of NEIGHBOURHOOD which defined it.
								      */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	return ase_neighbourhood_radius(XASE_NEIGHBOURHOOD(neighbourhood));
}

/* Measures */
DEFUN("ase-neighbourhood-lebesgue-measure",
      Fase_neighbourhood_lebesgue_measure, 1, 1, 0, /*
Return the Lebesgue measure of NEIGHBOURHOOD.
						    */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	return ase_neighbourhood_lebesgue_measure(
		XASE_NEIGHBOURHOOD(neighbourhood));
}

DEFUN("ase-neighbourhood-rational-measure",
      Fase_neighbourhood_rational_measure, 1, 1, 0, /*
Return the number of rational integers in NEIGHBOURHOOD.
						    */
      (neighbourhood))
{
	CHECK_ASE_NEIGHBOURHOOD(neighbourhood);

	return ase_neighbourhood_rational_measure(
		XASE_NEIGHBOURHOOD(neighbourhood));
}


/* initialiser code */
void
EMOD_PUBINIT(void)
{
	/* constructors */
	DEFSUBR(Fase_neighbourhood);
	/* predicates */
	DEFSUBR(Fase_neighbourhoodp);
	DEFSUBR(Fase_neighbourhood_open_p);
	DEFSUBR(Fase_neighbourhood_closed_p);
	DEFSUBR(Fase_neighbourhood_contains_p);
	DEFSUBR(Fase_neighbourhood_equal_p);
	DEFSUBR(Fase_neighbourhood_lssp);
	DEFSUBR(Fase_neighbourhood_gtrp);
	/* accessors */
	DEFSUBR(Fase_neighbourhood_point);
	DEFSUBR(Fase_neighbourhood_radius);
	/* measures */
	DEFSUBR(Fase_neighbourhood_lebesgue_measure);
	DEFSUBR(Fase_neighbourhood_rational_measure);

	defsymbol(&Qase_neighbourhood, "ase:neighbourhood");
	defsymbol(&Qase_neighbourhoodp, "ase:neighbourhoodp");

	Fprovide(intern("ase-neighbourhood"));
}

void
EMOD_PUBREINIT(void)
{
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-neighbourhood"));
}

/* ase-neighbourhood ends here */
