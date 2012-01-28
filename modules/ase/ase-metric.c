/*
  ase-metric.c -- Metrical Spaces and Distances
  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

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
#include "ase.h"
#include "ase-metric.h"
#include "ase-cartesian.h"
#include <bytecode.h>

PROVIDE(ase_metric);
REQUIRE(ase_metric, "ase", "ase-cartesian");

Lisp_Object Qase_metric, Qase_metricp;
Lisp_Object Qase_euclidean_metric, Qase_euclidean_square_metric;
Lisp_Object Qase_supremum_metric, Qase_trivial_metric;
Lisp_Object Qase_pmetric;
Lisp_Object Qmetric_distance_error;


/* stuff for the dynacat */
static inline void
_ase_metric_prnt(ase_metric_t n, Lisp_Object pcf)
{
	return;
}

static void
ase_metric_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_METR("m:0x%08x@0x%08x (rc:%d)\n",
			    (unsigned int)(XASE_METRIC(obj)),
			    (unsigned int)obj, 1);
	write_c_string("#<", pcf);
	print_internal(XDYNACAT_TYPE(obj), pcf, unused);
	{
		if (NILP(XASE_METRIC_LDIST(obj))) {
			write_hex_ptr(XASE_METRIC_DIST(obj),pcf);
		} else {
			Lisp_Object ldist = XASE_METRIC_LDIST(obj);
			if (SYMBOLP(ldist)) {
				Lisp_String *name =
					symbol_name(XSYMBOL(ldist));
				write_fmt_string(pcf, " #'%s", string_data(name));
			} else if (SUBRP(ldist)) {
				const char *name = subr_name(XSUBR(ldist));
				write_fmt_string(pcf, " #'%s", name);
			} else {
				write_c_string(" #'(lambda ...)", pcf);
			}
		}
	}
	write_c_string(">", pcf);
	return;
}

static void
ase_metric_fini(Lisp_Object obj, int unused)
{
	ase_metric_t m = XASE_METRIC(obj);

	if (ase_metric_data(m)) {
		xfree(ase_metric_data(m));
		ase_metric_data(m) = NULL;
	}

	EMOD_ASE_DEBUG_GC("m:0x%08x@0x%08x (rc:%d) shall be freed...\n",
			  (unsigned int)(m), (unsigned int)obj, 1);

	return;
}

static inline void
_ase_metric_mark(ase_metric_t n)
{
	if (n == NULL)
		return;

	mark_object(n->ldist);
	mark_object(n->colour);
	return;
}

static void
ase_metric_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_METR("m:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			    (unsigned int)(XASE_METRIC(obj)),
			    (unsigned int)obj, 1);
	_ase_metric_mark(XASE_METRIC(obj));
	return;
}


inline Lisp_Object
_ase_wrap_metric(ase_metric_t m)
{
	Lisp_Object result;

	result = make_dynacat(m);
	XDYNACAT_TYPE(result) = Qase_metric;

	set_dynacat_printer(result, ase_metric_prnt);
	set_dynacat_marker(result, ase_metric_mark);
	set_dynacat_finaliser(result, ase_metric_fini);
	set_dynacat_intprinter(result, NULL);

	EMOD_ASE_DEBUG_METR("m:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			    (unsigned int)m, 1, (unsigned int)result);

	return result;
}

static inline ase_metric_t
_ase_make_metric(ase_distance_f fn, void *data, Lisp_Object lambda)
{
	ase_metric_t m = NULL;

	m = xnew(struct ase_metric_s);

	ase_metric_dist(m) = fn;
	ase_metric_ldist(m) = lambda;
	m->colour = Qnil;
	ase_metric_data(m) = data;

	EMOD_ASE_DEBUG_METR("m:0x%08x (rc:0) shall be created...\n",
			    (unsigned int)m);
	return m;
}

Lisp_Object ase_make_metric(ase_distance_f fn, void *data, Lisp_Object lambda)
{
	ase_metric_t m = NULL;
	Lisp_Object result = Qnil;

	m = _ase_make_metric(fn, data, lambda);
	XSETASE_METRIC(result, m);

	return result;
}


/* some of the more common metrics */
static inline Lisp_Object
_ase_metric_euclidean_1dim_sq(Lisp_Object a, Lisp_Object b)
{
	Lisp_Object tmp = ent_binop(ASE_BINARY_OP_DIFF, a, b);
	return ent_binop(ASE_BINARY_OP_PROD, tmp, tmp);
}

static Lisp_Object
_ase_metric_euclidean_ndim_sq(Lisp_Object a, Lisp_Object b)
{
	int i, dim = XASE_CARTESIAN_DIMENSION(a);
	Lisp_Object tmp[dim];
	Lisp_Object *aos = XASE_CARTESIAN_OBJECTS(a);
	Lisp_Object *bos = XASE_CARTESIAN_OBJECTS(b);

	for (i = 0; i < dim; i++) {
		tmp[i] = _ase_metric_euclidean_1dim_sq(aos[i], bos[i]);
	}
	return Fent_binop_sum(dim, tmp);
}

static inline Lisp_Object
_ase_metric_euclidean_1dim_fast(Lisp_Object a, Lisp_Object b)
{
	return Fabs(ent_binop(ASE_BINARY_OP_DIFF, a, b));
}

static inline Lisp_Object
_ase_metric_euclidean_1dim(Lisp_Object a, Lisp_Object b)
{
	return Fsqrt(_ase_metric_euclidean_1dim_sq(a, b), Qnil);
}

static inline Lisp_Object
_ase_metric_euclidean_ndim(Lisp_Object a, Lisp_Object b)
{
	return Fsqrt(_ase_metric_euclidean_ndim_sq(a, b), Qnil);
}

static Lisp_Object
ase_metric_euclidean(void *unused, Lisp_Object a, Lisp_Object b)
{
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_euclidean_1dim(a, b);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_euclidean_ndim(a, b);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}

static Lisp_Object
ase_metric_euclidean_sq(void *unused, Lisp_Object a, Lisp_Object b)
{
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_euclidean_1dim_sq(a, b);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_euclidean_ndim_sq(a, b);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}

static inline Lisp_Object
_ase_metric_supremum_ndim(Lisp_Object a, Lisp_Object b)
{
	int i, dim = XASE_CARTESIAN_DIMENSION(a);
	Lisp_Object sup = Qzero;
	Lisp_Object *aos = XASE_CARTESIAN_OBJECTS(a);
	Lisp_Object *bos = XASE_CARTESIAN_OBJECTS(b);

	for (i = 0; i < dim; i++) {
		Lisp_Object tmp =
			_ase_metric_euclidean_1dim_fast(aos[i], bos[i]);
		if (ent_binrel(ASE_BINARY_REL_LESSP, sup, tmp)) {
			sup = tmp;
		}
	}
	return sup;
}

static Lisp_Object
ase_metric_supremum(void *unused, Lisp_Object a, Lisp_Object b)
{
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_euclidean_1dim_fast(a, b);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_supremum_ndim(a, b);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}

static inline Lisp_Object
_ase_metric_trivial_1dim(Lisp_Object a, Lisp_Object b)
{
	if (!ent_binrel(ASE_BINARY_REL_EQUALP, a, b))
		return make_int(1);
	else
		return Qzero;
}

static inline Lisp_Object
_ase_metric_trivial_ndim(Lisp_Object a, Lisp_Object b)
{
	int i, dim = XASE_CARTESIAN_DIMENSION(a);
	Lisp_Object *aos = XASE_CARTESIAN_OBJECTS(a);
	Lisp_Object *bos = XASE_CARTESIAN_OBJECTS(b);

	for (i = 0; i < dim; i++) {
		if (XINT(_ase_metric_trivial_1dim(aos[i], bos[i])) == 1)
			return make_int(1);
	}
	return Qzero;
}

static Lisp_Object
ase_metric_trivial(void *unused, Lisp_Object a, Lisp_Object b)
{
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_trivial_1dim(a, b);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_trivial_ndim(a, b);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}

static inline Lisp_Object
_ase_metric_p_1dim_p(Lisp_Object a, Lisp_Object b, unsigned int p)
{
	Lisp_Object tmp = ent_binop(ASE_BINARY_OP_DIFF, a, b);
	Lisp_Object result = ent_binop(ASE_BINARY_OP_POW, tmp, make_int(p));
	if ((p & 1) == 0)
		return result;
	else
		return Fabs(result);
}

static Lisp_Object
_ase_metric_p_ndim_p(Lisp_Object a, Lisp_Object b, unsigned int p)
{
	int i, dim = XASE_CARTESIAN_DIMENSION(a);
	Lisp_Object tmp[dim];
	Lisp_Object *aos = XASE_CARTESIAN_OBJECTS(a);
	Lisp_Object *bos = XASE_CARTESIAN_OBJECTS(b);

	for (i = 0; i < dim; i++) {
		tmp[i] = _ase_metric_p_1dim_p(aos[i], bos[i], p);
	}
	return Fent_binop_sum(dim, tmp);
}

#ifdef HAVE_MPFR
static inline Lisp_Object
_ase_metric_p_1dim(Lisp_Object a, Lisp_Object b, unsigned int p)
{
	return Froot(_ase_metric_p_1dim_p(a, b, p), make_int(p), Qnil);
}

static inline Lisp_Object
_ase_metric_p_ndim(Lisp_Object a, Lisp_Object b, unsigned int p)
{
	return Froot(_ase_metric_p_ndim_p(a, b, p), make_int(p), Qnil);
}

static Lisp_Object
ase_metric_p(void *data, Lisp_Object a, Lisp_Object b)
{
	unsigned int p = ((ase_pmetric_data_t)data)->p;
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_p_1dim(a, b, p);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_p_ndim(a, b, p);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}
#endif

static Lisp_Object
ase_metric_p_p(void *data, Lisp_Object a, Lisp_Object b)
{
	unsigned int p = ((ase_pmetric_data_t)data)->p;
	if (COMPARABLEP(a) && COMPARABLEP(b)) {
		return _ase_metric_p_1dim_p(a, b, p);
	} else if (ASE_CARTESIAN_INTERIOR_P(a) &&
		   ASE_CARTESIAN_INTERIOR_P(b) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(a)) &&
		   COMPARABLEP(XASE_CARTESIAN_FIRST_OBJECT(b)) &&
		   XASE_CARTESIAN_DIMENSION(a) ==
		   XASE_CARTESIAN_DIMENSION(b)) {
		return _ase_metric_p_ndim_p(a, b, p);
	}
	ase_cartesian_embedding_error(a, b);
	return Qzero;
}


/* ###autoload */
DEFUN("ase-p-metric", Fase_p_metric, 1, 1, 0, /*
Return a p-metric for some natural number P.
*/
      (p))
#ifndef HAVE_MPFR
{
	error("MPFR not available which is mandatory for p-metrics");
	return Qnull_pointer;
}
#else
{
	ase_pmetric_data_t data;
	CHECK_NATNUM(p);

	data = xnew(struct ase_pmetric_data_s);
	data->p = XUINT(p);
	return ase_make_metric(ase_metric_p, data, Qnil);
}
#endif

/* ###autoload */
DEFUN("ase-p-metric*", Fase_p_metricX, 1, 1, 0, /*
Return a p-metric without the final root for some natural number P.
*/
      (p))
{
	ase_pmetric_data_t data;
	CHECK_NATNUM(p);

	data = xnew(struct ase_pmetric_data_s);
	data->p = XUINT(p);
	return ase_make_metric(ase_metric_p_p, data, Qnil);
}

/* ###autoload */
DEFUN("ase-metric", Fase_metric, 1, 1, 0, /*
Return a metric from a distance function FN.

FN should take two arguments and return the distance between those,
a distance by definition lives in the reals.
*/
      (fn))
{
	if (!SUBRP(fn) && !SYMBOLP(fn) &&
	    !COMPILED_FUNCTIONP(fn) &&
	    !(CONSP(fn) && EQ(XCAR(fn), Qlambda))) {
		signal_invalid_function_error(fn);
		return Qnil;
	}

	return ase_make_metric(NULL, NULL, fn);
}

DEFUN("ase-metric-distance", Fase_metric_distance, 3, 3, 0, /*
Return the distance of P1 and P2 with respect to METRIC.
*/
      (metric, p1, p2))
{
	ase_distance_f dist;
	Lisp_Object ldist;
	CHECK_ASE_METRIC(metric);

	if ((dist = XASE_METRIC_DIST(metric))) {
		void *data = XASE_METRIC_DATA(metric);
		return dist(data, p1, p2);
	} else if (!NILP((ldist = XASE_METRIC_LDIST(metric)))) {
		/* This portion can GC */
		Lisp_Object args[3] = {ldist, p1, p2};
		Lisp_Object res = Qnil;
		struct gcpro ngcpro1, ngcpro2;
		NGCPRO1n(res, args, countof(args));
		res = Ffuncall(countof(args), args);
		NUNGCPRO;
		if (!NILP(Fnonnegativep(res))) {
			return res;
		} else {
			signal_error(Qmetric_distance_error, list1(ldist));
			return Qnil;
		}
	}

	dead_wrong_type_argument(Qase_metricp, metric);
	return Qnil;
}


/* initialiser code */
#define EMODNAME	ase_metric

void
EMOD_PUBINIT(void)
{
	DEFSUBR(Fase_p_metric);
	DEFSUBR(Fase_p_metricX);
	DEFSUBR(Fase_metric);
	DEFSUBR(Fase_metric_distance);

	defsymbol(&Qase_metric, "ase:metric");
	defsymbol(&Qase_metricp, "ase:metricp");

	DEFERROR(Qmetric_distance_error,
		 "Distance function must have non-negative image",
		 Qdomain_error);

	DEFVAR_CONST_LISP("ase-euclidean-metric", &Qase_euclidean_metric /*
									  */);
	DEFVAR_CONST_LISP("ase-euclidean-square-metric",
			  &Qase_euclidean_square_metric /*
							 */);
	DEFVAR_CONST_LISP("ase-supremum-metric", &Qase_supremum_metric /*
									*/);
	DEFVAR_CONST_LISP("ase-trivial-metric", &Qase_trivial_metric /*
								      */);

	EMOD_PUBREINIT();
	Fprovide(intern("ase-metric"));
}

void
EMOD_PUBREINIT(void)
{
	Qase_euclidean_metric =
		ase_make_metric(ase_metric_euclidean, NULL, Qnil);
	Qase_euclidean_square_metric =
		ase_make_metric(ase_metric_euclidean_sq, NULL, Qnil);
	Qase_supremum_metric =
		ase_make_metric(ase_metric_supremum, NULL, Qnil);
	Qase_trivial_metric =
		ase_make_metric(ase_metric_trivial, NULL, Qnil);
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-metric"));
}

/* ase-metric ends here */
