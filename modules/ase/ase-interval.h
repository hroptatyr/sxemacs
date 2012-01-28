/*
  ase-interval.h -- Interval Sorcery
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

#ifndef INCLUDED_ase_interval_h_
#define INCLUDED_ase_interval_h_ 1

#include "ase.h"
#include "ase-cartesian.h"

extern const ase_category_t ase_interval_cat;
typedef struct ase_interval_s *ase_interval_t;
typedef struct ase_interval_union_s *ase_interval_union_t;
typedef struct ase_interval_union_item_s *ase_interval_union_item_t;

#define EMOD_ASE_DEBUG_INTV(args...)	EMOD_ASE_DEBUG("[INTV]: " args)
#define EMOD_ASE_DEBUG_INTVU(args...)	EMOD_ASE_DEBUG("[INTVU]: " args)

extern Lisp_Object Q_open, Q_closed, Q_less, Q_greater, Q_eql, Q_unknown;
extern Lisp_Object Q_disjoint, Q_connected;
extern Lisp_Object Qase_interval, Qase_intervalp;
extern Lisp_Object Qase_interval_union, Qase_interval_union_p;
extern Lisp_Object Qase_empty_interval, Qase_universe_interval;

extern void LTX_PUBINIT(ase_interval)(void);
extern void LTX_PUBREINIT(ase_interval)(void);
extern void LTX_PUBDEINIT(ase_interval)(void);


struct ase_interval_s {
	struct ase_object_s obj;
	int lower_open_p;
	int upper_open_p;
	int lower_eq_upper_p;
	Lisp_Object lower;
	Lisp_Object upper;
	Lisp_Object lebesgue_measure;
	Lisp_Object rational_measure;
	Lisp_Object colour;
	/* just a ref counter for those nifty recycled items */
	struct sxe_refcounter_s refcnt;
};

struct ase_interval_union_item_s {
	Lisp_Object current;
	ase_interval_union_item_t next;
};

struct ase_interval_union_s {
	ase_interval_union_item_t union_ser;
	Lisp_Object lebesgue_measure;
	Lisp_Object rational_measure;
	Lisp_Object colour;
	int no_intv;
	/* just a ref counter for those nifty recycled items */
	struct sxe_refcounter_s refcnt;
};


#define ASE_INTERVALP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qase_interval))
#define CHECK_ASE_INTERVAL(x)						\
	do {								\
		if (!ASE_INTERVALP(x))					\
			dead_wrong_type_argument(Qase_intervalp, x);	\
	} while (0)
#define CONCHECK_ASE_INTERVAL(x)					\
	do {								\
		if (!ASE_INTERVALP(x))					\
			x = wrong_type_argument(Qase_intervalp, x);	\
	} while (0)
extern Lisp_Object _ase_wrap_interval(ase_interval_t);
#define XSETASE_INTERVAL(_res, _int)	(_res) = _ase_wrap_interval((_int))
#define ase_interval_empty_p(_x)	((_x) == NULL)
#define XASE_INTERVAL(_x)		((ase_interval_t)get_dynacat(_x))
#define ASE_INTERVAL_EMPTY_P(_x)	ase_interval_empty_p(XASE_INTERVAL(_x))

#define ase_interval_refcnt(_a)		(&((_a)->refcnt))
#define ase_interval_init_refcnt(_a)		\
	(sxe_refcounter_init(ase_interval_refcnt(_a)))
#define ase_interval_fini_refcnt(_a)		\
	(sxe_refcounter_finish(ase_interval_refcnt(_a)))
#define ase_interval_refval(_a)			\
	(sxe_refcounter_value(ase_interval_refcnt(_a)))
#define ase_interval_incref(_a)			\
	(sxe_refcounter_incref(ase_interval_refcnt(_a)))
#define ase_interval_decref(_a)			\
	(sxe_refcounter_decref(ase_interval_refcnt(_a)))
#define XASE_INTERVAL_REFVAL(_a)		\
	(ase_interval_refval(XASE_INTERVAL(_a)))
#define XASE_INTERVAL_INCREF(_a)		\
	(ase_interval_incref(XASE_INTERVAL(_a)))
#define XASE_INTERVAL_DECREF(_a)		\
	(ase_interval_decref(XASE_INTERVAL(_a)))

#define ASE_INTERVAL_UNION_P(_i)					\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qase_interval_union))
#define CHECK_ASE_INTERVAL_UNION(x)					\
	do {								\
		if (!ASE_INTERVAL_UNION_P(x))				\
			dead_wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)
#define CONCHECK_ASE_INTERVAL_UNION(x)					\
	do {								\
		if (!ASE_INTERVAL_UNION_P(x))				\
			x = wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)
extern Lisp_Object _ase_wrap_interval_union(ase_interval_union_t);
#define XSETASE_INTERVAL_UNION(_res, _intu)		\
	(_res) = _ase_wrap_interval_union((_intu))

#define ase_interval_union(_x)		((_x)->union_ser)
#define XASE_INTERVAL_UNION(_x)		((ase_interval_union_t)get_dynacat(_x))
#define XASE_INTERVAL_UNION_SER(_x)		\
	(ase_interval_union(XASE_INTERVAL_UNION(_x)))
#define XASE_INTERVAL_UNION_FIRST(_x)			\
	(XASE_INTERVAL_UNION_SER(_x)->current)

#define ase_interval_union_refcnt(_a)	(&((_a)->refcnt))
#define ase_interval_union_init_refcnt(_a)		\
	(sxe_refcounter_init(ase_interval_union_refcnt(_a)))
#define ase_interval_union_fini_refcnt(_a)		\
	(sxe_refcounter_finish(ase_interval_union_refcnt(_a)))
#define ase_interval_union_refval(_a)		\
	(sxe_refcounter_value(ase_interval_union_refcnt(_a)))
#define ase_interval_union_incref(_a)		\
	(sxe_refcounter_incref(ase_interval_union_refcnt(_a)))
#define ase_interval_union_decref(_a)		\
	(sxe_refcounter_decref(ase_interval_union_refcnt(_a)))
#define XASE_INTERVAL_UNION_REFVAL(_a)		\
	(ase_interval_union_refval(XASE_INTERVAL_UNION(_a)))
#define XASE_INTERVAL_UNION_INCREF(_a)		\
	(ase_interval_union_incref(XASE_INTERVAL_UNION(_a)))
#define XASE_INTERVAL_UNION_DECREF(_a)		\
	(ase_interval_union_decref(XASE_INTERVAL_UNION(_a)))

#define ASE_INTERVAL_OR_UNION_P(_i)					\
	(ASE_INTERVALP(_i) || ASE_INTERVAL_UNION_P(_i))
#define CHECK_ASE_INTERVAL_OR_UNION(x)					\
	do {								\
		if (!ASE_INTERVAL_OR_UNION_P(x))			\
			dead_wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)
#define CONCHECK_ASE_INTERVAL_OR_UNION(x)				\
	do {								\
		if (!ASE_INTERVAL_OR_UNION_P(x))			\
			x = wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)

#define ASE_INTERVAL_INTERIOR_P(_i)					\
	(ASE_CARTESIAN_INTERIOR_P(_i) &&				\
	 EQ(XASE_CARTESIAN_INTERIOR_TYPE(_i), Qase_interval))
#define CHECK_ASE_INTERVAL_INTERIOR(x)					\
	do {								\
		if (!ASE_INTERVAL_INTERIOR_P(x))			\
			dead_wrong_type_argument(			\
				Qase_cartesian_interior_p, x);		\
	} while (0)
#define CONCHECK_ASE_INTERVAL_INTERIOR_P(x)				\
	do {								\
		if (!ASE_INTERVAL_INTERIOR_P(x))			\
			x = wrong_type_argument(			\
				Qase_cartesian_interior_p, x);		\
	} while (0)

#define ASE_UBERINTERVALP(_i)						\
	(ASE_INTERVALP(_i) || ASE_INTERVAL_UNION_P(_i) ||		\
	 ASE_INTERVAL_INTERIOR_P(_i))
#define CHECK_ASE_UBERINTERVAL(x)					\
	do {								\
		if (!ASE_UBERINTERVALP(x))				\
			dead_wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)
#define CONCHECK_ASE_UBERINTERVAL(x)					\
	do {								\
		if (!ASE_UBERINTERVALP(x))				\
			x = wrong_type_argument(			\
				Qase_interval_union_p, x);		\
	} while (0)


/* constructors */
extern ase_interval_t
_ase_make_interval(Lisp_Object, Lisp_Object, int, int);
extern Lisp_Object
ase_make_interval(Lisp_Object lower, Lisp_Object upper, int, int);
extern Lisp_Object ase_copy_interval(Lisp_Object intv);
extern Lisp_Object ase_empty_interval(void);
extern Lisp_Object ase_universe_interval(void);
extern Lisp_Object ase_empty_interval_union(void);
extern Lisp_Object ase_interval_boundary(Lisp_Object intv);
extern Lisp_Object ase_interval_interior_boundary(Lisp_Object intv_intr_prod);
extern Lisp_Object ase_interval_union_boundary(Lisp_Object intv_union);
extern Lisp_Object ase_interval_closure(Lisp_Object intv);
extern Lisp_Object ase_interval_interior_closure(Lisp_Object intv_intr_prod);
extern Lisp_Object ase_interval_union_closure(Lisp_Object intv_union);
extern Lisp_Object ase_interval_interior(Lisp_Object intv);
extern Lisp_Object ase_interval_interior_interior(Lisp_Object intv_intr_prod);
extern Lisp_Object ase_interval_union_interior(Lisp_Object intv_union);

/* predicates */
extern bool
_ase_interval_contains_obj_p(ase_interval_t, Lisp_Object);
extern Lisp_Object
ase_interval_contains_obj_p(Lisp_Object i, Lisp_Object obj);
extern int
_ase_interval_contains_intv_p(ase_interval_t, ase_interval_t);
extern Lisp_Object
ase_interval_contains_intv_p(Lisp_Object i1, Lisp_Object i2);
extern Lisp_Object
ase_interval_contains_union_p(Lisp_Object i, Lisp_Object u);
extern Lisp_Object
ase_interval_union_contains_obj_p(Lisp_Object iu, Lisp_Object obj);
extern Lisp_Object
ase_interval_union_contains_intv_p(Lisp_Object, Lisp_Object);
extern Lisp_Object
ase_interval_union_contains_intr_p(Lisp_Object, Lisp_Object);
extern Lisp_Object
ase_interval_union_contains_union_p(Lisp_Object, Lisp_Object);
extern Lisp_Object
ase_interval_interior_contains_obj_p(Lisp_Object, Lisp_Object);
extern Lisp_Object
ase_interval_interior_contains_intr_p(Lisp_Object, Lisp_Object);
extern Lisp_Object
ase_interval_interior_contains_union_p(Lisp_Object, Lisp_Object);
extern int
ase_interval_connected_p(Lisp_Object i1, Lisp_Object i2);
extern int
ase_interval_connected_union_p(Lisp_Object i, Lisp_Object iu);
extern int
ase_interval_union_connected_intv_p(Lisp_Object iu, Lisp_Object i);
extern int
ase_interval_union_connected_intr_p(Lisp_Object iu, Lisp_Object iip);
extern int
ase_interval_union_connected_p(Lisp_Object iu1, Lisp_Object iu2);
extern int
ase_interval_interior_connected_union_p(Lisp_Object iip, Lisp_Object iu);
extern int
ase_interval_interior_connected_p(Lisp_Object iip1, Lisp_Object iip2);
extern int
ase_interval_disjoint_p(Lisp_Object i1, Lisp_Object i2);
extern int
ase_interval_disjoint_union_p(Lisp_Object i, Lisp_Object iu);
extern int
ase_interval_union_disjoint_intv_p(Lisp_Object iu, Lisp_Object i);
extern int
ase_interval_union_disjoint_intr_p(Lisp_Object iu, Lisp_Object iip);
extern int
ase_interval_union_disjoint_p(Lisp_Object iu1, Lisp_Object iu2);
extern int
ase_interval_interior_disjoint_union_p(Lisp_Object iip, Lisp_Object iu);
extern int
ase_interval_interior_disjoint_p(Lisp_Object iip1, Lisp_Object iip2);
extern int
ase_interval_open_p(Lisp_Object intv);
extern int
ase_interval_closed_p(Lisp_Object intv);
extern int
ase_interval_union_open_p(Lisp_Object intv_union);
extern int
ase_interval_union_closed_p(Lisp_Object intv_union);
extern int
ase_interval_interior_open_p(Lisp_Object);
extern int
ase_interval_interior_closed_p(Lisp_Object);
EXFUN(Fase_interval_contains_p, 2);
EXFUN(Fase_interval_connected_p, MANY);
EXFUN(Fase_interval_disjoint_p, MANY);
EXFUN(Fase_interval_equal_p, 2);

/* measures */
extern Lisp_Object
ase_interval_lebesgue_measure(ase_interval_t);
extern Lisp_Object
ase_interval_rational_measure(ase_interval_t);
extern Lisp_Object
ase_interval_union_lebesgue_measure(ase_interval_union_t);
extern Lisp_Object
ase_interval_union_rational_measure(ase_interval_union_t);
extern Lisp_Object
ase_interval_interior_lebesgue_measure(ase_cartesian_t);
extern Lisp_Object
ase_interval_interior_rational_measure(ase_cartesian_t);
EXFUN(Fase_interval_lebesgue_measure, 1);
EXFUN(Fase_interval_rational_measure, 1);

void ase_unite_interval(ase_interval_union_t, ase_interval_union_t);

#endif	/* INCLUDED_ase_interval_h_ */
