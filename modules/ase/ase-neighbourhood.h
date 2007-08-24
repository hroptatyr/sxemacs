/*
  ase-neighbourhood.h -- Neighbourhood of ASE objects
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

#ifndef INCLUDED_ase_neighbourhood_h_
#define INCLUDED_ase_neighbourhood_h_ 1

#include "ase.h"
#include "ase-interval.h"

EMOD_ANNOUNCE(ase_neighbourhood);
typedef struct ase_neighbourhood_s *ase_neighbourhood_t;

extern Lisp_Object Qase_neighbourhood, Qase_neighbourhoodp;
extern Lisp_Object Qase_empty_neighbourhood;

void reinit_vars_of_ase_neighbourhood(void);
void vars_of_ase_neighbourhood(void);
void syms_of_ase_neighbourhood(void);


struct ase_neighbourhood_s {
#if 0
	ase_metric_t *metric;
#endif
	int open_p;
	Lisp_Object point;
	Lisp_Object radius;

	/* for metric-dependent implementations */
	void *data;
	Lisp_Object ldata;

	Lisp_Object lebesgue_measure;
	Lisp_Object rational_measure;
	Lisp_Object colour;
	/* just a ref counter for those nifty recycled items */
	struct sxe_refcounter_s refcnt;
};


#define ASE_NEIGHBOURHOODP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_neighbourhood))
#define CHECK_ASE_NEIGHBOURHOOD(x)					\
	do {								\
		if (!ASE_NEIGHBOURHOODP(x))				\
			dead_wrong_type_argument(Qase_neighbourhoodp, x); \
	} while (0)
#define CONCHECK_ASE_NEIGHBOURHOOD(x)					\
	do {								\
		if (!ASE_NEIGHBOURHOODP(x))				\
			x = wrong_type_argument(Qase_neighbourhoodp, x); \
	} while (0)
inline Lisp_Object _ase_wrap_neighbourhood(ase_neighbourhood_t);
#define XSETASE_NEIGHBOURHOOD(_res, _int)	\
	(_res) = _ase_wrap_neighbourhood((_int))
#define XASE_NEIGHBOURHOOD(_x)			\
	((ase_neighbourhood_t)get_dynacat(_x))

#define ase_neighbourhood_refcnt(_a)	(&((_a)->refcnt))
#define ase_neighbourhood_init_refcnt(_a)	\
	(sxe_refcounter_init(ase_neighbourhood_refcnt(_a)))
#define ase_neighbourhood_fini_refcnt(_a)	\
	(sxe_refcounter_finish(ase_neighbourhood_refcnt(_a)))
#define ase_neighbourhood_refval(_a)		\
	(sxe_refcounter_value(ase_neighbourhood_refcnt(_a)))
#define ase_neighbourhood_incref(_a)		\
	(sxe_refcounter_incref(ase_neighbourhood_refcnt(_a)))
#define ase_neighbourhood_decref(_a)		\
	(sxe_refcounter_decref(ase_neighbourhood_refcnt(_a)))
#define XASE_NEIGHBOURHOOD_REFVAL(_a)			\
	(ase_neighbourhood_refval(XASE_NEIGHBOURHOOD(_a)))
#define XASE_NEIGHBOURHOOD_INCREF(_a)			\
	(ase_neighbourhood_incref(XASE_NEIGHBOURHOOD(_a)))
#define XASE_NEIGHBOURHOOD_DECREF(_a)			\
	(ase_neighbourhood_decref(XASE_NEIGHBOURHOOD(_a)))

#define ASE_NEIGHBOURHOOD_OR_COMPARABLE_P(_i)				\
	(COMPARABLEP(_i) || ASE_NEIGHBOURHOODP(_i))
#define CHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(x)			\
	do {								\
		if (!ASE_NEIGHBOURHOOD_OR_COMPARABLE_P(x))		\
			dead_wrong_type_argument(Qcomparablep, x);	\
	} while (0)
#define CONCHECK_ASE_NEIGHBOURHOOD_OR_COMPARABLE(x)			\
	do {								\
		if (!ASE_NEIGHBOURHOOD_OR_COMPARABLE_P(x))		\
			x = wrong_type_argument(Qcomparable, x);	\
	} while (0)


/* constructors */
extern inline Lisp_Object
ase_make_neighbourhood(Lisp_Object pt, Lisp_Object rad, Lisp_Object metric);
Lisp_Object ase_copy_neighbourhood(Lisp_Object nbh);
Lisp_Object ase_neighbourhood_boundary(Lisp_Object nbh);
Lisp_Object ase_neighbourhood_closure(Lisp_Object nbh);
Lisp_Object ase_neighbourhood_interior(Lisp_Object nbh);

/* predicates */
extern inline int
ase_neighbourhood_contains_obj_p(ase_neighbourhood_t, Lisp_Object);
extern inline int
ase_neighbourhood_contains_nbh_p(ase_neighbourhood_t, ase_neighbourhood_t);
extern inline int
ase_neighbourhood_open_p(ase_neighbourhood_t);
extern inline int
ase_neighbourhood_closed_p(ase_neighbourhood_t);
extern inline int
ase_neighbourhood_less_obj_p(ase_neighbourhood_t, Lisp_Object);
extern inline int
ase_neighbourhood_less_nbh_p(ase_neighbourhood_t, ase_neighbourhood_t);
extern inline int
ase_neighbourhood_greater_obj_p(ase_neighbourhood_t, Lisp_Object);
extern inline int
ase_neighbourhood_greater_nbh_p(ase_neighbourhood_t, ase_neighbourhood_t);

extern inline Lisp_Object
ase_neighbourhood_point(ase_neighbourhood_t);
extern inline Lisp_Object
ase_neighbourhood_radius(ase_neighbourhood_t);

/* measures */
extern inline Lisp_Object
ase_neighbourhood_lebesgue_measure(ase_neighbourhood_t);
extern inline Lisp_Object
ase_neighbourhood_rational_measure(ase_neighbourhood_t);

#endif
