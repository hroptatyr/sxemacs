/*
  ase-cartesian.h -- Cartesian (exterior) product of ASE objects
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

#ifndef INCLUDED_ase_cartesian_h_
#define INCLUDED_ase_cartesian_h_ 1

#include "ase.h"

#define EMOD_ASE_DEBUG_CART(args...)	EMOD_ASE_DEBUG("[CART]: " args)

typedef struct ase_cartesian_s *ase_cartesian_t;

extern Lisp_Object Qase_cartesian, Qase_cartesianp;
extern Lisp_Object Qase_cartesian_interior, Qase_cartesian_interior_p;
extern Lisp_Object Qembed_error, Qinterior_error;

extern void LTX_PUBINIT(ase_cartesian)(void);
extern void LTX_PUBREINIT(ase_cartesian)(void);
extern void LTX_PUBDEINIT(ase_cartesian)(void);

EXFUN(Ftype_of, 1);


struct ase_cartesian_s {
	struct ase_object_s obj;
	int dimension;
	Lisp_Object interior_type;
	Lisp_Object *objects;

	/* for measure freaks */
	Lisp_Object lebesgue_measure;
	Lisp_Object rational_measure;

	/* for friends of colour */
	Lisp_Object colour;

	/* just a ref counter for those nifty recycled items */
	struct sxe_refcounter_s refcnt;
};


#define ASE_CARTESIANP(_i)						\
	(DYNACATP(_i) &&						\
	 EQ(XDYNACAT(_i)->type, Qase_cartesian))
#define ASE_CARTESIAN_INTERIOR_P(_i)					\
	(DYNACATP(_i) &&						\
	 EQ(XDYNACAT(_i)->type, Qase_cartesian_interior))
#define ASE_UBERCARTESIANP(_i)						\
	(ASE_CARTESIANP(_i) || ASE_CARTESIAN_INTERIOR_P(_i))
#define CHECK_ASE_CARTESIAN(x)						\
	do {								\
		if (!ASE_CARTESIANP(x))					\
			dead_wrong_type_argument(			\
				Qase_cartesianp, x);			\
	} while (0)
#define CHECK_ASE_CARTESIAN_INTERIOR(x)					\
	do {								\
		if (!ASE_CARTESIAN_INTERIOR_P(x))			\
			dead_wrong_type_argument(			\
				Qase_cartesian_interior_p, x);		\
	} while (0)
#define CHECK_ASE_UBERCARTESIAN(x)					\
	do {								\
		if (!ASE_UBERCARTESIANP(x))				\
			dead_wrong_type_argument(			\
				Qase_cartesianp, x);			\
	} while (0)
#define CONCHECK_ASE_CARTESIAN(x)					\
	do {								\
		if (!ASE_CARTESIANP(x))					\
			x = wrong_type_argument(			\
				Qase_cartesianp, x);			\
	} while (0)
#define CONCHECK_ASE_CARTESIAN_INTERIOR(x)				\
	do {								\
		if (!ASE_CARTESIAN_INTERIOR_P(x))			\
			x = wrong_type_argument(			\
				Qase_cartesian_interior_p, x);		\
	} while (0)
#define CONCHECK_ASE_UBERCARTESIAN(x)					\
	do {								\
		if (!ASE_UBERCARTESIANP(x))				\
			x = wrong_type_argument(			\
				Qase_cartesianp, x);			\
	} while (0)
#define XSETASE_CARTESIAN(_res, _int)			\
	(_res) = _ase_wrap_cartesian((_int))
#define XSETASE_CARTESIAN_INTERIOR(_res, _int)		\
	(_res) = _ase_wrap_cartesian_interior((_int))
#define XASE_CARTESIAN(_x)		((ase_cartesian_t)get_dynacat(_x))
#define ase_cartesian_interior_type(_x)	((_x)->interior_type)
#define ase_cartesian_dimension(_x)	((_x)->dimension)
#define ase_cartesian_objects(_x)	((_x)->objects)
#define XASE_CARTESIAN_INTERIOR_TYPE(_x)		\
	(ase_cartesian_interior_type(XASE_CARTESIAN(_x)))
#define XASE_CARTESIAN_DIMENSION(_x)			\
	(ase_cartesian_dimension(XASE_CARTESIAN(_x)))
#define XASE_CARTESIAN_OBJECTS(_x)			\
	(ase_cartesian_objects(XASE_CARTESIAN(_x)))
#define XASE_CARTESIAN_FIRST_OBJECT(_x)			\
	(ase_cartesian_objects(XASE_CARTESIAN(_x))[0])

#define ASE_CARTESIAN_TRAVERSE(_c, _var, args...)		\
	do {							\
		int i;						\
		for (i = 0; i < c->dimension; i++) {		\
			Lisp_Object (_var) = _c->objects[i];	\
			args;					\
		}						\
	} while (0);

#define ase_cartesian_refcnt(_a)	(&((_a)->refcnt))
#define ase_cartesian_init_refcnt(_a)		\
	(sxe_refcounter_init(ase_cartesian_refcnt(_a)))
#define ase_cartesian_fini_refcnt(_a)		\
	(sxe_refcounter_finish(ase_cartesian_refcnt(_a)))
#define ase_cartesian_refval(_a)		\
	(sxe_refcounter_value(ase_cartesian_refcnt(_a)))
#define ase_cartesian_incref(_a)		\
	(sxe_refcounter_incref(ase_cartesian_refcnt(_a)))
#define ase_cartesian_decref(_a)		\
	(sxe_refcounter_decref(ase_cartesian_refcnt(_a)))
#define XASE_CARTESIAN_REFVAL(_a)		\
	(ase_interval_refval(XASE_CARTESIAN(_a)))
#define XASE_CARTESIAN_INCREF(_a)		\
	(ase_interval_incref(XASE_CARTESIAN(_a)))
#define XASE_CARTESIAN_DECREF(_a)		\
	(ase_interval_decref(XASE_CARTESIAN(_a)))


/* protos */
extern Lisp_Object ase_make_cartesian(int nargs, Lisp_Object *args, int interiorp);
extern Lisp_Object _ase_wrap_cartesian(ase_cartesian_t);
extern Lisp_Object _ase_wrap_cartesian_interior(ase_cartesian_t);
static inline int
ase_cartesian_pointwise_pred_p(ase_cartesian_t, int(*)(Lisp_Object));
static inline Lisp_Object
ase_cartesian_pointwise_erel_p(
	ase_cartesian_t, ase_cartesian_t, ase_element_relation_f);
static inline int
ase_cartesian_pointwise_rel_p(
	ase_cartesian_t, ase_cartesian_t, ase_relation_f);
static inline int
ase_cartesian_antipointwise_rel_p(
	ase_cartesian_t, ase_cartesian_t, ase_relation_f);
static inline ase_cartesian_t
	_ase_make_cartesian(int nargs, Lisp_Object*, int interiorp);
static inline Lisp_Object*
__ase_vectorise(int nargs, Lisp_Object *o);


/* for predicates this is a more intuitive function
 * it applies PREDFUN to every point in C and returns non-0 iff
 * all points met PREDFUN and 0 otherwise */
static inline int
ase_cartesian_pointwise_pred_p(ase_cartesian_t c, int(*predfun)(Lisp_Object))
{
	/* Apply PREDFUN to every point in C and returns non-0 iff
	 * all points met PREDFUN and 0 otherwise */
	int i;
	Lisp_Object *o = c->objects;

	for (i = 0; i < c->dimension; i++) {
		if (!predfun(o[i]))
			return 0;
	}
	return 1;
}

static inline Lisp_Object
ase_cartesian_pointwise_erel_p(
	ase_cartesian_t c1, ase_cartesian_t c2, ase_element_relation_f relf)
{
	/* Apply RELFUN pointwise to C1 and C2 and returns non-0 iff
	 * all points met RELF and 0 otherwise */
	int i;
	Lisp_Object *o1 = c1->objects;
	Lisp_Object *o2 = c2->objects;

	for (i = 0; i < c1->dimension && i < c2->dimension; i++) {
		if (NILP(relf(o1[i], o2[i])))
			return Qnil;
	}
	return Qt;
}

static inline int
ase_cartesian_pointwise_rel_p(
	ase_cartesian_t c1, ase_cartesian_t c2, ase_relation_f relf)
{
	/* Apply RELF pointwise to C1 and C2 and returns non-0 iff
	 * all points met RELF and 0 otherwise */
	int i;
	Lisp_Object *o1 = c1->objects;
	Lisp_Object *o2 = c2->objects;

	for (i = 0; i < c1->dimension && i < c2->dimension; i++) {
		if (!relf(o1[i], o2[i])) {
			return 0;
		}
	}
	return 1;
}

static inline int
ase_cartesian_antipointwise_rel_p(
	ase_cartesian_t c1, ase_cartesian_t c2, ase_relation_f relf)
{
	/* Apply RELF pointwise to C1 and C2 and returns non-0 iff
	 * at least one point met RELF and 0 otherwise */
	int i;
	Lisp_Object *o1 = c1->objects;
	Lisp_Object *o2 = c2->objects;

	for (i = 0; i < c1->dimension && i < c2->dimension; i++) {
		if (relf(o1[i], o2[i]))
			return 1;
	}
	return 0;
}


/* constructors */
static inline Lisp_Object*
__ase_vectorise(int nargs, Lisp_Object *o)
{
	Lisp_Object *result = NULL;
	int i;

	result = xnew_array(Lisp_Object, nargs);
	for (i = 0; i < nargs; i++)
		result[i] = o[i];

	return result;
}

static inline ase_cartesian_t
_ase_make_cartesian(int nargs, Lisp_Object *o, int interiorp)
{
	ase_cartesian_t n = NULL;

	n = xnew(struct ase_cartesian_s);

	n->dimension = nargs;
	n->lebesgue_measure = Qnil;
	n->rational_measure = Qnil;
	n->colour = Qnil;
	/* if we deal with an interior cartesian product, stick the type here */
	if (interiorp)
		n->interior_type = Ftype_of(o[0]);
	else
		n->interior_type = Qnil;

	n->objects = __ase_vectorise(nargs, o);

	ase_cartesian_init_refcnt(n);

	EMOD_ASE_DEBUG_CART("n:0x%08x (rc:0) shall be created...\n",
			    (unsigned int)n);
	return n;
}

/* predicates */

/* accessors */

/* errors */
DOESNT_RETURN ase_cartesian_embedding_error(Lisp_Object, Lisp_Object);

#endif	/* INCLUDED_ase_cartesian_h_ */
