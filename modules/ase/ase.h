/*
  ase.h -- Fancifying ENT a little
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

#ifndef INCLUDED_ase_h_
#define INCLUDED_ase_h_ 1

#include "semaphore.h"
#include <stdbool.h>

#ifdef ALL_DEBUG_FLAGS
#undef EMOD_ASE_DEBUG_FLAG
#define EMOD_ASE_DEBUG_FLAG
#endif

#define __EMOD_ASE_DEBUG__(args...)	fprintf(stderr, "emodule(ASE) " args)
#ifndef EMOD_ASE_DEBUG_FLAG
#define EMOD_ASE_DEBUG(args...)
#define EMOD_ASE_DEBUG_DMP(args...)
#else
#define EMOD_ASE_DEBUG(args...)		__EMOD_ASE_DEBUG__(args)
#define EMOD_ASE_DEBUG_DMP(args...)	fprintf(stderr, args)
#endif
#define EMOD_ASE_DEBUG_GC(args...)	EMOD_ASE_DEBUG("[GC]: " args)
#define EMOD_ASE_CRITICAL(args...)	__EMOD_ASE_DEBUG__("CRITICAL: " args)

#ifdef SXE_UNUSED
#elif defined(__GNUC__)
#  define SXE_UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
#  define SXE_UNUSED(x) /*@unused@*/ x
#else
#  define SXE_UNUSED(x) x
#endif

typedef long int Z_t;
typedef long unsigned int ase_hash_t;

#ifdef EMOD_ASE_MONOMOD
extern void ase_mono_LTX_init(void);
#endif
extern void ase_LTX_init(void);
extern void ase_LTX_reinit(void);
extern void ase_LTX_deinit(void);


typedef int(*ase_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_c_relation_f)(Lisp_Object, Lisp_Object); /* compatible */
typedef int(*ase_r_relation_f)(Lisp_Object, Lisp_Object); /* reflexive */
typedef int(*ase_s_relation_f)(Lisp_Object, Lisp_Object); /* symmetric */
typedef int(*ase_t_relation_f)(Lisp_Object, Lisp_Object); /* transitive */
typedef int(*ase_cr_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_cs_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_ct_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_rs_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_rt_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_st_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_crs_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_crt_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_cst_relation_f)(Lisp_Object, Lisp_Object);
typedef int(*ase_equivalence_relation_f)(Lisp_Object, Lisp_Object); /* r+s+t */
typedef int(*ase_congruence_relation_f)(Lisp_Object, Lisp_Object); /* c+r+s+t */
typedef Lisp_Object(*ase_element_relation_f)(Lisp_Object, Lisp_Object);

typedef ase_t_relation_f ase_order_relation_f; /* special context! */

/* generic ase objects */
typedef struct ase_object_impl_s *ase_object_impl_t;
typedef void *ase_object_options_t;
typedef struct ase_object_s *ase_object_t;

typedef ase_object_t(*ase_object_constr_f)(ase_object_options_t opts, ...);
typedef void(*ase_object_destr_f)(ase_object_t);
typedef ase_hash_t(*ase_object_hash_f)(ase_object_t);
typedef struct ase_category_s *ase_category_t;
typedef long unsigned int ase_description_t;
typedef ase_description_t ase_object_d;

struct ase_object_impl_s {
	ase_object_constr_f constrf;
	ase_object_destr_f destrf;
	ase_object_hash_f hashf;
};

struct ase_object_s {
	/* the `capabilities' of the particular object */
	ase_object_impl_t impl;
	ase_object_options_t options;
	/* the category of the object's world, i.e. where the object lives in */
	ase_category_t category;
	/* properties of this particular object
	 * these depend upon the world of the object */
	ase_object_d properties;
	sxe_mutex_t mtx;
	ase_hash_t hash;
};

struct ase_category_s {
	bool setoid_p:1;
	bool magma_p:1;
	bool algebra_p:1;
	bool mapping_p:1;
	bool relation_p:1;
	bool orderable_p:1;
};

/* inline decls */
static inline int
_ase_less_p(Lisp_Object, Lisp_Object);
static inline int
_ase_greater_p(Lisp_Object, Lisp_Object);
static inline int
_ase_equal_p(Lisp_Object, Lisp_Object);
static inline int
_ase_lessequal_p(Lisp_Object, Lisp_Object);
static inline int
_ase_greaterequal_p(Lisp_Object, Lisp_Object);
static inline void
_ase_swap(Lisp_Object *args, int idx1, int idx2);


static inline int
_ase_less_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_LESSP, a, b);
}
static inline int
_ase_greater_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_GREATERP, a, b);
}
static inline int
_ase_equal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_EQUALP, a, b);
}
static inline int
_ase_lessequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_LESSP,
			   ASE_BINARY_REL_EQUALP, a, b);
}
static inline int
_ase_greaterequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_GREATERP,
			   ASE_BINARY_REL_EQUALP, a, b);
}

static inline void
_ase_swap(Lisp_Object *args, int idx1, int idx2)
{
	Lisp_Object foo = args[idx1];
	args[idx1] = args[idx2];
	args[idx2] = foo;
	return;
}

#define DEFASETYPE_WITH_OPS(_tsym, _tstr)			\
	do {							\
		defsymbol(&_tsym, _tstr);			\
		ase_optable_add(_tsym);				\
	} while (0)

#endif	/* INCLUDED_ase_h_ */
