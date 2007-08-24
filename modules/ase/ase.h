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

#ifdef UNUSED
#elif defined(__GNUC__)
#  define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
#  define UNUSED(x) /*@unused@*/ x
#else
#  define UNUSED(x) x
#endif

EMOD_ANNOUNCE(ase);

extern dllist_t ase_empty_sets;

#ifdef EMOD_ASE_MONOMOD
void vars_of_ase_mono(void);
void syms_of_ase_mono(void);
#endif
void reinit_vars_of_ase(void);
void vars_of_ase(void);
void syms_of_ase(void);


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

extern inline int
_ase_less_p(Lisp_Object, Lisp_Object);
extern inline int
_ase_greater_p(Lisp_Object, Lisp_Object);
extern inline int
_ase_equal_p(Lisp_Object, Lisp_Object);
extern inline int
_ase_lessequal_p(Lisp_Object, Lisp_Object);
extern inline int
_ase_greaterequal_p(Lisp_Object, Lisp_Object);
extern inline void
_ase_swap(Lisp_Object *args, int idx1, int idx2);
extern inline void
_ase_heapsort_sift(
	Lisp_Object *args, int start, int count, ase_order_relation_f lessp);
extern inline void
_ase_heapsort(int nargs, Lisp_Object *args, ase_order_relation_f lessp);


extern inline int
_ase_less_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_LESSP, a, b);
}
extern inline int
_ase_greater_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_GREATERP, a, b);
}
extern inline int
_ase_equal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_EQUALP, a, b);
}
extern inline int
_ase_lessequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_LESSP,
			   ASE_BINARY_REL_EQUALP, a, b);
}
extern inline int
_ase_greaterequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_GREATERP,
			   ASE_BINARY_REL_EQUALP, a, b);
}

extern inline void
_ase_swap(Lisp_Object *args, int idx1, int idx2)
{
	Lisp_Object foo = args[idx1];
	args[idx1] = args[idx2];
	args[idx2] = foo;
	return;
}
extern inline void
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
extern inline void
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

#define ASE_LESS_P(_a, _b)	(_ase_less_p((_a), (_b)))
#define ASE_GREATER_P(_a, _b)	(_ase_greater_p((_a), (_b)))
#define ASE_EQUAL_P(_a, _b)	(_ase_equal_p((_a), (_b)))

#define DEFASETYPE_WITH_OPS(_tsym, _tstr)			\
	do {							\
		defsymbol(&_tsym, _tstr);			\
		ase_optable_add(_tsym);				\
	} while (0)

#endif

