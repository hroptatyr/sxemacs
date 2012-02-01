/*** ase-permutation.h -- Permutations
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

#ifndef INCLUDED_ase_permutation_h_
#define INCLUDED_ase_permutation_h_ 1

#include "ase.h"

typedef struct ase_permutation_s *ase_permutation_t;

#define EMOD_ASE_DEBUG_PERM(args...)	EMOD_ASE_DEBUG("[PERM]: " args)

extern Lisp_Object Qase_permutation, Qase_permutationp;
extern Lisp_Object Qase_identity_permutation;
extern Lisp_Object Qpermutation_error, Qoverlap_error;

extern void LTX_PUBINIT(ase_permutation)(void);
extern void LTX_PUBREINIT(ase_permutation)(void);
extern void LTX_PUBDEINIT(ase_permutation)(void);


struct ase_permutation_s {
	size_t degree;
	unsigned long *perm;
};


#define ASE_PERMUTATIONP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_permutation))
#define CHECK_ASE_PERMUTATION(x)					\
	do {								\
		if (!ASE_PERMUTATIONP(x))				\
			dead_wrong_type_argument(Qase_permutationp, x); \
	} while (0)
#define CONCHECK_ASE_PERMUTATION(x)					\
	do {								\
		if (!ASE_PERMUTATIONP(x))				\
			x = wrong_type_argument(Qase_permutationp, x); \
	} while (0)
extern Lisp_Object _ase_wrap_permutation(ase_permutation_t);
#define XSETASE_PERMUTATION(_res, _int)	\
	(_res) = _ase_wrap_permutation((_int))
#define XASE_PERMUTATION(_x)			\
	((ase_permutation_t)get_dynacat(_x))

#define ase_permutation_degree(_p)	((_p)->degree)
#define ase_permutation_perm(_p)	((_p)->perm)
#define XASE_PERMUTATION_DEGREE(_p)		\
	ase_permutation_degree(XASE_PERMUTATION(_p))
#define XASE_PERMUTATION_PERM(_p)		\
	ase_permutation_perm(XASE_PERMUTATION(_p))

#define ase_permutation_refcnt(_a)	(&((_a)->refcnt))
#define ase_permutation_init_refcnt(_a)	\
	(sxe_refcounter_init(ase_permutation_refcnt(_a)))
#define ase_permutation_fini_refcnt(_a)	\
	(sxe_refcounter_finish(ase_permutation_refcnt(_a)))
#define ase_permutation_refval(_a)		\
	(sxe_refcounter_value(ase_permutation_refcnt(_a)))
#define ase_permutation_incref(_a)		\
	(sxe_refcounter_incref(ase_permutation_refcnt(_a)))
#define ase_permutation_decref(_a)		\
	(sxe_refcounter_decref(ase_permutation_refcnt(_a)))
#define XASE_PERMUTATION_REFVAL(_a)			\
	(ase_permutation_refval(XASE_PERMUTATION(_a)))
#define XASE_PERMUTATION_INCREF(_a)			\
	(ase_permutation_incref(XASE_PERMUTATION(_a)))
#define XASE_PERMUTATION_DECREF(_a)			\
	(ase_permutation_decref(XASE_PERMUTATION(_a)))


/* constructors */
extern Lisp_Object
ase_make_permutation(Lisp_Object vector);
extern Lisp_Object ase_copy_permutation(Lisp_Object perm);

/* predicates */

#endif
