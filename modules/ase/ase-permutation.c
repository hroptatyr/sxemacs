/*** ase-permutation.c -- Permutations
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
#include "ent/ent.h"
#include "ase.h"
#include "ase-permutation.h"

PROVIDE(ase_permutation);
REQUIRE(ase_permutation, "ase");

Lisp_Object Qase_permutation, Qase_permutationp;
Lisp_Object Qase_identity_permutation;
Lisp_Object Qpermutation_error, Qoverlap_error;
static int sane_small;


/* stuff for the dynacat */
static void
_ase_permutation_prnt_cyc(unsigned long *p, unsigned long idx, Lisp_Object pcf)
{
	unsigned long q;

	write_fmt_string(pcf, "(%ld", idx+1);
	for (q = p[idx]; q != idx; q = p[q])
		write_fmt_str(pcf, " %ld", q+1);
	write_c_string(")", pcf);
}

static void
_ase_permutation_prnt(ase_permutation_t n, Lisp_Object pcf)
{
	size_t deg = ase_permutation_degree(n);
	unsigned long *perm = ase_permutation_perm(n);
	size_t i = 0;

	if (deg == 0) {
		write_c_string("()", pcf);
		return;
	}

	for (i = 0; i < deg; i++) {
		/* find the smallest element in this cycle                         */
		unsigned long q = perm[i];
		while (i < q)
			q = perm[q];

		/* if the smallest is the one we started with
		 * lets print the cycle */
		if (i == q && perm[i] != i) {
			_ase_permutation_prnt_cyc(perm, i, pcf);
		}
	}
	return;
}

static void
ase_permutation_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_PERM("p:0x%08x@0x%08x (rc:%d)\n",
			   (unsigned int)(XASE_PERMUTATION(obj)),
			    (unsigned int)obj, 1);
	write_c_string("#<ase:permutation ", pcf);
	_ase_permutation_prnt(XASE_PERMUTATION(obj), pcf);
	write_c_string(">", pcf);
}

static void
ase_permutation_fini(Lisp_Object obj, int unused)
{
	ase_permutation_t free_me = XASE_PERMUTATION(obj);

	EMOD_ASE_DEBUG_GC("p:%p@%p (rc:%d) shall be freed...\n",
			  free_me, (void*)obj, 1);

	xfree(ase_permutation_perm(free_me));
	xfree(free_me);
	return;
}

static inline void
_ase_permutation_mark(ase_permutation_t SXE_UNUSED(unused))
{
	return;
}

static void
ase_permutation_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_PERM("p:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			    (unsigned int)(XASE_PERMUTATION(obj)),
			    (unsigned int)obj, 1);
	_ase_permutation_mark(XASE_PERMUTATION(obj));
	return;
}


Lisp_Object
_ase_wrap_permutation(ase_permutation_t n)
{
	Lisp_Object result;

	result = make_dynacat(n);
	XDYNACAT(result)->type = Qase_permutation;

#if 0
	if (n)
		ase_permutation_incref(n);
#endif
	set_dynacat_printer(result, ase_permutation_prnt);
	set_dynacat_marker(result, ase_permutation_mark);
	set_dynacat_finaliser(result, ase_permutation_fini);

	EMOD_ASE_DEBUG_PERM("p:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			    (unsigned int)n,
			    1, (unsigned int)result);

	return result;
}

static inline ase_permutation_t
_ase_make_permutation(size_t deg, unsigned long *perm)
{
	ase_permutation_t n = xnew_and_zero(struct ase_permutation_s);

	ase_permutation_degree(n) = deg;
	ase_permutation_perm(n) = perm;

	EMOD_ASE_DEBUG_PERM("p:%p (rc:0, deg:%d) shall be created...\n",
			    n, (int)deg);
	return n;
}

static inline int
ase_permutation_vecr_id_p(Lisp_Object vec)
{
	size_t i = 0, deg = XVECTOR_LENGTH(vec);

	while (i < deg) {
		Lisp_Object tmp = XVECTOR_DATA(vec)[i];
		CHECK_NATNUM(tmp);
		if (tmp == Qzero) {
			dead_wrong_type_argument(Qnatnump, tmp);
		}
		if (XUINT(tmp) != ++i)
			return 0;
	}
	return 1;
}

static inline int
ase_permutation_cycr_id_p(Lisp_Object vec)
{
	size_t i, deg = XVECTOR_LENGTH(vec), id_p = 1;

	for (i = 0; i < deg; i++) {
		Lisp_Object cyc = XVECTOR_DATA(vec)[i];

		if (NILP(cyc))
			continue;

		CHECK_CONS(cyc);
		id_p = 0;
		while (!NILP(cyc)) {
			Lisp_Object img = XCAR(cyc);
			CHECK_NATNUM(img);
			if (img == Qzero) {
				dead_wrong_type_argument(Qnatnump, img);
			}
			cyc = XCDR(cyc);
		}
	}
	return id_p;
}

static inline size_t
ase_permutation_determine_deg(Lisp_Object vec)
{
	/* vec is assumed to be in cycle representation */
	size_t len = XVECTOR_LENGTH(vec);
	size_t i, deg = 0;

	for (i = 0; i < len; i++) {
		Lisp_Object cyc = XVECTOR_DATA(vec)[i];

		if (NILP(cyc))
			continue;

		while (!NILP(cyc)) {
			Lisp_Object img = XCAR(cyc);
			if (XUINT(img) > deg)
				deg = XUINT(img);
			cyc = XCDR(cyc);
		}
	}
	return deg;
}

static inline void
ase_permutation_init_cycr(size_t deg, unsigned long *p)
{
	/* vec is assumed to be in cycle representation */
	size_t i;

	for (i = 0; i < deg; i++) {
		p[i] = i;
	}
	return;
}

static inline void
ase_permutation_copy_cycr(unsigned long *p, Lisp_Object vec)
{
	/* vec is assumed to be in cycle representation */
	size_t len = XVECTOR_LENGTH(vec);
	size_t i;

	for (i = 0; i < len; i++) {
		Lisp_Object cyc = XVECTOR_DATA(vec)[i];
		Lisp_Object first;

		if (NILP(cyc))
			continue;

		first = XCAR(cyc);
		while (!NILP(XCDR(cyc))) {
			Lisp_Object pre = XCAR(cyc);
			Lisp_Object post = XCAR((cyc = XCDR(cyc)));
			p[XUINT(pre)-1] = XUINT(post)-1;
		}
		/* cyc should now look like (<elm> . nil) */
		p[XUINT(XCAR(cyc))-1] = XUINT(first)-1;
	}
	return;
}

static inline Lisp_Object
ase_make_permutation_cycr(Lisp_Object vec)
{
	ase_permutation_t a = NULL;
	size_t deg;
	unsigned long *perm;

	EMOD_ASE_DEBUG_PERM("Creating perm from cycle representation ...\n");

	if (ase_permutation_cycr_id_p(vec))
		return Qase_identity_permutation;

	deg = ase_permutation_determine_deg(vec);
	perm = xnew_array(unsigned long, deg);
	ase_permutation_init_cycr(deg, perm);
	ase_permutation_copy_cycr(perm, vec);

	a = _ase_make_permutation(deg, perm);
	return _ase_wrap_permutation(a);
}

static inline void
ase_permutation_copy_vecr(unsigned long *p, Lisp_Object vec)
{
	size_t i, deg = XVECTOR_LENGTH(vec);

	for (i = 0; i < deg; i++) {
		Lisp_Object tmp = XVECTOR_DATA(vec)[i];
		unsigned long m = XUINT(tmp)-1;
		p[i] = m;
	}
}

static inline Lisp_Object
ase_make_permutation_vecr(Lisp_Object vec)
{
	ase_permutation_t a = NULL;
	size_t deg;
	unsigned long *perm;

	EMOD_ASE_DEBUG_PERM("Creating perm from mapping representation ...\n");

	if (ase_permutation_vecr_id_p(vec))
		return Qase_identity_permutation;

	deg = XVECTOR_LENGTH(vec);
	perm = xnew_array(unsigned long, deg);
	ase_permutation_copy_vecr(perm, vec);

	a = _ase_make_permutation(deg, perm);
	return _ase_wrap_permutation(a);
}

static inline int
ase_permutation_cycrep_p(Lisp_Object vec)
{
	if (XVECTOR_LENGTH(vec) == 0)
		return 0;
	return (CONSP(XVECTOR_DATA(vec)[0]) ||
		NILP(XVECTOR_DATA(vec)[0]));
}

Lisp_Object
ase_make_permutation(Lisp_Object vec)
{
	if (ase_permutation_cycrep_p(vec))
		return ase_make_permutation_cycr(vec);
	else
		return ase_make_permutation_vecr(vec);
}

/* accessors */



/* lisp level */
DEFUN("ase-permutationp", Fase_permutationp, 1, 1, 0, /*
Return non-`nil' iff OBJECT is an ase permutation.
*/
      (object))
{
	if (ASE_PERMUTATIONP(object))
		return Qt;

	return Qnil;
}

/* ###autoload */
DEFUN("ase-permutation", Fase_permutation, 1, 1, 0, /*
Return a permutation around with POINT of radius RADIUS
with respect to METRIC (optional).

If no special metric is given, the supremum metric is used.
*/
      (vector))
{
	return ase_make_permutation(vector);
}


/* accessors */



/* initialiser code */
#define EMODNAME	ase_permutation

void
EMOD_PUBINIT(void)
{
	/* constructors */
	DEFSUBR(Fase_permutation);
	/* predicates */
	DEFSUBR(Fase_permutationp);
	/* accessors */

	defsymbol(&Qase_permutation, "ase:permutation");
	defsymbol(&Qase_permutationp, "ase:permutationp");

	DEFERROR(Qpermutation_error,
		 "Permutation error", Qdomain_error);
	DEFERROR(Qoverlap_error,
		 "Permutations must not overlap", Qpermutation_error);

	Fprovide(intern("ase-permutation"));

	DEFVAR_CONST_LISP("ase-identity-permutation",
			  &Qase_identity_permutation /*
The identity permutation.
						     */);
	EMOD_PUBREINIT();
}

void
EMOD_PUBREINIT(void)
{
	sane_small = (snprintf(NULL, 0, "%ld", EMACS_INT_MAX) + 7) & -3;
	Qase_identity_permutation =
		_ase_wrap_permutation(_ase_make_permutation(0, NULL));
	/* defined in lread.c, declared in ent.h */
	ase_permutation_f = ase_make_permutation;
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-permutation"));
}

/* ase-permutation ends here */
