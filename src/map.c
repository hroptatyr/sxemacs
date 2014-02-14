/*** map.c -- Maps
 *
 * Copyright (C) 2007 Sebastian Freundt
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
 ***
 * Comment:
 * All the code below is just a first (tacky) draught.  It is optimised
 * in a way, but still, the ardent worshippers of the DRY principle
 * would tar and feather me for that.
 *
 ***/

/* Synched up with: Not in FSF, not in XE */

#include <sxemacs.h>
#include "map.h"
#include "dict.h"
#include "skiplist.h"
#include "ent/ent.h"

Lisp_Object Qmap;
Lisp_Object Q_arity, Q_result_type, Q_mode, Q_glue;
Lisp_Object Q_separator, Q_initiator, Q_terminator;
Lisp_Object Qpntw, Qpointwise, Qpoints;
Lisp_Object Qkeyw, Qkeywise, Qkeys;
Lisp_Object Qcomb, Qcombination, Qcombinations;
Lisp_Object Qperm, Qpermutation, Qpermutations;
Lisp_Object Qcart, Qcartesian;

typedef Lisp_Object(*glue_f)(int nargs, Lisp_Object *args);

static Lisp_Object Qinplace, Qlitter, Qconcat;
static Lisp_Object Qvector, Qbit_vector;

EXFUN(Fstring, MANY);
EXFUN(Fbit_vector, MANY);

/* until this is available globally */
#define DICTP(x)	(HASH_TABLEP(x) || SKIPLISTP(x))

struct decoration_s {
	Lisp_Object ini, ter, sep;
};


/* auxiliary */
static inline Lisp_Object
__Flist(int nargs, Lisp_Object *args)
	__attribute__((always_inline));
static inline Lisp_Object
__Flist(int nargs, Lisp_Object *args)
{
	/* this is just Flist() but inlined */
	Lisp_Object val = Qnil;
	Lisp_Object *argp = args + nargs;

	while (argp > args)
		val = Fcons(*--argp, val);
	return val;
}

static long unsigned int
__ncombinations(register long unsigned int n, long unsigned int k)
{
/* == binomial(n, k) */
	if (UNLIKELY(n == k || k == 0)) {
		return 1UL;
	} else if (UNLIKELY(k == 1 || n - k == 1)) {
		return n;
	} else if (k == 2 || n - k == 2) {
		return (n * (n-1)) >> 1;
	} else {
		/* otherwise do the hard work */
		long unsigned int num = n*(n-1)*(n-k+1), den = k*(k-1);

		/* swap k if necessary */
		if (n - k < k) {
			k = n - k;
		}

		for (n -= 2, k -= 2; k > 1;) {
			num *= n--;
			den *= k--;
		}
		return num/den;
	}
}

static long unsigned int
__factorial(register long unsigned int n)
{
	register long unsigned int r = n;

	/* trivial cases first */
	switch (n) {
	case 0:
	case 1:
		return 1UL;
	case 2:
		return 2UL;
	case 3:
		return 6UL;
	case 4:
		return 24UL;
	case 5:
		return 120UL;
	case 6:
		return 720UL;
	case 7:
		return 5040UL;
	case 8:
		return 40320UL;
	default:
		r = 40320UL * n;
	}

	/* the loop */
	for (long unsigned int i = 9; i < n; i++) {
		r *= i;
	}
	return r;
}

static long unsigned int
__nvariations(register long unsigned int n, long unsigned int k)
{
/* == binomial(n, k) * factorial(k) */
	if (UNLIKELY(k == 0)) {
		return 1UL;
	} else if (UNLIKELY(k == n)) {
		return __factorial(k);
	} else if (UNLIKELY(k == 1)) {
		return n;
	} else if (UNLIKELY(n - k == 1)) {
		return __factorial(n);
	} else if (k == 2) {
		return n * (n-1);
	} else if (k == 3) {
		return n * (n-1) * (n-2);
	} else {
		/* otherwise do the hard work */
		long unsigned int num = n--;

		num *= n--;
		num *= n--;
		num *= n--;
		while (k-- > 4) {
			num *= n--;
		}
		return num;
	}
}

static long unsigned int
__ncart(register long unsigned int n, long unsigned int k)
{
/* == n^k */
	long unsigned int res;

	switch (k) {
	case 2:
		return n*n;
	case 3:
		return n*n*n;
	case 1:
		return n;
	case 0:
		return 1UL;
	default:
		break;
	}

	for (res = n * n * n * n, k -= 4; k > 0; k--) {
		res *= n;
	}
	return res;
}


static inline void
__advance_multi_index()
	__attribute__((always_inline));
static inline void
__advance_multi_index(long int idx[], long int j, long int fam_len)
{
	/* partially unroll */
	if (LIKELY(++idx[--j] < fam_len)) {
		return;
	}
	idx[j] = 0;
	if (LIKELY(++idx[--j] < fam_len)) {
		return;
	}
	idx[j] = 0;
	if (LIKELY(++idx[--j] < fam_len)) {
		return;
	}
	idx[j] = 0;
	while (j > 0) {
		if (LIKELY(++idx[--j] < fam_len)) {
			return;
		}
		idx[j] = 0;
	}
	return;
}

static inline void
__advance_multi_index_2()
	__attribute__((always_inline));
static inline void
__advance_multi_index_2(long int idx[], long int j, size_t flen[])
{
/* improved version of __a_m_v() which allows for differently-sized families */
	/* partially unroll */
	if (LIKELY((--j, ++idx[j] < (long int)flen[j]))) {
		return;
	}
	idx[j] = 0;
	if (LIKELY((--j, ++idx[j] < (long int)flen[j]))) {
		return;
	}
	idx[j] = 0;
	if (LIKELY((--j, ++idx[j] < (long int)flen[j]))) {
		return;
	}
	idx[j] = 0;
	while (j > 0) {
		if (LIKELY((--j, ++idx[j] < (long int)flen[j]))) {
			return;
		}
		idx[j] = 0;
	}
	return;
}

static inline void
__advance_multi_index_3()
	__attribute__((always_inline));
static inline void
__advance_multi_index_3(
	long int idx[], long int j, size_t flen[],
	long int nseqs, size_t arity[])
{
/* improved version of __a_m_v_2() which allows for differently-sized families
 * and multiplicities thereof
 * this is for cartesian indexing, i.e. the order goes
 * [1,0]->[1,1]->[1,2]->[2,0] for arity (., 3) */
	long int mlt = arity[--nseqs];

	if (LIKELY(++idx[--j] < (long int)flen[nseqs])) {
		return;
	}
	idx[j] = 0;
	if (mlt-- == 0) {
		mlt = arity[--nseqs];
	}
	if (LIKELY(++idx[--j] < (long int)flen[nseqs])) {
		return;
	}
	idx[j] = 0;
	if (mlt-- == 0) {
		mlt = arity[--nseqs];
	}
	if (LIKELY(++idx[--j] < (long int)flen[nseqs])) {
		return;
	}
	idx[j] = 0;
	if (mlt-- == 0) {
		mlt = arity[--nseqs];
	}
	while (j > 0 && nseqs >= 0) {
		if (LIKELY(++idx[--j] < (long int)flen[nseqs])) {
			return;
		}
		idx[j] = 0;
		if (mlt-- == 0) {
			mlt = arity[--nseqs];
		}
	}
	return;
}

static inline void
__initialise_multi_index()
	__attribute__((always_inline));
static inline void
__initialise_multi_index(size_t midx[], size_t arity)
{
	midx[0] = 0L;
	for (size_t j = 1; j < arity; j++) {
		midx[j] = j;
	}
	return;
}

static inline bool
__advance_multi_index_comb()
	__attribute__((always_inline));
static inline bool
__advance_multi_index_comb(size_t idx[], size_t len, int arity)
{
	register long int i;

	for (i = arity-1; (i >= 0) && idx[i] >= len - arity + i; i--);
	idx[i]++;
	for (; ++i < arity; ) {
		idx[i] = idx[i-1]+1;
	}
	return (idx[i-1] < len);
}

static inline void
__advance_multi_index_4()
	__attribute__((always_inline));
static inline void
__advance_multi_index_4(
	size_t *midx[], size_t flen[], long int j /*nseqs*/, size_t arity[])
{
/* like __a_m_v_3(), also allowing for differently-sized families
 * and multiplicities thereof, but for for combinatorial indexing,
 * i.e. the order goes
 * [1,2]->[1,3]->[2,3] for arity (., 3) */
	--j;
	if (LIKELY(__advance_multi_index_comb(midx[j], flen[j], arity[j]))) {
		/* if there's more to come, bingo */
		return;
	}
	/* otherwise reinitialise the mindex we're currently shagging */
	__initialise_multi_index(midx[j], arity[j]);

	--j;
	if (LIKELY(__advance_multi_index_comb(midx[j], flen[j], arity[j]))) {
		return;
	}
	/* otherwise reinitialise the mindex we're currently shagging */
	__initialise_multi_index(midx[j], arity[j]);

	/* now loop mode */
	while (j-- > 0) {
		if (LIKELY(__advance_multi_index_comb(
				   midx[j], flen[j], arity[j]))) {
			return;
		}
		/* otherwise reinitialise the mindex we're currently shagging */
		__initialise_multi_index(midx[j], arity[j]);
	}
	return;
}


/* This is the guts of several mapping functions.
   Apply FUNCTION to each element of SEQUENCE, one by one,
   storing the results into elements of VALS, a C vector of Lisp_Objects.
   LENI is the length of VALS, which should also be the length of SEQUENCE.

   If VALS is a null pointer, do not accumulate the results. */

static void
mapcar1(size_t leni, Lisp_Object * vals,
	Lisp_Object function, Lisp_Object sequence)
{
	Lisp_Object result;
	Lisp_Object args[2];
	struct gcpro gcpro1;

	args[0] = function;

	if (vals) {
		/* clean sweep */
		memset(vals, 0, leni * sizeof(Lisp_Object));
		GCPROn(vals, leni);
	}

	if (LISTP(sequence)) {
		/* A devious `function' could either:
		   - insert garbage into the list in front of us, causing XCDR to crash
		   - amputate the list behind us using (setcdr), causing the remaining
		   elts to lose their GCPRO status.

		   if (vals != 0) we avoid this by copying the elts into the
		   `vals' array.  By a stroke of luck, `vals' is exactly large
		   enough to hold the elts left to be traversed as well as the
		   results computed so far.

		   if (vals == 0) we don't have any free space available and
		   don't want to eat up any more stack with alloca().
		   So we use EXTERNAL_LIST_LOOP_3_NO_DECLARE and GCPRO the tail. */

		if (vals) {
			Lisp_Object *val = vals;
			size_t i;

			LIST_LOOP_2(elt, sequence) {
			    *val++ = elt;
			}

			for (i = 0; i < leni; i++) {
				args[1] = vals[i];
				vals[i] = Ffuncall(2, args);
			}
		} else {
			Lisp_Object elt, tail;
			EMACS_INT len_unused;
			struct gcpro ngcpro1;

			NGCPRO1(tail);

			{
				EXTERNAL_LIST_LOOP_4_NO_DECLARE(elt, sequence,
								tail,
								len_unused) {
					args[1] = elt;
					Ffuncall(2, args);
				}
			}

			NUNGCPRO;
		}
	} else if (VECTORP(sequence)) {
		Lisp_Object *objs = XVECTOR_DATA(sequence);

		for (size_t i = 0; i < leni; i++) {
			args[1] = *objs++;
			result = Ffuncall(2, args);
			if (vals) {
				vals[i] = result;
			}
		}
	} else if (DLLISTP(sequence)) {
		dllist_item_t elt = XDLLIST_FIRST(sequence);

		for (size_t i = 0; elt; i++) {
			args[1] = (Lisp_Object)elt->item;
			result = Ffuncall(2, args);
			if (vals) {
				vals[i] = result;
			}
			elt = elt->next;
		}
	} else if (STRINGP(sequence)) {
		/* The string data of `sequence' might be relocated during GC. */
		Bytecount slen = XSTRING_LENGTH(sequence);
		Bufbyte *p = NULL;
		Bufbyte *end = NULL;
		int speccount = specpdl_depth();
		size_t i = 0;

		XMALLOC_ATOMIC_OR_ALLOCA(p, slen, Bufbyte);
		end = p + slen;

		memcpy(p, XSTRING_DATA(sequence), slen);

		while (p < end) {
			args[1] = make_char(charptr_emchar(p));
			INC_CHARPTR(p);
			result = Ffuncall(2, args);
			if (vals) {
				vals[i++] = result;
			}
		}
		XMALLOC_UNBIND(p, slen, speccount);
	} else if (BIT_VECTORP(sequence)) {
		Lisp_Bit_Vector *v = XBIT_VECTOR(sequence);

		for (size_t i = 0; i < leni; i++) {
			args[1] = make_int(bit_vector_bit(v, i));
			result = Ffuncall(2, args);
			if (vals) {
				vals[i] = result;
			}
		}
	} else {
		/* unreachable, since Flength (sequence) did not get an error */
		abort();
	}

	if (vals) {
		UNGCPRO;
	}
}

static void
list_map_inplace(Lisp_Object function, Lisp_Object list)
{
	Lisp_Object args[2];
	struct gcpro gcpro1, gcpro2;
	Lisp_Object elt = list;

	GCPRO2(function, list);

	args[0] = function;
	while (!NILP(elt)) {
		args[1] = XCAR(elt);
		XCAR(elt) = Ffuncall(2, args);
		elt = XCDR(elt);
	}
	UNGCPRO;
}

static void
vector_map_inplace(Lisp_Object function, Lisp_Object tuple)
{
	Lisp_Object *objs = XVECTOR_DATA(tuple);
	Lisp_Object args[2];
	size_t i, len = XVECTOR_LENGTH(tuple);
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO2n(function, tuple, args, countof(args));

	args[0] = function;
	for (i = 0; i < len; i++) {
		args[1] = *objs;
		*objs++ = Ffuncall(2, args);
	}

	UNGCPRO;
}

static void
string_map_inplace(Lisp_Object function, Lisp_Object string)
{
	Lisp_Object args[2];
	size_t len = XSTRING_LENGTH(string);
	Bufbyte *p = XSTRING_DATA(string);
	Bufbyte *end = p + len;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO2n(function, string, args, countof(args));

	args[0] = function;
	while (p < end) {
		args[1] = make_char(charptr_emchar(p));
		args[1] = Ffuncall(2, args);
		if (CHARP(args[1]))
			set_charptr_emchar(p, XCHAR(args[1]));
		else
			set_charptr_emchar(p, '\000');
		INC_CHARPTR(p);
	}

	UNGCPRO;
}

static void
bit_vector_map_inplace(Lisp_Object function, Lisp_Object bitvec)
{
	Lisp_Bit_Vector *v = XBIT_VECTOR(bitvec);
	Lisp_Object args[2];
	struct gcpro gcpro1, gcpro2, gcpro3;
	size_t i, len = bit_vector_length(XBIT_VECTOR(bitvec));

	GCPRO2n(function, bitvec, args, countof(args));

	args[0] = function;
	for (i = 0; i < len; i++) {
		args[1] = make_int(bit_vector_bit(v, i));
		args[1] = Ffuncall(2, args);
		if ((NUMBERP(args[1]) && ent_unrel_zerop(args[1])) ||
		    NILP(args[1]))
			set_bit_vector_bit(v, i, 0);
		else
			set_bit_vector_bit(v, i, -1);
	}

	UNGCPRO;
}

/***
 * The mapfam approach
 */

/* auxiliary stuff */
static inline size_t
__fam_size(Lisp_Object fam)
{
	return seq_length((seq_t)(void*)fam);
}

static inline size_t
__nfam_min_size(Lisp_Object fam[], size_t nfam)
{
	size_t res;

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		return 0UL;
	}
	/* otherwise unroll a little */
	res = __fam_size(fam[0]);
	for (size_t j = 1; j < nfam; j++) {
		size_t tmp = __fam_size(fam[j]);
		if (tmp < res) {
			res = tmp;
		}
	}
	return res;
}

static inline size_t
__nfam_min_size_a(Lisp_Object fam[], size_t nfam, size_t arity[])
{
	size_t res;

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		return 0UL;
	}
	/* otherwise unroll a little */
	res = __fam_size(fam[0]) / arity[0];
	for (size_t j = 1; j < nfam; j++) {
		size_t tmp = __fam_size(fam[j]) / arity[j];
		if (tmp < res) {
			res = tmp;
		}
	}
	return res;
}

static inline size_t
__nfam_cart_sum_size(size_t *sum, size_t *cart, size_t nfsz[],
		     Lisp_Object fam[], size_t nfam)
{
/* computes the size of the cartesian set and the maximum size of
 * the union set, returns the sum of cartesian and union, and puts
 * intermediately computed family sizes int nfsz */

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		*sum = *cart = 0;
		return 0UL;
	} else if (nfam == 1) {
		/* another horst case
		 * just 1 fam should always call fam_size() */
		return *sum = *cart = nfsz[0] = __fam_size(fam[0]);
	}
	/* otherwise unroll a little */
	nfsz[0] = __fam_size(fam[0]);
	nfsz[1] = __fam_size(fam[1]);
	*sum = nfsz[0] + nfsz[1];
	*cart = nfsz[0] * nfsz[1];
	for (size_t j = 2; j < nfam; j++) {
		nfsz[j] = __fam_size(fam[j]);
		*sum += nfsz[j];
		*cart *= nfsz[j];
	}
	return *sum + *cart;
}

static inline void
__my_pow_insitu(size_t *base, size_t expon)
{
/* improve me and put me somewhere else, ase-arith.h? */
	for (size_t i = 1, b = *base; i < expon; i++) {
		*base *= b;
	}
	return;
}

static inline size_t
__my_pow_explicit(size_t base, size_t expon)
{
/* improve me and put me somewhere else, ase-arith.h? */
	size_t res = base;
	for (size_t i = 1; i < expon; i++) {
		res *= base;
	}
	return res;
}

static inline size_t
__nfam_cart_sum_size_a(size_t *sum, size_t *cart, size_t *midxsz,
		       size_t nfsz[],
		       Lisp_Object fam[], size_t nfam, size_t arity[])
{
/* computes the size of the cartesian set (put into *cart), the maximum
 * size of the union set (returned) and the multiplicity of the
 * multi-index (which is the cross sum of the arity array) returns the
 * sum of cartesian and union, and puts intermediately computed family
 * sizes into nfsz */

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		*sum = *cart = *midxsz = 0;
		return 0UL;
	} else if (nfam == 1) {
		/* another horst case
		 * just 1 fam should always call fam_size() */
		*sum = *cart = nfsz[0] = __fam_size(fam[0]);
		__my_pow_insitu(cart, *midxsz = arity[0]);
		return *sum + *cart;
	}
	/* otherwise unroll a little */
	nfsz[0] = __fam_size(fam[0]);
	nfsz[1] = __fam_size(fam[1]);
	*sum = nfsz[0] + nfsz[1];
	*midxsz = arity[0] + arity[1];
	*cart = __my_pow_explicit(nfsz[0], arity[0]) *
		__my_pow_explicit(nfsz[1], arity[1]);
	for (size_t j = 2; j < nfam; j++) {
		nfsz[j] = __fam_size(fam[j]);
		*sum += nfsz[j];
		*midxsz += arity[j];
		*cart *= __my_pow_explicit(nfsz[j], arity[j]);
	}
	return *sum + *cart;
}

static inline size_t
__nfam_comb_sum_size_a(size_t *sum, size_t *comb, size_t *midxsz,
		       size_t nfsz[],
		       Lisp_Object fam[], size_t nfam, size_t arity[])
{
/* computes the size of the cartesian set (returned), the maximum size of
 * the union set and the multiplicity of the multi-index (which is the
 * cross sum of the arity array) returns the sum of cartesian and union,
 * and puts intermediately computed family sizes into nfsz */

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		*sum = *comb = *midxsz = 0;
		return 0UL;
	} else if (nfam == 1) {
		/* another horst case
		 * just 1 fam should always call fam_size() */
		*sum = nfsz[0] = __fam_size(fam[0]);
		*comb = __ncombinations(nfsz[0], *midxsz = arity[0]);
		return *sum + *comb;
	}
	/* otherwise unroll a little */
	nfsz[0] = __fam_size(fam[0]);
	nfsz[1] = __fam_size(fam[1]);
	*sum = nfsz[0] + nfsz[1];
	*midxsz = arity[0] + arity[1];
	*comb = __ncombinations(nfsz[0], arity[0]) *
		__ncombinations(nfsz[1], arity[1]);
	for (size_t j = 2; j < nfam; j++) {
		nfsz[j] = __fam_size(fam[j]);
		*sum += nfsz[j];
		*midxsz += arity[j];
		*comb *= __ncombinations(nfsz[j], arity[j]);
	}
	return *sum + *comb;
}

static inline size_t
__nfam_perm_sum_size(size_t *sum, size_t *cart, size_t *perm, size_t nfsz[],
		     Lisp_Object fam[], size_t nfam)
{
/* computes the size of the cartesian set and the maximum size of
 * the union set, returns the sum of cartesian and union, and puts
 * intermediately computed family sizes int nfsz */

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		*sum = *cart = *perm = 0;
		return 0UL;
	} else if (nfam == 1) {
		/* another horst case
		 * just 1 fam should always call fam_size() */
		*perm = 1;
		return *sum = *cart = nfsz[0] = __fam_size(fam[0]);
	}
	/* otherwise unroll a little */
	nfsz[0] = __fam_size(fam[0]);
	nfsz[1] = __fam_size(fam[1]);
	*sum = nfsz[0] + nfsz[1];
	*cart = nfsz[0] * nfsz[1];
	for (size_t j = 2; j < nfam; j++) {
		nfsz[j] = __fam_size(fam[j]);
		*sum += nfsz[j];
		*cart *= nfsz[j];
	}
	*cart *= (*perm = __factorial(nfam));
	return *sum + *cart;
}

static inline size_t
__nfam_perm_sum_size_a(size_t *sum, size_t *var, size_t *perm, size_t *midxsz,
		       size_t nfsz[],
		       Lisp_Object fam[], size_t nfam, size_t arity[])
{
/* computes the size of the cartesian set (returned), the maximum size of
 * the union set and the multiplicity of the multi-index (which is the
 * cross sum of the arity array) returns the sum of cartesian and union,
 * and puts intermediately computed family sizes into nfsz */

	/* catch the horst-case */
	if (UNLIKELY(nfam == 0)) {
		*sum = *var = *perm = *midxsz = 0;
		return 0UL;
	} else if (nfam == 1) {
		/* another horst case
		 * just 1 fam should always call fam_size() */
		*sum = nfsz[0] = __fam_size(fam[0]);
		*perm = __factorial(*midxsz = arity[0]);
		*var = __ncombinations(nfsz[0], arity[0]) * *perm;
		return *sum + *var;
	}
	/* otherwise unroll a little */
	nfsz[0] = __fam_size(fam[0]);
	nfsz[1] = __fam_size(fam[1]);
	*sum = nfsz[0] + nfsz[1];
	*midxsz = arity[0] + arity[1];
	*var = __ncombinations(nfsz[0], arity[0]) *
		__ncombinations(nfsz[1], arity[1]);
	for (size_t j = 2; j < nfam; j++) {
		nfsz[j] = __fam_size(fam[j]);
		*sum += nfsz[j];
		*midxsz += arity[j];
		*var *= __ncombinations(nfsz[j], arity[j]);
	}
	/* we computed the number of combinations above, now to compute
	 * the number of variations we have to apply the S_{midxsz} on
	 * each element, hence we simply multiply with the factorial of
	 * midxsz (which is the cross sum of all arities) */
	*var *= (*perm = __factorial(*midxsz));
	return *sum + *var;
}

/* combinations
 * dedicated subroutines for 2-combs and 3-combs because they are soooo easy
 */
static void
__2comb(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	Lisp_Object supp[], size_t slen,
	Lisp_Object fun, glue_f gf)
{
/* assumes that everything is gcpro'd properly */
	Lisp_Object arr[3] = {fun, Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* apply fun */
				tgts[l++] = Ffuncall(countof(arr), arr);
			}
		}
	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* glue */
				arr[1] = gf(2, &arr[1]);
				/* apply fun */
				tgts[l++] = Ffuncall(2, arr);
			}
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* glue */
				tgts[l++] = tgf(2, &arr[1]);
			}
		}
	}
	return;
}

static void
__3comb(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	Lisp_Object supp[], size_t slen,
	Lisp_Object fun, glue_f gf)
{
/* assumes that everything is gcpro'd properly */
	Lisp_Object arr[4] = {fun, Qnil, Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* apply fun */
					tgts[l++] = Ffuncall(countof(arr), arr);
				}
			}
		}
	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* glue */
					arr[1] = gf(3, &arr[1]);
					/* apply fun */
					tgts[l++] = Ffuncall(2, arr);
				}
			}
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* glue */
					tgts[l++] = tgf(3, &arr[1]);
				}
			}
		}
	}
	return;
}

static void
__ncomb(Lisp_Object tgts[], size_t tlen,
	Lisp_Object supp[], size_t slen,
	Lisp_Object fun, glue_f gf,
	size_t arity)
{
/* assumes that everything is gcpro'd properly */
	size_t idx[arity+1];
	size_t l = 0;
	Lisp_Object fc[arity+1], *v = &fc[1];

	/* setup */
	memset(idx, 0, arity*sizeof(long int));
	memset(v, 0, arity*sizeof(Lisp_Object));
	fc[0] = fun;

	/* special case slen == arity */
	if (UNLIKELY(slen == arity)) {
		if (LIKELY(!NILP(fun) && gf == NULL)) {
			tgts[0] = Ffuncall(slen, supp);
		} else if (LIKELY(!NILP(fun))) {
			v[0] = gf(slen, supp);
			tgts[0] = Ffuncall(2, fc);
		} else {
			glue_f tgf = gf ? gf : Flist;
			tgts[0] = tgf(slen, supp);
		}
		return;
	}

	/* setup, partially unrolled */
	idx[0] = 0;
	idx[1] = 1;
	for (size_t i = 2; i < arity; i++) {
		idx[i] = i;
	}

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < tlen) {
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			for (size_t i = 2; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* apply fun */
			tgts[l++] = Ffuncall(countof(fc), fc);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVE THAT */
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < tlen) {
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			for (size_t i = 2; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* glue */
			v[0] = gf(arity, v);
			/* apply fun */
			tgts[l++] = Ffuncall(2, fc);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVE THAT */
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		while (l < tlen) {
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			for (size_t i = 2; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* glue */
			tgts[l++] = tgf(arity, v);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVE THAT */
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	}
	return;
}


/* permutations
 * dedicated subroutines for 2-perms and 3-perms because they are soooo easy
 * 2-perms (transpositions) is just a 2-cycle along with its transposition,
 * so we can directly reuse the comb algorithm
 * 3-perms are just as simple, since the generation of S_3 can simply be put
 * as (), a, a^2, b, a*b, a^2*b where a is a 3-cycle and b a 2-cycle.
 */
static inline size_t
__2perm_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	    Lisp_Object supp[], size_t SXE_UNUSED(slen),
	    Lisp_Object fun,
	    size_t offset)
	__attribute__((always_inline));
static inline size_t
__2perm_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	    Lisp_Object supp[], size_t SXE_UNUSED(slen),
	    Lisp_Object fun,
	    size_t offset)
{
/* apply fun on S_2 on (the first two elements of) supp */
	Lisp_Object arr[3] = {fun, Qnil, Qnil};

	/* set up the array */
	arr[1] = supp[0];
	arr[2] = supp[1];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* swap them == (1,2) */
	arr[1] = supp[1];
	arr[2] = supp[0];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);
	return offset;
}

static inline size_t
__2perm_glue_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
		 Lisp_Object supp[], size_t SXE_UNUSED(slen),
		 Lisp_Object fun, glue_f gf,
		 size_t offset)
	__attribute__((always_inline));
static inline size_t
__2perm_glue_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
		 Lisp_Object supp[], size_t SXE_UNUSED(slen),
		 Lisp_Object fun, glue_f gf,
		 size_t offset)
{
/* apply fun on the glue of S_2 on (the first two elements of) supp */
	Lisp_Object arr[3] = {fun, Qnil, Qnil};

	/* set up the array */
	arr[1] = supp[0];
	arr[2] = supp[1];
	/* apply glue */
	arr[1] = gf(2, &arr[1]);
	/* apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* swap them == (1,2) */
	arr[1] = supp[1];
	arr[2] = supp[0];
	/* apply glue */
	arr[1] = gf(2, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);
	return offset;
}

static inline size_t
__2perm_glue(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	     Lisp_Object supp[], size_t SXE_UNUSED(slen),
	     glue_f gf,
	     size_t offset)
	__attribute__((always_inline));
static inline size_t
__2perm_glue(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	     Lisp_Object supp[], size_t SXE_UNUSED(slen),
	     glue_f gf,
	     size_t offset)
{
/* glue of S_2 on (the first two elements of) supp */
	volatile Lisp_Object tmp = supp[0];

	/* directly apply glue */
	tgts[offset++] = gf(2, supp);

	/* swap them == (1,2) */
	supp[0] = supp[1];
	supp[1] = tmp;
	/* apply glue */
	tgts[offset++] = gf(2, supp);
	return offset;
}

static inline size_t
_2perm(Lisp_Object tgts[], size_t tlen,
       Lisp_Object supp[], size_t slen,
       Lisp_Object fun, glue_f gf,
       size_t offset)
{
/* assumes that everything is gcpro'd correctly */
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		return __2perm_fun(tgts, tlen, supp, slen, fun, offset);
	} else if (LIKELY(!NILP(fun))) {
		return __2perm_glue_fun(tgts, tlen, supp, slen,
					fun, gf, offset);
	} else {
		return __2perm_glue(tgts, tlen, supp, slen,
				    gf ? gf : Flist, offset);
	}
}

static void
_comb_2perm(Lisp_Object *tgts, size_t tlen,
	    Lisp_Object *supp, size_t slen,
	    Lisp_Object fun, glue_f gf)
{
/* loop over everything in supp and form combinations thereof,
 * apply S_2 on them
 * assumes that everything is gcpro'd correctly */
	Lisp_Object v[2] = {Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				v[0] = supp[i];
				v[1] = supp[j];
				l = __2perm_fun(tgts, tlen, v, 2, fun, l);
			}
		}

	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				v[0] = supp[i];
				v[1] = supp[j];
				l = __2perm_glue_fun(
					tgts, tlen, v, 2, fun, gf, l);
			}
		}

	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen-1; i++) {
			for (size_t j = i+1; j < slen; j++) {
				v[0] = supp[i];
				v[1] = supp[j];
				l = __2perm_glue(tgts, tlen, v, 2, tgf, l);
			}
		}
	}
	return;
}

/* 3 perms */
static inline size_t
__3perm_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	    Lisp_Object supp[], size_t SXE_UNUSED(slen),
	    Lisp_Object fun,
	    size_t offset)
	__attribute__((always_inline));
static inline size_t
__3perm_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	    Lisp_Object supp[], size_t SXE_UNUSED(slen),
	    Lisp_Object fun,
	    size_t offset)
{
/* apply fun on S_3 on (the first 3 elements of) supp */
	Lisp_Object arr[4] = {fun, Qnil, Qnil, Qnil};

	/* we use gap's order of the elements of S3
	 * gap> Elements(SymmetricGroup(3));
	 * [ (), (2,3), (1,2), (1,2,3), (1,3,2), (1,3) ] */

	/* () */
	arr[1] = supp[0];
	arr[2] = supp[1];
	arr[3] = supp[2];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* (2,3) */
	arr[2] = supp[2];
	arr[3] = supp[1];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* (1,2) */
	arr[1] = supp[1];
	arr[2] = supp[0];
	arr[3] = supp[2];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* (1,2,3) */
	arr[2] = supp[2];
	arr[3] = supp[0];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* (1,3,2) */
	arr[1] = supp[2];
	arr[2] = supp[0];
	arr[3] = supp[1];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	/* (1,3) */
	arr[2] = supp[1];
	arr[3] = supp[0];
	/* and apply fun */
	tgts[offset++] = Ffuncall(countof(arr), arr);

	return offset;
}

static inline size_t
__3perm_glue_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
		 Lisp_Object supp[], size_t SXE_UNUSED(slen),
		 Lisp_Object fun, glue_f gf,
		 size_t offset)
	__attribute__((always_inline));
static inline size_t
__3perm_glue_fun(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
		 Lisp_Object supp[], size_t SXE_UNUSED(slen),
		 Lisp_Object fun, glue_f gf,
		 size_t offset)
{
/* apply fun on the glue of S_3 on (the first 3 elements of) supp */
	Lisp_Object arr[4] = {fun, Qnil, Qnil, Qnil};

	/* we use gap's order of the elements of S3
	 * gap> Elements(SymmetricGroup(3));
	 * [ (), (2,3), (1,2), (1,2,3), (1,3,2), (1,3) ] */

	/* () */
	arr[1] = supp[0];
	arr[2] = supp[1];
	arr[3] = supp[2];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* (2,3) */
	arr[1] = supp[0];
	arr[2] = supp[2];
	arr[3] = supp[1];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* (1,2) */
	arr[1] = supp[1];
	arr[2] = supp[0];
	arr[3] = supp[2];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* (1,2,3) */
	arr[1] = supp[1];
	arr[2] = supp[2];
	arr[3] = supp[0];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* (1,3,2) */
	arr[1] = supp[2];
	arr[2] = supp[0];
	arr[3] = supp[1];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	/* (1,3) */
	arr[1] = supp[2];
	arr[2] = supp[1];
	arr[3] = supp[0];
	/* apply glue */
	arr[1] = gf(3, &arr[1]);
	/* and apply fun */
	tgts[offset++] = Ffuncall(2, arr);

	return offset;
}

static inline size_t
__3perm_glue(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	     Lisp_Object supp[], size_t SXE_UNUSED(slen),
	     glue_f gf,
	     size_t offset)
	__attribute__((always_inline));
static inline size_t
__3perm_glue(Lisp_Object tgts[], size_t SXE_UNUSED(tlen),
	     Lisp_Object supp[], size_t SXE_UNUSED(slen),
	     glue_f gf,
	     size_t offset)
{
/* glue of S_3 on (the first 3 elements of) supp */
	volatile Lisp_Object tmp;

	/* we use gap's order of the elements of S3
	 * gap> Elements(SymmetricGroup(3));
	 * [ (), (2,3), (1,2), (1,2,3), (1,3,2), (1,3) ] */

	/* (), directly apply glue */
	tgts[offset++] = gf(3, supp);

	/* (1,2) */
	tmp = supp[1];
	supp[1] = supp[2];
	supp[2] = tmp;
	/* apply glue */
	tgts[offset++] = gf(3, supp);

	/* (0,1) == (0,1)(1,2)(1,2) == (0,1,2)(1,2) */
	tmp = supp[2];
	supp[2] = supp[1];
	supp[1] = supp[0];
	supp[0] = tmp;
	/* apply glue */
	tgts[offset++] = gf(3, supp);

	/* (0,1,2) == (0,2)(0,1) */
	tmp = supp[1];
	supp[1] = supp[2];
	supp[2] = tmp;
	/* apply glue */
	tgts[offset++] = gf(3, supp);

	/* (0,2,1) == (0,1,2)(0,1,2) */
	tmp = supp[0];
	supp[0] = supp[1];
	supp[1] = supp[2];
	supp[2] = tmp;
	/* apply glue */
	tgts[offset++] = gf(3, supp);

	/* (0,2) == (0,1)(0,2,1) */
	tmp = supp[1];
	supp[1] = supp[2];
	supp[2] = tmp;
	/* apply glue */
	tgts[offset++] = gf(3, supp);

	return offset;
}

static void
_comb_3perm(Lisp_Object *tgts, size_t tlen,
	    Lisp_Object *supp, size_t slen,
	    Lisp_Object fun, glue_f gf)
{
/* loop over everything in supp and form combinations thereof,
 * apply S_3 on them
 * assumes that everything is gcpro'd correctly */
	Lisp_Object v[3] = {Qnil, Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					v[0] = supp[i];
					v[1] = supp[j];
					v[2] = supp[k];
					l = __3perm_fun(
						tgts, tlen, v, 3, fun, l);
				}
			}
		}

	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					v[0] = supp[i];
					v[1] = supp[j];
					v[2] = supp[k];
					l = __3perm_glue_fun(
						tgts, tlen, v, 3, fun, gf, l);
				}
			}
		}

	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen-2; i++) {
			for (size_t j = i+1; j < slen-1; j++) {
				for (size_t k = j+1; k < slen; k++) {
					v[0] = supp[i];
					v[1] = supp[j];
					v[2] = supp[k];
					l = __3perm_glue(
						tgts, tlen, v, 3, tgf, l);
				}
			}
		}
	}
	return;
}

static inline void
__transpose(Lisp_Object arr[], size_t i, size_t j)
	__attribute__((always_inline));
static inline void
__transpose(Lisp_Object arr[], size_t i, size_t j)
{
	/* use xchg assembly? */
	volatile Lisp_Object tmp = arr[i];
	arr[i] = arr[j];
	arr[j] = tmp;
	return;
}

static inline long int
__divmod3(long int *_div_, long int num)
	__attribute__((always_inline));
#if 0
/* idivl uses >48 cycles, which is too slow for division by constants */
static inline long int
__divmod3(long int *_div_, long int num)
{
	/* compute _DIV_ div 3 and _DIV_ mod 3,
	 * store the divisor in `_DIV_', the remainder in `_REM_' */
	long int _rem_;

	*_div_ = num;
	__asm__("idivl %[modulus];  /* eax,edx = eax idivl 3 */\n\t"
		: "=&d" (_rem_), "+%a" (*_div_)
		: [modulus] "rm" (3) : "cc");
	return _rem_;
}
#else
static inline long int
__divmod3(long int *_div_, long int num)
{
	long int rem = num % 3;
	*_div_ = num / 3;
	return rem;
}
#endif

static inline long int
__divmodk(long int *_div_, long int modulus)
	__attribute__((always_inline));
#if 0
static inline long int
__divmodk(long int *_div_, long int modulus)
{
/* compute _DIV_ div MODULUS and _DIV_ mod MODULUS,
 * store the divisor in `_DIV_', the remainder in `_REM_'
 * this assembler version takes ... cycles on x86 and x86_64 processors,
 * however the generated code below seems to be faster -- and is more
 * portable anyway, since it's C */
	long int _rem_ = 0;

	__asm__("idivl %[modulus];  /* eax,edx = eax idivl 3 */\n\t"
		: "=&d" (_rem_), "+%a" (*_div_)
		: [modulus] "rm" (modulus) : "cc");
	return _rem_;
}
#else
static inline long int
__divmodk(long int *_div_, long int modulus)
{
	long int rem = *_div_ % modulus;
	*_div_ /= modulus;
	return rem;
}
#endif

static inline void
__bruhat(Lisp_Object arr[], long int k)
	__attribute__((always_inline));
static inline void
__bruhat(Lisp_Object arr[], long int k)
{
/* computes the k-th transposition in quasi bruhat order and
 * applies it to arr */

	if (UNLIKELY(k == 0)) {
		/* trivial case */
		return;
	} else if (k & 1) {
		/* odd Ks always connote (0,1) */
		__transpose(arr, 0, 1);
		return;
	} else if (__divmod3(&k, (k >>= 1))) {
		/* 1 mod 3 and 2 mod 3 go to (1,2) */
		__transpose(arr, 1, 2);
		return;
	}

	/* otherwise k is 0 mod 3 (and we divided by 3 already)
	 * now we've factored out S_3 already */
	switch (k & 3 /* k % 4 */) {
	case 1:
		__transpose(arr, 2, 3);
		return;
	case 2:
		__transpose(arr, 0, 3);
		return;
	case 3:
		__transpose(arr, 1, 3);
		return;
	default:
		/* k is 0 mod 4 */
		k >>= 2;
	}

	/* S_2, S_3, and S_4 is handled about, go on with S_5 now */
	for (int i = 5; k; i++) {
		long int rem;
		if ((rem = __divmodk(&k, i))) {
			if (i & 1 || (rem -= 2) < 0) {
				/* odd i always induces the
				 * (i-1, i) transposition
				 * in C this is (i-2, i-1) */
				__transpose(arr, i-2, i-1);
			} else {
				/* even i is uglier :(
				 * if rem == 1 -> (i-1, i)
				 * if rem == 2 -> (1, i)
				 * if rem == 3 -> (2, i)
				 * etc. */
				__transpose(arr, rem, i-1);
				/* note: we treated the rem == 1 case above */
			}
			return;
		}
	}
	return;
}

static inline size_t
__Sn_fun(Lisp_Object tgts[], size_t tlen,
	 Lisp_Object supp[], size_t slen,
	 Lisp_Object fun,
	 size_t offset)
	__attribute__((always_inline));
static inline size_t
__Sn_fun(Lisp_Object tgts[], size_t tlen,
	 Lisp_Object supp[], size_t slen,
	 Lisp_Object fun,
	 size_t offset)
{
/* apply FUN on S_n on (the first SLEN elements of) SUPP
 * put results into TGTS
 * assumes that everything is gcpro'd correctly
 * also assumes that tlen == __factorial(slen) */
	Lisp_Object arr[slen+1], *v = &arr[1];

	/* setup, partially unrolled */
	arr[0] = fun;
	v[0] = supp[0];
	v[1] = supp[1];
	v[2] = supp[2];
	for (size_t i = 3; i < slen; i++) {
		v[i] = supp[i];
	}

	/* now we're in the setting ... */
	/* we enter the perm loop now, the first addition is the vector
	 * times identity permutation */
	while (tlen-- > 0) {
		tgts[offset++] = Ffuncall(countof(arr), arr);
		/* permute the working vector */
		__bruhat(v, offset);
	}
	return offset;
}

static inline size_t
__Sn_glue_fun(Lisp_Object tgts[], size_t tlen,
	      Lisp_Object supp[], size_t slen,
	      Lisp_Object fun, glue_f gf,
	      size_t offset)
	__attribute__((always_inline));
static inline size_t
__Sn_glue_fun(Lisp_Object tgts[], size_t tlen,
	      Lisp_Object supp[], size_t slen,
	      Lisp_Object fun, glue_f gf,
	      size_t offset)
{
/* apply FUN on glue of S_n on (the first SLEN elements of) SUPP
 * put results into TGTS
 * assumes that everything is gcpro'd correctly
 * also assumes that tlen == __factorial(slen) */
	Lisp_Object arr[slen+1], *v = &arr[1];

	/* setup, partially unrolled */
	arr[0] = fun;
	v[0] = supp[0];
	v[1] = supp[1];
	v[2] = supp[2];
	for (size_t i = 3; i < slen; i++) {
		v[i] = supp[i];
	}

	/* now we're in the setting ... */
	/* we enter the perm loop now, the first addition is the vector
	 * times identity permutation */
	while (tlen-- > 0) {
		/* backup that first slot */
		volatile Lisp_Object tmp = v[0];
		v[0] = gf(slen, v);
		tgts[offset++] = Ffuncall(2, arr);
		/* recover from backup slot */
		v[0] = tmp;
		/* permute the working vector */
		__bruhat(v, offset);
	}
	return offset;
}

static inline size_t
__Sn_glue(Lisp_Object tgts[], size_t tlen,
	  Lisp_Object supp[], size_t slen,
	  glue_f gf,
	  size_t offset)
	__attribute__((always_inline));
static inline size_t
__Sn_glue(Lisp_Object tgts[], size_t tlen,
	  Lisp_Object supp[], size_t slen,
	  glue_f gf,
	  size_t offset)
{
/* glue of S_n on (the first SLEN elements of) SUPP
 * put results into TGTS
 * assumes that everything is gcpro'd correctly
 * also assumes that tlen == __factorial(slen) */
	Lisp_Object arr[slen];

	/* setup, partially unrolled */
	arr[0] = supp[0];
	arr[1] = supp[1];
	arr[2] = supp[2];
	for (size_t i = 3; i < slen; i++) {
		arr[i] = supp[i];
	}

	/* now we're in the setting ... */
	/* we enter the perm loop now, the first addition is the vector
	 * times identity permutation */
	while (tlen-- > 0) {
		tgts[offset++] = gf(countof(arr), arr);
		/* permute the working vector */
		__bruhat(arr, offset);
	}
	return offset;
}

static inline void		/* inline this? */
_Sn(Lisp_Object tgts[], size_t tlen,
    Lisp_Object supp[], size_t slen,
    Lisp_Object fun, glue_f gf)
	__attribute__((always_inline));
static inline void
_Sn(Lisp_Object tgts[], size_t tlen,
    Lisp_Object supp[], size_t slen,
    Lisp_Object fun, glue_f gf)
{
/* assumes that everything is gcpro'd correctly
 * this is just an intermediate switch, the hard work happens in
 * __Sn_fun(), __Sn_glue_fun() and __Sn_glue() depending on whether
 * just a function and no glue has been specified, a function and a glue
 * function has been specified, or just a glue function has been
 * specified respectively */

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		(void)__Sn_fun(tgts, tlen, supp, slen, fun, 0);
	} else if (LIKELY(!NILP(fun))) {
		(void)__Sn_glue_fun(tgts, tlen, supp, slen, fun, gf, 0);
	} else {
		glue_f tgf = gf ? gf : Flist;
		(void)__Sn_glue(tgts, tlen, supp, slen, tgf, 0);
	}
	return;
}

static void
_comb_Sn(Lisp_Object tgts[], size_t tlen,
	 Lisp_Object supp[], size_t slen,
	 Lisp_Object fun, glue_f gf,
	 size_t arity)
{
/* assumes that everything is gcpro'd correctly
 * this has the same signature as _Sn() but additionally there's the
 * arity argument
 * this is basically the code for variations, i.e. applying the S_m
 * (m < n) on some subset of size m of a set of size n */
	Lisp_Object v[arity];
	size_t idx[arity+1];
	size_t l = 0, np = __factorial(arity);

	/* setup */
	memset(idx, 0, arity*sizeof(long int));

	/* more setup, partially unrolled */
	idx[0] = 0;
	idx[1] = 1;
	idx[2] = 2;
	for (size_t i = 3; i < arity; i++) {
		idx[i] = i;
	}

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < tlen) {
			/* get the combinations, serves as starting set,
			 * partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* do the rain dance */
			l = __Sn_fun(tgts, np, v, arity, fun, l);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVEME*/
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < tlen) {
			/* get the combinations, serves as starting set,
			 * partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* do the rain dance */
			l = __Sn_glue_fun(tgts, np, v, arity, fun, gf, l);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVEME*/
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		while (l < tlen) {
			/* get the combinations, serves as starting set,
			 * partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* do the rain dance */
			l = __Sn_glue(tgts, np, v, arity, tgf, l);
			/* increment, fooking back'n'forth-loop-based
			 * IMPROVEME*/
			(void)__advance_multi_index_comb(idx, slen, arity);
		}
	}
	return;
}


static void
_2cart(Lisp_Object tgts[], size_t tlen,
       Lisp_Object supp[], size_t slen,
       Lisp_Object fun, glue_f gf)
{
/* assumes that everything is gcpro'd properly
 * This function can GC */
	Lisp_Object arr[3] = {fun, Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* apply fun */
				tgts[l++] = Ffuncall(countof(arr), arr);
			}
		}
	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* apply glue */
				arr[1] = gf(2, &arr[1]);
				/* apply fun */
				tgts[l++] = Ffuncall(2, arr);
			}
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				/* set up the array */
				arr[1] = supp[i];
				arr[2] = supp[j];
				/* glue it */
				tgts[l++] = tgf(2, &arr[1]);
			}
		}
	}
	return;
}

static void
_3cart(Lisp_Object tgts[], size_t tlen,
       Lisp_Object supp[], size_t slen,
       Lisp_Object fun, glue_f gf)
{
/* assumes that everything is gcpro'd properly
 * This function can GC */
	Lisp_Object arr[4] = {fun, Qnil, Qnil, Qnil};

	if (LIKELY(!NILP(fun) && gf == NULL)) {
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				for (size_t k = 0; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* apply the fun */
					tgts[l++] = Ffuncall(countof(arr), arr);
				}
			}
		}
	} else if (LIKELY(!NILP(fun))) {
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				for (size_t k = 0; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* glue */
					arr[1] = gf(3, &arr[1]);
					/* apply the fun */
					tgts[l++] = Ffuncall(2, arr);
				}
			}
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		for (size_t i = 0, l = 0; i < slen; i++) {
			for (size_t j = 0; j < slen; j++) {
				for (size_t k = 0; k < slen; k++) {
					/* set up the array */
					arr[1] = supp[i];
					arr[2] = supp[j];
					arr[3] = supp[k];
					/* glue */
					tgts[l++] = tgf(3, &arr[1]);
				}
			}
		}
	}
	return;
}

static void
_ncart(Lisp_Object tgts[], size_t tlen,
       Lisp_Object supp[], size_t slen,
       Lisp_Object fun, glue_f gf,
       size_t arity)
{
/* assumes that everything is gcpro'd properly
 * This function can GC */
	long int idx[arity];	/* the multi-index */
	size_t l = 0;
	Lisp_Object fc[arity+1], *v = &fc[1];

	/* setup */
	memset(idx, 0, arity*sizeof(long int));
	memset(v, 0, arity*sizeof(Lisp_Object));
	fc[0] = fun;

	/* now we're in the setting ... */
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < tlen) {
			/* get the fam data, partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* apply fun */
			tgts[l++] = Ffuncall(countof(fc), fc);
			/* advance the multi-index, partially unrolled */
			__advance_multi_index(idx, arity, slen);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < tlen) {
			/* get the fam data, partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* glue */
			v[0] = gf(arity, v);
			/* apply fun */
			tgts[l++] = Ffuncall(2, fc);
			/* advance the multi-index, partially unrolled */
			__advance_multi_index(idx, arity, slen);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		while (l < tlen) {
			/* get the fam data, partially unrolled */
			v[0] = supp[idx[0]];
			v[1] = supp[idx[1]];
			v[2] = supp[idx[2]];
			for (size_t i = 3; i < arity; i++) {
				v[i] = supp[idx[i]];
			}
			/* glue */
			tgts[l++] = tgf(arity, v);
			/* advance the multi-index, partially unrolled */
			__advance_multi_index(idx, arity, slen);
		}
	}
	return;
}

/* more helpers */
static Lisp_Object
__dress_result(Lisp_Object rtype, Lisp_Object arr[], size_t len)
{
	/* from most likely to least likely */
	if (EQ(rtype, Qlist)) {
		return __Flist(len, arr);
	} else if (EQ(rtype, Qvector)) {
		return Fvector(len, arr);
	} else if (EQ(rtype, Qdllist)) {
		return Fdllist(len, arr);
	} else if (EQ(rtype, Qlitter) || EQ(rtype, Qvoid)) {
		return Qt;
	} else if (EQ(rtype, Qinplace)) {
		return Qt;
	} else if (EQ(rtype, Qstring)) {
		return Fstring(len, arr);
	} else if (EQ(rtype, Qbit_vector)) {
		return Fbit_vector(len, arr);
	} else if (EQ(rtype, Qconcat)) {
		return Fconcat(len, arr);
	}
	return Qnil;
}

static inline size_t
__explode_1dict(Lisp_Object *restrict tkeys, Lisp_Object *restrict tvals,
		Lisp_Object dict, size_t len)
	__attribute__((always_inline));
static inline size_t
__explode_1dict(Lisp_Object *restrict tkeys, Lisp_Object *restrict tvals,
		Lisp_Object dict, size_t len)
{
	size_t i = 0;
	dict_t d = (dict_t)(void*)dict;
	struct dict_iter_s _di, *di = &_di;

	dict_iter_init(d, di);

	while (1) {
		Lisp_Object key, val;
		dict_iter_next(di, &key, &val);
		if (LIKELY(key != Qnull_pointer)) {
			tkeys[i] = key;
			tvals[i] = val;
			i++;
		} else {
			break;
		}
	}

	dict_iter_fini(di);
	return i;
}

static Lisp_Object
__comb_1seq(Lisp_Object seq, Lisp_Object fun, size_t arity,
	    glue_f gluef, Lisp_Object result_type)
{
	size_t fs = __fam_size(seq);
	size_t nc = __ncombinations(fs, arity != -1UL ? arity : (arity = fs));
	/* C99 we need you */
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nc * 3 + fs < maxsz
		? nc + fs	/* actually we just need nc + arity - 1 */
		: 0;
	Lisp_Object __vals[leni], *vals, *rvals, result;
	int speccnt = 0;
	struct gcpro gcpro1;

	if (UNLIKELY(arity == 0 || nc == 0)) {
		/* expherts only */
		return __dress_result(result_type, NULL, 0);
	}

	if (UNLIKELY(leni == 0)) {
		speccnt = specpdl_depth();
		vals = xnew_array(Lisp_Object, nc + fs);
		record_unwind_protect(free_malloced_ptr, make_opaque_ptr(vals));
	} else {
		vals = __vals;
	}

	/* explode the sequence */
	memset(vals, 0, nc * sizeof(Lisp_Object));
	(void)seq_explode((void*restrict*)&vals[nc], fs, (seq_t)seq);

	GCPROn(vals, nc+fs);
	switch (arity) {
	case 1:
		/* the same as pntw mode */
		/* expherts only */
		if (UNLIKELY(NILP(fun) || nc == 0UL)) {
			rvals = &vals[nc];
			break;
		}

		for (size_t i = nc; i < nc + fs; i++) {
			Lisp_Object args[2] = {fun, vals[i]};
			vals[i] = Ffuncall(2, args);
		}
		rvals = &vals[nc];
		break;
	case 2:
		__2comb(vals, nc, &vals[nc], fs, fun, gluef);
		rvals = vals;
		break;
	case 3:
		__3comb(vals, nc, &vals[nc], fs, fun, gluef);
		rvals = vals;
		break;
	default:
		__ncomb(vals, nc, &vals[nc], fs, fun, gluef, arity);
		rvals = vals;
		break;
	}
	result = __dress_result(result_type, rvals, nc);
	UNGCPRO;
	if (UNLIKELY(leni == 0)) {
		unbind_to(speccnt, Qnil);
	}
	return result;
}

static Lisp_Object
__perm_1seq(Lisp_Object seq, Lisp_Object fun, size_t arity,
	    glue_f gluef, Lisp_Object result_type)
{
	size_t fs = __fam_size(seq);
	size_t nv = __nvariations(fs, arity != -1UL ? arity : (arity = fs));
	/* C99 we need you */
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nv * 3 < maxsz
		? nv + fs
		: 0;
	Lisp_Object __vals[leni], *vals, *rvals = NULL, result;
	int speccnt = 0;
	struct gcpro gcpro1;

	if (UNLIKELY(leni == 0)) {
		speccnt = specpdl_depth();
		vals = xnew_array(Lisp_Object, nv + fs);
		record_unwind_protect(free_malloced_ptr, make_opaque_ptr(vals));
	} else {
		vals = __vals;
	}

	if (UNLIKELY(arity == 0)) {
		/* expherts only */
		return __dress_result(result_type, NULL, 0);
	}

	/* explode the sequence */
	memset(vals, 0, (nv) * sizeof(Lisp_Object));
	(void)seq_explode((void*restrict*)&vals[nv], fs, (seq_t)seq);

	GCPROn(vals, nv + fs);
	switch (arity) {
	case 1:
		/* the same as pntw mode */
		/* expherts only */
		if (UNLIKELY(NILP(fun) || nv == 0UL)) {
			rvals = &vals[nv];
			break;
		}

		for (size_t i = nv; i < nv+fs; i++) {
			Lisp_Object args[2] = {fun, vals[i]};
			vals[i] = Ffuncall(2, args);
		}
		rvals = &vals[nv];
		break;
	case 2:
		_comb_2perm(vals, nv, &vals[nv], fs, fun, gluef);
		rvals = vals;
		break;
	case 3:
		_comb_3perm(vals, nv, &vals[nv], fs, fun, gluef);
		rvals = vals;
		break;
	default:
		if (LIKELY(fs != arity)) {
			_comb_Sn(vals, nv, &vals[nv], fs, fun, gluef, arity);
		} else {
			/* optimised for mere permutations */
			_Sn(vals, nv, &vals[nv], fs /*== arity*/, fun, gluef);
		}
		rvals = vals;
		break;
	}
	result = __dress_result(result_type, rvals, nv);
	UNGCPRO;
	if (UNLIKELY(leni == 0)) {
		unbind_to(speccnt, Qnil);
	}
	return result;
}

static Lisp_Object
__cart_1seq(Lisp_Object seq, Lisp_Object fun, size_t arity,
	    glue_f gluef, Lisp_Object result_type)
{
	size_t fs = __fam_size(seq);
	size_t nc = __ncart(fs, arity);
	/* C99 we need you */
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nc * 3 < maxsz
		? nc
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	int speccnt = 0;
	struct gcpro gcpro1;

	if (UNLIKELY(arity == 0)) {
		/* expherts only */
		return __dress_result(result_type, NULL, 0);
	}

	if (UNLIKELY(leni == 0)) {
		speccnt = specpdl_depth();
		vals = xnew_array(Lisp_Object, nc);
		record_unwind_protect(free_malloced_ptr, make_opaque_ptr(vals));
	} else {
		vals = __vals;
	}

	/* explode the sequence */
	memset(vals, 0, (nc - fs) * sizeof(Lisp_Object));
	seq_explode((void*restrict*)&vals[nc - fs], fs, (seq_t)seq);

	GCPROn(vals, nc);
	switch (arity) {
	case 1:
		/* the same as pntw mode */
		/* expherts only */
		if (UNLIKELY(NILP(fun) || nc == 0UL)) {
			break;
		}

		for (size_t i = 0; i < nc; i++) {
			Lisp_Object args[2] = {fun, vals[i]};
			vals[i] = Ffuncall(2, args);
		}
		break;
	case 2:
		_2cart(vals, nc, &vals[nc-fs], fs, fun, gluef);
		break;
	case 3:
		_3cart(vals, nc, &vals[nc-fs], fs, fun, gluef);
		break;
	default:
		_ncart(vals, nc, &vals[nc-fs], fs, fun, gluef, arity);
		break;
	}
	result = __dress_result(result_type, vals, nc);
	UNGCPRO;
	if (UNLIKELY(leni == 0)) {
		unbind_to(speccnt, Qnil);
	}
	return result;
}

static Lisp_Object
__pntw_1seq(Lisp_Object seq, Lisp_Object fun, size_t arity,
	    glue_f gluef, Lisp_Object result_type,
	    struct decoration_s deco)
{
	size_t nseq = __fam_size(seq);
	/* C99 we need you */
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t totlen = nseq + 2 /* for ini and ter */ + (deco.sep ? nseq : 0);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nseq * 3 < maxsz
		? totlen
		: 0;
	size_t len = 0;
	Lisp_Object __vals[leni+1], *vals, *seqelts, result;

	if (arity > nseq) {
		/* expherts alarm */
		return __dress_result(result_type, NULL, 0);
	}
	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, totlen);
	} else {
		vals = __vals;
	}

	/* start maybe with the initiator */
	if (UNLIKELY(deco.ini != Qnull_pointer)) {
		vals[len++] = deco.ini;
	}
	/* explode the sequence */
	if (LIKELY(deco.sep == Qnull_pointer)) {
		seqelts = &vals[len];
	} else {
		seqelts = vals + (deco.sep ? nseq : 0);
		memset(&vals[len], 0, sizeof(Lisp_Object) * nseq);
	}
	(void)seq_explode((void*restrict*)seqelts, nseq, (seq_t)seq);

	/* fill the rest with naughts */
	memset(&seqelts[nseq], 0, (totlen - len - nseq) * sizeof(Lisp_Object));

	if (NILP(fun)) {
		switch (arity) {
		case 1:
			if (deco.sep != Qnull_pointer) {
				/* weave */
				for (size_t i = 0; i < nseq; i++) {
					vals[len++] = seqelts[i];
					vals[len++] = deco.sep;
				}
				/* because we dont want the last element to
				 * be followed by a separator */
				len--;
			} else {
				len = nseq;
			}
			break;
		case 2:
			/* condense the stuff */
			for (size_t i = 0, bar = nseq & -2;
			     /* traverse to the previous even number */
			     i < bar;  i += 2) {
				vals[len++] = gluef
					? gluef(2, &seqelts[i])
					: list2(seqelts[i], seqelts[i+1]);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* strike the last separator */
				len--;
			}
			break;
		case 3:
			/* condense the stuff */
			for (size_t i = 0;
			     /* traverse to the last 3-divisible index */
			     i+3 <= nseq; i += 3) {
				vals[len++] = gluef
					? gluef(3, &seqelts[i])
					: list3(seqelts[i],
						seqelts[i+1],
						seqelts[i+2]);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* strike the last separator */
				len--;
			}
			break;
		default:
			/* condense the stuff */
			for (int i = 0;
			     /* traverse to the last sane index */
			     i+arity <= nseq; i += arity) {
				vals[len++] = gluef
					? gluef(arity, &seqelts[i])
					: Flist(arity, &seqelts[i]);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* kick the last one */
				len--;
			}
		}
	} else {
		struct gcpro gcpro1;

		switch (arity) {
		case 1:
			GCPROn(vals, totlen);

			for (size_t i = 0; i < nseq; i++) {
				Lisp_Object args[2] = {fun, seqelts[i]};
				vals[len++] = Ffuncall(2, args);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* strike the last separator */
				len--;
			}

			UNGCPRO;
			break;

		case 2:
			GCPROn(vals, totlen);

			for (size_t i = 0, bar = nseq & -2;
			     /* traverse to the last even index */
			     i < bar; i += 2) {
				Lisp_Object args[3] = {fun, seqelts[i], seqelts[i+1]};
				vals[len++] = Ffuncall(countof(args), args);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* strike the last separator */
				len--;
			}

			UNGCPRO;
			break;

		case 3:
			GCPROn(vals, len);

			for (size_t i = 0;
			     /* traverse to the last 3-divisible index */
			     i+3 <= nseq; i += 3) {
				Lisp_Object args[4] = {
					fun, seqelts[i], seqelts[i+1], 
					seqelts[i+2]};
				vals[len++] = Ffuncall(countof(args), args);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* strike the last separator */
				len--;
			}

			UNGCPRO;
			break;

		default:
			GCPROn(vals, len);

			for (size_t i = 0;
			     /* traverse to the last 3-divisible index */
			     i+arity <= nseq; i += arity) {
				Lisp_Object args[arity+1];

				args[0] = fun;
				args[1] = seqelts[i];
				args[2] = seqelts[i+1];
				args[3] = seqelts[i+2];
				args[4] = seqelts[i+3];
				for (size_t j = 4; j < arity; j++) {
					args[j+1] = seqelts[i+j];
				}
				vals[len++] = Ffuncall(countof(args), args);
				if (UNLIKELY(deco.sep != Qnull_pointer)) {
					/* add separator */
					vals[len++] = deco.sep;
				}
			}
			if (UNLIKELY(deco.sep != Qnull_pointer)) {
				/* kick the last one */
				len--;
			}

			UNGCPRO;
			break;
		}
	}
	/* top off with the terminator */
	if (UNLIKELY(deco.ter != Qnull_pointer)) {
		vals[len++] = deco.ter;
	}

	result = __dress_result(result_type, vals, len);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__pntw_1dict(Lisp_Object dict, Lisp_Object fun,
	     glue_f gluef, Lisp_Object result_type)
{
	/* basically like maphash/mapskiplist */
	size_t ndict = dict_size((dict_t)(void*)dict);
	/* C99 we need you */
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		ndict * 6 < maxsz
		? ndict
		: 0;
	size_t len;
	Lisp_Object __keys[leni], __vals[leni], *keys, *vals, result;

	if (UNLIKELY(leni == 0)) {
		keys = xnew_array(Lisp_Object, 2 * ndict);
		vals = &keys[ndict];
	} else {
		keys = __keys;
		vals = __vals;
	}

	/* explode the sequence */
	len = __explode_1dict(keys, vals, dict, ndict);

	if (LIKELY(!NILP(fun) && len > 0UL)) {
		struct gcpro gcpro1, gcpro2;

		GCPRO1n(dict, vals, len);

		for (size_t i = 0; i < len; i++) {
			Lisp_Object args[3] = {fun, keys[i], vals[i]};
			vals[i] = Ffuncall(countof(args), args);
		}

		UNGCPRO;
	} else {
		for (size_t i = 0; i < len; i++) {
			Lisp_Object args[2] = {keys[i], vals[i]};
			vals[i] = gluef
				? gluef(countof(args), args)
				: Flist(countof(args), args);
		}
	}

	result = __dress_result(result_type, vals, len);
	if (UNLIKELY(leni == 0)) {
		xfree(keys);
	}
	return result;
}

static Lisp_Object
__pntw_nseq(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun,
	    glue_f gluef, Lisp_Object result_type)
{
/* defaults to arity 1,1,...,1 */
	size_t nmin = __nfam_min_size(seqs, nseqs);
	/* C99 we need you */
	struct seq_iter_s its[nseqs];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nmin * 3 < maxsz
		? nmin
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nmin);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nmin * sizeof(Lisp_Object));
	/* initialise the iterators */
	for (size_t i = 0; i < nseqs; i++) {
		seq_iter_init((seq_t)seqs[i], &its[i]);
	}

	GCPRO1nn(fun, vals, nmin, seqs, nseqs);
	if (UNLIKELY(NILP(fun))) {
		for (size_t i = 0; i < nmin; i++) {
			Lisp_Object args[nseqs];

			/* unroll */
			seq_iter_next(&its[0], (void**)&args[0]);
			/* and one more */
			seq_iter_next(&its[1], (void**)&args[1]);
			/* ... and the rest */
			for (size_t j = 2; j < nseqs; j++) {
				seq_iter_next(&its[j], (void**)&args[j]);
			}
			vals[i] = gluef
				? gluef(countof(args), args)
				: Flist(countof(args), args);
		}
	} else {
		for (size_t i = 0; i < nmin; i++) {
			Lisp_Object args[nseqs+1];

			/* unroll */
			seq_iter_next(&its[0], (void**)&args[1]);
			/* and one more */
			seq_iter_next(&its[1], (void**)&args[2]);
			/* ... and the rest */
			for (size_t j = 2; j < nseqs; j++) {
				seq_iter_next(&its[j], (void**)&args[j+1]);
			}
			args[0] = fun;
			vals[i] = Ffuncall(countof(args), args);
		}
	}
	UNGCPRO;

	/* deinitialise the iterators */
	for (size_t i = 0; i < nseqs; i++) {
		seq_iter_fini(&its[i]);
	}

	result = __dress_result(result_type, vals, nmin);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static inline size_t
__arity_cross_sum(size_t arity[], size_t narity)
{
	size_t res = arity[0];
	for (size_t j = 1; j < narity; j++) {
		res += arity[j];
	}
	return res;
}

static inline void
__explode_n(seq_iter_t si, void *tgt[], size_t n)
{
/* explodes the sequence in SI N times, puts the stuff into tgt,
 * consequently tgt[] is N elements richer thereafter */

	seq_iter_next(si, &tgt[0]);
	for (size_t j = 1; j < n; j++) {
		seq_iter_next(si, &tgt[j]);
	}
	return;
}

static Lisp_Object
__pntw_nseq2(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun,
	     glue_f gluef, Lisp_Object result_type, size_t arity[])
{
	size_t nmin = __nfam_min_size_a(seqs, nseqs, arity);
	/* C99 we need you */
	struct seq_iter_s its[nseqs];
	size_t aXsum = __arity_cross_sum(arity, nseqs);
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nmin * 3 < maxsz
		? nmin
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nmin);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nmin * sizeof(Lisp_Object));
	/* initialise the iterators */
	for (size_t i = 0; i < nseqs; i++) {
		seq_iter_init((seq_t)seqs[i], &its[i]);
	}

	GCPRO1nn(fun, vals, nmin, seqs, nseqs);
	if (UNLIKELY(NILP(fun))) {
		for (size_t i = 0; i < nmin; i++) {
			Lisp_Object args[aXsum];
			size_t off, j;

			/* partially unroll this, as we know that it's
			 * definitely one seq to consider */
			__explode_n(&its[0], (void**)&args[0], off = arity[0]);
			/* ... actually we know it's even more than one
			 * seq otherwise we'd be in the 1seq counterpart
			 * of this */
			__explode_n(&its[1], (void**)&args[off], arity[1]);
			for (j = 2, off += arity[1];
			     j < nseqs; off += arity[j++]) {
				__explode_n(
					&its[j], (void**)&args[off], arity[j]);
			}
			vals[i] = gluef
				? gluef(countof(args), args)
				: Flist(countof(args), args);
		}
	} else {
		for (size_t i = 0; i < nmin; i++) {
			Lisp_Object args[aXsum+1];
			size_t off, j;

			/* partially unroll this, as we know that it's
			 * definitely one seq to consider */
			__explode_n(&its[0], (void**)&args[1], off = arity[0]);
			/* ... actually we know it's even more than one
			 * seq otherwise we'd be in the 1seq counterpart
			 * of this */
			__explode_n(&its[1], (void**)&args[++off], arity[1]);
			for (j = 2, off += arity[1];
			     j < nseqs; off += arity[j++]) {
				__explode_n(
					&its[j], (void**)&args[off], arity[j]);
			}
			args[0] = fun;
			vals[i] = Ffuncall(countof(args), args);
		}
	}
	UNGCPRO;

	/* deinitialise the iterators */
	for (size_t i = 0; i < nseqs; i++) {
		seq_iter_fini(&its[i]);
	}

	result = __dress_result(result_type, vals, nmin);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__cart_nseq(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun, size_t arity,
	    glue_f gf, Lisp_Object result_type)
{
/* defaults to arity 1,1,...,1
 * there is no __comb_nseq() as combinations are defined to be
 * (cart (comb s1) (comb s2) ...), so in the arity 1,1,...,1 case this
 * equals __cart_nseq() */
	size_t nseqsz[nseqs];
	size_t nsum, ncart, l = 0;
	size_t nsz = __nfam_cart_sum_size(&nsum, &ncart, nseqsz, seqs, nseqs);
	/* C99 we need you */
	Lisp_Object *expls[nseqs];
	long int idx[nseqs]; /* the multi index */
	Lisp_Object fc[nseqs+1], *v = &fc[1];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nsz * 3 < maxsz
		? nsz
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* catch some horst cases */
	if (ncart == 0) {
		return __dress_result(result_type, NULL, 0);
	} /* actually now we ought to catch the case ncart == nsum
	   * which is nseqs == 1 */

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nsz);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nsz * sizeof(Lisp_Object));
	/* initialise the explosion pointers */
	expls[0] = &vals[ncart];
	seq_explode((void**)expls[0], nseqsz[0], (seq_t)seqs[0]);
	expls[1] = expls[0] + nseqsz[0];
	seq_explode((void**)expls[1], nseqsz[1], (seq_t)seqs[1]);
	for (size_t i = 2; i < nseqs; i++) {
		expls[i] = expls[i-1] + nseqsz[i-1];
		seq_explode((void**)expls[i], nseqsz[i], (seq_t)seqs[i]);
	}
	/* setup multiindex */
	memset(idx, 0, nseqs * sizeof(long int));
	fc[0] = fun;

	GCPRO1nn(fun, vals, nsz, seqs, nseqs);
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < ncart) {
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			v[1] = expls[1][idx[1]];
			for (size_t i = 2; i < nseqs; i++) {
				v[i] = expls[i][idx[i]];
			}
			/* apply fun */
			vals[l++] = Ffuncall(countof(fc), fc);
			/* advance the multi-index */
			__advance_multi_index_2(idx, countof(idx), nseqsz);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < ncart) {
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			v[1] = expls[1][idx[1]];
			for (size_t i = 2; i < nseqs; i++) {
				v[i] = expls[i][idx[i]];
			}
			/* glue */
			v[0] = gf(countof(idx), v);
			/* apply fun */
			vals[l++] = Ffuncall(2, fc);
			/* advance the multi-index */
			__advance_multi_index_2(idx, countof(idx), nseqsz);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		while (l < ncart) {
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			v[1] = expls[1][idx[1]];
			for (size_t i = 2; i < nseqs; i++) {
				v[i] = expls[i][idx[i]];
			}
			/* glue */
			vals[l++] = tgf(countof(idx), v);
			/* advance the multi-index */
			__advance_multi_index_2(idx, countof(idx), nseqsz);
		}
	}
	UNGCPRO;

	result = __dress_result(result_type, vals, ncart);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__cart_nseq2(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun,
	     glue_f gf, Lisp_Object result_type, size_t arity[])
{
	size_t nseqsz[nseqs];
	size_t nsum, ncart, midxsz /* size of the multi index */, l = 0;
	size_t nsz = __nfam_cart_sum_size_a(
		&nsum, &ncart, &midxsz, nseqsz, seqs, nseqs, arity);
	/* C99 we need you */
	Lisp_Object *expls[nseqs];
	long int idx[midxsz]; /* the multi index */
	Lisp_Object fc[midxsz+1], *v = &fc[1];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nsz * 3 < maxsz
		? nsz
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* catch some horst cases */
	if (ncart == 0) {
		return __dress_result(result_type, NULL, 0);
	} /* actually now we ought to catch the case ncart == nsum
	   * which is nseqs == 1 */

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nsz);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nsz * sizeof(Lisp_Object));
	/* initialise the explosion pointers */
	expls[0] = &vals[ncart];
	seq_explode((void**)expls[0], nseqsz[0], (seq_t)seqs[0]);
	expls[1] = expls[0] + nseqsz[0];
	seq_explode((void**)expls[1], nseqsz[1], (seq_t)seqs[1]);
	for (size_t i = 2; i < nseqs; i++) {
		expls[i] = expls[i-1] + nseqsz[i-1];
		seq_explode((void**)expls[i], nseqsz[i], (seq_t)seqs[i]);
	}
	/* setup multiindex */
	memset(idx, 0, countof(idx) * sizeof(long int));
	fc[0] = fun;

	GCPRO1nn(fun, vals, nsz, seqs, nseqs);
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < ncart) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][idx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][idx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][idx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][idx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][idx[slot]];
				}
			}
			/* apply fun */
			vals[l++] = Ffuncall(countof(fc), fc);
			/* advance the multi-index */
			__advance_multi_index_3(
				idx, countof(idx), nseqsz, nseqs, arity);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < ncart) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][idx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][idx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][idx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][idx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][idx[slot]];
				}
			}
			/* glue */
			v[0] = gf(countof(idx), v);
			/* apply fun */
			vals[l++] = Ffuncall(2, fc);
			/* advance the multi-index */
			__advance_multi_index_3(
				idx, countof(idx), nseqsz, nseqs, arity);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;
		while (l < ncart) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][idx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][idx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][idx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][idx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][idx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][idx[slot]];
				}
			}
			/* glue */
			vals[l++] = tgf(countof(idx), v);
			/* advance the multi-index */
			__advance_multi_index_3(
				idx, countof(idx), nseqsz, nseqs, arity);
		}
	}
	UNGCPRO;

	result = __dress_result(result_type, vals, ncart);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__comb_nseq2(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun,
	     glue_f gf, Lisp_Object result_type, size_t arity[])
{
/* this is the dodgiest one, since
 * comb(seq1, seq2, ..., seqn) => cart(comb(seq1), comb(seq2), ..., comb(seqn))
 */
	size_t nseqsz[nseqs];
	size_t nsum, ncomb, midxsz /* size of the multi index */, l = 0;
	/* computes the size of the cartesian set, the maximum size of
	 * the union set and the multiplicity of the multi-index (which is the
	 * cross sum of the arity array) returns the sum of cartesian and union,
	 * and puts intermediately computed family sizes into nseqsz[] */
	size_t nsz = __nfam_comb_sum_size_a(
		&nsum, &ncomb, &midxsz, nseqsz, seqs, nseqs, arity);
	/* C99 we need you */
	Lisp_Object *expls[nseqs];
	/* the multi indices, we have a big one, and a custom one */
	size_t __midx[midxsz], *midx[nseqs]; /* the multi indices */
	Lisp_Object fc[midxsz+1], *v = &fc[1];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nsz * 3 < maxsz
		? nsz
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* catch some horst cases */
	if (ncomb == 0) {
		return __dress_result(result_type, NULL, 0);
	} /* actually now we ought to catch the case ncart == nsum
	   * which is nseqs == 1 */

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nsz);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nsz * sizeof(Lisp_Object));
	/* initialise the explosion pointers and ... */
	expls[0] = &vals[ncomb];
	seq_explode((void**)expls[0], nseqsz[0], (seq_t)seqs[0]);
	expls[1] = expls[0] + nseqsz[0];
	seq_explode((void**)expls[1], nseqsz[1], (seq_t)seqs[1]);
	/* ... the multi-multi-index */
	midx[0] = &__midx[0];
	__initialise_multi_index(midx[0], arity[0]);
	midx[1] = &__midx[arity[0]];
	__initialise_multi_index(midx[1], arity[1]);
	/* and the rest of the explosion pointers, gosh, that's going
	 * to be an Index War */
	for (size_t i = 2; i < nseqs; i++) {
		expls[i] = expls[i-1] + nseqsz[i-1];
		seq_explode((void**)expls[i], nseqsz[i], (seq_t)seqs[i]);
		midx[i] = &__midx[arity[i-1]];
		__initialise_multi_index(midx[i], arity[i]);
	}
	/* further setup */
	fc[0] = fun;

	GCPRO1nn(fun, vals, nsz, seqs, nseqs);
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < ncomb) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* apply fun */
			vals[l++] = Ffuncall(countof(fc), fc);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < ncomb) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* glue */
			v[0] = gf(countof(__midx), v);
			/* apply fun */
			vals[l++] = Ffuncall(2, fc);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;

		while (l < ncomb) {
			size_t slot;

			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* glue */
			vals[l++] = tgf(countof(__midx), v);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	}
	UNGCPRO;

	result = __dress_result(result_type, vals, ncomb);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__perm_nseq(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun, size_t arity,
	    glue_f gf, Lisp_Object result_type)
{
/* defaults to arity 1,1,...,1 */
	size_t nseqsz[nseqs];
	size_t ns, ncp, np, l = 0;
	size_t nsz = __nfam_perm_sum_size(&ns, &ncp, &np, nseqsz, seqs, nseqs);
	/* C99 we need you */
	Lisp_Object *expls[nseqs];
	long int idx[nseqs]; /* the multi index */
	Lisp_Object fc[nseqs+1], *v = &fc[1];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nsz * 3 < maxsz
		? nsz
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* catch some horst cases */
	if (ncp == 0) {
		return __dress_result(result_type, NULL, 0);
	} /* actually now we ought to catch the case nperm == nsum
	   * which is nseqs == 1 */

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nsz);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nsz * sizeof(Lisp_Object));
	/* initialise the explosion pointers */
	expls[0] = &vals[ncp];
	seq_explode((void**)expls[0], nseqsz[0], (seq_t)seqs[0]);
	expls[1] = expls[0] + nseqsz[0];
	seq_explode((void**)expls[1], nseqsz[1], (seq_t)seqs[1]);
	for (size_t i = 2; i < nseqs; i++) {
		expls[i] = expls[i-1] + nseqsz[i-1];
		seq_explode((void**)expls[i], nseqsz[i], (seq_t)seqs[i]);
	}
	/* setup multiindex */
	memset(idx, 0, nseqs * sizeof(long int));
	fc[0] = fun;

	GCPRO1nn(fun, vals, nsz, seqs, nseqs);
	switch (nseqs) {
	case 2:
		if (LIKELY(!NILP(fun) && gf == NULL)) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				l = __2perm_fun(vals, 2, v, 2, fun, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 2, nseqsz);
			}

		} else if (LIKELY(!NILP(fun))) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				l = __2perm_glue_fun(vals, 2, v, 2, fun, gf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 2, nseqsz);
			}

		} else {
			glue_f tgf = gf ? gf : Flist;
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				l = __2perm_glue(vals, 2, v, 2, tgf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 2, nseqsz);
			}
		}
		break;

	case 3:
		if (LIKELY(!NILP(fun) && gf == NULL)) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				v[2] = expls[2][idx[2]];
				l = __3perm_fun(vals, 0, v, 3, fun, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 3, nseqsz);
			}
		} else if (LIKELY(!NILP(fun))) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				v[2] = expls[2][idx[2]];
				l = __3perm_glue_fun(vals, 0, v, 3, fun, gf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 3, nseqsz);
			}
		} else {
			glue_f tgf = gf ? gf : Flist;
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				v[2] = expls[2][idx[2]];
				l = __3perm_glue(vals, 0, v, 3, tgf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, 3, nseqsz);
			}
		}
		break;

	default:
		if (LIKELY(!NILP(fun) && gf == NULL)) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				for (size_t i = 2; i < nseqs; i++) {
					v[i] = expls[i][idx[i]];
				}
				/* have Sn operating */
				l = __Sn_fun(vals, np, v, nseqs, fun, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, nseqs, nseqsz);
			}
		} else if (LIKELY(!NILP(fun))) {
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				for (size_t i = 2; i < nseqs; i++) {
					v[i] = expls[i][idx[i]];
				}
				/* have Sn operating */
				l = __Sn_glue_fun(
					vals, np, v, nseqs, fun, gf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, nseqs, nseqsz);
			}
		} else {
			glue_f tgf = gf ? gf : Flist;
			while (l < ncp) {
				/* fetch the data from the explosions */
				v[0] = expls[0][idx[0]];
				v[1] = expls[1][idx[1]];
				for (size_t i = 2; i < nseqs; i++) {
					v[i] = expls[i][idx[i]];
				}
				/* have Sn operating */
				l = __Sn_glue(vals, np, v, nseqs, tgf, l);
				/* advance the multi-index */
				__advance_multi_index_2(idx, nseqs, nseqsz);
			}
		}
		break;
	}
	UNGCPRO;

	result = __dress_result(result_type, vals, ncp);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}

static Lisp_Object
__perm_nseq2(Lisp_Object seqs[], size_t nseqs, Lisp_Object fun,
	     glue_f gf, Lisp_Object result_type, size_t arity[])
{
/* this is the utmost dodgiest one, since
 * perm(seq1, seq2, ..., seqn) => perm(comb(seq1, seq2, ..., seqn))
 */
	size_t nseqsz[nseqs];
	size_t nsum, nvar, nperm, midxsz /* size of the multi index */, l = 0;
	/* computes the size of the cartesian set, the maximum size of
	 * the union set and the multiplicity of the multi-index (which is the
	 * cross sum of the arity array) returns the sum of cartesian and union,
	 * and puts intermediately computed family sizes into nseqsz[] */
	size_t nsz = __nfam_perm_sum_size_a(
		&nsum, &nvar, &nperm, &midxsz, nseqsz, seqs, nseqs, arity);
	/* C99 we need you */
	Lisp_Object *expls[nseqs];
	/* the multi indices, we have a big one, and a custom one */
	size_t __midx[midxsz], *midx[nseqs]; /* the multi indices */
	Lisp_Object v[midxsz + 2 /*to survive the aggressive loop unrolling*/];
	size_t maxsz = __sys_stk_free() / sizeof(Lisp_Object);
	size_t leni =
		/* leave room for stuff after us,
		 * we call a function on this, so leave plenty of space */
		nsz * 3 < maxsz
		? nsz
		: 0;
	Lisp_Object __vals[leni], *vals, result;
	struct gcpro gcpro1, gcpro2, gcpro3;

	/* catch some horst cases */
	if (nvar == 0) {
		return __dress_result(result_type, NULL, 0);
	} /* actually now we ought to catch the case ncart == nsum
	   * which is nseqs == 1 */

	if (UNLIKELY(leni == 0)) {
		vals = xnew_array(Lisp_Object, nsz);
	} else {
		vals = __vals;
	}

	/* initialise the value space */
	memset(vals, 0, nsz * sizeof(Lisp_Object));
	/* initialise the explosion pointers and ... */
	expls[0] = &vals[nvar];
	seq_explode((void**)expls[0], nseqsz[0], (seq_t)seqs[0]);
	expls[1] = expls[0] + nseqsz[0];
	seq_explode((void**)expls[1], nseqsz[1], (seq_t)seqs[1]);
	/* ... the multi-multi-index */
	midx[0] = &__midx[0];
	__initialise_multi_index(midx[0], arity[0]);
	midx[1] = &__midx[arity[0]];
	__initialise_multi_index(midx[1], arity[1]);
	/* ... the multi-multi-index */
	midx[0] = &__midx[0];
	__initialise_multi_index(midx[0], arity[0]);
	/* and the rest of the explosion pointers, gosh, that's going
	 * to be an Index War */
	for (size_t i = 2; i < nseqs; i++) {
		expls[i] = expls[i-1] + nseqsz[i-1];
		seq_explode((void**)expls[i], nseqsz[i], (seq_t)seqs[i]);
		midx[i] = &__midx[arity[i-1]];
		__initialise_multi_index(midx[i], arity[i]);
	}

	GCPRO1nn(fun, vals, nsz, seqs, nseqs);
	/* actually we would have to distinguish between cross_sum(arity) >= 4
	 * and == 3 and == 2, because the __Sn functions unroll at least 3
	 * iterations, howbeit it seems to work so we stick with this for now */
	if (LIKELY(!NILP(fun) && gf == NULL)) {
		while (l < nvar) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* do the rain dance */
			l = __Sn_fun(vals, nperm, v, midxsz, fun, l);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	} else if (LIKELY(!NILP(fun))) {
		while (l < nvar) {
			size_t slot;
			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* do the rain dance */
			l = __Sn_glue_fun(vals, nperm, v, midxsz, fun, gf, l);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	} else {
		glue_f tgf = gf ? gf : Flist;

		while (l < nvar) {
			size_t slot;

			/* fetch the data from the explosions, p-unrolled */
			v[0] = expls[0][__midx[0]];
			for (slot = 1; slot < arity[0]; slot++) {
				/* offload arity[0] slots onto v */
				v[slot] = expls[0][__midx[slot]];
			}
			/* continue with the next arity[1] slots */
			v[slot] = expls[1][__midx[slot]];
			slot++;
			for (size_t j = 1; j < arity[1]; slot++, j++) {
				v[slot] = expls[1][__midx[slot]];
			}
			/* now the rest of the crowd */
			for (size_t i = 2; i < nseqs; i++) {
				v[slot] = expls[i][__midx[slot]];
				slot++;
				for (size_t j = 1; j < arity[i]; slot++, j++) {
					v[slot] = expls[i][__midx[slot]];
				}
			}
			/* do the rain dance */
			l = __Sn_glue(vals, nperm, v, midxsz, tgf, l);
			/* advance the multi-index */
			__advance_multi_index_4(midx, nseqsz, nseqs, arity);
		}
	}
	UNGCPRO;

	result = __dress_result(result_type, vals, nvar);
	if (UNLIKELY(leni == 0)) {
		xfree(vals);
	}
	return result;
}


static inline glue_f
_obtain_glue(Lisp_Object glue)
	__attribute__((always_inline));
static inline glue_f
_obtain_glue(Lisp_Object glue)
{
	if (EQ(glue, Qlist)) {
		return __Flist;
	} else if (EQ(glue, Qdllist)) {
		return Fdllist;
	} else if (EQ(glue, Qvector)) {
		return Fvector;
	} else if (EQ(glue, Qstring)) {
		return Fstring;
	} else if (EQ(glue, Qconcat)) {
		return Fconcat;
	} else {
		return NULL;
	}
}

static inline int
_maybe_downgrade(Lisp_Object *arity)
{
	bool downgrade = !NILP(*arity) && CONSP(*arity);
	int i = 0;

	for (Lisp_Object tmp = *arity; CONSP(tmp); i++, tmp = XCDR(tmp)) {
		if (UNLIKELY(!NATNUMP(XCAR(tmp)) || XCAR(tmp) == Qzero)) {
			signal_simple_error(
				":arity does not specify a valid multi-index",
				*arity);
		} else if (XCAR(tmp) != Qone) {
			downgrade = false;
		}
	}
	if (LIKELY(i != 1 && !downgrade)) {
		return i;
	} else if (UNLIKELY(i == 1)) {
		*arity = XCAR(*arity);
		return 0;
	} else if (UNLIKELY(downgrade)) {
		*arity = Qnil;
		return i;
	}
	/* not reached */
	return 0;
}


DEFUN("mapfam", Fmapfam, 1, MANY, 0, /*
Apply FUNCTION to elements in FAMILIES and collect the results
\(somehow\).

Arguments are:
FUNCTION &rest FAMILIES &key :result-type :mode :arity :glue
  :initiator :separator :terminator

The first argument FUNCTION is the function to use for the map.
If FUNCTION is `nil' the function #\'identity or one of its glue
counterparts (see :glue) is implicitly used.  This can be used
to convert one family to another, see examples below.

The rest of the arguments are FAMILIES, where a family is a
sequence \(see `sequencep'\) or a dict-like map (hash-table,
skiplist, etc.).  The family types need not coincide.

Keys may be specified as in :key value [:key value [...]], all
keys are optional and may appear anywhere.  In greater detail:

:result-type  specifies the container type of the result object, can be:
  - #'list to yield a list (default)
  - #'dllist to yield a dllist
  - #'vector to yield a vector
  - #'string to yield a string iff FUNCTION returns characters or
    integers within the character range
  - #'concat to yield a string iff FUNCTION returns character arrays or
    arrays of integers within the character range
  - #'bit-vector to yield a bit-vector, FUNCTION's return values will
    be treated 1 iff non-nil, and 0 otherwise.
  - 'litter or 'void  to not collect the results at all
  - 'inplace to modify the first family in FAMILIES by side-effect if
    it is a sequence, and modify the value destructively if it is a
    dict.  This works only in pointwise mode, see :mode.

  Generally, the result-type is a functor (most often a constructor)
  to be applied on the produced output sequence.  It behaves as if the
  elements of the output sequence had been passed to the constructor
  function argument-wise.  So it can be thought of as a shortcut to
  \(apply #'<constructor> result-sequence\).

  In the past result types were specified by the name of the map
  function which turned out to be extremely sluggish in case the
  result type is parametrised (i.e. passed as parameter).

:mode  specifies the way the arguments are passed to FUNCTION, can be:
  - 'pointwise or 'pntw (default): given FAMILIES consists of
    fam1, fam2, etc. this mode passes the first point of fam1 along
    with the first point of fam2 along with etc. to FUNCTION.  Hereby
    a point is just one element in case the family is a sequence, and
    a key-value pair (as two separate arguments) if family is a dict
    (and arity does not specify this otherwise).
  - 'keywise or 'keyw: like 'pointwise in case of sequences, for dicts
    this passes only the key cell to FUNCTION.
  - 'cartesian or 'cart: construct the cartesian product of the points
    in FAMILIES and pass the resulting tuples to FUNCTION.
  - 'combination or 'comb: construct the set of all combinations of
    the points, formally this is the set of (fixed-size) subsets of the
    set of points, disregarding different orders.
    Note: the implementation will always preserve orders though, that is
    the combinatorial subsets of an ordered family will be ordered wrt
    to the same overlying order.
  - 'permutation or 'perm or 'variation or 'var: construct the set of
    all permutations of the points (also known as variations), formally
    this is the set of (fixed-size) tuples arising from rearranging
    (different ordering) the subsets of the set of points.

  Note: The combinatorial modes (cart, comb and perm) produce giant
  amounts of data (using glues) or a neverending series of function
  calls.  In case you are using one of the above modes and pass user
  input to #'mapfam or allow your users to specify their own mapping
  functions make sure you restrain the (size of the) input arguments.

  To give a rough idea of the outcome sizes:
  family size   arity    #combinations   #permutations  #cartesians
	2         2            1               2               4
	4         2            6              12              16
	8         4           70            1680            4096
	9         4          126            3024            6561
	9         5          126           15120           59049
	9         6           84           60480          531441
	9         7           36          181440         4782969
	9         8            9          362880        43046721
	9         9            1          362880       387420489

  For the number of combinations:
  (binomial-coefficient SIZE ARITY)
  For the number of permutations:
  (* (binomial-coeeficient SIZE ARITY) (factorial ARITY))
  For the number of points in the cartesian product:
  (^ SIZE ARITY)

  Additional note: SXEmacs' implementation of explicit symmetric group
  traversal (wrt a Bruhat-like order) is currently the fastest on the
  planet, however it obviously cannot overcome the sheer size of large
  symmetric groups.  Be aware that explicit unrolling S_11 eats up at
  least 300 MB of RAM, unrolling S_12 requires at least 3.6 GB of RAM,
  for S_13 it's approx 48 GB and so on.

  Additional note: Cartesian products are highly exponential in space
  and time complexity.  However, unlike permutations (symm. groups)
  the cartesian points can be constructed rather easily using nested
  loops.  So if you are just after a couple of cartesian points do not
  bother using mapfam to create them all and filter afterwards but
  directly use nested loops to create the points you need.

:arity  specifies how to choose and pass points from the families to
  FUNCTION.  The value of :arity can be a normal index (positive
  integer) if there is only one family, and a multi-index if points
  are to be picked from multiple families.

  Defaults:
  - 1 if there is only one family which is not a dictionary and mode
    'pointwise or 'combination
  - 1 if there is only one family (including dictionaries) and mode is
    keywise
  - 2 if there is only one family and mode is 'cartesian
  - the length of the family if there is only one family and mode is
    'permutation
  - (1 1) if family is a dictionary and mode is 'pointwise or
    'combination
  - (1 1 ... 1)  if there are n families, irrespective of mode.
     +-+- n -+
    So it is '(1 1) if two families are given, '(1 1 1) for 3 families
    and so forth.

  Indices, multi-indices and modes:
  The general multi-index form of the :arity keyword specifies how many
  points are taking from each family to form a glue cell which is passed
  directly to FUNCTION (exploded of course) if that is non-nil, and
  passed to the glue if that is nil.
  The first index in the arity multi-index list corresponds to the
  number of points to choose from the first family, the second one to
  the second family respectively and so on.
  An ordinary index always refers to the first family irrespective how
  many families have been specified.

  The exact meaning of this multi-index depends on the mode (see also
  examples):
  - In pointwise or keywise mode, always pick this number of points
    or elements (consecutively), example:
    Let '(1 2 3 a b c) be the family and 1 its arity, the sequence of
    picks goes: 1, 2, 3, a, b, c.
    Let '(1 2 3 a b c) be the family and 2 its arity, the sequence of
    picks goes: [1 2], [3 a], [b c]
    If a cell is not formable because there are too few elements left in
    the family the mapping will not take place at all, so be '(1 2 3)
    the family and 2 its arity, the sequence of picks goes: [1 2].

    Multiple families in pointwise or keywise mode behave similarly
    Be '(1 2 3) '(a b c) two families and '(1 1) the arity (which is the
    default arity anyway), the pick then goes: [1 a] [2 b] [3 c], which
    is exactly how CL's #'map behaves in this situation.
    Be '(1 2 3) '(a b c one-more) two families and '(1 1) the arity,
    then the pick sequence again is: [1 a] [2 b] [3 c].
    In general the family with the least elements determines the number
    of picks in this mode.

    For arbitrary multi-indices the same rules hold, example:
    Let '(1 2 3) '(a b c one-more) be two families and '(1 2) its arity,
    then the pick sequence will be: [1 a b] [2 c one-more]

  - In cartesian mode, the arity, if an ordinary index, specifies the
    number of cartesian copies of the first given family, example:
    Let [a b c] be a sequence and arity be 2, then the mapping will
    yield:
    [a a] [a b] [a c] [b a] [b b] [b c] [c a] [c b] [c c]

    If given a multi-index the cross sum denotes the total dimension of
    the yield while each index specifies the number of copies of the
    respective family, so fundamentally each cartesian mapping can be
    rewritten by a multi-index consisting solely of ones and
    correspondingly many copies of the input families, example:
    Let [a b c] and [1 2 3] be two families and '(1 1) the arity, then
    the cartesian mode will give:
    [a 1] [a 2] [a 3] [b 1] [b 2] [b 3] [c 1] [c 2] [c 3]
    Clearly the input sequence [a b c] of arity 2 can be rewritten as
    two input sequences [a b c] [a b c] and arity '(1 1) and will yield
    the sequence shown above.
    Next example:
    Let [a b] and [1 2] be two families and '(1 2) the arity, the result
    would be:
    [a 1 1] [a 1 2] [a 2 1] [a 2 2] [b 1 1] [b 1 2] [b 2 1] [b 2 2]

  - In combination mode, the arity, if an ordinary index, specifies the
    combination size, example:
    Let \'(1 2 3 a) be the input sequence and 2 its arity, then the
    sequence of picks goes:
    [1 2] [1 3] [1 a] [2 3] [2 a] [3 a].

    A multi-index over several families specifies the subset sizes of
    each of the families.  The total combination set is then formed by
    taking the cartesian product of these, example:
    Let [1 2 3] and [a b c] be two sets and '(2 2) the considered arity,
    then the first family yields [1 2] [1 3] [2 3] and the second one
    [a b] [a c] [b c], thence the final outcome will be:
    [1 2 a b] [1 2 a c] [1 2 b c] [1 3 a b] [1 3 a c] [1 3 b c] ...

    Again, the combination mode is strictly order-preserving, both
    the order of the families (as a sequence of families) and the order
    of each family will be preserved upon mapping.

  - In permuation mode, an ordinary index as arity will specify the
    cardinality, read size, of the combinatorial subset which will
    thence be permuted.
    Note: the default arity for the permutation mode if just one
    sequence is given is the length of this sequence!

    Example:
    Let \'(a b c) be a family and no arity be given, then the sequence
    of picks goes:
    [a b c] [a c b] [b a c] [b c a] [c a b] [c b a]
    Let "abcd" be a family and the arity be 2, then the pick sequence
    looks like:
    "ab" "ba" "ac" "ca" "ad" "da" "bc" "cb" "bd" "db" "cd" "dc"

    Note: while order 2 and order 3 permutations look carefully
    constructed and easily predictable this is not true for higher order
    permutations!  They are specially designed to be mappable as fast as
    possible and seem to have no predictable pattern whatsoever, the
    order is based on a 1-orbit representation of the underlying
    symmetric group which needs merely one transposition to get from one
    orbit element to the next one; for details cf. source code.

    If given a multi-index
    Let "abc" and "123" be two families and arity (2 2), the pick
    sequence is:
    (perms-of "ab12"), (perms-of "ab13"), (perms-of "ab23"),
    (perms-of "ac12"), (perms-of "ac13"), (perms-of "ac23")
    where #'perms-of denotes all permutations of that one give sequence,
    and can be implemented as (mapfam nil :mode \'perm <seq>)

:glue  when multiple values are to be passed to FUNCTION (or if FUNCTION
  is `nil' in particular) this specifies which (container) structure to
  use to glue them together.
  IOW, if FUNCTION is just a single-valued function but the family, the
  arity and/or the mode induce more than just one value, glue can turn
  so-called exploded form into a single value.  Possible constructors:
  - #'list (default)  to glue the arguments as lists
  - #'vector  to glue the arguments as vectors
  - #'dllist  to glue the arguments as dllists
  - #'string  to glue the arguments as strings, iff they are characters
  - #'concat  to glue the arguments as strings from character sequences

In pointwise and keywise mode the result sequence can be decorated:

:initiator  insert this object at the beginning of the output sequence
  only works in 'pointwise and 'keywise mode

:terminator  insert this object at the end of the output sequence
  only works in 'pointwise and 'keywise mode

:separator  insert this object between each pair of elements of the
  output sequence.  Use this to mimic a #'mapconcat-like behaviour,
  but this works for any sequence not just strings.
  only works in 'pointwise and 'keywise mode


Examples:
=========
Normal mapcar-like behaviour:
\(mapfam #'1+ '(1 2 3 4)\)
  => (2 3 4 5)
\(mapfam #'1+ :result-type 'vector '(1 2 3 4)\)
  => [2 3 4 5]
\(mapfam #'1- :result-type 'dllist [1 2 3 4]\)
  => (dllist 0 1 2 3)

Normal mapcar*-like behaviour:
\(mapfam #'+ (1 2 3 4) (10 20 30 40)\)
  => (11 22 33 44)
\(mapfam #'+ [1 2 3 4] (dllist 10 20 30 40) :result-type 'vector\)
  => [11 22 33 44]

Construct an alist from a plist:
\(mapfam #'cons '(a 1 b 2 c 3) :arity 2\)
  => ((a . 1) (b . 2) (c . 3))
\(mapfam #'list '(a 1 b 2 c 3) :arity 3 :result-type 'vector\)
  => [(a 1 b) (2 c 3)]
\(mapfam nil '(a 1 b 2 c 3) :arity 2 :glue 'list\)
  => ((a 1) (b 2) (c 3))
\(mapfam nil '(a 1 b 2 c 3) :arity 2 :glue 'vector :result-type 'dllist\)
  => (dllist [a 1] [b 2] [c 3])

Apply cons to 2-sets (subsets of order 2) of a list:
\(mapfam #'cons :mode 'comb :arity 2 '(a b c d)\)
  => ((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))
\(mapfam #'cons :mode 'comb :arity 2 '(a b a c)\)
  => ((a . b) (a . a) (a . c) (b . a) (b . c) (a . c))

The same for 3-sets (using the automatic glue):
\(mapfam nil :mode 'comb :arity 3 '(a b c d)\)
  => ((a b c) (a b d) (b c d))
\(mapfam nil :mode 'comb :arity 3 '(a b c d) :glue 'vector\)
  => ([a b c] [a b d] [b c d])
Note: This is exactly what `ncombs' is doing.

Given a tuple of elements determine all combinations of three
elements thereof (the 3-sets of the the tuple):
\(mapfam nil :mode 'comb :arity 3 [a b c d]\)
  => ((a b c) (a b d) (a c d) (b c d))
\(mapfam nil :mode 'comb :arity 3 [a b c d e] :glue #'vector\)
  => ([a b c] [a b d] [a b e] [a c d] [a c e] [a d e]
  [b c d] [b c e] [b d e] [c d e])

Glueing the combinations of two different lists:
\(mapfam nil :mode 'comb '(a b c) '(1 2)\)
  => ((a 1) (b 1) (c 1) (a 2) (b 2) (c 2))
\(mapfam nil :mode 'comb '(a b c) '(1 2) :arity '(2 1)\)
  => ((a b 1) (a c 1) (b c 1) (a b 2) (a c 2) (b c 2))
\(mapfam nil :mode 'comb '(a b c) '(1 2 3) :arity '(2 2)\)
  => ((a b 1 2) (a c 1 2) (b c 1 2)
      (a b 1 3) (a c 1 3) (b c 1 3)
      (a b 2 3) (a c 2 3) (b c 2 3))

Applying the plus function immediately:
\(mapfam #'+ :mode 'comb '(10 20 30) '(1 2)\)
  => (11 21 31 12 22 32)
\(mapfam #'+ :mode 'comb '(10 20 30) '(1 2) :arity '(2 1)\)
  => (31 41 51 22 42 52)

Mimicking #'mapconcat:
\(mapconcat #'identity '("the" "inverse" "of" "#'split-string") " "\)
  => "the inverse of #'split-string"
\(mapfam nil :separator " " '("the" "inverse" "of" "#'split-string")\)
  => ("the" " " "inverse" " " "of" " " "#'split-string")
\(mapfam nil :separator " " :result-type #'concat
  '("the inverse of #'split-string")\)
  => "the inverse of #'split-string"

Using cartesian mode and #'concat to emulate :separator
\(mapfam #'concat :result-type #'concat :mode 'cartesian
  '\("the" "inverse" "of" "#'split-string"\) '(" ")\)
  => "the inverse of #'split-string "
\(mapfam #'concat :result-type #'concat :mode 'cartesian
  [" "] '\("the" "inverse" "of" "#'split-string"\)\)
  => " the inverse of #'split-string"

Note a separator is not exactly like doing cartesian mapping over
two sequences since it affects only pairs of elements and so the
last/first tuple is missing.
However, pointwise mode is still use full if each pair of elements
requires a `different separator'.

\(mapfam #'concat :result-type #'concat :mode 'pointwise
  '\("the" "inverse" "of" "#'split-string"\) '(" " "_" "-" "."\)\)
  => "the inverse_of-#'split-string."

*/
      (int nargs, Lisp_Object *args))
{
/* this is just one, huuuuge case distinctor */
	Lisp_Object fun = Qnil;
	Lisp_Object mode = Qnil, arity = Qnil;
	Lisp_Object res_type = Qlist;
	struct decoration_s deco = {
		Qnull_pointer, Qnull_pointer, Qnull_pointer
	};
	int nfams = 0, arity_len;
	bool found_fun_p = false;
	glue_f gluef = NULL;

	/* snarf the function */
	if (!KEYWORDP(args[0])) {
		fun = args[0];
		found_fun_p = true;
	}
	/* snarf the keys and families */
	for (int i = found_fun_p; i < nargs; i++) {
		if (EQ(args[i], Q_result_type)) {
			res_type = args[++i];
		} else if (EQ(args[i], Q_arity)) {
			arity = args[++i];
		} else if (EQ(args[i], Q_mode)) {
			mode = args[++i];
		} else if (EQ(args[i], Q_glue)) {
			gluef = _obtain_glue(args[++i]);
		} else if (EQ(args[i], Q_separator)) {
			deco.sep = args[++i];
		} else if (EQ(args[i], Q_initiator)) {
			deco.ini = args[++i];
		} else if (EQ(args[i], Q_terminator)) {
			deco.ter = args[++i];
		} else if (!found_fun_p) {
			/* we found the function cell */
			fun = args[i];
			found_fun_p = true;
		} else {
			/* must be a family */
			args[nfams++] = args[i];
		}
	}

	/* check the integrity of the options */
	/* first kick the most idiotic situations */
	if (nfams == 0 ||
	    (NILP(fun) && EQ(mode, Qvoid)) ||
	    EQ(arity, Qzero)) {
		/* looks like an exphert is here */
		return __dress_result(res_type, NULL, 0);
	}
	/* now, fill in default values */
	if (NILP(mode)) {
		mode = Qpntw;
	}
	/* degrade a thoroughly fledges '(1 1 ... 1) arity to nil */
	arity_len = _maybe_downgrade(&arity);

#define POINTWISEP(mode)						\
	(EQ(mode, Qpntw) || EQ(mode, Qpointwise) || EQ(mode, Qpoints))
#define KEYWISEP(mode)							\
	(EQ(mode, Qkeyw) || EQ(mode, Qkeywise) || EQ(mode, Qkeys))
#define COMBINATIONP(mode)						\
	(EQ(mode, Qcomb) || EQ(mode, Qcombination) || EQ(mode, Qcombinations))
#define PERMUTATIONP(mode)						\
	(EQ(mode, Qperm) || EQ(mode, Qpermutation) || EQ(mode, Qpermutations))
#define CARTESIANP(mode)						\
	(EQ(mode, Qcart) || EQ(mode, Qcartesian))

	if (POINTWISEP(mode) && nfams == 1 && NILP(arity) && !DICTP(args[0])) {
		/* the arity is not specified and it's just one sequence */
		return __pntw_1seq(args[0], fun, 1UL, gluef, res_type, deco);

	} else if (POINTWISEP(mode) && NILP(arity) && !DICTP(args[0])) {
		/* the arity is not specified and it's more than one sequence */
		return __pntw_nseq(args, nfams, fun, gluef, res_type);

	} else if (KEYWISEP(mode) && nfams == 1 && NILP(arity)) {
		/* the arity is not specified and it's just one sequence,
		 * also we dont have to care about dicts since
		 * keywise is specified */
		return __pntw_1seq(args[0], fun, 1UL, gluef, res_type, deco);

	} else if (KEYWISEP(mode) && NILP(arity)) {
		/* the arity is not specified and it's more than one sequence,
		 * also we dont have to care about dicts since
		 * keywise is specified */
		return __pntw_nseq(args, nfams, fun, gluef, res_type);

	} else if (POINTWISEP(mode) && (nfams == 1 && NILP(arity))) {
		/* the arity is not specified, it's one sequence, and it
		 * must be a dict, since the non-dict case was check already */
		return __pntw_1dict(args[0], fun, gluef, res_type);

	} else if (NATNUMP(arity) && (POINTWISEP(mode) || KEYWISEP(mode))) {
		/* the arity is a natnum, so we consider just the
		 * first sequence, in case of dicts this equals keywise
		 * mode */
		return __pntw_1seq(args[0], fun, XUINT(arity),
				   gluef, res_type, deco);
	} else if (POINTWISEP(mode) || KEYWISEP(mode)) {
		/* the most general case */
		size_t a[arity_len];
		volatile Lisp_Object tmp;
		long int i = 0;

		for (i = 0, tmp = arity;
		     CONSP(tmp) && i < nfams && i < arity_len;
		     i++, tmp = XCDR(tmp)) {
			a[i] = XUINT(XCAR(tmp));
		}
		return __pntw_nseq2(args, i, fun, gluef, res_type, a);
	}

	if (COMBINATIONP(mode) && NATNUMP(arity)) {
		/* the arity is a natnum, so it's just one sequence,
		 * if not who cares :) */
		return __comb_1seq(args[0], fun, XUINT(arity),
				   gluef, res_type);
	} else if (COMBINATIONP(mode) && (nfams == 1 && NILP(arity))) {
		/* the arity is a natnum, so it's just one sequence,
		 * if not who cares :) */
		return __comb_1seq(args[0], fun, -1UL, gluef, res_type);

	} else if (COMBINATIONP(mode) && NILP(arity)) {
		/* the arity is not specified and it's more than one sequence */
		return __cart_nseq(args, nfams, fun, 1UL, gluef, res_type);

	} else if (COMBINATIONP(mode)) {
		/* the most general case */
		size_t a[arity_len];
		volatile Lisp_Object tmp;
		long int i = 0;

		for (i = 0, tmp = arity;
		     CONSP(tmp) && i < nfams && i < arity_len;
		     i++, tmp = XCDR(tmp)) {
			a[i] = XUINT(XCAR(tmp));
		}
		return __comb_nseq2(args, i, fun, gluef, res_type, a);
	}

	if (CARTESIANP(mode) && NATNUMP(arity)) {
		/* the arity is a natnum, so it's just one sequence,
		 * if not who cares :) */
		return __cart_1seq(args[0], fun, XUINT(arity),
				   gluef, res_type);
	} else if (CARTESIANP(mode) &&
		   (nfams == 1 && NILP(arity))) {
		/* it's one sequence and arity isnt specified, go with 2 then */
		return __cart_1seq(args[0], fun, 2UL, gluef, res_type);

	} else if (CARTESIANP(mode) && NILP(arity)) {
		/* the arity is not specified and it's more than one sequence */
		return __cart_nseq(args, nfams, fun, 1UL, gluef, res_type);

	} else if (CARTESIANP(mode)) {
		/* the most general case */
		size_t a[arity_len];
		volatile Lisp_Object tmp;
		long int i = 0;

		for (i = 0, tmp = arity;
		     CONSP(tmp) && i < nfams && i < arity_len;
		     i++, tmp = XCDR(tmp)) {
			a[i] = XUINT(XCAR(tmp));
		}
		return __cart_nseq2(args, i, fun, gluef, res_type, a);
	}

	if (PERMUTATIONP(mode) && NATNUMP(arity)) {
		/* the arity is a natnum, so it's just one sequence,
		 * if not who cares :) */
		return __perm_1seq(args[0], fun, XUINT(arity),
				   gluef, res_type);
	} else if (PERMUTATIONP(mode) && (nfams == 1 && NILP(arity))) {
		/* the arity is a natnum, so it's just one sequence,
		 * if not who cares :) */
		return __perm_1seq(args[0], fun, -1UL, gluef, res_type);

	} else if (PERMUTATIONP(mode) && NILP(arity)) {
		/* the arity is not specified and it's more than one sequence */
		return __perm_nseq(args, nfams, fun, 1UL, gluef, res_type);

	} else if (PERMUTATIONP(mode)) {
		/* the most general case */
		size_t a[arity_len];
		volatile Lisp_Object tmp;
		long int i = 0;

		for (i = 0, tmp = arity;
		     CONSP(tmp) && i < nfams && i < arity_len;
		     i++, tmp = XCDR(tmp)) {
			a[i] = XUINT(XCAR(tmp));
		}
		return __perm_nseq2(args, i, fun, gluef, res_type, a);
	}
	return Qnil;
}

DEFUN("mapconcat", Fmapconcat, 3, 3, 0,	/*
Apply FUNCTION to each element of SEQUENCE, and concat the results to a string.
Between each pair of results, insert SEPARATOR.

Each result, and SEPARATOR, should be strings.  Thus, using " " as SEPARATOR
results in spaces between the values returned by FUNCTION.  SEQUENCE itself
may be a list, a vector, a dllist, a bit vector, or a string.
*/
      (function, sequence, separator))
{
	EMACS_INT len = XINT(Flength(sequence));
	Lisp_Object *args;
	Lisp_Object result;
	EMACS_INT i;
	EMACS_INT nargs = len + len - 1;
	int speccount = specpdl_depth();

	if (len == 0)
		return build_string("");

	XMALLOC_OR_ALLOCA(args, nargs, Lisp_Object);

	mapcar1(len, args, function, sequence);

	for (i = len - 1; i >= 0; i--)
		args[i + i] = args[i];

	for (i = 1; i < nargs; i += 2)
		args[i] = separator;

	result = Fconcat(nargs, args);
	XMALLOC_UNBIND(args, nargs, speccount);
	return result;
}

DEFUN("mapcar", Fmapcar, 2, 2, 0,	/*
Apply FUNCTION to each element of SEQUENCE; return a list of the results.
The result is a list of the same length as SEQUENCE.
SEQUENCE may be a list, a vector, a dllist, a bit vector, or a string.
*/
      (function, sequence))
{
	size_t len = XINT(Flength(sequence));
	Lisp_Object *args = NULL;
	Lisp_Object result;
	int speccount = specpdl_depth();

	XMALLOC_OR_ALLOCA(args, len, Lisp_Object);

	mapcar1(len, args, function, sequence);

	result = Flist(len, args);
	XMALLOC_UNBIND(args, len, speccount);
	return result;
}

DEFUN("mapdllist", Fmapdllist, 2, 2, 0,	/*
Apply FUNCTION to each element of SEQUENCE; return a dllist of the results.
The result is a list of the same length as SEQUENCE.
SEQUENCE may be a list, a vector, a dllist, a bit vector, or a string.
*/
      (function, sequence))
{
	size_t len = XINT(Flength(sequence));
	Lisp_Object *args = NULL;
	Lisp_Object result;
	int speccount = specpdl_depth();

	XMALLOC_OR_ALLOCA(args, len, Lisp_Object);

	mapcar1(len, args, function, sequence);

	result = Fdllist(len, args);
	XMALLOC_UNBIND(args, len, speccount);
	return result;
}

DEFUN("mapvector", Fmapvector, 2, 2, 0,	/*
Apply FUNCTION to each element of SEQUENCE; return a vector of the results.
The result is a vector of the same length as SEQUENCE.
SEQUENCE may be a list, a vector, a dllist, a bit vector, or a string.
*/
      (function, sequence))
{
	size_t len = XINT(Flength(sequence));
	Lisp_Object result = make_vector(len, Qnil);
	struct gcpro gcpro1;

	GCPRO1(result);
	mapcar1(len, XVECTOR_DATA(result), function, sequence);
	UNGCPRO;

	return result;
}

DEFUN("mapc-internal", Fmapc_internal, 2, 2, 0,	/*
Apply FUNCTION to each element of SEQUENCE.
SEQUENCE may be a list, a vector, a bit vector, or a string.
This function is like `mapcar' but does not accumulate the results,
which is more efficient if you do not use the results.

The difference between this and `mapc' is that `mapc' supports all
the spiffy Common Lisp arguments.  You should normally use `mapc'.
*/
      (function, sequence))
{
	mapcar1(XINT(Flength(sequence)), 0, function, sequence);

	return sequence;
}

DEFUN("mapc-inplace", Fmapc_inplace, 2, 2, 0, /*
Apply FUNCTION to each element of SEQUENCE and replace the
element with the result.
Return the (destructively) modified sequence.

At the moment, SEQUENCE can be a list, a dllist, a vector,
a bit-vector, or a string.

Containers with type restrictions -- strings or bit-vectors here --
cannot handle all results of FUNCTION.  In case of bit-vectors,
if the function yields `nil' or 0 the current bit is set to 0,
if the function yields anything else, the bit is set to 1.
Similarly in the string case any non-char result of FUNCTION sets
the currently processed character to ^@ (octal value: 000).
*/
      (function, sequence))
{
	if (0);
	else if (LISTP(sequence))
		list_map_inplace(function, sequence);
	else if (DLLISTP(sequence))
		dllist_map_inplace(function, sequence);
	else if (STRINGP(sequence))
		string_map_inplace(function, sequence);
	else if (VECTORP(sequence))
		vector_map_inplace(function, sequence);
	else if (BIT_VECTORP(sequence))
		bit_vector_map_inplace(function, sequence);

	return sequence;
}


/* to be emodule compliant */
void
map_LTX_init(void)
{
	DEFSYMBOL(Qmap);
	/* the keys */
	DEFKEYWORD(Q_mode);
	DEFKEYWORD(Q_glue);
	DEFKEYWORD(Q_arity);
	DEFKEYWORD(Q_result_type);
	DEFKEYWORD(Q_initiator);
	DEFKEYWORD(Q_separator);
	DEFKEYWORD(Q_terminator);
	/* symbols for result and glue */
	DEFSYMBOL(Qinplace);
	DEFSYMBOL(Qlitter);
	DEFSYMBOL(Qlist);
	DEFSYMBOL(Qdllist);
	DEFSYMBOL(Qvector);
	DEFSYMBOL(Qbit_vector);
	DEFSYMBOL(Qstring);
	DEFSYMBOL(Qconcat);
	/* mode symbols */
	DEFSYMBOL(Qpntw);
	DEFSYMBOL(Qpointwise);
	DEFSYMBOL(Qpoints);
	DEFSYMBOL(Qkeyw);
	DEFSYMBOL(Qkeywise);
	DEFSYMBOL(Qkeys);
	DEFSYMBOL(Qcomb);
	DEFSYMBOL(Qcombination);
	DEFSYMBOL(Qcombinations);
	DEFSYMBOL(Qperm);
	DEFSYMBOL(Qpermutation);
	DEFSYMBOL(Qpermutations);
	DEFSYMBOL(Qcart);
	DEFSYMBOL(Qcartesian);
	/* the super map */
	DEFSUBR(Fmapfam);
	/* special map*s, compatibility */
	DEFSUBR(Fmapcar);
	DEFSUBR(Fmapdllist);
	DEFSUBR(Fmapvector);
	DEFSUBR(Fmapc_internal);
	DEFSUBR(Fmapconcat);
	DEFSUBR(Fmapc_inplace);
	return;
}

/* map.c ends here */
