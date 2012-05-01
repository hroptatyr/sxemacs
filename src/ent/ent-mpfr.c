/*
  ent-mpfr.c -- Numeric types for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "ent.h"
#include "ent-optable.h"
#include "ent-lift.h"
#include "ent-indef.h"
#include "ent-mpfr.h"


Lisp_Object Veuler;
Lisp_Object Veuler_mascheroni;
Lisp_Object Vpi;

bigfr ent_scratch_bigfr;
static ase_nullary_operation_f Qent_mpfr_zero, Qent_mpfr_one;


static void
bigfr_print(Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(unused))
{
	Bufbyte *fstr = bigfr_to_string(XBIGFR_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	xfree(fstr);
	fstr = (Bufbyte *)NULL;
	return;
}

static int
bigfr_equal (Lisp_Object obj1, Lisp_Object obj2, int unused)
{
	return bigfr_eq(XBIGFR_DATA(obj1), XBIGFR_DATA(obj2));
}

static unsigned long
bigfr_hash (Lisp_Object obj, int unused)
{
	return bigfr_hashcode(XBIGFR_DATA(obj));
}

static Lisp_Object
bigfr_mark (Lisp_Object unused)
{
	return Qnil;
}

static void
bigfr_finalise (void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing MPFR objects", Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description bigfr_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigfr, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigfr", bigfr,
				    bigfr_mark, bigfr_print, bigfr_finalise,
				    bigfr_equal, bigfr_hash,
				    bigfr_description, Lisp_Bigfr);


DEFUN ("bigfr-get-precision", Fbigfr_get_precision, 1, 1, 0, /*
Return the precision of bigfr F as an integer.
*/
       (f))
{
	CHECK_BIGFR(f);
	return make_integer((signed long)XBIGFR_GET_PREC(f));
}

#ifndef MPFR_PREC_MIN
#define MPFR_PREC_MIN 2UL
#endif

DEFUN ("bigfr-set-precision", Fbigfr_set_precision, 2, 2, 0, /*
Set the precision of F, a bigfr, to PRECISION, a nonnegative integer.
The new precision of F is returned.  Note that the return value may differ
from PRECISION if the underlying library is unable to support exactly
PRECISION bits of precision.
*/
       (f, precision))
{
	unsigned long prec;

	CHECK_BIGFR(f);
	if (INTP(precision)) {
		prec = (XINT(precision) <= 0)
			? MPFR_PREC_MIN : (unsigned long)XINT(precision);
	}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	else if (BIGZP(precision)) {
		prec = bigz_fits_ulong_p(XBIGZ_DATA(precision))
			? bigz_to_ulong(XBIGZ_DATA(precision))
			: UINT_MAX;
	}
#endif	/* HAVE_MPZ */
	else {
		dead_wrong_type_argument(Qintegerp, f);
		return Qnil;
	}

	XBIGFR_SET_PREC(f, prec);
	return Fbigfr_get_precision(f);
}


Bufbyte *bigfr_to_string(mpfr_t f, int base)
{
	mp_exp_t expt;
	Bufbyte *str;
	int len;
	const int sign = mpfr_sgn(f);
	const int neg = (sign < 0) ? 1 : 0;

	if (mpfr_inf_p(f)) {
		if (sign > 0) {
			str = indef_to_string((indef)POS_INFINITY);
		} else {
			str = indef_to_string((indef)NEG_INFINITY);
		}
		return str;
	} else if (mpfr_nan_p(f)) {
		str = indef_to_string((indef)NOT_A_NUMBER);
		return str;
	} else {
		if (base <= 1)
			base = 10;

		str = (Bufbyte *)mpfr_get_str(NULL, &expt, base, 0, f,
					      GMP_RNDN);
		len = strlen((char *)str) + 1;  /* Count the null terminator */

		/* Move digits down to insert a radix point */
		if (expt <= 0) {
			/* We need room for a radix point and leading zeroes */
			const int space = -expt + 2;
			xrealloc_array(str, Bufbyte, len + space);
			memmove(&str[space + neg], &str[neg], len - neg);
			memset(&str[neg], '0', space);
			str[neg + 1] = '.';
			len += space;
		} else if (expt < len) {
			/* We just need room for a radix point */
			xrealloc_array(str, Bufbyte, len + 1);
			memmove(&str[expt + neg + 1],
				&str[expt + neg],
				len - (expt + neg));
			str[expt + neg] = '.';
			len++;
		} else {
			/* We need room for trailing zeroes */
			xrealloc_array(str, Bufbyte, expt + 1);
			memset(&str[len-1], '0', expt+2-len);
			str[expt] = '\0';
			len = expt + 1;
		}

		return str;
	}
}

Lisp_Object read_bigfr_string(char *cp)
{
	/* The mpfr version of bigfr_set_string (mpfr_set_str)
	   has the following limitation: if p starts with a '+'
	   sign, it does nothing; i.e., it leaves its bigfloat
	   argument untouched.
	   Therefore, move p past any leading '+' signs. */
	bigfr bfr;
	Lisp_Object result;

	bigfr_init_prec(bfr, bigfr_get_default_prec());

	if (*cp == '+')
		cp++;

	bigfr_set_string(bfr, (const char*)cp, 0);
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;
}

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
Lisp_Object read_bigc_string(char *cp)
{
	bigfr bf_re, bf_im;
	int sign;
	Lisp_Object result;

	BIGFR_INIT_PREC(bf_re, Qnil);
	BIGFR_INIT_PREC(bf_im, Qnil);

	/* MPC bigc_set_string has no effect
	 * with initial + sign */
	if (*cp == '+')
		cp++;
	bigfr_set_string(bf_re, cp, 0);

	if (*cp == '-') {
		/* jump over a leading minus */
		cp++;
	}

	while ((*cp >= '0' && *cp <= '9') ||
	       (*cp == '.'))
		cp++;

	/* read the imaginary part */
	sign = 0;
	if (*cp == '+') {
		cp++;
		sign = 1;
	}
	if (*cp == '-') {
		cp++;
		sign = -1;
	}
	if ((*cp == 'i' || *cp == 'I') &&
	    (sign != 0)) {
		/* expand +i to +1i and -i to -1i */
		bigfr_set_long(bf_im, 1L);
	} else if (sign == 0) {
		/* obviously we did not have a+bi,
		 * but merely bi
		 */
		bigfr_set(bf_im, bf_re);
		bigfr_set_long(bf_re, 0L);
	} else {
		bigfr_set_string(bf_im, cp, 0);
	}

	if (sign < 0)
		bigfr_neg(bf_im, bf_im);

	result = make_bigc_bfr(bf_re, bf_im,
			       max(bigfr_get_prec(bf_re),
				   bigfr_get_prec(bf_im)));

	bigfr_fini(bf_re);
	bigfr_fini(bf_im);
	return result;
}
#endif


/* bigfr ops */
static Lisp_Object
ent_sum_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGFR_T_INT_T(r, l);
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ent_sum_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGFR_T_BIGZ_T(r, l);
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static Lisp_Object
ent_sum_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGFR_T_BIGQ_T(r, l);
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static Lisp_Object
ent_sum_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGFR_T_BIGF_T(r, l);
}
#endif
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_sum_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_sum_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGFR_T_FLOAT_T(r, l);
}
#endif


static Lisp_Object
ent_diff_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ent_diff_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static Lisp_Object
ent_diff_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static Lisp_Object
ent_diff_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_diff_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_diff_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif

static Lisp_Object
ent_neg_BIGFR_T(Lisp_Object l)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_neg(ent_scratch_bigfr, XBIGFR_DATA(l));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static Lisp_Object
ent_prod_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGFR_T_INT_T(r, l);
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ent_prod_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGFR_T_BIGZ_T(r, l);
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static Lisp_Object
ent_prod_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGFR_T_BIGQ_T(r, l);
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static Lisp_Object
ent_prod_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGFR_T_BIGF_T(r, l);
}
#endif
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_prod_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_prod_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGFR_T_FLOAT_T(r, l);
}
#endif

static Lisp_Object
ent_div_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	bigfr_set_prec(ent_scratch_bigfr,
		      max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_int(r) == 0) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ent_div_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static Lisp_Object
ent_div_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		int lsgn = bigq_sign(XBIGQ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static Lisp_Object
ent_div_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_div_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0f) {
		int lsgn = bigfr_sign(XBIGFR_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_div_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		if (XFLOAT_DATA(l) > 0.0f)
			return make_indef(POS_INFINITY);
		else if (XFLOAT_DATA(r) < 0.0f)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif

static Lisp_Object
ent_inv_BIGFR_T(Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		return make_indef(POS_INFINITY);
	}
	bigfr_set_long(ent_scratch_bigfr, 1L);
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
static Lisp_Object
ent_rem_BIGFR_T(Lisp_Object unused, Lisp_Object r)
{
	return Qent_mpfr_zero;
}
static Lisp_Object
ent_mod_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0) {
		return Qent_mpfr_zero;
	}
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	bigfr_trunc(ent_scratch_bigfr, ent_scratch_bigfr);
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#ifdef bigfr_pow
static Lisp_Object
ent_pow_BIGFR_T_integer(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT expo = 0;

	if (NILP(Fnonnegativep(r))) {
		return ent_unop_inv(
			ent_pow_BIGFR_T_integer(l, ent_unop_neg(r)));
	}

	if (INTP(r)) {
		expo = ent_int(r);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(r)) {
		if (bigz_fits_long_p(XBIGZ_DATA(r)))
			expo = bigz_to_long(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, list1(r));
#endif
	} else {
		Fsignal(Qarith_error, list1(r));
		return Qnil;
	}

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_pow(ent_scratch_bigfr, XBIGFR_DATA(l), expo);
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}
#endif

/* relations */
static inline int
ent_lt_bigfr(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_lt(XBIGFR_DATA(l), XBIGFR_DATA(r)));
}
static inline int
ent_lt_bigfr_int(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	return (bigfr_lt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_lt_int_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	return (bigfr_lt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline int
ent_lt_bigfr_bigz(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	return (bigfr_lt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_lt_bigz_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	return (bigfr_lt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static inline int
ent_lt_bigfr_bigq(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	return (bigfr_lt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_lt_bigq_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	return (bigfr_lt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static inline int
ent_lt_bigfr_bigf(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	return (bigfr_lt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_lt_bigf_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	return (bigfr_lt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#ifdef HAVE_FPFLOAT
static inline int
ent_lt_bigfr_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	return (bigfr_lt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_lt_fpfloat_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	return (bigfr_lt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif

static inline int
ent_gt_bigfr(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_gt(XBIGFR_DATA(l), XBIGFR_DATA(r)));
}
static inline int
ent_gt_bigfr_int(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	return (bigfr_gt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_gt_int_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	return (bigfr_gt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline int
ent_gt_bigfr_bigz(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	return (bigfr_gt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_gt_bigz_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	return (bigfr_gt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static inline int
ent_gt_bigfr_bigq(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	return (bigfr_gt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_gt_bigq_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	return (bigfr_gt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static inline int
ent_gt_bigfr_bigf(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	return (bigfr_gt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_gt_bigf_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	return (bigfr_gt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#ifdef HAVE_FPFLOAT
static inline int
ent_gt_bigfr_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	return (bigfr_gt(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_gt_fpfloat_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	return (bigfr_gt(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif

static inline int
ent_eq_bigfr(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eq(XBIGFR_DATA(l), XBIGFR_DATA(r)));
}
static inline int
ent_eq_bigfr_int(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	return (bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_eq_int_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	return (bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline int
ent_eq_bigfr_bigz(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	return (bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_eq_bigz_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	return (bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static inline int
ent_eq_bigfr_bigq(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	return (bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_eq_bigq_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	return (bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static inline int
ent_eq_bigfr_bigf(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	return (bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_eq_bigf_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	return (bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#ifdef HAVE_FPFLOAT
static inline int
ent_eq_bigfr_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	return (bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_eq_fpfloat_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	return (bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif

static inline int
ent_ne_bigfr(Lisp_Object l, Lisp_Object r)
{
	return !(bigfr_eq(XBIGFR_DATA(l), XBIGFR_DATA(r)));
}
static inline int
ent_ne_bigfr_int(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(r));
	return !(bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_ne_int_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_long(ent_scratch_bigfr, ent_int(l));
	return !(bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline int
ent_ne_bigfr_bigz(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	return !(bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_ne_bigz_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	return !(bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
static inline int
ent_ne_bigfr_bigq(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	return !(bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_ne_bigq_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	return !(bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#if defined HAVE_MPF && defined WITH_GMP
static inline int
ent_ne_bigfr_bigf(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	return !(bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_ne_bigf_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	return !(bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif
#ifdef HAVE_FPFLOAT
static inline int
ent_ne_bigfr_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(r));
	return !(bigfr_eq(XBIGFR_DATA(l), ent_scratch_bigfr));
}
static inline int
ent_ne_fpfloat_bigfr(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(l));
	return !(bigfr_eq(ent_scratch_bigfr, XBIGFR_DATA(r)));
}
#endif


static inline Lisp_Object
_ent_lift_INT_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;
	return make_bigfr(ent_int(number), precision);
}

static inline Lisp_Object
_ent_lift_BIGFR_T_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_int(bigfr_to_long(XBIGFR_DATA(number)));
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
_ent_lift_BIGZ_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static inline Lisp_Object
_ent_lift_BIGFR_T_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_set_bigfr(ent_scratch_bigz, XBIGFR_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}
#endif	/* HAVE_MPZ */

#if defined HAVE_MPQ && defined WITH_GMP
static inline Lisp_Object
_ent_lift_BIGQ_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif	/* HAVE_MPQ */

#if defined HAVE_MPF && defined WITH_GMP
static inline Lisp_Object
_ent_lift_BIGF_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static inline Lisp_Object
_ent_lift_BIGFR_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigfr(ent_scratch_bigf, XBIGFR_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif	/* HAVE_MPF */

#ifdef HAVE_FPFLOAT
static inline Lisp_Object
_ent_lift_FLOAT_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_fpfloat(ent_scratch_bigfr, XFLOAT_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static inline Lisp_Object
_ent_lift_BIGFR_T_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_float(bigfr_to_fpfloat(XBIGFR_DATA(number)));
}
#endif

static inline Lisp_Object
_ent_lift_BIGFR_T_BIGFR_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set(ent_scratch_bigfr, XBIGFR_DATA(number));
	return ent_mpfr_wipe_indef(ent_scratch_bigfr);
}

static inline int
ent_mpfr_zerop(Lisp_Object l)
{
	return (bigfr_sign(XBIGFR_DATA(l)) == 0);
}

static inline int
ent_mpfr_onep(Lisp_Object l)
{
	return (bigfr_to_fpfloat(XBIGFR_DATA(l)) == 1.0f);
}

static inline int
ent_mpfr_unitp(Lisp_Object unused)
{
	return 1;
}


static inline void
ent_mpfr_nullary_optable_init(void)
{
	Qent_mpfr_zero = make_bigfr(0.0f, internal_get_precision(Qnil));
	Qent_mpfr_one = make_bigfr(1.0f, internal_get_precision(Qnil));
	staticpro(&Qent_mpfr_zero);
	staticpro(&Qent_mpfr_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, BIGFR_T, Qent_mpfr_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, BIGFR_T, Qent_mpfr_one);
}

static inline void
ent_mpfr_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, BIGFR_T, ent_neg_BIGFR_T);
	ent_unop_register(ASE_UNARY_OP_INV, BIGFR_T, ent_inv_BIGFR_T);
}

static inline void
ent_mpfr_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, BIGFR_T, ent_sum_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, INT_T, ent_sum_BIGFR_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, BIGFR_T, ent_sum_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, BIGZ_T, ent_sum_BIGFR_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, BIGFR_T, ent_sum_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, BIGQ_T, ent_sum_BIGFR_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, BIGFR_T, ent_sum_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, BIGF_T, ent_sum_BIGFR_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, BIGFR_T, ent_sum_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, BIGFR_T, ent_sum_FLOAT_T_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, FLOAT_T, ent_sum_BIGFR_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, BIGFR_T, ent_diff_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, INT_T, ent_diff_BIGFR_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, BIGFR_T, ent_diff_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, BIGZ_T, ent_diff_BIGFR_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, BIGFR_T, ent_diff_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, BIGQ_T, ent_diff_BIGFR_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, BIGFR_T, ent_diff_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, BIGF_T, ent_diff_BIGFR_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, BIGFR_T, ent_diff_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, BIGFR_T, ent_diff_FLOAT_T_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, FLOAT_T, ent_diff_BIGFR_T_FLOAT_T);
#endif

	/* products */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, BIGFR_T, ent_prod_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, INT_T, ent_prod_BIGFR_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, BIGFR_T, ent_prod_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, BIGZ_T, ent_prod_BIGFR_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, BIGFR_T, ent_prod_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, BIGQ_T, ent_prod_BIGFR_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, BIGFR_T, ent_prod_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, BIGF_T, ent_prod_BIGFR_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, BIGFR_T, ent_prod_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, BIGFR_T, ent_prod_FLOAT_T_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, FLOAT_T, ent_prod_BIGFR_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, BIGFR_T, ent_div_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, INT_T, ent_div_BIGFR_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, BIGFR_T, ent_div_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, BIGZ_T, ent_div_BIGFR_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, BIGFR_T, ent_div_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, BIGQ_T, ent_div_BIGFR_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, BIGFR_T, ent_div_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, BIGF_T, ent_div_BIGFR_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, BIGFR_T, ent_div_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, BIGFR_T, ent_div_FLOAT_T_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, FLOAT_T, ent_div_BIGFR_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGFR_T, ent_div_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, INT_T, ent_div_BIGFR_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, BIGFR_T, ent_div_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGZ_T, ent_div_BIGFR_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGFR_T, ent_div_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGQ_T, ent_div_BIGFR_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGFR_T, ent_div_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGF_T, ent_div_BIGFR_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGFR_T, ent_div_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGFR_T, ent_div_FLOAT_T_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, FLOAT_T, ent_div_BIGFR_T_FLOAT_T);
#endif

	/* remainders */
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGFR_T, BIGFR_T, ent_mod_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGFR_T, BIGFR_T, ent_rem_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   INT_T, BIGFR_T, ent_rem_BIGFR_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   INDEF_T, BIGFR_T, ent_rem_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGZ_T, BIGFR_T, ent_rem_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGQ_T, BIGFR_T, ent_rem_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGF_T, BIGFR_T, ent_rem_BIGFR_T);
#endif
#if HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_REM,
			   FLOAT_T, BIGFR_T, ent_rem_BIGFR_T);
#endif

#ifdef bigfr_pow
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGFR_T, INT_T, ent_pow_BIGFR_T_integer);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGFR_T, BIGZ_T, ent_pow_BIGFR_T_integer);
#endif
#endif
}

static inline void
ent_mpfr_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, BIGFR_T, ent_mpfr_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, BIGFR_T, ent_mpfr_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, BIGFR_T, ent_mpfr_unitp);
}

static inline void
ent_mpfr_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, BIGFR_T, ent_lt_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, BIGFR_T, ent_gt_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, BIGFR_T, ent_eq_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, BIGFR_T, ent_ne_bigfr);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, INT_T, ent_lt_bigfr_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, INT_T, ent_gt_bigfr_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, INT_T, ent_eq_bigfr_int);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, INT_T, ent_ne_bigfr_int);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, BIGFR_T, ent_lt_int_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, BIGFR_T, ent_gt_int_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, BIGFR_T, ent_eq_int_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, BIGFR_T, ent_ne_int_bigfr);

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, BIGZ_T, ent_lt_bigfr_bigz);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, BIGZ_T, ent_gt_bigfr_bigz);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, BIGZ_T, ent_eq_bigfr_bigz);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, BIGZ_T, ent_ne_bigfr_bigz);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, BIGFR_T, ent_lt_bigz_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, BIGFR_T, ent_gt_bigz_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, BIGFR_T, ent_eq_bigz_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, BIGFR_T, ent_ne_bigz_bigfr);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, BIGQ_T, ent_lt_bigfr_bigq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, BIGQ_T, ent_gt_bigfr_bigq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, BIGQ_T, ent_eq_bigfr_bigq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, BIGQ_T, ent_ne_bigfr_bigq);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, BIGFR_T, ent_lt_bigq_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, BIGFR_T, ent_gt_bigq_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, BIGFR_T, ent_eq_bigq_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, BIGFR_T, ent_ne_bigq_bigfr);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, BIGF_T, ent_lt_bigfr_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, BIGF_T, ent_gt_bigfr_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, BIGF_T, ent_eq_bigfr_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, BIGF_T, ent_ne_bigfr_bigf);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, BIGFR_T, ent_lt_bigf_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, BIGFR_T, ent_gt_bigf_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, BIGFR_T, ent_eq_bigf_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, BIGFR_T, ent_ne_bigf_bigfr);
#endif
#ifdef HAVE_FPFLOAT
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGFR_T, FLOAT_T, ent_lt_bigfr_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGFR_T, FLOAT_T, ent_gt_bigfr_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGFR_T, FLOAT_T, ent_eq_bigfr_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGFR_T, FLOAT_T, ent_ne_bigfr_fpfloat);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, BIGFR_T, ent_lt_fpfloat_bigfr);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, BIGFR_T, ent_gt_fpfloat_bigfr);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, BIGFR_T, ent_eq_fpfloat_bigfr);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, BIGFR_T, ent_ne_fpfloat_bigfr);
#endif
}

static inline void
ent_mpfr_lifttable_init(void)
{
	ent_lift_register(BIGFR_T, BIGFR_T, _ent_lift_BIGFR_T_BIGFR_T);
	ent_lift_register(BIGFR_T, INT_T, _ent_lift_BIGFR_T_INT_T);
	ent_lift_register(INT_T, BIGFR_T, _ent_lift_INT_T_BIGFR_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_lift_register(BIGFR_T, BIGZ_T, _ent_lift_BIGFR_T_BIGZ_T);
	ent_lift_register(BIGZ_T, BIGFR_T, _ent_lift_BIGZ_T_BIGFR_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_lift_register(BIGQ_T, BIGFR_T, _ent_lift_BIGQ_T_BIGFR_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_lift_register(BIGFR_T, BIGF_T, _ent_lift_BIGFR_T_BIGF_T);
	ent_lift_register(BIGF_T, BIGFR_T, _ent_lift_BIGF_T_BIGFR_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_lift_register(BIGFR_T, FLOAT_T, _ent_lift_BIGFR_T_FLOAT_T);
	ent_lift_register(FLOAT_T, BIGFR_T, _ent_lift_FLOAT_T_BIGFR_T);
#endif
	ent_lift_register(INDEF_T, BIGFR_T, ent_lift_INDEF_T_COMPARABLE);
}


void init_optables_BIGFR_T(void)
{
	ent_mpfr_nullary_optable_init();
	ent_mpfr_unary_optable_init();
	ent_mpfr_binary_optable_init();
	ent_mpfr_unary_reltable_init();
	ent_mpfr_binary_reltable_init();
	ent_mpfr_lifttable_init();
}

void init_ent_mpfr(void)
{
	bigfr_init(ent_scratch_bigfr);

	Veuler = make_bigfr(0.0, 2048UL);
	bigfr_set_long(ent_scratch_bigfr, 1L);
	mpfr_exp(XBIGFR_DATA(Veuler), ent_scratch_bigfr, GMP_RNDN);

	Veuler_mascheroni = make_bigfr(0.0, 2048UL);
	mpfr_const_euler(XBIGFR_DATA(Veuler_mascheroni), GMP_RNDN);

	Vpi = make_bigfr(0.0, 2048UL);
	mpfr_const_pi(XBIGFR_DATA(Vpi), GMP_RNDN);
}

void syms_of_ent_mpfr(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigfr);

	DEFSUBR(Fbigfr_get_precision);
	DEFSUBR(Fbigfr_set_precision);

	bigfr_set_default_prec(128UL);
}

void vars_of_ent_mpfr(void)
{
	/* define pi and e */

	/* just some dummy values atm, to make the dumper smile */
	Veuler = make_int(1L);
	Veuler_mascheroni = make_int(1L);
	Vpi = make_int(1L);

	DEFVAR_CONST_LISP("euler", &Veuler /*
The value of Euler's constant e (2.7182818...).
					   */);
	DEFVAR_CONST_LISP("euler-mascheroni", &Veuler_mascheroni /*
The value of the Euler-Mascheroni constant (0.5772156...).
								 */);
	DEFVAR_CONST_LISP("pi", &Vpi /*
The value of pi (3.1415926...).
				     */);

	Fprovide(intern("bigfr"));
}
