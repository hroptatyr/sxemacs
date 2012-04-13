/*
  ent-gmp.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005, 2006 Sebastian Freundt

  Author:  Jerry James
  Backport:  Sebastian Freundt

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

static mpf_t float_print_min, float_print_max;
gmp_randstate_t random_state;

bigz ent_scratch_bigz;
bigq ent_scratch_bigq;
bigf ent_scratch_bigf;

static ase_nullary_operation_f Qent_mpz_zero, Qent_mpz_one;
static ase_nullary_operation_f Qent_mpq_zero, Qent_mpq_one;
static ase_nullary_operation_f Qent_mpf_zero, Qent_mpf_one;


#define yrealloc_array(ptr, type, len)		\
	((void)(ptr = (type*)yrealloc(ptr, (len) * sizeof(type))))



/************************* Big Rational Integers ****************************/
static void
bigz_print (Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *bstr;

	bstr = (Bufbyte*)bigz_to_string(XBIGZ_DATA(obj), 10);
	write_c_string((char*)bstr, printcharfun);
	xfree(bstr);
	bstr = (Bufbyte *)NULL;
}

static int
bigz_equal (Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return bigz_eql(XBIGZ_DATA(obj1), XBIGZ_DATA(obj2));
}

static unsigned long
bigz_hash (Lisp_Object obj, int SXE_UNUSED(depth))
{
	return (unsigned long)bigz_hashcode(XBIGZ_DATA(obj));
}

static const struct lrecord_description bigz_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigz, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigz", bigz,
				    NULL, bigz_print, NULL,
				    bigz_equal, bigz_hash,
				    bigz_description, Lisp_Bigz);


/************************** Rational Integer Fractions **********************/
static void
bigq_print (Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *rstr;

	rstr = (Bufbyte*)bigq_to_string(XBIGQ_DATA(obj), 10);
	write_c_string((char *)rstr, printcharfun);
	xfree(rstr);
	rstr = (Bufbyte *)NULL;
	return;
}

static int
bigq_equal (Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return bigq_eql(XBIGQ_DATA(obj1), XBIGQ_DATA(obj2));
}

static unsigned long
bigq_hash (Lisp_Object obj, int SXE_UNUSED(depth))
{
	return bigq_hashcode(XBIGQ_DATA(obj));
}

static const struct lrecord_description bigq_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof (Lisp_Bigq, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigq", bigq,
				    NULL, bigq_print, NULL,
				    bigq_equal, bigq_hash,
				    bigq_description, Lisp_Bigq);


/********************************** Bigfs ***********************************/
static void
bigf_print(Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *fstr = bigf_to_string(XBIGF_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	xfree(fstr);
	fstr = (Bufbyte *)NULL;
	return;
}

static int
bigf_equal(Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return bigf_eq(XBIGF_DATA(obj1), XBIGF_DATA(obj2));
}

static unsigned long
bigf_hash(Lisp_Object obj, int SXE_UNUSED(depth))
{
	return bigf_hashcode(XBIGF_DATA(obj));
}

static const struct lrecord_description bigf_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigf, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigf", bigf,
				    NULL, bigf_print, NULL,
				    bigf_equal, bigf_hash,
				    bigf_description, Lisp_Bigf);

DEFUN("bigf-get-precision", Fbigf_get_precision, 1, 1, 0, /*
Return the precision of bigf F as an integer.
*/
       (f))
{
	CHECK_BIGF(f);
	return make_integer((signed long)XBIGF_GET_PREC(f));
}

DEFUN("bigf-set-precision", Fbigf_set_precision, 2, 2, 0, /*
Set the precision of F, a bigf, to PRECISION, a nonnegative integer.
The new precision of F is returned.  Note that the return value may differ
from PRECISION if the underlying library is unable to support exactly
PRECISION bits of precision.
*/
       (f, precision))
{
	unsigned long prec;

	CHECK_BIGF(f);
	if (INTP(precision)) {
		prec = (XINT(precision) <= 0)
			? 1UL : (unsigned long)XINT(precision);
	}
#ifdef HAVE_MPZ
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

	XBIGF_SET_PREC(f, prec);
	return Fbigf_get_precision(f);
}



Bufbyte *
bigf_to_string(mpf_t f, int base)
{
	mp_exp_t expt;
	Bufbyte *str = (Bufbyte*)mpf_get_str(NULL, &expt, base, 0, f);
	const int sign = mpf_sgn(f);
	const int neg = (sign < 0) ? 1 : 0;
	int len = strlen((char *)str) + 1;  /* Count the null terminator */

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


/* reader funs */

Lisp_Object
read_bigz_string(const char *cp, int base)
{
	Lisp_Object result;
	bigz bz;

	bigz_init(bz);

	/* MPZ bigz_set_string has no effect
	 * with initial + sign */
	if (*cp == '+') {
		cp++;
	}

	bigz_set_string(bz, (const char*)cp, base);
	result = make_bigz_bz(bz);

	bigz_fini(bz);
	return result;
}


Lisp_Object read_bigq_string(char *cp)
{
	bigq bq;
	Lisp_Object result;

	bigq_init(bq);

	/* The GMP version of bigq_set_string (mpq_set_str) has the following
	   limitations:
	   - If p starts with a '+' sign, it does nothing; i.e., it leaves its
	   ratio argument untouched.
	   - If p has a '+' sign after the '/' (e.g., 300/+400), it sets the
	   numerator from the string, but *leaves the denominator unchanged*.
	   - If p has trailing nonnumeric characters, it sets the numerator from
	   the string, but leaves the denominator unchanged.
	   - If p has more than one '/', (e.g., 1/2/3), then it sets the
	   numerator from the string, but leaves the denominator unchanged.

	   Therefore, move p past any leading '+' signs, temporarily drop a null
	   after the numeric characters we are trying to convert, and then put
	   the nulled character back afterward.  I am not going to fix problem
	   #2; just don't write ratios that look like that. */
	if (*cp == '+') {
		cp++;
	}

	bigq_set_string(bq, cp, 0);
	bigq_canonicalize(bq);

	result = ent_mpq_downgrade_maybe(bq);

	bigq_fini(bq);
	return result;
}

Lisp_Object read_bigf_string(char *cp)
{
	bigf bf;
	Lisp_Object result;

	bigf_init_prec(bf, internal_get_precision(Qnil));

	/* The GMP version of bigf_set_string (mpf_set_str)
	   has the following limitation: if p starts with a '+'
	   sign, it does nothing; i.e., it leaves its bigfloat
	   argument untouched.
	   Therefore, move p past any leading '+' signs. */
	if (*cp == '+') {
		cp++;
	}

	bigf_set_string(bf, cp, 0);
	result = make_bigf_bf(bf);

	bigf_fini(bf);
	return result;
}

/* bigz ops */
static inline int
_ent_mpz_zerop(bigz n)
{
	return (bigz_sign(n) == 0);
}

static int
ent_mpz_zerop(Lisp_Object l)
{
	return _ent_mpz_zerop(XBIGZ_DATA(l));
}

static inline int
_ent_mpz_onep(bigz n)
{
	return (bigz_fits_long_p(n) && bigz_to_long(n) == 1L);
}

static int
ent_mpz_onep(Lisp_Object l)
{
	return _ent_mpz_onep(XBIGZ_DATA(l));
}

static inline int
_ent_mpz_unitp(bigz n)
{
	return (bigz_fits_long_p(n) &&
		(bigz_to_long(n) == 1L || bigz_to_long(n) == -1L));
}

static int
ent_mpz_unitp(Lisp_Object l)
{
	return _ent_mpz_unitp(XBIGZ_DATA(l));
}

static Lisp_Object
ent_sum_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_sum_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_sum_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGZ_T_INT_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_sum_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_SUM, BIGZ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_sum_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_SUM, FLOAT_T, l, BIGZ_T, r, NULL);
}
#endif

static Lisp_Object
ent_diff_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_diff_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_diff_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(l));
	bigz_sub(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_diff_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_DIFF, BIGZ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_diff_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_DIFF, FLOAT_T, l, BIGZ_T, r, NULL);
}
#endif

static Lisp_Object
ent_neg_BIGZ_T(Lisp_Object l)
{
	bigz_neg(ent_scratch_bigz, XBIGZ_DATA(l));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object
ent_prod_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_prod_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_prod_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGZ_T_INT_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_prod_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_PROD, BIGZ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_prod_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_PROD, FLOAT_T, l, BIGZ_T, r, NULL);
}
#endif

static Lisp_Object
ent_div_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_div_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_int(r) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigz_set_long(ent_scratch_bigz, ent_int(r));
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_div_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigz_set_long(ent_scratch_bigz, ent_int(l));
	bigz_div(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_div_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_DIV, BIGZ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_div_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_DIV, FLOAT_T, l, BIGZ_T, r, NULL);
}
#endif

static Lisp_Object
ent_quo_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigq_set_bigz_bigz(ent_scratch_bigq,
			   XBIGZ_DATA(l), XBIGZ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_quo_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rr = ent_int(r);
	if (rr == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigz_set_long(ent_scratch_bigz, rr);
	return make_bigq_bz(XBIGZ_DATA(l), ent_scratch_bigz);
}
static Lisp_Object
ent_quo_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigz_set_long(ent_scratch_bigz, ent_int(l));
	return make_bigq_bz(ent_scratch_bigz, XBIGZ_DATA(r));
}

static Lisp_Object
ent_inv_BIGZ_T(Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		return make_indef(POS_INFINITY);
	} else if (ent_mpz_unitp(r)) {
		return r;
	} else {
		return make_bigq_bz(XBIGZ_DATA(Qent_mpz_one), XBIGZ_DATA(r));
	}
	return Qnil;
}
static Lisp_Object
ent_rem_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		return Qzero;
	}
	bigz_mod(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object
ent_rem_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rr = ent_int(r);
	if (rr == 0) {
		return Qzero;
	}
	bigz_set_long(ent_scratch_bigz, rr);
	bigz_mod(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}
static Lisp_Object
ent_rem_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		return Qzero;
	}
	bigz_set_long(ent_scratch_bigz, ent_int(l));
	bigz_mod(ent_scratch_bigz,  ent_scratch_bigz, XBIGZ_DATA(r));
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}

static Lisp_Object
ent_pow_BIGZ_T_integer_trivial(Lisp_Object l, Lisp_Object r)
{
	if (!bigz_fits_long_p(XBIGZ_DATA(l)))
		return Qnull_pointer;

	if (bigz_to_long(XBIGZ_DATA(l)) == 1L) {
		return l;
	} else if (bigz_to_long(XBIGZ_DATA(l)) == -1L) {
		if (!NILP(Fevenp(r)))
			return Qent_mpz_one;
		else
			return l;
	} else if (ent_mpz_zerop(l)) {
		/* actually only idiots want to compute 0-powers
		 * think I put a sleep(10) here to pretend that we're
		 * working rilly hard to solve that problem */
		if (ent_unrel_zerop(r))
			return Qent_mpz_one;
		else
			return Qent_mpz_zero;
	}
	return Qnull_pointer;
}

static Lisp_Object
ent_pow_INT_T_integer_trivial(Lisp_Object l, Lisp_Object r)
{
	if (XINT(l) == 1L) {
		return l;
	} else if (XINT(l) == -1L) {
		if (!NILP(Fevenp(r)))
			return Qone;
		else
			return l;
	} else if (XINT(l) == 0L) {
		/* actually only idiots want to compute 0-powers
		 * think I put a sleep(10) here to pretend that we're
		 * working rilly hard to solve that problem */
		if (ent_unrel_zerop(r))
			return Qone;
		else
			return Qzero;
	}
	return Qnull_pointer;
}

static Lisp_Object
ent_pow_integer_integer(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT expo = 0;
	Lisp_Object result;

	/* trivial cases */
	if (INTP(l) && (result = ent_pow_INT_T_integer_trivial(l, r)))
		return result;
	else if (BIGZP(l) && (result = ent_pow_BIGZ_T_integer_trivial(l, r)))
		return result;

	if (NILP(Fnonnegativep(r))) {
		/* l can't be inverted in Z, we return 0 or the inverse in Q
		 * depending on `common_lisp_slash' */
		if (!common_lisp_slash)
			return Qent_mpz_zero;
		else
			return ent_unop_inv(
				ent_pow_integer_integer(l, ent_unop_neg(r)));
	}

	if (INTP(r)) {
		expo = XINT(r);
	} else if (BIGZP(r)) {
		if (bigz_fits_long_p(XBIGZ_DATA(r)))
			expo = bigz_to_long(XBIGZ_DATA(r));
		else
			Fsignal(Qrange_error, list1(r));
	} else {
		Fsignal(Qoperation_error, list1(r));
		return Qnil;
	}

	if (INTP(l)) {
		bigz_set_long(ent_scratch_bigz, XINT(l));
	} else if (BIGZP(l)) {
		bigz_set(ent_scratch_bigz, XBIGZ_DATA(l));
	} else {
		Fsignal(Qoperation_error, list1(l));
		return Qnil;
	}

	bigz_pow(ent_scratch_bigz, ent_scratch_bigz, expo);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);
}

/* relations */
static int
ent_lt_bigz(Lisp_Object l, Lisp_Object r)
{
	return (bigz_lt(XBIGZ_DATA(l), XBIGZ_DATA(r)));
}
static int
ent_lt_bigz_int(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	return (bigz_lt(XBIGZ_DATA(l), ent_scratch_bigz));
}
static int
ent_lt_int_bigz(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(l));
	return (bigz_lt(ent_scratch_bigz, XBIGZ_DATA(r)));
}

static int
ent_gt_bigz(Lisp_Object l, Lisp_Object r)
{
	return (bigz_gt(XBIGZ_DATA(l), XBIGZ_DATA(r)));
}
static int
ent_gt_bigz_int(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	return (bigz_gt(XBIGZ_DATA(l), ent_scratch_bigz));
}
static int
ent_gt_int_bigz(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(l));
	return (bigz_gt(ent_scratch_bigz, XBIGZ_DATA(r)));
}

static int
ent_eq_bigz(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)));
}
static int
ent_eq_bigz_int(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, ent_int(r));
	return (bigz_eql(XBIGZ_DATA(l), ent_scratch_bigz));
}
static int
ent_eq_int_bigz(Lisp_Object l, Lisp_Object r)
{
	return ent_eq_bigz_int(r, l);
}

static int
ent_ne_bigz(Lisp_Object l, Lisp_Object r)
{
	return !ent_eq_bigz(l, r);
}
static int
ent_ne_bigz_int(Lisp_Object l, Lisp_Object r)
{
	return !ent_eq_bigz_int(l, r);
}
static int
ent_ne_int_bigz(Lisp_Object l, Lisp_Object r)
{
	return ent_ne_bigz_int(r, l);
}

#ifdef HAVE_FPFLOAT
static int
ent_lt_bigzq_float(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_1(ASE_BINARY_REL_LESSP, l, r, NULL);
}
static int
ent_lt_float_bigzq(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_2(ASE_BINARY_REL_LESSP, l, r, NULL);
}
static int
ent_gt_bigzq_float(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_1(ASE_BINARY_REL_GREATERP, l, r, NULL);
}
static int
ent_gt_float_bigzq(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_2(ASE_BINARY_REL_GREATERP, l, r, NULL);
}
static int
ent_eq_bigzq_float(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_1(ASE_BINARY_REL_EQUALP, l, r, NULL);
}
static int
ent_eq_float_bigzq(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_2(ASE_BINARY_REL_EQUALP, l, r, NULL);
}
static int
ent_ne_bigzq_float(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_1(ASE_BINARY_REL_NEQP, l, r, NULL);
}
static int
ent_ne_float_bigzq(Lisp_Object l, Lisp_Object r)
{
	return ent_binrel_lift_2(ASE_BINARY_REL_NEQP, l, r, NULL);
}
#endif


static Lisp_Object
_ent_lift_INT_T_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_bigz(ent_int(number));
}

static Lisp_Object
_ent_lift_BIGZ_T_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_int(bigz_to_long(XBIGZ_DATA(number)));
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
_ent_lift_FLOAT_T_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_set_fpfloat(ent_scratch_bigz, XFLOAT_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object
_ent_lift_BIGZ_T_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	/* can result in an indef object */
	return make_float(bigz_to_fpfloat(XBIGZ_DATA(number)));
}
#endif


/* bigq ops */
static int
ent_mpq_zerop(Lisp_Object l)
{
	return (bigq_sign(XBIGQ_DATA(l)) == 0);
}

static int
ent_mpq_onep(Lisp_Object l)
{
#define	num	bigq_numerator(XBIGQ_DATA(l))
#define den	bigq_denominator(XBIGQ_DATA(l))
	return (bigz_fits_long_p(num) &&
		bigz_to_long(num) == 1L &&
		bigz_fits_long_p(den) &&
		bigz_to_long(den) == 1L);
#undef num
#undef den
}

static int
ent_mpq_unitp(Lisp_Object unused)
{
	return 1;
}

static Lisp_Object
ent_sum_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_add(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_sum_BIGQ_T_scratch(Lisp_Object l)
{
	bigq_add(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_sum_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	return ent_sum_BIGQ_T_scratch(l);
}
static Lisp_Object
ent_sum_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	return ent_sum_BIGQ_T_scratch(r);
}
static Lisp_Object
ent_sum_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return ent_sum_BIGQ_T_scratch(l);
}
static Lisp_Object
ent_sum_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGQ_T_BIGZ_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_sum_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_SUM, BIGQ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_sum_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_SUM, FLOAT_T, l, BIGQ_T, r, NULL);
}
#endif

static Lisp_Object
ent_diff_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_diff_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_diff_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	bigq_sub(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_diff_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_diff_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	bigq_sub(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_diff_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_DIFF, BIGQ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_diff_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_SUM, FLOAT_T, l, BIGQ_T, r, NULL);
}
#endif

static Lisp_Object
ent_neg_BIGQ_T(Lisp_Object l)
{
	bigq_neg(ent_scratch_bigq, XBIGQ_DATA(l));
	return make_bigq_bq(ent_scratch_bigq);
}

static Lisp_Object
ent_prod_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_prod_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_prod_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGQ_T_INT_T(r, l);
}
static Lisp_Object
ent_prod_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_prod_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGQ_T_BIGZ_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_prod_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_PROD, BIGQ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_prod_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_PROD, FLOAT_T, l, BIGQ_T, r, NULL);
}
#endif

static Lisp_Object
ent_div_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		int lsgn = bigq_sign(XBIGQ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_div_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rr = ent_int(r);
	if (rr == 0) {
		int lsgn = bigq_sign(XBIGQ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigq_set_long(ent_scratch_bigq, rr);
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_div_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigq_set_long(ent_scratch_bigq, ent_int(l));
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_div_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		int lsgn = bigq_sign(XBIGQ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
static Lisp_Object
ent_div_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_div_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_1(
		ASE_BINARY_OP_DIV, BIGQ_T, l, FLOAT_T, r, NULL);
}
static Lisp_Object
ent_div_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return __ent_binop_lift_2(
		ASE_BINARY_OP_DIV, FLOAT_T, l, BIGQ_T, r, NULL);
}
#endif

static Lisp_Object
ent_quo_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_int(r) == 0) {
		if (ent_int(l) > 0)
			return make_indef(POS_INFINITY);
		else if (ent_int(l) < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	if (ent_int(r) < 0) {
		bigq_set_long_ulong(ent_scratch_bigq,
				    -ent_int(l), (unsigned long)-ent_int(r));
		return ent_mpq_downgrade_maybe(ent_scratch_bigq);
	} else {
		bigq_set_long_ulong(ent_scratch_bigq,
				    ent_int(l), (unsigned long)ent_int(r));
		return ent_mpq_downgrade_maybe(ent_scratch_bigq);
	}
}

static Lisp_Object
ent_inv_BIGQ_T(Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		return make_indef(POS_INFINITY);
	}
	bigq_set_long(ent_scratch_bigq, 1L);
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}

static Lisp_Object
ent_inv_INT_T(Lisp_Object l)
{
	if (ent_int(l) == 0) {
		return make_indef(POS_INFINITY);
	}
	return make_bigq(1L, ent_int(l));
}

static Lisp_Object
ent_rem_BIGQ_T(Lisp_Object unused, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		return Qzero;
	}
	/* actually the zero constructor should be called */
	return Qent_mpq_zero;
}
static Lisp_Object
ent_mod_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		return Qzero;
	}
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	bigz_div(ent_scratch_bigz,
		 bigq_numerator(ent_scratch_bigq),
		 bigq_denominator(ent_scratch_bigq));

	bigq_set_bigz(ent_scratch_bigq, ent_scratch_bigz);
	bigq_mul(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return ent_mpq_downgrade_maybe(ent_scratch_bigq);
}

static Lisp_Object
ent_pow_BIGQ_T_integer_trivial(Lisp_Object l, Lisp_Object r)
{
	if (!_ent_mpz_onep(XBIGQ_DENOMINATOR(l)) ||
	    !bigz_fits_long_p(XBIGQ_NUMERATOR(l)))
		return Qnull_pointer;

	if (bigz_to_long(XBIGQ_NUMERATOR(l)) == 1L) {
		return l;
	} else if (bigz_to_long(XBIGQ_NUMERATOR(l)) == -1L) {
		if (!NILP(Fevenp(r)))
			return Qent_mpq_one;
		else
			return l;
	} else if (ent_mpq_zerop(l)) {
		/* actually only idiots want to compute 0-powers
		 * think I put a sleep(10) here to pretend that we're
		 * working rilly hard to solve that problem */
		if (ent_unrel_zerop(r))
			return Qent_mpq_one;
		else
			return Qent_mpq_zero;
	}
	return Qnull_pointer;
}

static Lisp_Object
ent_pow_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object num, den, result;

	/* trivial cases */
	if ((result = ent_pow_BIGQ_T_integer_trivial(l, r)))
		return result;

	if (NILP(Fnonnegativep(r))) {
		return ent_unop_inv(
			ent_pow_BIGQ_T_integer(l, ent_unop_neg(r)));
	}

	num = ent_binop(ASE_BINARY_OP_POW,
			make_bigz_bz(XBIGQ_NUMERATOR(l)), r);
	den = ent_binop(ASE_BINARY_OP_POW,
			make_bigz_bz(XBIGQ_DENOMINATOR(l)), r);

	if (!INTEGERP(num) || !INTEGERP(den)) {
		Fsignal(Qdomain_error, list2(num, den));
		return Qnil;
	}

	if (BIGZP(num) && BIGZP(den))
		return make_bigq_bz(XBIGZ_DATA(num), XBIGZ_DATA(den));
	else if (INTP(num) && INTP(den))
		return make_bigq(ent_int(num), ent_int(den));
	else {
		num = ent_lift(num, BIGZ_T, NULL);
		den = ent_lift(den, BIGZ_T, NULL);
		return make_bigq_bz(XBIGZ_DATA(num), XBIGZ_DATA(den));
	}

	return Qzero;
}

/* relations */
static int
ent_lt_bigq(Lisp_Object l, Lisp_Object r)
{
	return (bigq_lt(XBIGQ_DATA(l), XBIGQ_DATA(r)));
}
static int
ent_lt_bigq_int(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	return (bigq_lt(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_lt_int_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	return (bigq_lt(ent_scratch_bigq, XBIGQ_DATA(r)));
}
static int
ent_lt_bigq_bigz(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return (bigq_lt(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_lt_bigz_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	return (bigq_lt(ent_scratch_bigq, XBIGQ_DATA(r)));
}

static int
ent_gt_bigq(Lisp_Object l, Lisp_Object r)
{
	return (bigq_gt(XBIGQ_DATA(l), XBIGQ_DATA(r)));
}
static int
ent_gt_bigq_int(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	return (bigq_gt(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_gt_int_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	return (bigq_gt(ent_scratch_bigq, XBIGQ_DATA(r)));
}
static int
ent_gt_bigq_bigz(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return (bigq_gt(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_gt_bigz_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	return (bigq_gt(ent_scratch_bigq, XBIGQ_DATA(r)));
}

static int
ent_eq_bigq(Lisp_Object l, Lisp_Object r)
{
	return (bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)));
}
static int
ent_eq_bigq_int(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	return (bigq_eql(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_eq_int_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	return (bigq_eql(ent_scratch_bigq, XBIGQ_DATA(r)));
}
static int
ent_eq_bigq_bigz(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return (bigq_eql(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_eq_bigz_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	return (bigq_eql(ent_scratch_bigq, XBIGQ_DATA(r)));
}

static int
ent_ne_bigq(Lisp_Object l, Lisp_Object r)
{
	return !(bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)));
}
static int
ent_ne_bigq_int(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(r));
	return !(bigq_eql(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_ne_int_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, ent_int(l));
	return !(bigq_eql(ent_scratch_bigq, XBIGQ_DATA(r)));
}
static int
ent_ne_bigq_bigz(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return !(bigq_eql(XBIGQ_DATA(l), ent_scratch_bigq));
}
static int
ent_ne_bigz_bigq(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	return !(bigq_eql(ent_scratch_bigq, XBIGQ_DATA(r)));
}


static inline Lisp_Object
_ent_lift_INT_T_BIGQ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_bigq(ent_int(number), 1UL);
}

static inline Lisp_Object
_ent_lift_BIGQ_T_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_div(ent_scratch_bigz,
		 XBIGQ_NUMERATOR(number),
		 XBIGQ_DENOMINATOR(number));
	return make_int(bigz_to_long(ent_scratch_bigz));
}

static inline Lisp_Object
_ent_lift_BIGZ_T_BIGQ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_set_long(ent_scratch_bigz, 1L);
	return make_bigq_bz(XBIGZ_DATA(number), ent_scratch_bigz);
}

static inline Lisp_Object
_ent_lift_BIGQ_T_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_div(ent_scratch_bigz,
		 XBIGQ_NUMERATOR(number),
		 XBIGQ_DENOMINATOR(number));
	return make_bigz_bz(ent_scratch_bigz);
}

#ifdef HAVE_FPFLOAT
static inline Lisp_Object
_ent_lift_FLOAT_T_BIGQ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigq_set_fpfloat(ent_scratch_bigq, XFLOAT_DATA(number));
	return make_bigq_bq(ent_scratch_bigq);
}

static inline Lisp_Object
_ent_lift_BIGQ_T_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	/* can result in an indef object */
	return make_float(bigq_to_fpfloat(XBIGQ_DATA(number)));
}
#endif


/* bigf ops */
static int
ent_mpf_zerop(Lisp_Object l)
{
	return (bigf_sign(XBIGF_DATA(l)) == 0);
}

static int
ent_mpf_onep(Lisp_Object l)
{
	return (bigf_to_fpfloat(XBIGF_DATA(l)) == 1.0f);
}

static int
ent_mpf_unitp(Lisp_Object unused)
{
	return 1;
}

static Lisp_Object
ent_sum_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_sum_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_sum_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGF_T_INT_T(r, l);
}
static Lisp_Object
ent_sum_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_sum_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGF_T_BIGZ_T(r, l);
}
static Lisp_Object
ent_sum_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_sum_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGF_T_BIGQ_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_sum_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_sum_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_sum_BIGF_T_FLOAT_T(r, l);
}
#endif

static Lisp_Object
ent_diff_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, ent_int(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_diff_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_diff_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

static Lisp_Object
ent_neg_BIGF_T(Lisp_Object l)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_neg(ent_scratch_bigf, XBIGF_DATA(l));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_prod_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_prod_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_prod_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGF_T_INT_T(r, l);
}
static Lisp_Object
ent_prod_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_prod_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGF_T_BIGZ_T(r, l);
}
static Lisp_Object
ent_prod_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_prod_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGF_T_BIGQ_T(r, l);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_prod_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_prod_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return ent_prod_BIGF_T_FLOAT_T(r, l);
}
#endif

static Lisp_Object
ent_div_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rr = ent_int(r);
	if (rr == 0) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, rr);
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, ent_int(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		int lsgn = bigz_sign(XBIGZ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		int lsgn = bigq_sign(XBIGQ_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_div_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0f) {
		int lsgn = bigf_sign(XBIGF_DATA(l));
		if (lsgn > 0)
			return make_indef(POS_INFINITY);
		else if (lsgn < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object
ent_div_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		if (XFLOAT_DATA(l) > 0.0f)
			return make_indef(POS_INFINITY);
		else if (XFLOAT_DATA(l) < 0.0f)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

static Lisp_Object
ent_inv_BIGF_T(Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		return make_indef(POS_INFINITY);
	}
	bigf_set_long(ent_scratch_bigf, 1L);
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_rem_BIGF_T(Lisp_Object unused, Lisp_Object r)
{
	return Qent_mpf_zero;;
}

static Lisp_Object
ent_mod_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0) {
		return Qent_mpf_zero;;
	}
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	bigf_trunc(ent_scratch_bigf, ent_scratch_bigf);
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
#if defined(bigf_pow)
static Lisp_Object
ent_pow_BIGF_T_integer_trivial(Lisp_Object l, Lisp_Object r)
{
	if (bigf_to_fpfloat(XBIGF_DATA(l)) == 1.0f) {
		return l;
	} else if (bigf_to_fpfloat(XBIGF_DATA(l)) == -1.0f) {
		if (!NILP(Fevenp(r)))
			return Qent_mpf_one;
		else
			return l;
	} else if (ent_mpf_zerop(l)) {
		/* actually only idiots want to compute 0-powers
		 * think I put a sleep(10) here to pretend that we're
		 * working rilly hard to solve that problem */
		if (ent_unrel_zerop(r))
			return Qent_mpf_one;
		else
			return Qent_mpf_zero;
	}
	return Qnull_pointer;
}

static Lisp_Object
ent_pow_BIGF_T_integer(Lisp_Object l, Lisp_Object r)
{
	unsigned long expo = 0;
	Lisp_Object result;

	/* trivial cases */
	if ((result = ent_pow_BIGF_T_integer_trivial(l, r)))
		return result;

	if (NILP(Fnonnegativep(r))) {
		return ent_unop_inv(
			ent_pow_BIGF_T_integer(l, ent_unop_neg(r)));
	}

	if (INTP(r)) {
		expo = XINT(r);
	} else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qrange_error, list1(r));
	} else {
		Fsignal(Qoperation_error, list1(r));
		return Qnil;
	}

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_pow(ent_scratch_bigf, XBIGF_DATA(l), expo);
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

/* relations */
static int
ent_lt_bigf(Lisp_Object l, Lisp_Object r)
{
	return (bigf_lt(XBIGF_DATA(l), XBIGF_DATA(r)));
}
static int
ent_lt_bigf_int(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	return (bigf_lt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_lt_int_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(l));
	return (bigf_lt(ent_scratch_bigf, XBIGF_DATA(r)));
}
static int
ent_lt_bigf_bigz(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	return (bigf_lt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_lt_bigz_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	return (bigf_lt(ent_scratch_bigf, XBIGF_DATA(r)));
}
static int
ent_lt_bigf_bigq(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	return (bigf_lt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_lt_bigq_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	return (bigf_lt(ent_scratch_bigf, XBIGF_DATA(r)));
}
#ifdef HAVE_FPFLOAT
static int
ent_lt_bigf_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	return (bigf_lt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_lt_fpfloat_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	return (bigf_lt(ent_scratch_bigf, XBIGF_DATA(r)));
}
#endif

static int
ent_gt_bigf(Lisp_Object l, Lisp_Object r)
{
	return (bigf_gt(XBIGF_DATA(l), XBIGF_DATA(r)));
}
static int
ent_gt_bigf_int(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	return (bigf_gt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_gt_int_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(l));
	return (bigf_gt(ent_scratch_bigf, XBIGF_DATA(r)));
}
static int
ent_gt_bigf_bigz(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	return (bigf_gt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_gt_bigz_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	return (bigf_gt(ent_scratch_bigf, XBIGF_DATA(r)));
}
static int
ent_gt_bigf_bigq(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	return (bigf_gt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_gt_bigq_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	return (bigf_gt(ent_scratch_bigf, XBIGF_DATA(r)));
}
#ifdef HAVE_FPFLOAT
static int
ent_gt_bigf_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	return (bigf_gt(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_gt_fpfloat_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	return (bigf_gt(ent_scratch_bigf, XBIGF_DATA(r)));
}
#endif

static int
ent_eq_bigf(Lisp_Object l, Lisp_Object r)
{
	return (bigf_eq(XBIGF_DATA(l), XBIGF_DATA(r)));
}
static int
ent_eq_bigf_int(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	return (bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_eq_int_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_eq_bigf_int(r, l);
}
static int
ent_eq_bigf_bigz(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	return (bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_eq_bigz_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_eq_bigf_bigz(r, l);
}
static int
ent_eq_bigf_bigq(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	return (bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_eq_bigq_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_eq_bigf_bigq(r, l);
}
#ifdef HAVE_FPFLOAT
static int
ent_eq_bigf_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	return (bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_eq_fpfloat_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	return (bigf_eq(ent_scratch_bigf, XBIGF_DATA(r)));
}
#endif

static int
ent_ne_bigf(Lisp_Object l, Lisp_Object r)
{
	return !(bigf_eq(XBIGF_DATA(l), XBIGF_DATA(r)));
}
static int
ent_ne_bigf_int(Lisp_Object l, Lisp_Object r)
{
	bigf_set_long(ent_scratch_bigf, ent_int(r));
	return !(bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_ne_int_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_ne_bigf_int(r, l);
}
static int
ent_ne_bigf_bigz(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	return !(bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_ne_bigz_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_ne_bigf_bigz(r, l);
}
static int
ent_ne_bigf_bigq(Lisp_Object l, Lisp_Object r)
{
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	return !(bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_ne_bigq_bigf(Lisp_Object l, Lisp_Object r)
{
	return ent_ne_bigf_bigq(r, l);
}
#ifdef HAVE_FPFLOAT
static int
ent_ne_bigf_fpfloat(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	return !(bigf_eq(XBIGF_DATA(l), ent_scratch_bigf));
}
static int
ent_ne_fpfloat_bigf(Lisp_Object l, Lisp_Object r)
{
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	return !(bigf_eq(ent_scratch_bigf, XBIGF_DATA(r)));
}
#endif


static Lisp_Object
ent_lift_INT_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	return make_bigf(ent_int(number), precision);
}

static Lisp_Object
ent_lift_BIGF_T_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	return make_int(bigf_to_long(XBIGF_DATA(number)));
}

static Lisp_Object
ent_lift_BIGZ_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigz_set_bigf(ent_scratch_bigz, XBIGF_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object
ent_lift_BIGQ_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_BIGQ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	bigq_set_bigf(ent_scratch_bigq, XBIGF_DATA(number));
	return make_bigq_bq(ent_scratch_bigq);
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_lift_FLOAT_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	/* can result in an indef object */
	return make_float(bigf_to_fpfloat(XBIGF_DATA(number)));
}
#endif

static Lisp_Object
ent_lift_BIGF_T_BIGF_T(Lisp_Object number, ent_lift_args_t la)
{
	unsigned long precision = la->precision;

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set(ent_scratch_bigf, XBIGF_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}


static inline void
ent_mpz_nullary_optable_init(void)
{
	Qent_mpz_zero = make_bigz(0L);
	Qent_mpz_one = make_bigz(1L);
	staticpro(&Qent_mpz_zero);
	staticpro(&Qent_mpz_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, BIGZ_T, Qent_mpz_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, BIGZ_T, Qent_mpz_one);
}

static inline void
ent_mpz_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, BIGZ_T, ent_neg_BIGZ_T);
	ent_unop_register(ASE_UNARY_OP_INV, BIGZ_T, ent_inv_BIGZ_T);
}

static inline void
ent_mpz_binary_optable_init(void)
{
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, BIGZ_T, ent_sum_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, INT_T, ent_sum_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, BIGZ_T, ent_sum_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, BIGZ_T, ent_sum_FLOAT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, FLOAT_T, ent_sum_BIGZ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, BIGZ_T, ent_diff_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, INT_T, ent_diff_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, BIGZ_T, ent_diff_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, BIGZ_T, ent_diff_FLOAT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, FLOAT_T, ent_diff_BIGZ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, BIGZ_T, ent_prod_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, INT_T, ent_prod_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, BIGZ_T, ent_prod_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, BIGZ_T, ent_prod_FLOAT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, FLOAT_T, ent_prod_BIGZ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, BIGZ_T, ent_div_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, INT_T, ent_div_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, BIGZ_T, ent_div_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, BIGZ_T, ent_div_FLOAT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, FLOAT_T, ent_div_BIGZ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGZ_T, ent_quo_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, INT_T, ent_quo_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, BIGZ_T, ent_quo_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGZ_T, ent_div_FLOAT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, FLOAT_T, ent_div_BIGZ_T_FLOAT_T);
#endif

	/* remainders */
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGZ_T, BIGZ_T, ent_rem_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGZ_T, INT_T, ent_rem_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   INT_T, BIGZ_T, ent_rem_INT_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGZ_T, BIGZ_T, ent_rem_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGZ_T, INT_T, ent_rem_BIGZ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   INT_T, BIGZ_T, ent_rem_INT_T_BIGZ_T);

	/* powers */
	ent_binop_register(ASE_BINARY_OP_POW,
			   INT_T, INT_T, ent_pow_integer_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   INT_T, BIGZ_T, ent_pow_integer_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGZ_T, INT_T, ent_pow_integer_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGZ_T, BIGZ_T, ent_pow_integer_integer);
}

static inline void
ent_mpz_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, BIGZ_T, ent_mpz_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, BIGZ_T, ent_mpz_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, BIGZ_T, ent_mpz_unitp);
}

static inline void
ent_mpz_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, BIGZ_T, ent_lt_bigz);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, BIGZ_T, ent_gt_bigz);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, BIGZ_T, ent_eq_bigz);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, BIGZ_T, ent_ne_bigz);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, INT_T, ent_lt_bigz_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, INT_T, ent_gt_bigz_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, INT_T, ent_eq_bigz_int);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, INT_T, ent_ne_bigz_int);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, BIGZ_T, ent_lt_int_bigz);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, BIGZ_T, ent_gt_int_bigz);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, BIGZ_T, ent_eq_int_bigz);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, BIGZ_T, ent_ne_int_bigz);

#ifdef HAVE_FPFLOAT
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, FLOAT_T, ent_lt_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, FLOAT_T, ent_gt_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, FLOAT_T, ent_eq_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, FLOAT_T, ent_ne_bigzq_float);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, BIGZ_T, ent_lt_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, BIGZ_T, ent_gt_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, BIGZ_T, ent_eq_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, BIGZ_T, ent_ne_float_bigzq);
#endif
}

static inline void
ent_mpz_lifttable_init(void)
{
	ent_lift_register(BIGZ_T, INT_T, _ent_lift_BIGZ_T_INT_T);
	ent_lift_register(INT_T, BIGZ_T, _ent_lift_INT_T_BIGZ_T);
#ifdef HAVE_FPFLOAT
	ent_lift_register(BIGZ_T, FLOAT_T, _ent_lift_BIGZ_T_FLOAT_T);
	ent_lift_register(FLOAT_T, BIGZ_T, _ent_lift_FLOAT_T_BIGZ_T);
#endif
	ent_lift_register(INDEF_T, BIGZ_T, ent_lift_INDEF_T_COMPARABLE);
}

void init_optables_BIGZ_T (void)
{
	ent_mpz_nullary_optable_init();
	ent_mpz_unary_optable_init();
	ent_mpz_binary_optable_init();
	ent_mpz_unary_reltable_init();
	ent_mpz_binary_reltable_init();
	ent_mpz_lifttable_init();
}


static inline void
ent_mpq_nullary_optable_init(void)
{
	Qent_mpq_zero = make_bigq(0L, 1L);
	Qent_mpq_one = make_bigq(1L, 1L);
	staticpro(&Qent_mpq_zero);
	staticpro(&Qent_mpq_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, BIGQ_T, Qent_mpq_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, BIGQ_T, Qent_mpq_one);
}

static inline void
ent_mpq_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, BIGQ_T, ent_neg_BIGQ_T);
	ent_unop_register(ASE_UNARY_OP_INV, BIGQ_T, ent_inv_BIGQ_T);
	ent_unop_register(ASE_UNARY_OP_INV, INT_T, ent_inv_INT_T);
}

static inline void
ent_mpq_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, BIGQ_T, ent_sum_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, INT_T, ent_sum_BIGQ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, BIGQ_T, ent_sum_INT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, BIGZ_T, ent_sum_BIGQ_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, BIGQ_T, ent_sum_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, BIGQ_T, ent_sum_FLOAT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, FLOAT_T, ent_sum_BIGQ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, BIGQ_T, ent_diff_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, INT_T, ent_diff_BIGQ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, BIGQ_T, ent_diff_INT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, BIGZ_T, ent_diff_BIGQ_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, BIGQ_T, ent_diff_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, BIGQ_T, ent_diff_FLOAT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, FLOAT_T, ent_diff_BIGQ_T_FLOAT_T);
#endif

	/* products */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, BIGQ_T, ent_prod_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, INT_T, ent_prod_BIGQ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, BIGQ_T, ent_prod_INT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, BIGZ_T, ent_prod_BIGQ_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, BIGQ_T, ent_prod_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, BIGQ_T, ent_prod_FLOAT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, FLOAT_T, ent_prod_BIGQ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, BIGQ_T, ent_div_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, INT_T, ent_div_BIGQ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, BIGQ_T, ent_div_INT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, BIGZ_T, ent_div_BIGQ_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, BIGQ_T, ent_div_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, BIGQ_T, ent_div_FLOAT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, FLOAT_T, ent_div_BIGQ_T_FLOAT_T);
#endif

	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, INT_T, ent_quo_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGQ_T, ent_div_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, INT_T, ent_div_BIGQ_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, BIGQ_T, ent_div_INT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGZ_T, ent_div_BIGQ_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGQ_T, ent_div_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGQ_T, ent_div_FLOAT_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, FLOAT_T, ent_div_BIGQ_T_FLOAT_T);
#endif

	/* remainders */
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGQ_T, BIGQ_T, ent_rem_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGQ_T, BIGQ_T, ent_mod_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGQ_T, INT_T, ent_pow_BIGQ_T_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGQ_T, BIGZ_T, ent_pow_BIGQ_T_integer);
}

static inline void
ent_mpq_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, BIGQ_T, ent_mpq_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, BIGQ_T, ent_mpq_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, BIGQ_T, ent_mpq_unitp);
}

static inline void
ent_mpq_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, BIGQ_T, ent_lt_bigq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, BIGQ_T, ent_gt_bigq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, BIGQ_T, ent_eq_bigq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, BIGQ_T, ent_ne_bigq);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, INT_T, ent_lt_bigq_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, INT_T, ent_gt_bigq_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, INT_T, ent_eq_bigq_int);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, INT_T, ent_ne_bigq_int);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, BIGQ_T, ent_lt_int_bigq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, BIGQ_T, ent_gt_int_bigq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, BIGQ_T, ent_eq_int_bigq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, BIGQ_T, ent_ne_int_bigq);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, BIGZ_T, ent_lt_bigq_bigz);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, BIGZ_T, ent_gt_bigq_bigz);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, BIGZ_T, ent_eq_bigq_bigz);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, BIGZ_T, ent_ne_bigq_bigz);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, BIGQ_T, ent_lt_bigz_bigq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, BIGQ_T, ent_gt_bigz_bigq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, BIGQ_T, ent_eq_bigz_bigq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, BIGQ_T, ent_ne_bigz_bigq);

#ifdef HAVE_FPFLOAT
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, FLOAT_T, ent_lt_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, FLOAT_T, ent_gt_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, FLOAT_T, ent_eq_bigzq_float);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, FLOAT_T, ent_ne_bigzq_float);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, BIGQ_T, ent_lt_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, BIGQ_T, ent_gt_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, BIGQ_T, ent_eq_float_bigzq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, BIGQ_T, ent_ne_float_bigzq);
#endif
}

static inline void
ent_mpq_lifttable_init(void)
{
	ent_lift_register(BIGQ_T, INT_T, _ent_lift_BIGQ_T_INT_T);
	ent_lift_register(INT_T, BIGQ_T, _ent_lift_INT_T_BIGQ_T);
	ent_lift_register(BIGQ_T, BIGZ_T, _ent_lift_BIGQ_T_BIGZ_T);
	ent_lift_register(BIGZ_T, BIGQ_T, _ent_lift_BIGZ_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_lift_register(BIGQ_T, FLOAT_T, _ent_lift_BIGQ_T_FLOAT_T);
	ent_lift_register(FLOAT_T, BIGQ_T, _ent_lift_FLOAT_T_BIGQ_T);
#endif
	ent_lift_register(INDEF_T, BIGQ_T, ent_lift_INDEF_T_COMPARABLE);
}

void init_optables_BIGQ_T (void)
{
	ent_mpq_nullary_optable_init();
	ent_mpq_unary_optable_init();
	ent_mpq_binary_optable_init();
	ent_mpq_unary_reltable_init();
	ent_mpq_binary_reltable_init();
	ent_mpq_lifttable_init();
}


static inline void
ent_mpf_nullary_optable_init(void)
{
	Qent_mpf_zero = make_bigf(0.0f, internal_get_precision(Qnil));
	Qent_mpf_one = make_bigf(1.0f, internal_get_precision(Qnil));
	staticpro(&Qent_mpf_zero);
	staticpro(&Qent_mpf_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, BIGF_T, Qent_mpf_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, BIGF_T, Qent_mpf_one);
}

static inline void
ent_mpf_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, BIGF_T, ent_neg_BIGF_T);
	ent_unop_register(ASE_UNARY_OP_INV, BIGF_T, ent_inv_BIGF_T);
}

static inline void
ent_mpf_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, BIGF_T, ent_sum_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, INT_T, ent_sum_BIGF_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, BIGF_T, ent_sum_INT_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, BIGZ_T, ent_sum_BIGF_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, BIGF_T, ent_sum_BIGZ_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, BIGQ_T, ent_sum_BIGF_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, BIGF_T, ent_sum_BIGQ_T_BIGF_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, FLOAT_T, ent_sum_BIGF_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, BIGF_T, ent_sum_FLOAT_T_BIGF_T);
#endif
	/* diffs */
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, BIGF_T, ent_diff_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, INT_T, ent_diff_BIGF_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, BIGF_T, ent_diff_INT_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, BIGZ_T, ent_diff_BIGF_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, BIGF_T, ent_diff_BIGZ_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, BIGQ_T, ent_diff_BIGF_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, BIGF_T, ent_diff_BIGQ_T_BIGF_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, FLOAT_T, ent_diff_BIGF_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, BIGF_T, ent_diff_FLOAT_T_BIGF_T);
#endif
	/* prods */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, BIGF_T, ent_prod_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, INT_T, ent_prod_BIGF_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, BIGF_T, ent_prod_INT_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, BIGZ_T, ent_prod_BIGF_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, BIGF_T, ent_prod_BIGZ_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, BIGQ_T, ent_prod_BIGF_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, BIGF_T, ent_prod_BIGQ_T_BIGF_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, FLOAT_T, ent_prod_BIGF_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, BIGF_T, ent_prod_FLOAT_T_BIGF_T);
#endif

	/* divisions and quotients */
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, BIGF_T, ent_div_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, INT_T, ent_div_BIGF_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, BIGF_T, ent_div_INT_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, BIGZ_T, ent_div_BIGF_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, BIGF_T, ent_div_BIGZ_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, BIGQ_T, ent_div_BIGF_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, BIGF_T, ent_div_BIGQ_T_BIGF_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, FLOAT_T, ent_div_BIGF_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, BIGF_T, ent_div_FLOAT_T_BIGF_T);
#endif
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGF_T, ent_div_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, INT_T, ent_div_BIGF_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, BIGF_T, ent_div_INT_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGZ_T, ent_div_BIGF_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGF_T, ent_div_BIGZ_T_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGQ_T, ent_div_BIGF_T_BIGQ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGF_T, ent_div_BIGQ_T_BIGF_T);
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, FLOAT_T, ent_div_BIGF_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGF_T, ent_div_FLOAT_T_BIGF_T);
#endif
	/* remainders */
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGF_T, BIGF_T, ent_rem_BIGF_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGF_T, BIGF_T, ent_mod_BIGF_T);
#ifdef bigf_pow
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGF_T, INT_T, ent_pow_BIGF_T_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGF_T, BIGZ_T, ent_pow_BIGF_T_integer);
#endif
}

static inline void
ent_mpf_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, BIGF_T, ent_mpf_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, BIGF_T, ent_mpf_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, BIGF_T, ent_mpf_unitp);
}

static inline void
ent_mpf_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, BIGF_T, ent_lt_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, BIGF_T, ent_gt_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, BIGF_T, ent_eq_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, BIGF_T, ent_ne_bigf);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, INT_T, ent_lt_bigf_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, INT_T, ent_gt_bigf_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, INT_T, ent_eq_bigf_int);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, INT_T, ent_ne_bigf_int);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, BIGF_T, ent_lt_int_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, BIGF_T, ent_gt_int_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, BIGF_T, ent_eq_int_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, BIGF_T, ent_ne_int_bigf);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, BIGZ_T, ent_lt_bigf_bigz);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, BIGZ_T, ent_gt_bigf_bigz);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, BIGZ_T, ent_eq_bigf_bigz);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, BIGZ_T, ent_ne_bigf_bigz);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGZ_T, BIGF_T, ent_lt_bigz_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGZ_T, BIGF_T, ent_gt_bigz_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGZ_T, BIGF_T, ent_eq_bigz_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGZ_T, BIGF_T, ent_ne_bigz_bigf);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, BIGQ_T, ent_lt_bigf_bigq);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, BIGQ_T, ent_gt_bigf_bigq);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, BIGQ_T, ent_eq_bigf_bigq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, BIGQ_T, ent_ne_bigf_bigq);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGQ_T, BIGF_T, ent_lt_bigq_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGQ_T, BIGF_T, ent_gt_bigq_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGQ_T, BIGF_T, ent_eq_bigq_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGQ_T, BIGF_T, ent_ne_bigq_bigf);

#ifdef HAVE_FPFLOAT
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    BIGF_T, FLOAT_T, ent_lt_bigf_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    BIGF_T, FLOAT_T, ent_gt_bigf_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGF_T, FLOAT_T, ent_eq_bigf_fpfloat);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGF_T, FLOAT_T, ent_ne_bigf_fpfloat);

	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, BIGF_T, ent_lt_fpfloat_bigf);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, BIGF_T, ent_gt_fpfloat_bigf);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, BIGF_T, ent_eq_fpfloat_bigf);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, BIGF_T, ent_ne_fpfloat_bigf);
#endif
}

static inline void
ent_mpf_lifttable_init(void)
{
	ent_lift_register(INT_T, BIGF_T, ent_lift_INT_T_BIGF_T);
	ent_lift_register(BIGF_T, INT_T, ent_lift_BIGF_T_INT_T);
	ent_lift_register(BIGZ_T, BIGF_T, ent_lift_BIGZ_T_BIGF_T);
	ent_lift_register(BIGF_T, BIGZ_T, ent_lift_BIGF_T_BIGZ_T);
	ent_lift_register(BIGQ_T, BIGF_T, ent_lift_BIGQ_T_BIGF_T);
	ent_lift_register(BIGF_T, BIGQ_T, ent_lift_BIGF_T_BIGQ_T);
#ifdef HAVE_FPFLOAT
	ent_lift_register(FLOAT_T, BIGF_T, ent_lift_FLOAT_T_BIGF_T);
	ent_lift_register(BIGF_T, FLOAT_T, ent_lift_BIGF_T_FLOAT_T);
#endif
	ent_lift_register(BIGF_T, BIGF_T, ent_lift_BIGF_T_BIGF_T);
	ent_lift_register(INDEF_T, BIGF_T, ent_lift_INDEF_T_COMPARABLE);
}

void init_optables_BIGF_T (void)
{
	ent_mpf_nullary_optable_init();
	ent_mpf_unary_optable_init();
	ent_mpf_binary_optable_init();
	ent_mpf_unary_reltable_init();
	ent_mpf_binary_reltable_init();
	ent_mpf_lifttable_init();
}

void init_ent_mpz()
{
	/* Prepare the bignum/bigfloat random number generator */
	gmp_randinit_default(random_state);
	gmp_randseed_ui(random_state, getpid() + time (NULL));

	bigz_init(ent_scratch_bigz);
}

void init_ent_mpq(void)
{
	bigq_init(ent_scratch_bigq);
}

void init_ent_mpf()
{
	/* The smallest number that is printed without exponents */
	mpf_init_set_d(float_print_min, 0.001);

	/* The largest number that is printed without exponents */
	mpf_init_set_ui(float_print_max, 10000000UL);

	bigf_init(ent_scratch_bigf);
}

void syms_of_ent_mpz(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigz);
}

void syms_of_ent_mpq(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigq);
}

void syms_of_ent_mpf(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigf);

	DEFSUBR(Fbigf_get_precision);
	DEFSUBR(Fbigf_set_precision);
}

void vars_of_ent_mpz(void)
{
	Fprovide(intern("bignum")); /* for XE compatibility */
	Fprovide(intern("bigz"));
}

void vars_of_ent_mpq(void)
{
	Fprovide(intern("ratio")); /* for XE compatibility */
	Fprovide(intern("bigq"));
}

void vars_of_ent_mpf(void)
{
	bigf_set_default_prec(128UL);

	Fprovide(intern("bigfloat")); /* for XE compatibility */
	Fprovide(intern("bigf"));
}
