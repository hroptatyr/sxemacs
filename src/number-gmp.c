/*
  number.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005 Sebastian Freundt

  Author:  Jerry James
  Backport:  Sebastian Freundt

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "number-gmp.h"

static mpf_t float_print_min, float_print_max;
gmp_randstate_t random_state;

Bufbyte *
bigf_to_string(mpf_t f, int base)
{
	mp_exp_t expt;
	Bufbyte *str = (Bufbyte *)mpf_get_str(NULL, &expt, base, 0, f);
	const int sign = mpf_sgn(f);
	const int neg = (sign < 0) ? 1 : 0;
	int len = strlen((char *)str) + 1;  /* Count the null terminator */

	/* Move digits down to insert a radix point */
	if (expt <= 0) {
		/* We need room for a radix point and leading zeroes */
		const int space = -expt + 2;
		XREALLOC_ARRAY(str, Bufbyte, len + space);
		memmove(&str[space + neg], &str[neg], len - neg);
		memset(&str[neg], '0', space);
		str[neg + 1] = '.';
		len += space;
	} else if (expt < len) {
		/* We just need room for a radix point */
		XREALLOC_ARRAY(str, Bufbyte, len + 1);
		memmove(&str[expt + neg + 1],
			&str[expt + neg],
			len - (expt + neg));
		str[expt + neg] = '.';
		len++;
	} else {
		/* We need room for trailing zeroes */
		XREALLOC_ARRAY(str, Bufbyte, expt + 1);
		memset(&str[len-1], '0', expt+2-len);
		str[expt] = '\0';
		len = expt + 1;
	}
#if 0
	/* never want this here */
	/* Computerized scientific notation */
	/* We need room for a radix point, format identifier, and exponent */
	const int space = (expt < 0)
		? (int)(log(-expt) / log(base)) + 3
		: (int)(log(expt) / log(base)) + 2;
	XREALLOC_ARRAY(str, Bufbyte, len + space);
	memmove(&str[neg + 2], &str[neg + 1], len - neg);
	str[len + 1] = 'l';
	sprintf ((char *)&str[len + 2], "%ld", expt);
}
#endif
	return str;
}

/* We need the next two functions since GNU MP insists on giving us an extra
   parameter. */
static void *gmp_realloc (void *ptr, size_t old_size, size_t new_size)
{
	if (old_size);
	return xrealloc(ptr, new_size);
}

static void gmp_free (void *ptr, size_t size)
{
	if (size);
	free(ptr);
	ptr = NULL;
}


/* reader funs */

Lisp_Object read_bigz_string(char *cp)
{
	Lisp_Object result;
	bigz bz;
	bigz_init(bz);
		
	/* MPZ bigz_set_string has no effect
	 * with initial + sign */
	if (*cp == '+')
		cp++;

	bigz_set_string(bz, (const char*)cp, 0);
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
	if (*cp == '+')
		cp++;

	bigq_set_string(bq, cp, 0);
	bigq_canonicalize(bq);

	result = Fcanonicalize_number(make_bigq_bq(bq));

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
	if (*cp == '+')
		cp++;

	bigf_set_string(bf, cp, 0);
	result = make_bigf_bf(bf);

	bigf_fini(bf);
	return result;
}

/* bigz ops */
static Lisp_Object ent_sum_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_sum_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_sum_BIGZ_T_generic(Lisp_Object l, Lisp_Object r)
{
	/* generic addition code, just coerce and call auto-sum fun */
	l = Fcoerce_number(l, Qbigz, Qnil);
	r = Fcoerce_number(r, Qbigz, Qnil);
	return ent_sum_BIGZ_T(l, r);
}
static Lisp_Object ent_diff_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_diff_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_diff_BIGZ_T_generic(Lisp_Object l, Lisp_Object r)
{
	/* generic diff code, just coerce and call auto-diff fun */
	l = Fcoerce_number(l, Qbigz, Qnil);
	r = Fcoerce_number(r, Qbigz, Qnil);
	return ent_diff_BIGZ_T(l, r);
}
static Lisp_Object ent_neg_BIGZ_T(Lisp_Object l)
{
	bigz_neg(ent_scratch_bigz, XBIGZ_DATA(l));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_prod_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_div_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_MPQ
static Lisp_Object ent_quo_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_bigq_bz(XBIGZ_DATA(l), XBIGZ_DATA(r));
}
#endif
static Lisp_Object ent_inv_BIGZ_T(Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_set_long(ent_scratch_bigz, 1L);
	bigz_div(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_rem_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_mod(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_pow_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
{
	unsigned long expo;
	int sign;
	Lisp_Object result;

	if (NILP(Fnonnegativep(r))) {
		sign = -1;
		r = ent_optable_neg[get_number_type(r)](r);
	} else
		sign = 1;

	if (INTP(r))
		expo = XINT(r);
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, Qnil);
	} else
		Fsignal(Qdomain_error, Qnil);

	bigz_pow(ent_scratch_bigz, XBIGZ_DATA(l), expo);

	result = make_bigz_bz(ent_scratch_bigz);

	if (sign == 1)
		return result;
	else if (sign == -1) {
		sign = bigz_to_int(XBIGZ_DATA(result));

		if (sign == 1)
			return make_bigz(1L);
		else if (sign == -1)
			return (expo & 1) ? make_bigz(-1L) : make_bigz(1L);
		else
			return make_bigz(0L);
	}

	/* not reached */
	Fsignal(Qarith_error, Qnil);
	return Qzero;
}

/* relations */
static Lisp_Object ent_lt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_lt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_gt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qnil : Qt;
}

/* bigq ops */
static Lisp_Object ent_sum_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_add(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_diff_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_neg_BIGQ_T(Lisp_Object l)
{
	bigq_neg(ent_scratch_bigq, XBIGQ_DATA(l));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_prod_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_div_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_inv_BIGQ_T(Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigq_set_long(ent_scratch_bigq, 1L);
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_rem_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	/* actually the zero constructor should be called */
	return make_bigq(0L, 1UL);
}
static Lisp_Object ent_mod_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	bigz_div(ent_scratch_bigz,
		 bigq_numerator(ent_scratch_bigq),
		 bigq_denominator(ent_scratch_bigq));

	bigq_set_bigz(ent_scratch_bigq, ent_scratch_bigz);
	bigq_mul(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_pow_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object num, den;
	number_type n;
	int sign;

	n = get_number_type(r);

	if (NILP(Fnonnegativep(r))) {
		sign = -1;
		r = ent_optable_neg[n](r);
	} else
		sign = 1;

	num = ent_optable_pow[BIGZ_T][n](make_bigz_bz(XBIGQ_NUMERATOR(l)), r);
	den = ent_optable_pow[BIGZ_T][n](make_bigz_bz(XBIGQ_DENOMINATOR(l)), r);

	if (!BIGZP(num) || !BIGZP(den))
		Fsignal(Qdomain_error, Qnil);

	if (sign == 1)
		return make_bigq_bz(XBIGZ_DATA(num), XBIGZ_DATA(den));
	else if (sign == -1)
		return make_bigq_bz(XBIGZ_DATA(den), XBIGZ_DATA(num));

	/* not reached */
	Fsignal(Qarith_error, Qnil);
	return Qzero;
}

/* relations */
static Lisp_Object ent_lt_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_lt(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_gt(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qnil : Qt;
}

/* bigf ops */
static Lisp_Object ent_sum_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_neg_BIGF_T(Lisp_Object l)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_neg(ent_scratch_bigf, XBIGF_DATA(l));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_inv_BIGF_T(Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigf_set_long(ent_scratch_bigf, 1L);
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_rem_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	return make_bigf(0.0, internal_get_precision(Qnil));
}
static Lisp_Object ent_mod_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	bigf_trunc(ent_scratch_bigf, ent_scratch_bigf);
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef bigf_pow
static Lisp_Object ent_pow_BIGF_T_integer(Lisp_Object l, Lisp_Object r)
{
	unsigned long expo;
	int sign;
	Lisp_Object result;

	if (NILP(Fnonnegativep(r))) {
		sign = -1;
		r = ent_optable_neg[get_number_type(r)](r);
	} else
		sign = 1;

	if (INTP(r))
		expo = XREALINT(r);
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, Qnil);
	} else
		Fsignal(Qdomain_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_pow(ent_scratch_bigf, XBIGF_DATA(l), expo);
	result = make_bigf_bf(ent_scratch_bigf);

	if (sign == 1)
		return result;
	else if (sign == -1)
		return ent_optable_inv[BIGF_T](result);

	/* not reached */
	Fsignal(Qarith_error, Qnil);
	return Qzero;
}
#endif

/* relations */
static Lisp_Object ent_lt_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_lt(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_gt(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_eql(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_eql(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qnil : Qt;
}

void init_optables_BIGZ_T (void)
{
	number_type i;

	for (i = 0; i < NUMBER_OF_TYPES; i++) {
		ent_optable_sum[i][BIGZ_T] = ent_sum_BIGZ_T_generic;
		ent_optable_sum[BIGZ_T][i] = ent_sum_BIGZ_T_generic;
		ent_optable_diff[i][BIGZ_T] = ent_diff_BIGZ_T_generic;
		ent_optable_diff[BIGZ_T][i] = ent_diff_BIGZ_T_generic;
	}
	/* now the specialised sums and diffs */
	ent_optable_sum[BIGZ_T][BIGZ_T] = ent_sum_BIGZ_T;
	ent_optable_sum[BIGZ_T][INT_T] = ent_sum_BIGZ_T_INT_T;
	ent_optable_diff[BIGZ_T][BIGZ_T] = ent_diff_BIGZ_T;
	ent_optable_diff[BIGZ_T][INT_T] = ent_diff_BIGZ_T_INT_T;

	ent_optable_prod[BIGZ_T][BIGZ_T] = ent_prod_BIGZ_T;
	ent_optable_neg[BIGZ_T] = ent_neg_BIGZ_T;
	ent_optable_div[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
	ent_optable_inv[BIGZ_T] = ent_inv_BIGZ_T;
#ifdef HAVE_MPQ
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_quo_BIGZ_T;
#else
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
#endif
	ent_optable_rem[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_mod[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_pow[BIGZ_T][INT_T] = ent_pow_BIGZ_T_integer;
	ent_optable_pow[BIGZ_T][BIGZ_T] = ent_pow_BIGZ_T_integer;

	ent_optable_lt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_gt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_eq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_ne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;
	ent_optable_vallt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_valgt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_valeq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_valne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;
}

void init_optables_BIGQ_T (void)
{
	ent_optable_sum[BIGQ_T][BIGQ_T] = ent_sum_BIGQ_T;
	ent_optable_diff[BIGQ_T][BIGQ_T] = ent_diff_BIGQ_T;
	ent_optable_prod[BIGQ_T][BIGQ_T] = ent_prod_BIGQ_T;
	ent_optable_neg[BIGQ_T] = ent_neg_BIGQ_T;
	ent_optable_div[BIGQ_T][BIGQ_T] = ent_div_BIGQ_T;
	ent_optable_inv[BIGQ_T] = ent_inv_BIGQ_T;
	ent_optable_quo[BIGQ_T][BIGQ_T] = ent_div_BIGQ_T;
	ent_optable_rem[BIGQ_T][BIGQ_T] = ent_rem_BIGQ_T;
	ent_optable_mod[BIGQ_T][BIGQ_T] = ent_mod_BIGQ_T;
	ent_optable_pow[BIGQ_T][INT_T] = ent_pow_BIGQ_T_integer;
	ent_optable_pow[BIGQ_T][BIGZ_T] = ent_pow_BIGQ_T_integer;

	ent_optable_lt[BIGQ_T][BIGQ_T] = ent_lt_BIGQ_T;
	ent_optable_gt[BIGQ_T][BIGQ_T] = ent_gt_BIGQ_T;
	ent_optable_eq[BIGQ_T][BIGQ_T] = ent_eq_BIGQ_T;
	ent_optable_ne[BIGQ_T][BIGQ_T] = ent_ne_BIGQ_T;
	ent_optable_vallt[BIGQ_T][BIGQ_T] = ent_lt_BIGQ_T;
	ent_optable_valgt[BIGQ_T][BIGQ_T] = ent_gt_BIGQ_T;
	ent_optable_valeq[BIGQ_T][BIGQ_T] = ent_eq_BIGQ_T;
	ent_optable_valne[BIGQ_T][BIGQ_T] = ent_ne_BIGQ_T;
}

void init_optables_BIGF_T (void)
{
	ent_optable_sum[BIGF_T][BIGF_T] = ent_sum_BIGF_T;
	ent_optable_diff[BIGF_T][BIGF_T] = ent_diff_BIGF_T;
	ent_optable_prod[BIGF_T][BIGF_T] = ent_prod_BIGF_T;
	ent_optable_neg[BIGF_T] = ent_neg_BIGF_T;
	ent_optable_div[BIGF_T][BIGF_T] = ent_div_BIGF_T;
	ent_optable_inv[BIGF_T] = ent_inv_BIGF_T;
	ent_optable_quo[BIGF_T][BIGF_T] = ent_div_BIGF_T;
	ent_optable_rem[BIGF_T][BIGF_T] = ent_rem_BIGF_T;
	ent_optable_mod[BIGF_T][BIGF_T] = ent_mod_BIGF_T;
#ifdef bigf_pow
	ent_optable_pow[BIGF_T][INT_T] = ent_pow_BIGF_T_integer;
	ent_optable_pow[BIGF_T][BIGZ_T] = ent_pow_BIGF_T_integer;
#endif

	ent_optable_lt[BIGF_T][BIGF_T] = ent_lt_BIGF_T;
	ent_optable_gt[BIGF_T][BIGF_T] = ent_gt_BIGF_T;
	ent_optable_eq[BIGF_T][BIGF_T] = ent_eq_BIGF_T;
	ent_optable_ne[BIGF_T][BIGF_T] = ent_ne_BIGF_T;
	ent_optable_vallt[BIGF_T][BIGF_T] = ent_lt_BIGF_T;
	ent_optable_valgt[BIGF_T][BIGF_T] = ent_gt_BIGF_T;
	ent_optable_valeq[BIGF_T][BIGF_T] = ent_eq_BIGF_T;
	ent_optable_valne[BIGF_T][BIGF_T] = ent_ne_BIGF_T;
}

void
init_number_gmp ()
{
	mp_set_memory_functions((void *(*) (size_t))xmalloc,
				gmp_realloc,
				gmp_free);

	/* The smallest number that is printed without exponents */
	mpf_init_set_d(float_print_min, 0.001);

	/* The largest number that is printed without exponents */
	mpf_init_set_ui(float_print_max, 10000000UL);

	/* Prepare the bignum/bigfloat random number generator */
	gmp_randinit_default(random_state);
	gmp_randseed_ui(random_state, getpid() + time (NULL));
}
