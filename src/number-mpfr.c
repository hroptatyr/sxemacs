/*
  number-mpfr.c -- Numeric types for SXEmacs
  Copyright (C) 2005 Sebastian Freundt

  Author:  Sebastian Freundt

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

#include "number-mpfr.h"


Lisp_Object Veuler;
Lisp_Object Vpi;


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

		return str;
	}
}

#ifndef WITH_GMP
/* We need the next two functions since GNU MP insists on giving us an extra
   parameter. */
static void *gmp_realloc (void *ptr, size_t old_size, size_t new_size)
{
	return xrealloc(ptr, new_size);
}

static void gmp_free (void *ptr, size_t size)
{
	free(ptr);
	ptr = NULL;
}
#endif	/* WITH_GMP */

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

#ifdef HAVE_MPC
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
static Lisp_Object ent_sum_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_neg_BIGFR_T(Lisp_Object l)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_neg(ent_scratch_bigfr, XBIGFR_DATA(l));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigfr_set_prec(ent_scratch_bigfr,
		      max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_inv_BIGFR_T(Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigfr_set_long(ent_scratch_bigfr, 1L);
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_rem_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	return make_bigfr(0.0, internal_get_precision(Qnil));
}
static Lisp_Object ent_mod_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigfr_set_prec(ent_scratch_bigfr,
		      max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	bigfr_trunc(ent_scratch_bigfr, ent_scratch_bigfr);
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#ifdef bigfr_pow
static Lisp_Object ent_pow_BIGFR_T_integer(Lisp_Object l, Lisp_Object r)
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
#ifdef HAVE_MPZ
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, r);
	} 
#endif
	else
		Fsignal(Qarith_error, r);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_pow(ent_scratch_bigfr, XBIGFR_DATA(l), expo);
	result = make_bigfr_bfr(ent_scratch_bigfr);

	if (sign == 1)
		return result;
	else if (sign == -1)
		return ent_optable_inv[BIGFR_T](result);

	/* not reached */
	Fsignal(Qarith_error, Qnil);
	return Qzero;
}
#endif

/* relations */
static Lisp_Object ent_lt_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_lt(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_gt(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qnil : Qt;
}

void init_optables_BIGFR_T(void)
{
	ent_optable_sum[BIGFR_T][BIGFR_T] = ent_sum_BIGFR_T;
	ent_optable_diff[BIGFR_T][BIGFR_T] = ent_diff_BIGFR_T;
	ent_optable_prod[BIGFR_T][BIGFR_T] = ent_prod_BIGFR_T;
	ent_optable_neg[BIGFR_T] = ent_neg_BIGFR_T;
	ent_optable_div[BIGFR_T][BIGFR_T] = ent_div_BIGFR_T;
	ent_optable_inv[BIGFR_T] = ent_inv_BIGFR_T;
	ent_optable_quo[BIGFR_T][BIGFR_T] = ent_div_BIGFR_T;
	ent_optable_rem[BIGFR_T][BIGFR_T] = ent_rem_BIGFR_T;
	ent_optable_mod[BIGFR_T][BIGFR_T] = ent_mod_BIGFR_T;
#ifdef bigfr_pow
	ent_optable_pow[BIGFR_T][INT_T] = ent_pow_BIGFR_T_integer;
#ifdef HAVE_MPZ
	ent_optable_pow[BIGFR_T][BIGZ_T] = ent_pow_BIGFR_T_integer;
#endif
#endif

	ent_optable_lt[BIGFR_T][BIGFR_T] = ent_lt_BIGFR_T;
	ent_optable_gt[BIGFR_T][BIGFR_T] = ent_gt_BIGFR_T;
	ent_optable_eq[BIGFR_T][BIGFR_T] = ent_eq_BIGFR_T;
	ent_optable_ne[BIGFR_T][BIGFR_T] = ent_ne_BIGFR_T;
	ent_optable_vallt[BIGFR_T][BIGFR_T] = ent_lt_BIGFR_T;
	ent_optable_valgt[BIGFR_T][BIGFR_T] = ent_gt_BIGFR_T;
	ent_optable_valeq[BIGFR_T][BIGFR_T] = ent_eq_BIGFR_T;
	ent_optable_valne[BIGFR_T][BIGFR_T] = ent_ne_BIGFR_T;
}

void
init_number_mpfr ()
{
#ifndef WITH_GMP
	mp_set_memory_functions((void *(*) (size_t))xmalloc,
				gmp_realloc,
				gmp_free);
#endif

	if (initialized) {
		Veuler = make_bigfr(0.0, 4096UL);
		mpfr_const_euler(XBIGFR_DATA(Veuler), GMP_RNDN);
		Vpi = make_bigfr(0.0, 4096UL);
		mpfr_const_pi(XBIGFR_DATA(Vpi), GMP_RNDN);
	}
}
