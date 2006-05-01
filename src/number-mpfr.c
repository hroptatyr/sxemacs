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
static Lisp_Object ent_sum_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, XINT(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_sum_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, XINT(l));
	bigfr_add(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#ifdef HAVE_MPZ
static Lisp_Object ent_sum_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_sum_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_add(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_sum_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_sum_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_add(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPF
static Lisp_Object ent_sum_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_sum_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_add(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef LISP_FLOAT_TYPE
static Lisp_Object ent_sum_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_add(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_sum_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_add(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif

static Lisp_Object ent_diff_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr,
		       max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, XINT(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, XINT(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#ifdef HAVE_MPZ
static Lisp_Object ent_diff_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_diff_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPF
static Lisp_Object ent_diff_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef LISP_FLOAT_TYPE
static Lisp_Object ent_diff_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_sub(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_diff_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_sub(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif

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
static Lisp_Object ent_prod_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, XINT(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, XINT(l));
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#ifdef HAVE_MPZ
static Lisp_Object ent_prod_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_prod_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPF
static Lisp_Object ent_prod_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef LISP_FLOAT_TYPE
static Lisp_Object ent_prod_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_mul(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_prod_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_mul(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif

static Lisp_Object ent_div_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (bigfr_sign(XBIGFR_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigfr_set_prec(ent_scratch_bigfr,
		      max(XBIGFR_GET_PREC(l), XBIGFR_GET_PREC(r)));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_BIGFR_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_long(ent_scratch_bigfr, XINT(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_INT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_long(ent_scratch_bigfr, XINT(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#ifdef HAVE_MPZ
static Lisp_Object ent_div_BIGFR_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_BIGZ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_div_BIGFR_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_BIGQ_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef HAVE_MPF
static Lisp_Object ent_div_BIGFR_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_BIGF_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif
#ifdef LISP_FLOAT_TYPE
static Lisp_Object ent_div_BIGFR_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(l));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(r));
	bigfr_div(ent_scratch_bigfr, XBIGFR_DATA(l), ent_scratch_bigfr);
	return make_bigfr_bfr(ent_scratch_bigfr);
}
static Lisp_Object ent_div_FLOAT_T_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(r));
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(l));
	bigfr_div(ent_scratch_bigfr, ent_scratch_bigfr, XBIGFR_DATA(r));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif

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
	unsigned long expo = 0;
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
#define ent_rel_BIGFR_T_intfloat(rel, l, r) do				\
{									\
	number_type nt;							\
									\
	nt = get_number_type(l);					\
	l = ent_optable_lift[nt][BIGFR_T](l, 0UL);			\
	nt = get_number_type(r);					\
	r = ent_optable_lift[nt][BIGFR_T](r, 0UL);			\
									\
	return ent_optable_##rel[BIGFR_T][BIGFR_T](l, r);		\
} while(0);

static Lisp_Object ent_lt_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_lt(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_lt_BIGFR_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGFR_T_intfloat(lt, l, r);
}

static Lisp_Object ent_gt_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_gt(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGFR_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGFR_T_intfloat(gt, l, r);
}

static Lisp_Object ent_eq_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGFR_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGFR_T_intfloat(eq, l, r);
}

static Lisp_Object ent_ne_BIGFR_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(XBIGFR_DATA(l), XBIGFR_DATA(r)))
		? Qnil : Qt;
}
static Lisp_Object ent_ne_BIGFR_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGFR_T_intfloat(ne, l, r);
}


static Lisp_Object
ent_lift_INT_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);
	number = ent_normalise_number(number);
	return make_bigfr(XREALINT(number), precision);
}

static Lisp_Object
ent_lift_BIGFR_T_INT_T(Lisp_Object number, unsigned long precision)
{
	/* warn about coercions of indefinite symbols */
	if (bigfr_inf_p(XBIGFR_DATA(number)))
		return make_indef(POS_INFINITY);
	if (bigfr_nan_p(XBIGFR_DATA(number)))
		return make_indef(NOT_A_NUMBER);

	return make_int(bigfr_to_long(XBIGFR_DATA(number)));
}

#ifdef HAVE_MPZ
static Lisp_Object
ent_lift_BIGZ_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static Lisp_Object
ent_lift_BIGFR_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	/* warn about coercions of indefinite symbols */
	if (bigfr_inf_p(XBIGFR_DATA(number)))
		return make_indef(POS_INFINITY);
	if (bigfr_nan_p(XBIGFR_DATA(number)))
		return make_indef(NOT_A_NUMBER);

	bigz_set_bigfr(ent_scratch_bigz, XBIGFR_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}
#endif	/* HAVE_MPZ */

#ifdef HAVE_MPQ
static Lisp_Object
ent_lift_BIGQ_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

#if 0				/* IMPLEMENT ME */
static Lisp_Object
ent_lift_BIGFR_T_BIGQ_T(Lisp_Object number, unsigned long precision)
{
	bigq_set_bigfr(ent_scratch_bigq, XBIGFR_DATA(number));
	return make_bigq_bq(ent_scratch_bigq);
}
#endif
#endif	/* HAVE_MPQ */

#ifdef HAVE_MPF
static Lisp_Object
ent_lift_BIGF_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static Lisp_Object
ent_lift_BIGFR_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	/* warn about coercions of indefinite symbols */
	if (bigfr_inf_p(XBIGFR_DATA(number)))
		return make_indef(POS_INFINITY);
	if (bigfr_nan_p(XBIGFR_DATA(number)))
		return make_indef(NOT_A_NUMBER);

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigfr(ent_scratch_bigf, XBIGFR_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif	/* HAVE_MPF */

#ifdef LISP_FLOAT_TYPE
static Lisp_Object
ent_lift_FLOAT_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_double(ent_scratch_bigfr, XFLOAT_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}

static Lisp_Object
ent_lift_BIGFR_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	return make_float(bigfr_to_double(XBIGFR_DATA(number)));
}
#endif

static Lisp_Object
ent_lift_BIGFR_T_BIGFR_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set(ent_scratch_bigfr, XBIGFR_DATA(number));
	return make_bigfr_bfr(ent_scratch_bigfr);
}


void init_optables_BIGFR_T(void)
{
	/* sums */
	ent_optable_sum[BIGFR_T][BIGFR_T] = ent_sum_BIGFR_T;
	ent_optable_sum[BIGFR_T][INT_T] = ent_sum_BIGFR_T_INT_T;
	ent_optable_sum[INT_T][BIGFR_T] = ent_sum_INT_T_BIGFR_T;
#ifdef HAVE_MPZ
	ent_optable_sum[BIGFR_T][BIGZ_T] = ent_sum_BIGFR_T_BIGZ_T;
	ent_optable_sum[BIGZ_T][BIGFR_T] = ent_sum_BIGZ_T_BIGFR_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_sum[BIGFR_T][BIGQ_T] = ent_sum_BIGFR_T_BIGQ_T;
	ent_optable_sum[BIGQ_T][BIGFR_T] = ent_sum_BIGQ_T_BIGFR_T;
#endif
#ifdef HAVE_MPF
	ent_optable_sum[BIGFR_T][BIGF_T] = ent_sum_BIGFR_T_BIGF_T;
	ent_optable_sum[BIGF_T][BIGFR_T] = ent_sum_BIGF_T_BIGFR_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_sum[BIGFR_T][FLOAT_T] = ent_sum_BIGFR_T_FLOAT_T;
	ent_optable_sum[FLOAT_T][BIGFR_T] = ent_sum_FLOAT_T_BIGFR_T;
#endif
	/* diffs */
	ent_optable_diff[BIGFR_T][BIGFR_T] = ent_diff_BIGFR_T;
	ent_optable_diff[BIGFR_T][INT_T] = ent_diff_BIGFR_T_INT_T;
	ent_optable_diff[INT_T][BIGFR_T] = ent_diff_INT_T_BIGFR_T;
#ifdef HAVE_MPZ
	ent_optable_diff[BIGFR_T][BIGZ_T] = ent_diff_BIGFR_T_BIGZ_T;
	ent_optable_diff[BIGZ_T][BIGFR_T] = ent_diff_BIGZ_T_BIGFR_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_diff[BIGFR_T][BIGQ_T] = ent_diff_BIGFR_T_BIGQ_T;
	ent_optable_diff[BIGQ_T][BIGFR_T] = ent_diff_BIGQ_T_BIGFR_T;
#endif
#ifdef HAVE_MPF
	ent_optable_diff[BIGFR_T][BIGF_T] = ent_diff_BIGFR_T_BIGF_T;
	ent_optable_diff[BIGF_T][BIGFR_T] = ent_diff_BIGF_T_BIGFR_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_diff[BIGFR_T][FLOAT_T] = ent_diff_BIGFR_T_FLOAT_T;
	ent_optable_diff[FLOAT_T][BIGFR_T] = ent_diff_FLOAT_T_BIGFR_T;
#endif
	/* prods */
	ent_optable_prod[BIGFR_T][BIGFR_T] = ent_prod_BIGFR_T;
	ent_optable_prod[BIGFR_T][INT_T] = ent_prod_BIGFR_T_INT_T;
	ent_optable_prod[INT_T][BIGFR_T] = ent_prod_INT_T_BIGFR_T;
#ifdef HAVE_MPZ
	ent_optable_prod[BIGFR_T][BIGZ_T] = ent_prod_BIGFR_T_BIGZ_T;
	ent_optable_prod[BIGZ_T][BIGFR_T] = ent_prod_BIGZ_T_BIGFR_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_prod[BIGFR_T][BIGQ_T] = ent_prod_BIGFR_T_BIGQ_T;
	ent_optable_prod[BIGQ_T][BIGFR_T] = ent_prod_BIGQ_T_BIGFR_T;
#endif
#ifdef HAVE_MPF
	ent_optable_prod[BIGFR_T][BIGF_T] = ent_prod_BIGFR_T_BIGF_T;
	ent_optable_prod[BIGF_T][BIGFR_T] = ent_prod_BIGF_T_BIGFR_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_prod[BIGFR_T][FLOAT_T] = ent_prod_BIGFR_T_FLOAT_T;
	ent_optable_prod[FLOAT_T][BIGFR_T] = ent_prod_FLOAT_T_BIGFR_T;
#endif

	ent_optable_neg[BIGFR_T] = ent_neg_BIGFR_T;
	/* divisions and quotients */
	ent_optable_inv[BIGFR_T] = ent_inv_BIGFR_T;
	ent_optable_div[BIGFR_T][BIGFR_T] = ent_div_BIGFR_T;
	ent_optable_div[BIGFR_T][INT_T] = ent_div_BIGFR_T_INT_T;
	ent_optable_div[INT_T][BIGFR_T] = ent_div_INT_T_BIGFR_T;
#ifdef HAVE_MPZ
	ent_optable_div[BIGFR_T][BIGZ_T] = ent_div_BIGFR_T_BIGZ_T;
	ent_optable_div[BIGZ_T][BIGFR_T] = ent_div_BIGZ_T_BIGFR_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_div[BIGFR_T][BIGQ_T] = ent_div_BIGFR_T_BIGQ_T;
	ent_optable_div[BIGQ_T][BIGFR_T] = ent_div_BIGQ_T_BIGFR_T;
#endif
#ifdef HAVE_MPF
	ent_optable_div[BIGFR_T][BIGF_T] = ent_div_BIGFR_T_BIGF_T;
	ent_optable_div[BIGF_T][BIGFR_T] = ent_div_BIGF_T_BIGFR_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_div[BIGFR_T][FLOAT_T] = ent_div_BIGFR_T_FLOAT_T;
	ent_optable_div[FLOAT_T][BIGFR_T] = ent_div_FLOAT_T_BIGFR_T;
#endif
	ent_optable_quo[BIGFR_T][BIGFR_T] = ent_div_BIGFR_T;
	ent_optable_quo[BIGFR_T][INT_T] = ent_div_BIGFR_T_INT_T;
	ent_optable_quo[INT_T][BIGFR_T] = ent_div_INT_T_BIGFR_T;
#ifdef HAVE_MPZ
	ent_optable_quo[BIGFR_T][BIGZ_T] = ent_div_BIGFR_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][BIGFR_T] = ent_div_BIGZ_T_BIGFR_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_quo[BIGFR_T][BIGQ_T] = ent_div_BIGFR_T_BIGQ_T;
	ent_optable_quo[BIGQ_T][BIGFR_T] = ent_div_BIGQ_T_BIGFR_T;
#endif
#ifdef HAVE_MPF
	ent_optable_quo[BIGFR_T][BIGF_T] = ent_div_BIGFR_T_BIGF_T;
	ent_optable_quo[BIGF_T][BIGFR_T] = ent_div_BIGF_T_BIGFR_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_quo[BIGFR_T][FLOAT_T] = ent_div_BIGFR_T_FLOAT_T;
	ent_optable_quo[FLOAT_T][BIGFR_T] = ent_div_FLOAT_T_BIGFR_T;
#endif
	/* remainders */
	ent_optable_rem[BIGFR_T][BIGFR_T] = ent_rem_BIGFR_T;
	ent_optable_mod[BIGFR_T][BIGFR_T] = ent_mod_BIGFR_T;
#ifdef bigfr_pow
	ent_optable_pow[BIGFR_T][INT_T] = ent_pow_BIGFR_T_integer;
#ifdef HAVE_MPZ
	ent_optable_pow[BIGFR_T][BIGZ_T] = ent_pow_BIGFR_T_integer;
#endif
#endif

	/* orderings */
	ent_optable_lt[BIGFR_T][BIGFR_T] = ent_lt_BIGFR_T;
	ent_optable_lt[BIGFR_T][INT_T] = ent_lt_BIGFR_T_intfloat;
	ent_optable_lt[INT_T][BIGFR_T] = ent_lt_BIGFR_T_intfloat;
#ifdef HAVE_MPZ
	ent_optable_lt[BIGFR_T][BIGZ_T] = ent_lt_BIGFR_T_intfloat;
	ent_optable_lt[BIGZ_T][BIGFR_T] = ent_lt_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPQ
	ent_optable_lt[BIGFR_T][BIGQ_T] = ent_lt_BIGFR_T_intfloat;
	ent_optable_lt[BIGQ_T][BIGFR_T] = ent_lt_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPF
	ent_optable_lt[BIGFR_T][BIGF_T] = ent_lt_BIGFR_T_intfloat;
	ent_optable_lt[BIGF_T][BIGFR_T] = ent_lt_BIGFR_T_intfloat;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_lt[BIGFR_T][BIGQ_T] = ent_lt_BIGFR_T_intfloat;
	ent_optable_lt[BIGQ_T][BIGFR_T] = ent_lt_BIGFR_T_intfloat;
#endif
	ent_optable_gt[BIGFR_T][BIGFR_T] = ent_gt_BIGFR_T;
	ent_optable_gt[BIGFR_T][INT_T] = ent_gt_BIGFR_T_intfloat;
	ent_optable_gt[INT_T][BIGFR_T] = ent_gt_BIGFR_T_intfloat;
#ifdef HAVE_MPZ
	ent_optable_gt[BIGFR_T][BIGZ_T] = ent_gt_BIGFR_T_intfloat;
	ent_optable_gt[BIGZ_T][BIGFR_T] = ent_gt_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPQ
	ent_optable_gt[BIGFR_T][BIGQ_T] = ent_gt_BIGFR_T_intfloat;
	ent_optable_gt[BIGQ_T][BIGFR_T] = ent_gt_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPF
	ent_optable_gt[BIGFR_T][BIGF_T] = ent_gt_BIGFR_T_intfloat;
	ent_optable_gt[BIGF_T][BIGFR_T] = ent_gt_BIGFR_T_intfloat;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_gt[BIGFR_T][FLOAT_T] = ent_gt_BIGFR_T_intfloat;
	ent_optable_gt[FLOAT_T][BIGFR_T] = ent_gt_BIGFR_T_intfloat;
#endif
	ent_optable_eq[BIGFR_T][BIGFR_T] = ent_eq_BIGFR_T;
	ent_optable_eq[BIGFR_T][INT_T] = ent_eq_BIGFR_T_intfloat;
	ent_optable_eq[INT_T][BIGFR_T] = ent_eq_BIGFR_T_intfloat;
#ifdef HAVE_MPZ
	ent_optable_eq[BIGFR_T][BIGZ_T] = ent_eq_BIGFR_T_intfloat;
	ent_optable_eq[BIGZ_T][BIGFR_T] = ent_eq_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPQ
	ent_optable_eq[BIGFR_T][BIGQ_T] = ent_eq_BIGFR_T_intfloat;
	ent_optable_eq[BIGQ_T][BIGFR_T] = ent_eq_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPF
	ent_optable_eq[BIGFR_T][BIGF_T] = ent_eq_BIGFR_T_intfloat;
	ent_optable_eq[BIGF_T][BIGFR_T] = ent_eq_BIGFR_T_intfloat;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_eq[BIGFR_T][FLOAT_T] = ent_eq_BIGFR_T_intfloat;
	ent_optable_eq[FLOAT_T][BIGFR_T] = ent_eq_BIGFR_T_intfloat;
#endif
	ent_optable_ne[BIGFR_T][BIGFR_T] = ent_ne_BIGFR_T;
	ent_optable_ne[BIGFR_T][INT_T] = ent_ne_BIGFR_T_intfloat;
	ent_optable_ne[INT_T][BIGFR_T] = ent_ne_BIGFR_T_intfloat;
#ifdef HAVE_MPZ
	ent_optable_ne[BIGFR_T][BIGZ_T] = ent_ne_BIGFR_T_intfloat;
	ent_optable_ne[BIGZ_T][BIGFR_T] = ent_ne_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPQ
	ent_optable_ne[BIGFR_T][BIGQ_T] = ent_ne_BIGFR_T_intfloat;
	ent_optable_ne[BIGQ_T][BIGFR_T] = ent_ne_BIGFR_T_intfloat;
#endif
#ifdef HAVE_MPF
	ent_optable_ne[BIGFR_T][BIGF_T] = ent_ne_BIGFR_T_intfloat;
	ent_optable_ne[BIGF_T][BIGFR_T] = ent_ne_BIGFR_T_intfloat;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_ne[BIGFR_T][FLOAT_T] = ent_ne_BIGFR_T_intfloat;
	ent_optable_ne[FLOAT_T][BIGFR_T] = ent_ne_BIGFR_T_intfloat;
#endif
	/* valuations */
	ent_optable_vallt[BIGFR_T][BIGFR_T] = ent_lt_BIGFR_T;
	ent_optable_valgt[BIGFR_T][BIGFR_T] = ent_gt_BIGFR_T;
	ent_optable_valeq[BIGFR_T][BIGFR_T] = ent_eq_BIGFR_T;
	ent_optable_valne[BIGFR_T][BIGFR_T] = ent_ne_BIGFR_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGFR_T] = ent_lift_INT_T_BIGFR_T;
	ent_optable_lift[BIGFR_T][INT_T] = ent_lift_BIGFR_T_INT_T;
#ifdef HAVE_MPZ
	ent_optable_lift[BIGZ_T][BIGFR_T] = ent_lift_BIGZ_T_BIGFR_T;
	ent_optable_lift[BIGFR_T][BIGZ_T] = ent_lift_BIGFR_T_BIGZ_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_lift[BIGQ_T][BIGFR_T] = ent_lift_BIGQ_T_BIGFR_T;
	/* ent_optable_lift[BIGFR_T][BIGQ_T] = ent_lift_BIGFR_T_BIGQ_T; */
#endif
#ifdef HAVE_MPF
	ent_optable_lift[BIGF_T][BIGFR_T] = ent_lift_BIGF_T_BIGFR_T;
	ent_optable_lift[BIGFR_T][BIGF_T] = ent_lift_BIGFR_T_BIGF_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_lift[FLOAT_T][BIGFR_T] = ent_lift_FLOAT_T_BIGFR_T;
	ent_optable_lift[BIGFR_T][FLOAT_T] = ent_lift_BIGFR_T_FLOAT_T;
#endif
	ent_optable_lift[BIGFR_T][BIGFR_T] = ent_lift_BIGFR_T_BIGFR_T;
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
