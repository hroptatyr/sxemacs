/*
  ent-gmp.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005, 2006 Sebastian Freundt

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

#include "ent-gmp.h"

static mpf_t float_print_min, float_print_max;
gmp_randstate_t random_state;

bigz ent_scratch_bigz;
bigq ent_scratch_bigq;
bigf ent_scratch_bigf;

static Lisp_Object ent_lift_BIGZ_T_FLOAT_T(Lisp_Object, unsigned long);
static Lisp_Object ent_lift_BIGQ_T_FLOAT_T(Lisp_Object, unsigned long);


/************************* Big Rational Integers ****************************/
static void
bigz_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *bstr;

	bstr = (Bufbyte*)bigz_to_string(XBIGZ_DATA(obj), 10);
	write_c_string((char *)bstr, printcharfun);
	free(bstr);
	bstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigz_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigz_eql(XBIGZ_DATA(obj1), XBIGZ_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigz_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)bigz_hashcode(XBIGZ_DATA(obj));

	/* less warnings */
	if (depth);
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
bigq_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *rstr;

	rstr = (Bufbyte*)bigq_to_string(XBIGQ_DATA(obj), 10);
	write_c_string((char *)rstr, printcharfun);
	free(rstr);
	rstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigq_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigq_eql(XBIGQ_DATA(obj1), XBIGQ_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigq_hash (Lisp_Object obj, int depth)
{
	return bigq_hashcode(XBIGQ_DATA(obj));

	/* less warnings */
	if (depth);
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
bigf_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = bigf_to_string(XBIGF_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigf_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigf_eql(XBIGF_DATA(obj1), XBIGF_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigf_hash (Lisp_Object obj, int depth)
{
	return bigf_hashcode(XBIGF_DATA(obj));

	/* less warnings */
	if (depth);
}

static const struct lrecord_description bigf_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigf, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigf", bigf,
				    NULL, bigf_print, NULL,
				    bigf_equal, bigf_hash,
				    bigf_description, Lisp_Bigf);

DEFUN ("bigf-get-precision", Fbigf_get_precision, 1, 1, 0, /*
Return the precision of bigf F as an integer.
							   */
       (f))
{
	CHECK_BIGF(f);
	return make_integer((signed long)XBIGF_GET_PREC(f));
}

DEFUN ("bigf-set-precision", Fbigf_set_precision, 2, 2, 0, /*
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
	//CHECK_INTEGER(r);
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_sum_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_add(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_sum_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_sum[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_sum_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_sum[FLOAT_T][nt](l, tmp);
}
#endif
static Lisp_Object ent_sum_BIGZ_T_generic(Lisp_Object l, Lisp_Object r)
{
	number_type ntl, ntr;

	/* generic addition code, just coerce and call auto-diff fun */
	ntl = get_number_type(l);
	l = ent_optable_lift[ntl][BIGZ_T](l, 0UL);
	ntr = get_number_type(r);
	r = ent_optable_lift[ntr][BIGZ_T](r, 0UL);
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
static Lisp_Object ent_diff_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_sub(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_diff_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_diff[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_diff_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_diff[FLOAT_T][nt](l, tmp);
}
#endif
static Lisp_Object ent_diff_BIGZ_T_generic(Lisp_Object l, Lisp_Object r)
{
	number_type ntl, ntr;

	/* generic diff code, just coerce and call auto-diff fun */
	ntl = get_number_type(l);
	l = ent_optable_lift[ntl][BIGZ_T](l, 0UL);
	ntr = get_number_type(r);
	r = ent_optable_lift[ntr][BIGZ_T](r, 0UL);
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
static Lisp_Object ent_prod_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_prod_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_mul(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_prod_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_prod[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_prod_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_prod[FLOAT_T][nt](l, tmp);
}
#endif

static Lisp_Object ent_div_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_div_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XINT(r) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_div_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_div(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_div_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_div[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_div_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGZ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_div[FLOAT_T][nt](l, tmp);
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_quo_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_bigq_bz(XBIGZ_DATA(l), XBIGZ_DATA(r));
}
static Lisp_Object ent_quo_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XINT(r) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(r));
	return make_bigq_bz(XBIGZ_DATA(l), ent_scratch_bigz);
}
static Lisp_Object ent_quo_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(l));
	return make_bigq_bz(ent_scratch_bigz, XBIGZ_DATA(r));
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
static Lisp_Object ent_rem_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_set_long(ent_scratch_bigz, XREALINT(r));
	bigz_mod(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_rem_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_set_long(ent_scratch_bigz, XREALINT(l));
	bigz_mod(ent_scratch_bigz,  ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object ent_pow_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
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
#define ent_rel_BIGZ_T_integer(rel, l, r) do				\
{									\
	number_type nt;							\
									\
	nt = get_number_type(l);					\
	l = ent_optable_lift[nt][BIGZ_T](l, 0UL);			\
	nt = get_number_type(r);					\
	r = ent_optable_lift[nt][BIGZ_T](r, 0UL);			\
									\
	return ent_optable_##rel[BIGZ_T][BIGZ_T](l, r);			\
} while(0);

static Lisp_Object ent_lt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_lt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_lt_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZ_T_integer(lt, l, r);
}

static Lisp_Object ent_gt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_gt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZ_T_integer(gt, l, r);
}

static Lisp_Object ent_eq_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZ_T_integer(eq, l, r);
}

static Lisp_Object ent_ne_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qnil : Qt;
}
static Lisp_Object ent_ne_BIGZ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZ_T_integer(ne, l, r);
}

#ifdef HAVE_FPFLOAT
#define ent_rel_BIGZQ_T_float(rel, l, r) do				\
{									\
	number_type ntl, ntr;						\
									\
	ntl = get_number_type(l);					\
	l = ent_optable_lift[ntl][FLOAT_T](l, 0UL);			\
	ntr = get_number_type(r);					\
	r = ent_optable_lift[ntr][FLOAT_T](r, 0UL);			\
									\
	ntl = get_number_type(l);					\
	ntr = get_number_type(r);					\
	/* our float lift could result in an indef */			\
	return ent_optable_##rel[ntl][ntr](l, r);			\
} while(0);
static Lisp_Object ent_lt_BIGZQ_T_float(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZQ_T_float(lt, l, r);
}
static Lisp_Object ent_gt_BIGZQ_T_float(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZQ_T_float(gt, l, r);
}
static Lisp_Object ent_eq_BIGZQ_T_float(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZQ_T_float(eq, l, r);
}
static Lisp_Object ent_ne_BIGZQ_T_float(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGZQ_T_float(ne, l, r);
}
#endif


static Lisp_Object
ent_lift_INT_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	number = ent_normalise_number(number);
	return make_bigz(XREALINT(number));
}

static Lisp_Object
ent_lift_BIGZ_T_INT_T(Lisp_Object number, unsigned long precision)
{
	return make_int(bigz_to_long(XBIGZ_DATA(number)));
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_lift_FLOAT_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	bigz_set_fpfloat(ent_scratch_bigz, XFLOAT_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object
ent_lift_BIGZ_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	/* can result in an indef object, so be careful! */
	return make_float(bigz_to_fpfloat(XBIGZ_DATA(number)));
}
#endif


/* bigq ops */
static Lisp_Object ent_sum_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_add(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_sum_BIGQ_T_scratch(Lisp_Object l)
{
	bigq_add(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_sum_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(r));
	return ent_sum_BIGQ_T_scratch(l);
}
static Lisp_Object ent_sum_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(l));
	return ent_sum_BIGQ_T_scratch(r);
}
static Lisp_Object ent_sum_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	return ent_sum_BIGQ_T_scratch(l);
}
static Lisp_Object ent_sum_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	return ent_sum_BIGQ_T_scratch(r);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_sum_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_sum[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_sum_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_sum[FLOAT_T][nt](l, tmp);
}
#endif
static Lisp_Object ent_sum_BIGQ_T_generic(Lisp_Object l, Lisp_Object r)
{
	number_type ntl, ntr;

	/* generic addition code, just coerce and call auto-diff fun */
	ntl = get_number_type(l);
	l = ent_optable_lift[ntl][BIGQ_T](l, 0UL);
	ntr = get_number_type(r);
	r = ent_optable_lift[ntr][BIGQ_T](r, 0UL);
	return ent_sum_BIGQ_T(l, r);
}

static Lisp_Object ent_diff_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_diff_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_diff_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(l));
	bigq_sub(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_diff_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_sub(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_diff_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	bigq_sub(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_diff_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_diff[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_diff_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_diff[FLOAT_T][nt](l, tmp);
}
#endif
static Lisp_Object ent_diff_BIGQ_T_generic(Lisp_Object l, Lisp_Object r)
{
	number_type ntl, ntr;

	/* generic diff code, just coerce and call auto-diff fun */
	ntl = get_number_type(l);
	l = ent_optable_lift[ntl][BIGQ_T](l, 0UL);
	ntr = get_number_type(r);
	r = ent_optable_lift[ntr][BIGQ_T](r, 0UL);
	return ent_diff_BIGQ_T(l, r);
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
static Lisp_Object ent_prod_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(r));
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_prod_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_long(ent_scratch_bigq, XINT(l));
	bigq_mul(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_prod_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_mul(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_prod_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	bigq_mul(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_prod_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_prod[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_prod_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_prod[FLOAT_T][nt](l, tmp);
}
#endif

static Lisp_Object ent_div_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigq_sign(XBIGQ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_div_BIGQ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigq_set_long(ent_scratch_bigq, XINT(r));
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_div_INT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigq_set_long(ent_scratch_bigq, XINT(l));
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_div_BIGQ_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(r));
	bigq_div(ent_scratch_bigq, XBIGQ_DATA(l), ent_scratch_bigq);
	return make_bigq_bq(ent_scratch_bigq);
}
static Lisp_Object ent_div_BIGZ_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigq_set_bigz(ent_scratch_bigq, XBIGZ_DATA(l));
	bigq_div(ent_scratch_bigq, ent_scratch_bigq, XBIGQ_DATA(r));
	return make_bigq_bq(ent_scratch_bigq);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_div_BIGQ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(l, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_div[nt][FLOAT_T](tmp, r);
}
static Lisp_Object ent_div_FLOAT_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;
	Lisp_Object tmp = ent_lift_BIGQ_T_FLOAT_T(r, 0UL);
	nt = get_number_type(tmp);

	return ent_optable_div[FLOAT_T][nt](l, tmp);
}
#endif

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
#define ent_rel_BIGQ_T_integer(rel, l, r) do				\
{									\
	number_type nt;							\
									\
	nt = get_number_type(l);					\
	l = ent_optable_lift[nt][BIGQ_T](l, 0UL);			\
	nt = get_number_type(r);					\
	r = ent_optable_lift[nt][BIGQ_T](r, 0UL);			\
									\
	return ent_optable_##rel[BIGQ_T][BIGQ_T](l, r);			\
} while(0);

static Lisp_Object ent_lt_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_lt(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_lt_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGQ_T_integer(lt, l, r);
}

static Lisp_Object ent_gt_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_gt(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGQ_T_integer(gt, l, r);
}

static Lisp_Object ent_eq_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGQ_T_integer(eq, l, r);
}

static Lisp_Object ent_ne_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigq_eql(XBIGQ_DATA(l), XBIGQ_DATA(r)))
		? Qnil : Qt;
}
static Lisp_Object ent_ne_BIGQ_T_integer(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGQ_T_integer(ne, l, r);
}


static Lisp_Object
ent_lift_INT_T_BIGQ_T(Lisp_Object number, unsigned long precision)
{
	number = ent_normalise_number(number);
	return make_bigq(XREALINT(number), 1UL);
}

static Lisp_Object
ent_lift_BIGQ_T_INT_T(Lisp_Object number, unsigned long precision)
{
	bigz_div(ent_scratch_bigz,
		 XBIGQ_NUMERATOR(number),
		 XBIGQ_DENOMINATOR(number));
	return make_int(bigz_to_long(ent_scratch_bigz));
}

static Lisp_Object
ent_lift_BIGZ_T_BIGQ_T(Lisp_Object number, unsigned long precision)
{
	bigz_set_long(ent_scratch_bigz, 1L);
	return make_bigq_bz(XBIGZ_DATA(number), ent_scratch_bigz);
}

static Lisp_Object
ent_lift_BIGQ_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	bigz_div(ent_scratch_bigz,
		 XBIGQ_NUMERATOR(number),
		 XBIGQ_DENOMINATOR(number));
	return make_bigz_bz(ent_scratch_bigz);
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_lift_FLOAT_T_BIGQ_T(Lisp_Object number, unsigned long precision)
{
	bigq_set_fpfloat(ent_scratch_bigq, XFLOAT_DATA(number));
	return make_bigq_bq(ent_scratch_bigq);
}

static Lisp_Object
ent_lift_BIGQ_T_FLOAT_T(Lisp_Object number, unsigned long precisiony)
{
	/* can result in an indef object */
	return make_float(bigq_to_fpfloat(XBIGQ_DATA(number)));
}
#endif


/* bigf ops */
static Lisp_Object ent_sum_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, XINT(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, XINT(l));
	bigf_add(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_add(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_add(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_sum_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_add(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_sum_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_add(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

static Lisp_Object ent_diff_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, XINT(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, XINT(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_diff_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_sub(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_diff_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_sub(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

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
static Lisp_Object ent_prod_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, XINT(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, XINT(l));
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_prod_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_mul(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_prod_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_mul(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

static Lisp_Object ent_div_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (bigf_sign(XBIGF_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigf_set_prec(ent_scratch_bigf,
		      max(XBIGF_GET_PREC(l), XBIGF_GET_PREC(r)));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGF_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_long(ent_scratch_bigf, XINT(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_INT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_long(ent_scratch_bigf, XINT(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGF_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGZ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGF_T_BIGQ_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_BIGQ_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_div_BIGF_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(l));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(r));
	bigf_div(ent_scratch_bigf, XBIGF_DATA(l), ent_scratch_bigf);
	return make_bigf_bf(ent_scratch_bigf);
}
static Lisp_Object ent_div_FLOAT_T_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(r));
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(l));
	bigf_div(ent_scratch_bigf, ent_scratch_bigf, XBIGF_DATA(r));
	return make_bigf_bf(ent_scratch_bigf);
}
#endif

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
#define ent_rel_BIGF_T_intfloat(rel, l, r) do				\
{									\
	number_type nt;							\
									\
	nt = get_number_type(l);					\
	l = ent_optable_lift[nt][BIGF_T](l, 0UL);			\
	nt = get_number_type(r);					\
	r = ent_optable_lift[nt][BIGF_T](r, 0UL);			\
									\
	return ent_optable_##rel[BIGF_T][BIGF_T](l, r);			\
} while(0);

static Lisp_Object ent_lt_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_lt(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_lt_BIGF_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGF_T_intfloat(lt, l, r);
}

static Lisp_Object ent_gt_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_gt(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGF_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGF_T_intfloat(gt, l, r);
}

static Lisp_Object ent_eq_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_eql(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGF_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGF_T_intfloat(eq, l, r);
}

static Lisp_Object ent_ne_BIGF_T(Lisp_Object l, Lisp_Object r)
{
	return (bigf_eql(XBIGF_DATA(l), XBIGF_DATA(r)))
		? Qnil : Qt;
}
static Lisp_Object ent_ne_BIGF_T_intfloat(Lisp_Object l, Lisp_Object r)
{
	ent_rel_BIGF_T_intfloat(ne, l, r);
}


static Lisp_Object
ent_lift_INT_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	number = ent_normalise_number(number);
	return make_bigf(XREALINT(number), precision);
}

static Lisp_Object
ent_lift_BIGF_T_INT_T(Lisp_Object number, unsigned long precision)
{
	return make_int(bigf_to_long(XBIGF_DATA(number)));
}

static Lisp_Object
ent_lift_BIGZ_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigz(ent_scratch_bigf, XBIGZ_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	bigz_set_bigf(ent_scratch_bigz, XBIGF_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object
ent_lift_BIGQ_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_bigq(ent_scratch_bigf, XBIGQ_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_BIGQ_T(Lisp_Object number, unsigned long precision)
{
	bigq_set_bigf(ent_scratch_bigq, XBIGF_DATA(number));
	return make_bigq_bq(ent_scratch_bigq);
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_lift_FLOAT_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set_fpfloat(ent_scratch_bigf, XFLOAT_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}

static Lisp_Object
ent_lift_BIGF_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	/* can result in an indef object */
	return make_float(bigf_to_fpfloat(XBIGF_DATA(number)));
}
#endif

static Lisp_Object
ent_lift_BIGF_T_BIGF_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigf_set_prec(ent_scratch_bigf, precision);
	bigf_set(ent_scratch_bigf, XBIGF_DATA(number));
	return make_bigf_bf(ent_scratch_bigf);
}


void init_optables_BIGZ_T (void)
{
	/* sum code */
	ent_optable_sum[BIGZ_T][BIGZ_T] = ent_sum_BIGZ_T;
	ent_optable_sum[BIGZ_T][INT_T] = ent_sum_BIGZ_T_INT_T;
	ent_optable_sum[INT_T][BIGZ_T] = ent_sum_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_sum[FLOAT_T][BIGZ_T] = ent_sum_FLOAT_T_BIGZ_T;
	ent_optable_sum[BIGZ_T][FLOAT_T] = ent_sum_BIGZ_T_FLOAT_T;
#endif
	/* diff code */
	ent_optable_diff[BIGZ_T][BIGZ_T] = ent_diff_BIGZ_T;
	ent_optable_diff[BIGZ_T][INT_T] = ent_diff_BIGZ_T_INT_T;
	ent_optable_diff[INT_T][BIGZ_T] = ent_diff_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_diff[FLOAT_T][BIGZ_T] = ent_diff_FLOAT_T_BIGZ_T;
	ent_optable_diff[BIGZ_T][FLOAT_T] = ent_diff_BIGZ_T_FLOAT_T;
#endif
	/* prod code */
	ent_optable_prod[BIGZ_T][BIGZ_T] = ent_prod_BIGZ_T;
	ent_optable_prod[BIGZ_T][INT_T] = ent_prod_BIGZ_T_INT_T;
	ent_optable_prod[INT_T][BIGZ_T] = ent_prod_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_prod[FLOAT_T][BIGZ_T] = ent_prod_FLOAT_T_BIGZ_T;
	ent_optable_prod[BIGZ_T][FLOAT_T] = ent_prod_BIGZ_T_FLOAT_T;
#endif

	ent_optable_neg[BIGZ_T] = ent_neg_BIGZ_T;

	/* div/quo/invert code */
	ent_optable_div[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
	ent_optable_div[BIGZ_T][INT_T] = ent_div_BIGZ_T_INT_T;
	ent_optable_div[INT_T][BIGZ_T] = ent_div_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_div[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_div[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#endif

	ent_optable_inv[BIGZ_T] = ent_inv_BIGZ_T;
#ifdef HAVE_MPQ
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_quo_BIGZ_T;
	ent_optable_quo[BIGZ_T][INT_T] = ent_quo_BIGZ_T_INT_T;
	ent_optable_quo[INT_T][BIGZ_T] = ent_quo_INT_T_BIGZ_T;
#  ifdef HAVE_FPFLOAT
	ent_optable_quo[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#  endif
#else
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
	ent_optable_quo[BIGZ_T][INT_T] = ent_div_BIGZ_T_INT_T;
	ent_optable_quo[INT_T][BIGZ_T] = ent_div_INT_T_BIGZ_T;
#  ifdef HAVE_FPFLOAT
	ent_optable_quo[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#  endif
#endif
	/* remainder code */
	ent_optable_rem[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_mod[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_rem[BIGZ_T][INT_T] = ent_rem_BIGZ_T_INT_T;
	ent_optable_mod[BIGZ_T][INT_T] = ent_rem_BIGZ_T_INT_T;
	ent_optable_rem[INT_T][BIGZ_T] = ent_rem_INT_T_BIGZ_T;
	ent_optable_mod[INT_T][BIGZ_T] = ent_rem_INT_T_BIGZ_T;
	/* powers */
	ent_optable_pow[BIGZ_T][INT_T] = ent_pow_BIGZ_T_integer;
	ent_optable_pow[BIGZ_T][BIGZ_T] = ent_pow_BIGZ_T_integer;

	/* ordering tables */
	ent_optable_lt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_lt[BIGZ_T][INT_T] = ent_lt_BIGZ_T_integer;
	ent_optable_lt[INT_T][BIGZ_T] = ent_lt_BIGZ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_lt[BIGZ_T][FLOAT_T] = ent_lt_BIGZQ_T_float;
	ent_optable_lt[FLOAT_T][BIGZ_T] = ent_lt_BIGZQ_T_float;
#endif
	ent_optable_gt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_gt[BIGZ_T][INT_T] = ent_gt_BIGZ_T_integer;
	ent_optable_gt[INT_T][BIGZ_T] = ent_gt_BIGZ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_gt[BIGZ_T][FLOAT_T] = ent_gt_BIGZQ_T_float;
	ent_optable_gt[FLOAT_T][BIGZ_T] = ent_gt_BIGZQ_T_float;
#endif
	ent_optable_eq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_eq[BIGZ_T][INT_T] = ent_eq_BIGZ_T_integer;
	ent_optable_eq[INT_T][BIGZ_T] = ent_eq_BIGZ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_eq[BIGZ_T][FLOAT_T] = ent_eq_BIGZQ_T_float;
	ent_optable_eq[FLOAT_T][BIGZ_T] = ent_eq_BIGZQ_T_float;
#endif
	ent_optable_ne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;
	ent_optable_ne[BIGZ_T][INT_T] = ent_ne_BIGZ_T_integer;
	ent_optable_ne[INT_T][BIGZ_T] = ent_ne_BIGZ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_ne[BIGZ_T][FLOAT_T] = ent_ne_BIGZQ_T_float;
	ent_optable_ne[FLOAT_T][BIGZ_T] = ent_ne_BIGZQ_T_float;
#endif
	/* valuations */
	ent_optable_vallt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_valgt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_valeq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_valne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGZ_T] = ent_lift_INT_T_BIGZ_T;
	ent_optable_lift[BIGZ_T][INT_T] = ent_lift_BIGZ_T_INT_T;
#ifdef HAVE_FPFLOAT
	ent_optable_lift[FLOAT_T][BIGZ_T] = ent_lift_FLOAT_T_BIGZ_T;
	ent_optable_lift[BIGZ_T][FLOAT_T] = ent_lift_BIGZ_T_FLOAT_T;
#endif
	ent_optable_lift[INDEF_T][BIGZ_T] = ent_lift_INDEF_T_COMPARABLE;

	/* dedicated to my friend: the gcc compiler warnings generator */
	if (0) {
		ent_sum_BIGZ_T_generic(0, 0);
		ent_diff_BIGZ_T_generic(0, 0);
	}
}

void init_optables_BIGQ_T (void)
{
	/* sums */
	ent_optable_sum[BIGQ_T][BIGQ_T] = ent_sum_BIGQ_T;
	ent_optable_sum[BIGQ_T][INT_T] = ent_sum_BIGQ_T_INT_T;
	ent_optable_sum[INT_T][BIGQ_T] = ent_sum_INT_T_BIGQ_T;
	ent_optable_sum[BIGQ_T][BIGZ_T] = ent_sum_BIGQ_T_BIGZ_T;
	ent_optable_sum[BIGZ_T][BIGQ_T] = ent_sum_BIGZ_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_sum[FLOAT_T][BIGQ_T] = ent_sum_FLOAT_T_BIGQ_T;
	ent_optable_sum[BIGQ_T][FLOAT_T] = ent_sum_BIGQ_T_FLOAT_T;
#endif
	/* diffs */
	ent_optable_diff[BIGQ_T][BIGQ_T] = ent_diff_BIGQ_T;
	ent_optable_diff[BIGQ_T][INT_T] = ent_diff_BIGQ_T_INT_T;
	ent_optable_diff[INT_T][BIGQ_T] = ent_diff_INT_T_BIGQ_T;
	ent_optable_diff[BIGQ_T][BIGZ_T] = ent_diff_BIGQ_T_BIGZ_T;
	ent_optable_diff[BIGZ_T][BIGQ_T] = ent_diff_BIGZ_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_diff[FLOAT_T][BIGQ_T] = ent_diff_FLOAT_T_BIGQ_T;
	ent_optable_diff[BIGQ_T][FLOAT_T] = ent_diff_BIGQ_T_FLOAT_T;
#endif
	/* prods */
	ent_optable_prod[BIGQ_T][BIGQ_T] = ent_prod_BIGQ_T;
	ent_optable_prod[BIGQ_T][INT_T] = ent_prod_BIGQ_T_INT_T;
	ent_optable_prod[INT_T][BIGQ_T] = ent_prod_INT_T_BIGQ_T;
	ent_optable_prod[BIGQ_T][BIGZ_T] = ent_prod_BIGQ_T_BIGZ_T;
	ent_optable_prod[BIGZ_T][BIGQ_T] = ent_prod_BIGZ_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_prod[FLOAT_T][BIGQ_T] = ent_prod_FLOAT_T_BIGQ_T;
	ent_optable_prod[BIGQ_T][FLOAT_T] = ent_prod_BIGQ_T_FLOAT_T;
#endif

	ent_optable_neg[BIGQ_T] = ent_neg_BIGQ_T;

	/* divisions and quotients */
	ent_optable_div[BIGQ_T][BIGQ_T] = ent_div_BIGQ_T;
	ent_optable_div[BIGQ_T][INT_T] = ent_div_BIGQ_T_INT_T;
	ent_optable_div[INT_T][BIGQ_T] = ent_div_INT_T_BIGQ_T;
	ent_optable_div[BIGQ_T][BIGZ_T] = ent_div_BIGQ_T_BIGZ_T;
	ent_optable_div[BIGZ_T][BIGQ_T] = ent_div_BIGZ_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_div[FLOAT_T][BIGQ_T] = ent_div_FLOAT_T_BIGQ_T;
	ent_optable_div[BIGQ_T][FLOAT_T] = ent_div_BIGQ_T_FLOAT_T;
#endif
	ent_optable_inv[BIGQ_T] = ent_inv_BIGQ_T;
	ent_optable_quo[BIGQ_T][BIGQ_T] = ent_div_BIGQ_T;
	ent_optable_quo[BIGQ_T][INT_T] = ent_div_BIGQ_T_INT_T;
	ent_optable_quo[INT_T][BIGQ_T] = ent_div_INT_T_BIGQ_T;
	ent_optable_quo[BIGQ_T][BIGZ_T] = ent_div_BIGQ_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][BIGQ_T] = ent_div_BIGZ_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_quo[FLOAT_T][BIGQ_T] = ent_div_FLOAT_T_BIGQ_T;
	ent_optable_quo[BIGQ_T][FLOAT_T] = ent_div_BIGQ_T_FLOAT_T;
#endif
	/* remainders */
	ent_optable_rem[BIGQ_T][BIGQ_T] = ent_rem_BIGQ_T;
	ent_optable_mod[BIGQ_T][BIGQ_T] = ent_mod_BIGQ_T;
	ent_optable_pow[BIGQ_T][INT_T] = ent_pow_BIGQ_T_integer;
	ent_optable_pow[BIGQ_T][BIGZ_T] = ent_pow_BIGQ_T_integer;

	/* orderings */
	ent_optable_lt[BIGQ_T][BIGQ_T] = ent_lt_BIGQ_T;
	ent_optable_lt[BIGQ_T][INT_T] = ent_lt_BIGQ_T_integer;
	ent_optable_lt[INT_T][BIGQ_T] = ent_lt_BIGQ_T_integer;
	ent_optable_lt[BIGQ_T][BIGZ_T] = ent_lt_BIGQ_T_integer;
	ent_optable_lt[BIGZ_T][BIGQ_T] = ent_lt_BIGQ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_lt[BIGQ_T][FLOAT_T] = ent_lt_BIGZQ_T_float;
	ent_optable_lt[FLOAT_T][BIGQ_T] = ent_lt_BIGZQ_T_float;
#endif
	ent_optable_gt[BIGQ_T][BIGQ_T] = ent_gt_BIGQ_T;
	ent_optable_gt[BIGQ_T][INT_T] = ent_gt_BIGQ_T_integer;
	ent_optable_gt[INT_T][BIGQ_T] = ent_gt_BIGQ_T_integer;
	ent_optable_gt[BIGQ_T][BIGZ_T] = ent_gt_BIGQ_T_integer;
	ent_optable_gt[BIGZ_T][BIGQ_T] = ent_gt_BIGQ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_gt[BIGQ_T][FLOAT_T] = ent_gt_BIGZQ_T_float;
	ent_optable_gt[FLOAT_T][BIGQ_T] = ent_gt_BIGZQ_T_float;
#endif
	ent_optable_eq[BIGQ_T][BIGQ_T] = ent_eq_BIGQ_T;
	ent_optable_eq[BIGQ_T][INT_T] = ent_eq_BIGQ_T_integer;
	ent_optable_eq[INT_T][BIGQ_T] = ent_eq_BIGQ_T_integer;
	ent_optable_eq[BIGQ_T][BIGZ_T] = ent_eq_BIGQ_T_integer;
	ent_optable_eq[BIGZ_T][BIGQ_T] = ent_eq_BIGQ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_eq[BIGQ_T][FLOAT_T] = ent_eq_BIGZQ_T_float;
	ent_optable_eq[FLOAT_T][BIGQ_T] = ent_eq_BIGZQ_T_float;
#endif
	ent_optable_ne[BIGQ_T][BIGQ_T] = ent_ne_BIGQ_T;
	ent_optable_ne[BIGQ_T][INT_T] = ent_ne_BIGQ_T_integer;
	ent_optable_ne[INT_T][BIGQ_T] = ent_ne_BIGQ_T_integer;
	ent_optable_ne[BIGQ_T][BIGZ_T] = ent_ne_BIGQ_T_integer;
	ent_optable_ne[BIGZ_T][BIGQ_T] = ent_ne_BIGQ_T_integer;
#ifdef HAVE_FPFLOAT
	ent_optable_ne[BIGQ_T][FLOAT_T] = ent_ne_BIGZQ_T_float;
	ent_optable_ne[FLOAT_T][BIGQ_T] = ent_ne_BIGZQ_T_float;
#endif
	/* valuations */
	ent_optable_vallt[BIGQ_T][BIGQ_T] = ent_lt_BIGQ_T;
	ent_optable_valgt[BIGQ_T][BIGQ_T] = ent_gt_BIGQ_T;
	ent_optable_valeq[BIGQ_T][BIGQ_T] = ent_eq_BIGQ_T;
	ent_optable_valne[BIGQ_T][BIGQ_T] = ent_ne_BIGQ_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGQ_T] = ent_lift_INT_T_BIGQ_T;
	ent_optable_lift[BIGQ_T][INT_T] = ent_lift_BIGQ_T_INT_T;
	ent_optable_lift[BIGZ_T][BIGQ_T] = ent_lift_BIGZ_T_BIGQ_T;
	ent_optable_lift[BIGQ_T][BIGZ_T] = ent_lift_BIGQ_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_lift[FLOAT_T][BIGQ_T] = ent_lift_FLOAT_T_BIGQ_T;
	ent_optable_lift[BIGQ_T][FLOAT_T] = ent_lift_BIGQ_T_FLOAT_T;
#endif
	ent_optable_lift[INDEF_T][BIGQ_T] = ent_lift_INDEF_T_COMPARABLE;

	/* dedicated to my friend: the gcc compiler warnings generator */
	if (0) {
		ent_sum_BIGQ_T_generic(0, 0);
		ent_diff_BIGQ_T_generic(0, 0);
	}
}

void init_optables_BIGF_T (void)
{
	/* sums */
	ent_optable_sum[BIGF_T][BIGF_T] = ent_sum_BIGF_T;
	ent_optable_sum[BIGF_T][INT_T] = ent_sum_BIGF_T_INT_T;
	ent_optable_sum[INT_T][BIGF_T] = ent_sum_INT_T_BIGF_T;
	ent_optable_sum[BIGF_T][BIGZ_T] = ent_sum_BIGF_T_BIGZ_T;
	ent_optable_sum[BIGZ_T][BIGF_T] = ent_sum_BIGZ_T_BIGF_T;
	ent_optable_sum[BIGF_T][BIGQ_T] = ent_sum_BIGF_T_BIGQ_T;
	ent_optable_sum[BIGQ_T][BIGF_T] = ent_sum_BIGQ_T_BIGF_T;
#ifdef HAVE_FPFLOAT
	ent_optable_sum[BIGF_T][FLOAT_T] = ent_sum_BIGF_T_FLOAT_T;
	ent_optable_sum[FLOAT_T][BIGF_T] = ent_sum_FLOAT_T_BIGF_T;
#endif
	/* diffs */
	ent_optable_diff[BIGF_T][BIGF_T] = ent_diff_BIGF_T;
	ent_optable_diff[BIGF_T][INT_T] = ent_diff_BIGF_T_INT_T;
	ent_optable_diff[INT_T][BIGF_T] = ent_diff_INT_T_BIGF_T;
	ent_optable_diff[BIGF_T][BIGZ_T] = ent_diff_BIGF_T_BIGZ_T;
	ent_optable_diff[BIGZ_T][BIGF_T] = ent_diff_BIGZ_T_BIGF_T;
	ent_optable_diff[BIGF_T][BIGQ_T] = ent_diff_BIGF_T_BIGQ_T;
	ent_optable_diff[BIGQ_T][BIGF_T] = ent_diff_BIGQ_T_BIGF_T;
#ifdef HAVE_FPFLOAT
	ent_optable_diff[BIGF_T][FLOAT_T] = ent_diff_BIGF_T_FLOAT_T;
	ent_optable_diff[FLOAT_T][BIGF_T] = ent_diff_FLOAT_T_BIGF_T;
#endif
	/* prods */
	ent_optable_prod[BIGF_T][BIGF_T] = ent_prod_BIGF_T;
	ent_optable_prod[BIGF_T][INT_T] = ent_prod_BIGF_T_INT_T;
	ent_optable_prod[INT_T][BIGF_T] = ent_prod_INT_T_BIGF_T;
	ent_optable_prod[BIGF_T][BIGZ_T] = ent_prod_BIGF_T_BIGZ_T;
	ent_optable_prod[BIGZ_T][BIGF_T] = ent_prod_BIGZ_T_BIGF_T;
	ent_optable_prod[BIGF_T][BIGQ_T] = ent_prod_BIGF_T_BIGQ_T;
	ent_optable_prod[BIGQ_T][BIGF_T] = ent_prod_BIGQ_T_BIGF_T;
#ifdef HAVE_FPFLOAT
	ent_optable_prod[BIGF_T][FLOAT_T] = ent_prod_BIGF_T_FLOAT_T;
	ent_optable_prod[FLOAT_T][BIGF_T] = ent_prod_FLOAT_T_BIGF_T;
#endif

	ent_optable_neg[BIGF_T] = ent_neg_BIGF_T;

	/* divisions and quotients */
	ent_optable_inv[BIGF_T] = ent_inv_BIGF_T;
	ent_optable_div[BIGF_T][BIGF_T] = ent_div_BIGF_T;
	ent_optable_div[BIGF_T][INT_T] = ent_div_BIGF_T_INT_T;
	ent_optable_div[INT_T][BIGF_T] = ent_div_INT_T_BIGF_T;
	ent_optable_div[BIGF_T][BIGZ_T] = ent_div_BIGF_T_BIGZ_T;
	ent_optable_div[BIGZ_T][BIGF_T] = ent_div_BIGZ_T_BIGF_T;
	ent_optable_div[BIGF_T][BIGQ_T] = ent_div_BIGF_T_BIGQ_T;
	ent_optable_div[BIGQ_T][BIGF_T] = ent_div_BIGQ_T_BIGF_T;
#ifdef HAVE_FPFLOAT
	ent_optable_div[BIGF_T][FLOAT_T] = ent_div_BIGF_T_FLOAT_T;
	ent_optable_div[FLOAT_T][BIGF_T] = ent_div_FLOAT_T_BIGF_T;
#endif
	ent_optable_quo[BIGF_T][BIGF_T] = ent_div_BIGF_T;
	ent_optable_quo[BIGF_T][INT_T] = ent_div_BIGF_T_INT_T;
	ent_optable_quo[INT_T][BIGF_T] = ent_div_INT_T_BIGF_T;
	ent_optable_quo[BIGF_T][BIGZ_T] = ent_div_BIGF_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][BIGF_T] = ent_div_BIGZ_T_BIGF_T;
	ent_optable_quo[BIGF_T][BIGQ_T] = ent_div_BIGF_T_BIGQ_T;
	ent_optable_quo[BIGQ_T][BIGF_T] = ent_div_BIGQ_T_BIGF_T;
#ifdef HAVE_FPFLOAT
	ent_optable_quo[BIGF_T][FLOAT_T] = ent_div_BIGF_T_FLOAT_T;
	ent_optable_quo[FLOAT_T][BIGF_T] = ent_div_FLOAT_T_BIGF_T;
#endif
	/* remainders */
	ent_optable_rem[BIGF_T][BIGF_T] = ent_rem_BIGF_T;
	ent_optable_mod[BIGF_T][BIGF_T] = ent_mod_BIGF_T;
#ifdef bigf_pow
	ent_optable_pow[BIGF_T][INT_T] = ent_pow_BIGF_T_integer;
	ent_optable_pow[BIGF_T][BIGZ_T] = ent_pow_BIGF_T_integer;
#endif
	/* orderings */
	ent_optable_lt[BIGF_T][BIGF_T] = ent_lt_BIGF_T;
	ent_optable_lt[BIGF_T][INT_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[INT_T][BIGF_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[BIGF_T][BIGZ_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[BIGZ_T][BIGF_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[BIGF_T][BIGQ_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[BIGQ_T][BIGF_T] = ent_lt_BIGF_T_intfloat;
#ifdef HAVE_FPFLOAT
	ent_optable_lt[BIGF_T][BIGQ_T] = ent_lt_BIGF_T_intfloat;
	ent_optable_lt[BIGQ_T][BIGF_T] = ent_lt_BIGF_T_intfloat;
#endif
	ent_optable_gt[BIGF_T][BIGF_T] = ent_gt_BIGF_T;
	ent_optable_gt[BIGF_T][INT_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[INT_T][BIGF_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[BIGF_T][BIGZ_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[BIGZ_T][BIGF_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[BIGF_T][BIGQ_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[BIGQ_T][BIGF_T] = ent_gt_BIGF_T_intfloat;
#ifdef HAVE_FPFLOAT
	ent_optable_gt[BIGF_T][FLOAT_T] = ent_gt_BIGF_T_intfloat;
	ent_optable_gt[FLOAT_T][BIGF_T] = ent_gt_BIGF_T_intfloat;
#endif
	ent_optable_eq[BIGF_T][BIGF_T] = ent_eq_BIGF_T;
	ent_optable_eq[BIGF_T][INT_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[INT_T][BIGF_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[BIGF_T][BIGZ_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[BIGZ_T][BIGF_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[BIGF_T][BIGQ_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[BIGQ_T][BIGF_T] = ent_eq_BIGF_T_intfloat;
#ifdef HAVE_FPFLOAT
	ent_optable_eq[BIGF_T][FLOAT_T] = ent_eq_BIGF_T_intfloat;
	ent_optable_eq[FLOAT_T][BIGF_T] = ent_eq_BIGF_T_intfloat;
#endif
	ent_optable_ne[BIGF_T][BIGF_T] = ent_ne_BIGF_T;
	ent_optable_ne[BIGF_T][INT_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[INT_T][BIGF_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[BIGF_T][BIGZ_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[BIGZ_T][BIGF_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[BIGF_T][BIGQ_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[BIGQ_T][BIGF_T] = ent_ne_BIGF_T_intfloat;
#ifdef HAVE_FPFLOAT
	ent_optable_ne[BIGF_T][FLOAT_T] = ent_ne_BIGF_T_intfloat;
	ent_optable_ne[FLOAT_T][BIGF_T] = ent_ne_BIGF_T_intfloat;
#endif
	/* valuations */
	ent_optable_vallt[BIGF_T][BIGF_T] = ent_lt_BIGF_T;
	ent_optable_valgt[BIGF_T][BIGF_T] = ent_gt_BIGF_T;
	ent_optable_valeq[BIGF_T][BIGF_T] = ent_eq_BIGF_T;
	ent_optable_valne[BIGF_T][BIGF_T] = ent_ne_BIGF_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGF_T] = ent_lift_INT_T_BIGF_T;
	ent_optable_lift[BIGF_T][INT_T] = ent_lift_BIGF_T_INT_T;
	ent_optable_lift[BIGZ_T][BIGF_T] = ent_lift_BIGZ_T_BIGF_T;
	ent_optable_lift[BIGF_T][BIGZ_T] = ent_lift_BIGF_T_BIGZ_T;
	ent_optable_lift[BIGQ_T][BIGF_T] = ent_lift_BIGQ_T_BIGF_T;
	ent_optable_lift[BIGF_T][BIGQ_T] = ent_lift_BIGF_T_BIGQ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_lift[FLOAT_T][BIGF_T] = ent_lift_FLOAT_T_BIGF_T;
	ent_optable_lift[BIGF_T][FLOAT_T] = ent_lift_BIGF_T_FLOAT_T;
#endif
	ent_optable_lift[BIGF_T][BIGF_T] = ent_lift_BIGF_T_BIGF_T;
	ent_optable_lift[INDEF_T][BIGF_T] = ent_lift_INDEF_T_COMPARABLE;
}

void init_ent_mpz()
{
	mp_set_memory_functions((void *(*) (size_t))xmalloc,
				gmp_realloc, gmp_free);

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
