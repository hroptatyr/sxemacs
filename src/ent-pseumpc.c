/*
  number-pseumpc.c -- Numeric types for SXEmacs
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

#include "ent-pseumpc.h"



/* basic functions */
void bigc_init(bigc c)
{
	bigfr_init(bigc_re(c));
	bigfr_init(bigc_im(c));
}

void bigc_init_prec(bigc c, unsigned long prec)
{
	bigfr_init_prec(bigc_re(c), prec);
	bigfr_init_prec(bigc_im(c), prec);
}

void bigc_init_2prec(bigc c, unsigned long prec1, unsigned long prec2)
{
	bigfr_init_prec(bigc_re(c), prec1);
	bigfr_init_prec(bigc_im(c), prec2);
}

void bigc_fini(bigc c)
{
	bigfr_fini(bigc_re(c));
	bigfr_fini(bigc_im(c));
}

#if 0
unsigned long bigc_hashcode(bigc c)
{
	return (bigfr_hashcode(bigc_re(c)) ^
		bigfr_hashcode(bigc_im(c)));
}
#endif	/* already have a macro */


void bigc_set_prec(bigc c, unsigned long prec)
{
	bigfr_set_prec(bigc_re(c), prec);
	bigfr_set_prec(bigc_im(c), prec);
}



int bigc_nan_p(bigc c)
{
	return (bigfr_nan_p(bigc_re(c)) ||
		bigfr_nan_p(bigc_im(c)));
}

int bigc_inf_p(bigc c)
{
	return (bigfr_inf_p(bigc_re(c)) ||
		bigfr_inf_p(bigc_im(c)));
}


Bufbyte *bigc_to_string(bigc c, int base)
{
	Bufbyte *re_str;
	Bufbyte *im_str;
	int re_len, im_len;

	/* if one of the components is infinity or not a number,
	 * just print the respective component
	 * +infinity+2i does not really make sense, that's why!
	 */
	if (bigc_nan_p(c)) {
		re_str = indef_to_string((indef)NOT_A_NUMBER);
		return re_str;
	} else if (bigc_inf_p(c)) {
		re_str = indef_to_string((indef)COMPLEX_INFINITY);
		return re_str;
	} else {
		/* fetch the components' strings */
		re_str = bigfr_to_string(bigc_re(c), base);
		im_str = bigfr_to_string(bigc_im(c), base);

		re_len = strlen((char*)re_str);
		im_len = strlen((char*)im_str);

		const int sign = bigfr_sign(bigc_im(c));
		const int neg = (sign >= 0) ? 1 : 0;

		/* now append the imaginary string */
		XREALLOC_ARRAY(re_str, Bufbyte, re_len + neg + im_len + 2);
		if (neg)
			re_str[re_len] = '+';
		memmove(&re_str[re_len + neg],
			&im_str[0],
			im_len);
		re_str[re_len+neg+im_len] = 'i';
		re_str[re_len+neg+im_len+1] = '\0';
		free(im_str);

		return re_str;
	}
}

/***** Bigg: converting assignments *****/
void bigc_set(bigc c1,bigc c2)
{
	bigfr_set(bigc_re(c1), bigc_re(c2));
	bigfr_set(bigc_im(c1), bigc_im(c2));
}

void bigc_set_long(bigc c, long l)
{
	bigfr_set_long(bigc_re(c), l);
	bigfr_set_long(bigc_im(c), 0L);
}

void bigc_set_long_long(bigc c, long l1, long l2)
{
	bigfr_set_long(bigc_re(c), l1);
	bigfr_set_long(bigc_im(c), l2);
}

void bigc_set_ulong(bigc c, unsigned long ul)
{
	bigfr_set_ulong(bigc_re(c), ul);
	bigfr_set_ulong(bigc_im(c), 0UL);
}

void bigc_set_ulong_ulong(bigc c, unsigned long ul1, unsigned long ul2)
{
	bigfr_set_ulong(bigc_re(c), ul1);
	bigfr_set_ulong(bigc_im(c), ul2);
}

void bigc_set_double(bigc c, double f)
{
	bigfr_set_double(bigc_re(c), f);
	bigfr_set_long(bigc_im(c), 0L);
}

void bigc_set_double_double(bigc c, double f1, double f2)
{
	bigfr_set_double(bigc_re(c), f1);
	bigfr_set_double(bigc_im(c), f2);
}

void bigc_set_bigfr(bigc c, bigfr f)
{
	bigfr_set(bigc_re(c), f);
	bigfr_set_long(bigc_im(c), 0L);
}

void bigc_set_bigfr_bigfr(bigc c, bigfr f1, bigfr f2)
{
	bigfr_set(bigc_re(c), f1);
	bigfr_set(bigc_im(c), f2);
}

/* void bigc_set_bigc(bigc c, bigc c)
 * {
 * 	bigc_set_bigfr_bigfr(bigc_re(c), z1);
 * }
 */

/***** Bigc: comparisons *****/
int bigc_eql(bigc c1, bigc c2)
{
	return ((bigfr_eql(bigc_re(c1), bigc_re(c2))) &&
		(bigfr_eql(bigc_im(c1), bigc_im(c2))));
}

/***** Bigc: arithmetic *****/
void bigc_abs(bigfr res, bigc c)
{
	/* the absolute archimedean valuation of a+bi is defined as:
	 * (a^2 + b^2)^(1/2)
	 */
	bigfr accu1, accu2, bf;
	bigfr_init(accu1);
	bigfr_init(accu2);
	bigfr_init(bf);

	bigfr_mul(accu1, bigc_re(c), bigc_re(c));
	bigfr_mul(accu2, bigc_im(c), bigc_im(c));
	bigfr_add(bf, accu1, accu2);

	bigfr_sqrt(res, bf);

	bigfr_fini(accu1);
	bigfr_fini(accu2);
	bigfr_fini(bf);
}

void bigc_norm(bigfr res, bigc c)
{
	/* norm is the square of the absolute archimedean valuation */
	bigfr accu1, accu2;
	bigfr_init(accu1);
	bigfr_init(accu2);

	bigfr_mul(accu1, bigc_re(c), bigc_re(c));
	bigfr_mul(accu2, bigc_im(c), bigc_im(c));
	bigfr_add(res, accu1, accu2);

	bigfr_fini(accu1);
	bigfr_fini(accu2);
}

void bigc_neg(bigc res, bigc c)
{
	/* negation is defined point-wise */
	bigfr_neg(bigc_re(res), bigc_re(c));
	bigfr_neg(bigc_im(res), bigc_im(c));
}

void bigc_conj(bigc res, bigc c)
{
	bigc_set(res, c);
	bigfr_neg(bigc_im(res), bigc_im(res));
}

void bigc_add(bigc res, bigc g1, bigc g2)
{
	/* addition is defined point-wise */
	bigfr accu1, accu2;
	bigfr_init(accu1);
	bigfr_init(accu2);

	bigfr_add(accu1, bigc_re(g1), bigc_re(g2));
	bigfr_add(accu2, bigc_im(g1), bigc_im(g2));
	bigc_set_bigfr_bigfr(res, accu1, accu2);

	bigfr_fini(accu1);
	bigfr_fini(accu2);
}

void bigc_sub(bigc res, bigc g1, bigc g2)
{
	/* subtraction is defined point-wise */
	bigfr_sub(bigc_re(res), bigc_re(g1), bigc_re(g2));
	bigfr_sub(bigc_im(res), bigc_im(g1), bigc_im(g2));
}

void bigc_mul(bigc res, bigc g1, bigc g2)
{
	/* multiplication is defined as:
	 * (a + bi)*(c + di) = (ac - bd) + (ad + bc)i
	 */
	bigfr accu1, accu2, accu3, accu4;
	bigfr_init(accu1);
	bigfr_init(accu2);
	bigfr_init(accu3);
	bigfr_init(accu4);

	bigfr_mul(accu1, bigc_re(g1), bigc_re(g2));
	bigfr_mul(accu2, bigc_im(g1), bigc_im(g2));
	bigfr_mul(accu3, bigc_re(g1), bigc_im(g2));
	bigfr_mul(accu4, bigc_im(g1), bigc_re(g2));

	bigfr_sub(bigc_re(res), accu1, accu2);
	bigfr_add(bigc_im(res), accu3, accu4);

	bigfr_fini(accu1);
	bigfr_fini(accu2);
	bigfr_fini(accu3);
	bigfr_fini(accu4);
}

void bigc_div(bigc res, bigc g1, bigc g2)
{
	/* division is defined as:
	 * (a + bi) div (c + di) = ((a+bi)*(c-di)) div (c*c+d*d)
	 */
	bigfr accu1, accu2;
	bigc accug;
	bigfr_init(accu1);
	bigfr_init(accu2);
	bigc_init(accug);

	/* compute: c^2 + d^2 */
	bigfr_mul(accu1, bigc_re(g2), bigc_re(g2));
	bigfr_mul(accu2, bigc_im(g2), bigc_im(g2));
	bigfr_add(accu1, accu1, accu2);

	/* do normal multiplication with conjugate of g2 */
	bigc_conj(accug, g2);
	bigc_mul(accug, g1, accug);

	bigc_set(res, accug);

	/* now divide (g1*conj(g2)) by c^2+d^2 (point-wise) */
	bigfr_div(bigc_re(res), bigc_re(accug), accu1);
	bigfr_div(bigc_im(res), bigc_im(accug), accu1);

	bigc_fini(accug);
	bigfr_fini(accu2);
	bigfr_fini(accu1);
}

void bigc_mod(bigc res, bigc g1, bigc g2)
{
	/* the modulo relation is defined as:
	 * (a + bi) mod (c + di) ~
	 * (a+bi) - ((a+bi) div (c-di)) * (c+di)
	 */
	bigc accug;
	bigc_init(accug);

	/* do normal division */
	bigc_div(accug, g1, g2);

	/* now re-multiply g2 */
	bigc_mul(accug, accug, g2);

	/* and find the difference */
	bigc_sub(res, g1, accug);

	bigc_fini(accug);
}

void bigc_pow(bigc res, bigc g1, unsigned long g2)
{
#if defined(HAVE_MPZ) && defined(WITH_GMP)
	unsigned long i;
	bigfr bin, resintg, resimag, tmpbz1, tmpbz2, tmpbz3, intg, imag;

	bigfr_init(bin);
	bigfr_init(resintg);
	bigfr_init(resimag);
	bigfr_init(intg);
	bigfr_init(imag);
	bigfr_init(tmpbz1);
	bigfr_init(tmpbz2);
	bigfr_init(tmpbz3);

	bigfr_set_long(resintg, 0L);
	bigfr_set_long(resimag, 0L);

	bigfr_set(intg, bigc_re(g1));
	bigfr_set(imag, bigc_im(g1));

	/* we compute using the binomial coefficients */
	for (i=0; i<=g2; i++) {
		mpz_bin_uiui(bin, g2, i);
		if (i % 2 == 0) {
			/* real part changes */
			bigfr_pow(tmpbz1, intg, g2-i);
			bigfr_pow(tmpbz2, imag, i);
			bigfr_mul(tmpbz3, tmpbz1, tmpbz2);
			bigfr_mul(bin, bin, tmpbz3);
			if (i % 4 == 0) {
				bigfr_add(resintg, resintg, bin);
			} else if (i % 4 == 2) {
				bigfr_sub(resintg, resintg, bin);
			}
		} else {
			/* imag part changes */
			bigfr_pow(tmpbz1, intg, g2-i);
			bigfr_pow(tmpbz2, imag, i);
			bigfr_mul(tmpbz3, tmpbz1, tmpbz2);
			bigfr_mul(bin, bin, tmpbz3);
			if (i % 4 == 1) {
				bigfr_add(resimag, resimag, bin);
			} else if (i % 4 == 3) {
				bigfr_sub(resimag, resimag, bin);
			}
		}
	}

	bigc_set_bigfr_bigfr(res, resintg, resimag);

	bigfr_fini(bin);
	bigfr_fini(intg);
	bigfr_fini(imag);
	bigfr_init(resintg);
	bigfr_init(resimag);
	bigfr_fini(tmpbz1);
	bigfr_fini(tmpbz2);
	bigfr_fini(tmpbz3);
#else  /* !WITH_MPZ */
	bigc_set_long_long(res, 0L, 0L);
#endif	/* WITH_MPZ */
}

void bigc_sqrt(bigc res, bigc c)
{
	bigfr_set_nan(bigc_re(res));
	bigfr_set_nan(bigc_im(res));
}

/* bigc ops */
static Lisp_Object ent_sum_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigc_set_prec(ent_scratch_bigc,
		       max(XBIGC_GET_PREC(l), XBIGC_GET_PREC(r)));
	bigc_add(ent_scratch_bigc, XBIGC_DATA(l), XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_sum_BIGC_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGFR_T](r, XBIGC_GET_PREC(l));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(l));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(r));
	bigc_add(ent_scratch_bigc, XBIGC_DATA(l), ent_scratch_bigc);
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_sum_COMPARABLE_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGFR_T](l, XBIGC_GET_PREC(r));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(r));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(l));
	bigc_add(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_sum_BIGC_T_COMPLEX(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGC_T](r, XBIGC_GET_PREC(l));

	return ent_sum_BIGC_T(l, r);
}
static Lisp_Object ent_sum_COMPLEX_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGC_T](l, XBIGC_GET_PREC(l));

	return ent_sum_BIGC_T(l, r);
}

static Lisp_Object ent_diff_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigc_set_prec(ent_scratch_bigc,
		       max(XBIGC_GET_PREC(l), XBIGC_GET_PREC(r)));
	bigc_sub(ent_scratch_bigc, XBIGC_DATA(l), XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_diff_BIGC_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGFR_T](r, XBIGC_GET_PREC(l));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(l));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(r));
	bigc_sub(ent_scratch_bigc, XBIGC_DATA(l), ent_scratch_bigc);
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_diff_COMPARABLE_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGFR_T](l, XBIGC_GET_PREC(r));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(r));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(l));
	bigc_sub(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_diff_BIGC_T_COMPLEX(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGC_T](r, XBIGC_GET_PREC(l));

	return ent_diff_BIGC_T(l, r);
}
static Lisp_Object ent_diff_COMPLEX_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGC_T](l, XBIGC_GET_PREC(l));

	return ent_diff_BIGC_T(l, r);
}

static Lisp_Object ent_neg_BIGC_T(Lisp_Object l)
{
	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(l));
	bigc_neg(ent_scratch_bigc, XBIGC_DATA(l));
	return make_bigc_bc(ent_scratch_bigc);
}

static Lisp_Object ent_prod_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigc_set_prec(ent_scratch_bigc,
		       max(XBIGC_GET_PREC(l), XBIGC_GET_PREC(r)));
	bigc_mul(ent_scratch_bigc, XBIGC_DATA(l), XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_prod_BIGC_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGFR_T](r, XBIGC_GET_PREC(l));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(l));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(r));
	bigc_mul(ent_scratch_bigc, XBIGC_DATA(l), ent_scratch_bigc);
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_prod_COMPARABLE_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGFR_T](l, XBIGC_GET_PREC(r));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(r));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(l));
	bigc_mul(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_prod_BIGC_T_COMPLEX(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(r);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGC_T](r, XBIGC_GET_PREC(l));

	return ent_prod_BIGC_T(l, r);
}
static Lisp_Object ent_prod_COMPLEX_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(l);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGC_T](l, XBIGC_GET_PREC(l));

	return ent_prod_BIGC_T(l, r);
}

static Lisp_Object ent_div_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	if ((bigfr_sign(bigc_re(XBIGC_DATA(r))) == 0) &&
	    (bigfr_sign(bigc_im(XBIGC_DATA(r))) == 0))
		Fsignal(Qarith_error, Qnil);
	bigc_set_prec(ent_scratch_bigc,
		      max(XBIGC_GET_PREC(l), XBIGC_GET_PREC(r)));
	bigc_div(ent_scratch_bigc, XBIGC_DATA(l), XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_div_BIGC_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(r);

	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGFR_T](r, XBIGC_GET_PREC(l));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(l));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(r));
	bigc_div(ent_scratch_bigc, XBIGC_DATA(l), ent_scratch_bigc);
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_div_COMPARABLE_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPARABLE(l);

	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGFR_T](l, XBIGC_GET_PREC(r));

	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(r));
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(l));
	bigc_div(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_div_BIGC_T_COMPLEX(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(r);

	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	nt = get_number_type(r);
	r = ent_optable_lift[nt][BIGC_T](r, XBIGC_GET_PREC(l));

	return ent_div_BIGC_T(l, r);
}
static Lisp_Object ent_div_COMPLEX_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	number_type nt;

	CHECK_COMPLEX(l);

	if (!NILP(Fzerop(r)))
		Fsignal(Qarith_error, Qnil);

	nt = get_number_type(l);
	l = ent_optable_lift[nt][BIGC_T](l, XBIGC_GET_PREC(l));

	return ent_div_BIGC_T(l, r);
}

static Lisp_Object ent_inv_BIGC_T(Lisp_Object r)
{
	if ((bigfr_sign(bigc_re(XBIGC_DATA(r))) == 0) &&
	    (bigfr_sign(bigc_im(XBIGC_DATA(r))) == 0))
		Fsignal(Qarith_error, Qnil);
	bigc_set_long(ent_scratch_bigc, 1L);
	bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(r));
	bigc_div(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	return make_bigc_bc(ent_scratch_bigc);
}
static Lisp_Object ent_rem_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	if ((bigfr_sign(bigc_re(XBIGC_DATA(r))) == 0) &&
	    (bigfr_sign(bigc_im(XBIGC_DATA(r))) == 0))
		Fsignal(Qarith_error, Qnil);
	return make_bigc(0.0, 0.0, internal_get_precision(Qnil));
}
static Lisp_Object ent_mod_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	if ((bigfr_sign(bigc_re(XBIGC_DATA(r))) == 0) &&
	    (bigfr_sign(bigc_im(XBIGC_DATA(r))) == 0))
		Fsignal(Qarith_error, Qnil);
	bigc_set_prec(ent_scratch_bigc,
		      max(XBIGC_GET_PREC(l), XBIGC_GET_PREC(r)));
	bigc_div(ent_scratch_bigc, XBIGC_DATA(l), XBIGC_DATA(r));
	bigfr_trunc(bigc_re(ent_scratch_bigc), bigc_re(ent_scratch_bigc));
	bigfr_trunc(bigc_im(ent_scratch_bigc), bigc_im(ent_scratch_bigc));
	bigc_mul(ent_scratch_bigc, ent_scratch_bigc, XBIGC_DATA(r));
	bigc_sub(ent_scratch_bigc, XBIGC_DATA(l), ent_scratch_bigc);
	return make_bigc_bc(ent_scratch_bigc);
}

static Lisp_Object ent_vallt_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigfr b2;
	int result;

	bigfr_init(b2);
	bigfr_set_prec(ent_scratch_bigfr, internal_get_precision(Qnil));
	bigfr_set_prec(b2, internal_get_precision(Qnil));
	bigc_norm(ent_scratch_bigfr, XBIGC_DATA(l));
	bigc_norm(b2, XBIGC_DATA(r));
	result = bigfr_lt(ent_scratch_bigfr, b2);

	bigfr_fini(b2);
	return (result) ? Qt : Qnil;
}
static Lisp_Object ent_valgt_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigfr b2;
	int result;

	bigfr_init(b2);
	bigfr_set_prec(ent_scratch_bigfr, internal_get_precision(Qnil));
	bigfr_set_prec(b2, internal_get_precision(Qnil));
	bigc_norm(ent_scratch_bigfr, XBIGC_DATA(l));
	bigc_norm(b2, XBIGC_DATA(r));
	result = bigfr_gt(ent_scratch_bigfr, b2);

	bigfr_fini(b2);
	return (result) ? Qt : Qnil;
}
static Lisp_Object ent_valeq_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigfr b2;
	int result;

	bigfr_init(b2);
	bigfr_set_prec(ent_scratch_bigfr, internal_get_precision(Qnil));
	bigfr_set_prec(b2, internal_get_precision(Qnil));
	bigc_norm(ent_scratch_bigfr, XBIGC_DATA(l));
	bigc_norm(b2, XBIGC_DATA(r));
	result = bigfr_eql(ent_scratch_bigfr, b2);

	bigfr_fini(b2);
	return (result) ? Qt : Qnil;
}
static Lisp_Object ent_valne_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	bigfr b2;
	int result;

	bigfr_init(b2);
	bigfr_set_prec(ent_scratch_bigfr, internal_get_precision(Qnil));
	bigfr_set_prec(b2, internal_get_precision(Qnil));
	bigc_norm(ent_scratch_bigfr, XBIGC_DATA(l));
	bigc_norm(b2, XBIGC_DATA(r));
	result = bigfr_eql(ent_scratch_bigfr, b2);

	bigfr_fini(b2);
	return (result) ? Qnil : Qt;
}

static Lisp_Object
ent_lift_INT_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	number = ent_normalise_number(number);
	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_long(ent_scratch_bigc, XINT(number));
	return make_bigc_bc(ent_scratch_bigc);
}

#ifdef HAVE_MPZ
static Lisp_Object
ent_lift_BIGZ_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigz(ent_scratch_bigfr, XBIGZ_DATA(number));
	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_bigfr(ent_scratch_bigc, ent_scratch_bigfr);
	return make_bigc_bc(ent_scratch_bigc);
}
#endif	/* HAVE_MPZ */

#ifdef HAVE_MPQ
static Lisp_Object
ent_lift_BIGQ_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigq(ent_scratch_bigfr, XBIGQ_DATA(number));
	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_bigfr(ent_scratch_bigc, ent_scratch_bigfr);
	return make_bigc_bc(ent_scratch_bigc);
}
#endif	/* HAVE_MPQ */

#ifdef HAVE_MPF
static Lisp_Object
ent_lift_BIGF_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigfr_set_prec(ent_scratch_bigfr, precision);
	bigfr_set_bigf(ent_scratch_bigfr, XBIGF_DATA(number));
	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_bigfr(ent_scratch_bigc, ent_scratch_bigfr);
	return make_bigc_bc(ent_scratch_bigc);
}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
static Lisp_Object
ent_lift_BIGFR_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	/* warn about coercions of indefinite symbols */
	if (bigfr_inf_p(XBIGFR_DATA(number)))
		return make_indef(COMPLEX_INFINITY);
	if (bigfr_nan_p(XBIGFR_DATA(number)))
		return make_indef(NOT_A_NUMBER);

	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_bigfr(ent_scratch_bigc, XBIGFR_DATA(number));
	return make_bigc_bc(ent_scratch_bigc);
}
#endif	/* HAVE_MPF */

#ifdef LISP_FLOAT_TYPE
static Lisp_Object
ent_lift_FLOAT_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_double(ent_scratch_bigc, XFLOAT_DATA(number));
	return make_bigc_bc(ent_scratch_bigc);
}
#endif

#ifdef HAVE_PSEUG
static Lisp_Object
ent_lift_BIGG_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	number_type nt;
	bigfr bfr_im, bfr_re;
	Lisp_Object result, re, im;

	precision = ent_normalise_precision(precision);

	re = Freal_part(number);
	nt = get_number_type(re);
	re = ent_optable_lift[nt][BIGFR_T](re, precision);
	im = Fimaginary_part(number);
	nt = get_number_type(im);
	im = ent_optable_lift[nt][BIGFR_T](im, precision);

	bigfr_init(bfr_re);
	bigfr_init(bfr_im);

	bigfr_set(bfr_re, XBIGFR_DATA(re));
	bigfr_set(bfr_im, XBIGFR_DATA(im));
	result = make_bigc_bfr(bfr_re, bfr_im, precision);

	bigfr_fini(bfr_re);
	bigfr_fini(bfr_im);

	return result;
}
#endif

static Lisp_Object
ent_lift_BIGC_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set(ent_scratch_bigc, XBIGC_DATA(number));
	return make_bigc_bc(ent_scratch_bigc);
}


void init_optables_BIGC_T(void)
{
	/* sums */
	ent_optable_sum[BIGC_T][BIGC_T] = ent_sum_BIGC_T;
	ent_optable_sum[BIGC_T][INT_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[INT_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_sum[BIGC_T][BIGZ_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[BIGZ_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_sum[BIGC_T][BIGQ_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[BIGQ_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_sum[BIGC_T][BIGF_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[BIGF_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_sum[BIGC_T][BIGFR_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[BIGFR_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_sum[BIGC_T][FLOAT_T] = ent_sum_BIGC_T_COMPARABLE;
	ent_optable_sum[FLOAT_T][BIGC_T] = ent_sum_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_sum[BIGC_T][BIGG_T] = ent_sum_BIGC_T_COMPLEX;
	ent_optable_sum[BIGG_T][BIGC_T] = ent_sum_COMPLEX_BIGC_T;
#endif
	/* diffs */
	ent_optable_diff[BIGC_T][BIGC_T] = ent_diff_BIGC_T;
	ent_optable_diff[BIGC_T][INT_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[INT_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_diff[BIGC_T][BIGZ_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[BIGZ_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_diff[BIGC_T][BIGQ_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[BIGQ_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_diff[BIGC_T][BIGF_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[BIGF_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_diff[BIGC_T][BIGFR_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[BIGFR_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_diff[BIGC_T][FLOAT_T] = ent_diff_BIGC_T_COMPARABLE;
	ent_optable_diff[FLOAT_T][BIGC_T] = ent_diff_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_diff[BIGC_T][BIGG_T] = ent_diff_BIGC_T_COMPLEX;
	ent_optable_diff[BIGG_T][BIGC_T] = ent_diff_COMPLEX_BIGC_T;
#endif
	/* prods */
	ent_optable_prod[BIGC_T][BIGC_T] = ent_prod_BIGC_T;
	ent_optable_prod[BIGC_T][INT_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[INT_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_prod[BIGC_T][BIGZ_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[BIGZ_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_prod[BIGC_T][BIGQ_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[BIGQ_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_prod[BIGC_T][BIGF_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[BIGF_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_prod[BIGC_T][BIGFR_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[BIGFR_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_prod[BIGC_T][FLOAT_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[FLOAT_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_prod[BIGC_T][BIGG_T] = ent_prod_BIGC_T_COMPLEX;
	ent_optable_prod[BIGG_T][BIGC_T] = ent_prod_COMPLEX_BIGC_T;
#endif

	ent_optable_neg[BIGC_T] = ent_neg_BIGC_T;
	ent_optable_div[BIGC_T][BIGC_T] = ent_div_BIGC_T;
	/* divisions and quotients */
	ent_optable_div[BIGC_T][BIGC_T] = ent_div_BIGC_T;
	ent_optable_div[BIGC_T][INT_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[INT_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_div[BIGC_T][BIGZ_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[BIGZ_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_div[BIGC_T][BIGQ_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[BIGQ_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_div[BIGC_T][BIGF_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[BIGF_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_div[BIGC_T][BIGFR_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[BIGFR_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_div[BIGC_T][FLOAT_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_div[FLOAT_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_div[BIGC_T][BIGG_T] = ent_div_BIGC_T_COMPLEX;
	ent_optable_div[BIGG_T][BIGC_T] = ent_div_COMPLEX_BIGC_T;
#endif
	ent_optable_quo[BIGC_T][BIGC_T] = ent_div_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_quo[BIGC_T][BIGZ_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[BIGZ_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_quo[BIGC_T][BIGQ_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[BIGQ_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_quo[BIGC_T][BIGF_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[BIGF_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_quo[BIGC_T][BIGFR_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[BIGFR_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_quo[BIGC_T][FLOAT_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[FLOAT_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_quo[BIGC_T][BIGG_T] = ent_div_BIGC_T_COMPLEX;
	ent_optable_quo[BIGG_T][BIGC_T] = ent_div_COMPLEX_BIGC_T;
#endif
	ent_optable_inv[BIGC_T] = ent_inv_BIGC_T;
	/* remainders */
	ent_optable_rem[BIGC_T][BIGC_T] = ent_rem_BIGC_T;
	ent_optable_mod[BIGC_T][BIGC_T] = ent_mod_BIGC_T;

	ent_optable_vallt[BIGC_T][BIGC_T] = ent_vallt_BIGC_T;
	ent_optable_valgt[BIGC_T][BIGC_T] = ent_valgt_BIGC_T;
	ent_optable_valeq[BIGC_T][BIGC_T] = ent_valeq_BIGC_T;
	ent_optable_valne[BIGC_T][BIGC_T] = ent_valne_BIGC_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGC_T] = ent_lift_INT_T_BIGC_T;
#ifdef HAVE_MPZ
	ent_optable_lift[BIGZ_T][BIGC_T] = ent_lift_BIGZ_T_BIGC_T;
#endif
#ifdef HAVE_MPQ
	ent_optable_lift[BIGQ_T][BIGC_T] = ent_lift_BIGQ_T_BIGC_T;
#endif
#ifdef HAVE_MPF
	ent_optable_lift[BIGF_T][BIGC_T] = ent_lift_BIGF_T_BIGC_T;
#endif
#ifdef HAVE_MPFR
	ent_optable_lift[BIGFR_T][BIGC_T] = ent_lift_BIGFR_T_BIGC_T;
#endif
#ifdef LISP_FLOAT_TYPE
	ent_optable_lift[FLOAT_T][BIGC_T] = ent_lift_FLOAT_T_BIGC_T;
#endif
	ent_optable_lift[BIGG_T][BIGC_T] = ent_lift_BIGG_T_BIGC_T;
	ent_optable_lift[BIGC_T][BIGC_T] = ent_lift_BIGC_T_BIGC_T;
}

void init_number_mpc(void)
{
}
