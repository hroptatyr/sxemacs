/*
  ent-mpc.c -- Numeric types for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

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

#include "ent-mpc.h"

bigc ent_scratch_bigc;


static void
bigc_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = bigc_to_string(XBIGC_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigc_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigc_eql(XBIGC_DATA(obj1), XBIGC_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigc_hash (Lisp_Object obj, int depth)
{
	return bigc_hashcode(XBIGC_DATA(obj));

	/* less warnings */
	if (depth);
}

static Lisp_Object
bigc_mark (Lisp_Object obj)
{
	return Qnil;

	/* less warnings */
	if (obj == Qnil);
}

static void
bigc_finalise (void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing MPC objects",Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description bigc_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigc, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigc", bigc,
				    bigc_mark, bigc_print, bigc_finalise,
				    bigc_equal, bigc_hash,
				    bigc_description, Lisp_Bigc);



DEFUN ("bigc-get-precision", Fbigc_get_precision, 1, 1, 0, /*
Return the precision of bigc C as an integer.
							   */
       (c))
{
	CHECK_BIGC(c);
	return make_integer((signed long)XBIGC_GET_PREC(c));
}

DEFUN ("bigc-set-precision", Fbigc_set_precision, 2, 2, 0, /*
Set the precision of C, a bigc, to PRECISION, a nonnegative integer.
The new precision of C is returned.  Note that the return value may differ
from PRECISION if the underlying library is unable to support exactly
PRECISION bits of precision.
							   */
       (c, precision))
{
	unsigned long prec;

	CHECK_BIGC(c);
	if (INTP(precision)) {
		prec = (XINT(precision) <= 0)
			? MPFR_PREC_MIN : (unsigned long)XINT(precision);
	}
#ifdef HAVE_MPZ
	else if (BIGZP(precision)) {
		prec = bigz_fits_ulong_p(XBIGZ_DATA(precision))
			? bigz_to_ulong(XBIGZ_DATA(precision))
			: UINT_MAX;
	}
#endif	/* HAVE_MPZ */
	else {
		dead_wrong_type_argument(Qintegerp, c);
		return Qnil;
	}

	XBIGC_SET_PREC(c, prec);
	return Fbigc_get_precision(c);
}

DEFUN ("make-bigc", Fmake_bigc, 2, 2, 0, /*
Return the bigc number whose real component is REAL-PART and
whose imaginary component is IMAGINARY-PART.
					 */
       (real_part, imaginary_part))
{
	Lisp_Object result;

	CHECK_COMPARABLE(real_part);
	CHECK_COMPARABLE(imaginary_part);

	real_part = Fcoerce_number(
		real_part, Qbigfr, Qnil);
	imaginary_part = Fcoerce_number(
		imaginary_part, Qbigfr, Qnil);

	/* check if one of the components is not-a-number
	 * set both components NaN in that case
	 */
	if (bigfr_nan_p(XBIGFR_DATA(real_part)) ||
	    bigfr_nan_p(XBIGFR_DATA(imaginary_part))) {
		bigfr_set_nan(XBIGFR_DATA(real_part));
		bigfr_set_nan(XBIGFR_DATA(imaginary_part));
	} else if (bigfr_inf_p(XBIGFR_DATA(real_part)) ||
		   bigfr_inf_p(XBIGFR_DATA(imaginary_part))) {
		bigfr_set_pinf(XBIGFR_DATA(real_part));
		bigfr_set_pinf(XBIGFR_DATA(imaginary_part));
	}

	result =  make_bigc_bfr(XBIGFR_DATA(real_part),
				XBIGFR_DATA(imaginary_part),
				internal_get_precision(Qnil));

	return result;
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


Bufbyte *bigc_to_string(mpc_t c, int base)
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


void bigc_pow(bigc res, bigc g1, unsigned long g2)
{
#if defined(HAVE_MPZ) && defined(WITH_GMP)
	unsigned long i;
	bigz bin;
	bigfr binfr, resintg, resimag, tmpbz1, tmpbz2, tmpbz3, intg, imag;

	bigz_init(bin);
	bigfr_init(binfr);
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
		bigfr_set_bigz(binfr, bin);
		if (i % 2 == 0) {
			/* real part changes */
			bigfr_pow(tmpbz1, intg, g2-i);
			bigfr_pow(tmpbz2, imag, i);
			bigfr_mul(tmpbz3, tmpbz1, tmpbz2);
			bigfr_mul(binfr, binfr, tmpbz3);
			if (i % 4 == 0) {
				bigfr_add(resintg, resintg, binfr);
			} else if (i % 4 == 2) {
				bigfr_sub(resintg, resintg, binfr);
			}
		} else {
			/* imag part changes */
			bigfr_pow(tmpbz1, intg, g2-i);
			bigfr_pow(tmpbz2, imag, i);
			bigfr_mul(tmpbz3, tmpbz1, tmpbz2);
			bigfr_mul(binfr, binfr, tmpbz3);
			if (i % 4 == 1) {
				bigfr_add(resimag, resimag, binfr);
			} else if (i % 4 == 3) {
				bigfr_sub(resimag, resimag, binfr);
			}
		}
	}

	bigc_set_bigfr_bigfr(res, resintg, resimag);

	bigz_fini(bin);
	bigfr_fini(binfr);
	bigfr_fini(intg);
	bigfr_fini(imag);
	bigfr_init(resintg);
	bigfr_init(resimag);
	bigfr_fini(tmpbz1);
	bigfr_fini(tmpbz2);
	bigfr_fini(tmpbz3);
#else  /* !WITH_GMP */
	bigc_set_long_long(res, 0L, 0L);
#endif	/* WITH_GMP */
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
	/* make the compiler smile */
	if (NILP(l));
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

/* relations */
static Lisp_Object
ent_eq_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(bigc_re(XBIGC_DATA(l)), bigc_re(XBIGC_DATA(r))) &&
		bigfr_eql(bigc_im(XBIGC_DATA(l)), bigc_im(XBIGC_DATA(r))))
		? Qt : Qnil;
}

static Lisp_Object
ent_ne_BIGC_T(Lisp_Object l, Lisp_Object r)
{
	return (bigfr_eql(bigc_re(XBIGC_DATA(l)), bigc_re(XBIGC_DATA(r))) &&
		bigfr_eql(bigc_im(XBIGC_DATA(l)), bigc_im(XBIGC_DATA(r))))
		? Qnil : Qt;
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

#ifdef HAVE_FPFLOAT
static Lisp_Object
ent_lift_FLOAT_T_BIGC_T(Lisp_Object number, unsigned long precision)
{
	precision = ent_normalise_precision(precision);

	bigc_set_prec(ent_scratch_bigc, precision);
	bigc_set_fpfloat(ent_scratch_bigc, XFLOAT_DATA(number));
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
#ifdef HAVE_FPFLOAT
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
#ifdef HAVE_FPFLOAT
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
#ifdef HAVE_FPFLOAT
	ent_optable_prod[BIGC_T][FLOAT_T] = ent_prod_BIGC_T_COMPARABLE;
	ent_optable_prod[FLOAT_T][BIGC_T] = ent_prod_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_prod[BIGC_T][BIGG_T] = ent_prod_BIGC_T_COMPLEX;
	ent_optable_prod[BIGG_T][BIGC_T] = ent_prod_COMPLEX_BIGC_T;
#endif

	ent_optable_neg[BIGC_T] = ent_neg_BIGC_T;
	ent_optable_inv[BIGC_T] = ent_inv_BIGC_T;
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
#ifdef HAVE_FPFLOAT
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
#ifdef HAVE_FPFLOAT
	ent_optable_quo[BIGC_T][FLOAT_T] = ent_div_BIGC_T_COMPARABLE;
	ent_optable_quo[FLOAT_T][BIGC_T] = ent_div_COMPARABLE_BIGC_T;
#endif
#ifdef HAVE_PSEUG
	ent_optable_quo[BIGC_T][BIGG_T] = ent_div_BIGC_T_COMPLEX;
	ent_optable_quo[BIGG_T][BIGC_T] = ent_div_COMPLEX_BIGC_T;
#endif
	ent_optable_rem[BIGC_T][BIGC_T] = ent_rem_BIGC_T;
	ent_optable_mod[BIGC_T][BIGC_T] = ent_mod_BIGC_T;

	ent_optable_eq[BIGC_T][BIGC_T] = ent_eq_BIGC_T;
	ent_optable_ne[BIGC_T][BIGC_T] = ent_ne_BIGC_T;
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
#ifdef HAVE_FPFLOAT
	ent_optable_lift[FLOAT_T][BIGC_T] = ent_lift_FLOAT_T_BIGC_T;
#endif
	ent_optable_lift[BIGG_T][BIGC_T] = ent_lift_BIGG_T_BIGC_T;
	ent_optable_lift[BIGC_T][BIGC_T] = ent_lift_BIGC_T_BIGC_T;
}

void init_ent_mpc(void)
{
	bigc_init(ent_scratch_bigc);
}

void syms_of_ent_mpc(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigc);

	DEFSUBR(Fbigc_get_precision);
	DEFSUBR(Fbigc_set_precision);
	DEFSUBR(Fmake_bigc);
}

void vars_of_ent_mpc(void)
{
	Fprovide(intern("bigc"));
}
