/*
  ent-gaussian.c -- Numeric types for SXEmacs
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
#include "ent-float.h"
#ifdef HAVE_MPFR
#include "ent-mpfr.h"
#endif
#include "ent-gaussian.h"
#ifdef HAVE_MPC
#include "ent-mpc.h"
#endif

bigg ent_scratch_bigg;
static ase_nullary_operation_f Qent_gaussian_zero, Qent_gaussian_one;


static void
bigg_print(Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *fstr = bigg_to_string(XBIGG_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	xfree(fstr);
	fstr = (Bufbyte *)NULL;
	return;
}

static int
bigg_equal(Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return bigg_eql(XBIGG_DATA(obj1), XBIGG_DATA(obj2));
}

static unsigned long
bigg_hash(Lisp_Object obj, int SXE_UNUSED(depth))
{
	return bigg_hashcode(XBIGG_DATA(obj));
}

static Lisp_Object
bigg_mark(Lisp_Object SXE_UNUSED(obj))
{
	return Qnil;
}

static void
bigg_finalise(void *SXE_UNUSED(header), int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing "
			 "pseudo-gaussian objects",Qt);
}

static const struct lrecord_description bigg_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigg, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigg", bigg,
				    bigg_mark, bigg_print, bigg_finalise,
				    bigg_equal, bigg_hash,
				    bigg_description, Lisp_Bigg);

DEFUN("make-bigg", Fmake_bigg, 2, 2, 0, /*
Return the Gaussian number whose rational component is REAL-PART
and whose imaginary component is IMAGINARY-PART.
*/
       (real_part, imaginary_part))
{
	CHECK_COMPARABLE(real_part);
	CHECK_COMPARABLE(imaginary_part);

	{
		Lisp_Object tmp_r = Fcoerce_number(real_part, Qbigz, Qnil);
		Lisp_Object tmp_i = Fcoerce_number(imaginary_part, Qbigz, Qnil);
		return make_bigg_bz(XBIGZ_DATA(tmp_r), XBIGZ_DATA(tmp_i));
	}
}


/* basic functions */
void bigg_init(bigg g)
{
	bigz_init(bigg_re(g));
	bigz_init(bigg_im(g));
}

void bigg_fini(bigg g)
{
	bigz_fini(bigg_re(g));
	bigz_fini(bigg_im(g));
}

unsigned long bigg_hashcode(bigg g)
{
	return (bigz_hashcode(bigg_re(g)) ^
		bigz_hashcode(bigg_im(g)));
}

Bufbyte *bigg_to_string(bigg g, int base)
{
	Bufbyte *intg_str;
	Bufbyte *imag_str;
	int intg_len, imag_len;
	int sign, neg;

	intg_str = (Bufbyte*)bigz_to_string(bigg_re(g), base);
	imag_str = (Bufbyte*)bigz_to_string(bigg_im(g), base);

	intg_len = strlen((char*)intg_str);
	imag_len = strlen((char*)imag_str);

	sign = bigz_sign(bigg_im(g));
	neg = (sign >= 0) ? 1 : 0;

	/* now append the imaginary string */
	XREALLOC_ARRAY(intg_str, Bufbyte, intg_len + neg + imag_len + 2);
	if (neg)
		intg_str[intg_len] = '+';
	memmove(&intg_str[intg_len + neg],
		&imag_str[0],
		imag_len);
	intg_str[intg_len+neg+imag_len] = 'i';
	intg_str[intg_len+neg+imag_len+1] = '\0';
	xfree(imag_str);

	return intg_str;
}

/***** Bigg: converting assignments *****/
void bigg_set(bigg g1,bigg g2)
{
	bigz_set(bigg_re(g1), bigg_re(g2));
	bigz_set(bigg_im(g1), bigg_im(g2));
}

void bigg_set_long(bigg g, long l)
{
	bigz_set_long(bigg_re(g), l);
	bigz_set_long(bigg_im(g), 0L);
}

void bigg_set_long_long(bigg g, long l1, long l2)
{
	bigz_set_long(bigg_re(g), l1);
	bigz_set_long(bigg_im(g), l2);
}

void bigg_set_ulong(bigg g, unsigned long ul)
{
	bigz_set_ulong(bigg_re(g), ul);
	bigz_set_ulong(bigg_im(g), 0UL);
}

void bigg_set_ulong_ulong(bigg g, unsigned long ul1, unsigned long ul2)
{
	bigz_set_ulong(bigg_re(g), ul1);
	bigz_set_ulong(bigg_im(g), ul2);
}

void bigg_set_bigz(bigg g, bigz z)
{
	bigz_set(bigg_re(g), z);
	bigz_set_long(bigg_im(g), 0L);
}

void bigg_set_bigz_bigz(bigg g, bigz z1, bigz z2)
{
	bigz_set(bigg_re(g), z1);
	bigz_set(bigg_im(g), z2);
}

/* void bigc_set_bigg(bigc c, bigg g)
 * {
 *	bigc_set_bigfr_bigfr(bigg_re(g), z1);
 * }
 */

/***** Bigg: comparisons *****/
int bigg_eql(bigg g1, bigg g2)
{
	return ((bigz_eql(bigg_re(g1), bigg_re(g2))) &&
		(bigz_eql(bigg_im(g1), bigg_im(g2))));
}

/***** Bigg: arithmetic *****/
#if defined HAVE_MPFR && defined WITH_MPFR
void bigg_abs(bigfr res, bigg g)
{
	/* the absolute archimedean valuation of a+bi is defined as:
	 * (a^2 + b^2)^(1/2)
	 */
	bigz accu1, accu2, bz;
	bigz_init(accu1);
	bigz_init(accu2);
	bigz_init(bz);

	bigz_mul(accu1, bigg_re(g), bigg_re(g));
	bigz_mul(accu2, bigg_im(g), bigg_im(g));
	bigz_add(bz, accu1, accu2);

	bigfr_set_bigz(res, bz);
	bigfr_sqrt(res, res);

	bigz_fini(accu1);
	bigz_fini(accu2);
	bigz_fini(bz);
}
#endif

void bigg_norm(bigz res, bigg g)
{
	/* norm is the square of the absolute archimedean valuation */
	bigz accu1, accu2;
	bigz_init(accu1);
	bigz_init(accu2);

	bigz_mul(accu1, bigg_re(g), bigg_re(g));
	bigz_mul(accu2, bigg_im(g), bigg_im(g));
	bigz_add(res, accu1, accu2);

	bigz_fini(accu1);
	bigz_fini(accu2);
}

void bigg_neg(bigg res, bigg g)
{
	/* negation is defined point-wise */
	bigz_neg(bigg_re(res), bigg_re(g));
	bigz_neg(bigg_im(res), bigg_im(g));
}

void bigg_conj(bigg res, bigg g)
{
	bigg_set(res, g);
	bigz_neg(bigg_im(res), bigg_im(res));
}

void bigg_add(bigg res, bigg g1, bigg g2)
{
	/* addition is defined point-wise */
	bigz accu1, accu2;
	bigz_init(accu1);
	bigz_init(accu2);

	bigz_add(accu1, bigg_re(g1), bigg_re(g2));
	bigz_add(accu2, bigg_im(g1), bigg_im(g2));
	bigg_set_bigz_bigz(res, accu1, accu2);

	bigz_fini(accu1);
	bigz_fini(accu2);
}

void bigg_sub(bigg res, bigg g1, bigg g2)
{
	/* subtraction is defined point-wise */
	bigz_sub(bigg_re(res), bigg_re(g1), bigg_re(g2));
	bigz_sub(bigg_im(res), bigg_im(g1), bigg_im(g2));
}

void bigg_mul(bigg res, bigg g1, bigg g2)
{
	/* multiplication is defined as:
	 * (a + bi)*(c + di) = (ac - bd) + (ad + bc)i
	 */
	bigz accu1, accu2, accu3, accu4;
	bigz_init(accu1);
	bigz_init(accu2);
	bigz_init(accu3);
	bigz_init(accu4);

	bigz_mul(accu1, bigg_re(g1), bigg_re(g2));
	bigz_mul(accu2, bigg_im(g1), bigg_im(g2));
	bigz_mul(accu3, bigg_re(g1), bigg_im(g2));
	bigz_mul(accu4, bigg_im(g1), bigg_re(g2));

	bigz_sub(bigg_re(res), accu1, accu2);
	bigz_add(bigg_im(res), accu3, accu4);

	bigz_fini(accu1);
	bigz_fini(accu2);
	bigz_fini(accu3);
	bigz_fini(accu4);
}

void bigg_div(bigg res, bigg g1, bigg g2)
{
	/* division is defined as:
	 * (a + bi) div (c + di) = ((a+bi)*(c-di)) div (c*c+d*d)
	 */
	bigz accu1, accu2;
	bigg accug;
	bigz_init(accu1);
	bigz_init(accu2);
	bigg_init(accug);

	/* compute: c^2 + d^2 */
	bigz_mul(accu1, bigg_re(g2), bigg_re(g2));
	bigz_mul(accu2, bigg_im(g2), bigg_im(g2));
	bigz_add(accu1, accu1, accu2);

	/* do normal multiplication with conjugate of g2 */
	bigg_conj(accug, g2);
	bigg_mul(accug, g1, accug);

	bigg_set(res, accug);

	/* now divide (g1*conj(g2)) by c^2+d^2 (point-wise) */
	bigz_div(bigg_re(res), bigg_re(accug), accu1);
	bigz_div(bigg_im(res), bigg_im(accug), accu1);

	bigg_fini(accug);
	bigz_fini(accu2);
	bigz_fini(accu1);
}

void bigg_mod(bigg res, bigg g1, bigg g2)
{
	/* the modulo relation is defined as:
	 * (a + bi) mod (c + di) ~
	 * (a+bi) - ((a+bi) div (c-di)) * (c+di)
	 */
	bigg accug;
	bigg_init(accug);

	/* do normal division */
	bigg_div(accug, g1, g2);

	/* now re-multiply g2 */
	bigg_mul(accug, accug, g2);

	/* and find the difference */
	bigg_sub(res, g1, accug);

	bigg_fini(accug);
}

void bigg_pow(bigg res, bigg g1, unsigned long g2)
{
#if defined(HAVE_MPZ) && defined(WITH_GMP)
	unsigned long i;
	bigz bin, resintg, resimag, tmpbz1, tmpbz2, tmpbz3, intg, imag;

	bigz_init(bin);
	bigz_init(resintg);
	bigz_init(resimag);
	bigz_init(intg);
	bigz_init(imag);
	bigz_init(tmpbz1);
	bigz_init(tmpbz2);
	bigz_init(tmpbz3);

	bigz_set_long(resintg, 0L);
	bigz_set_long(resimag, 0L);

	bigz_set(intg, bigg_re(g1));
	bigz_set(imag, bigg_im(g1));

	/* we compute using the binomial coefficients */
	for (i=0; i<=g2; i++) {
		mpz_bin_uiui(bin, g2, i);
		if (i % 2 == 0) {
			/* real part changes */
			bigz_pow(tmpbz1, intg, g2-i);
			bigz_pow(tmpbz2, imag, i);
			bigz_mul(tmpbz3, tmpbz1, tmpbz2);
			bigz_mul(bin, bin, tmpbz3);
			if (i % 4 == 0) {
				bigz_add(resintg, resintg, bin);
			} else if (i % 4 == 2) {
				bigz_sub(resintg, resintg, bin);
			}
		} else {
			/* imag part changes */
			bigz_pow(tmpbz1, intg, g2-i);
			bigz_pow(tmpbz2, imag, i);
			bigz_mul(tmpbz3, tmpbz1, tmpbz2);
			bigz_mul(bin, bin, tmpbz3);
			if (i % 4 == 1) {
				bigz_add(resimag, resimag, bin);
			} else if (i % 4 == 3) {
				bigz_sub(resimag, resimag, bin);
			}
		}
	}

	bigg_set_bigz_bigz(res, resintg, resimag);

	bigz_fini(bin);
	bigz_fini(intg);
	bigz_fini(imag);
	bigz_init(resintg);
	bigz_init(resimag);
	bigz_fini(tmpbz1);
	bigz_fini(tmpbz2);
	bigz_fini(tmpbz3);
#else
	bigg_set_long_long(res, 0L, 0L);
#endif
}

Lisp_Object read_bigg_string(char *cp)
{
	bigz bz_re, bz_im;
	int sign;
	Lisp_Object result;
	Bufbyte *end;
	Bufbyte tmp;

	bigz_init(bz_re);
	bigz_init(bz_im);

	/* MPZ bigz_set_string has no effect
	 * with initial + sign */
	if (*cp == '+')
		cp++;

	end = (Bufbyte *)cp;

	if (*cp == '-') {
		/* jump over a leading minus */
		cp++;
	}

	while ((*cp >= '0' && *cp <= '9'))
		cp++;

	/* MPZ cannot read numbers with characters after them.
	 * See limitations below in convert GMP-MPZ strings
	 */
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	bigz_set_string(bz_re, (char *)end, 0);
	*cp = tmp;

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
	if ((*cp == 'i' || *cp == 'I') && (sign == 1)) {
		/* expand +i to +1i and -i to -1i */
		bigz_set_long(bz_im, 1L);
	} else if ((*cp == 'i' || *cp == 'I') && (sign == -1)) {
		/* expand +i to +1i and -i to -1i */
		bigz_set_long(bz_im, -1L);
	} else if (sign == 0) {
		/* obviously we did not have a+bi,
		 * but merely bi
		 */
		bigz_set(bz_im, bz_re);
		bigz_set_long(bz_re, 0L);
	} else {
		end = (Bufbyte*)cp;
		if (sign == -1)
			end--;
		while ((*cp >= '0' && *cp <= '9'))
			cp++;
		tmp = (Bufbyte)*cp;
		*cp = '\0';
		bigz_set_string(bz_im, (char *)end, 0);
		*cp = tmp;
	}

	result = make_bigg_bz(bz_re, bz_im);

	bigz_fini(bz_re);
	bigz_fini(bz_im);
	return result;
}

/* bigg ops */
static inline int
ent_gaussian_zerop(Lisp_Object o)
{
	return (bigz_sign(bigg_re(XBIGG_DATA(o))) == 0 &&
		bigz_sign(bigg_im(XBIGG_DATA(o))) == 0);
}

static inline int
ent_gaussian_onep(Lisp_Object o)
{
	return ((bigz_fits_long_p(bigg_re(XBIGG_DATA(o))) &&
		 bigz_to_long(bigg_re(XBIGG_DATA(o))) == 1L) &&
		bigz_sign(bigg_im(XBIGG_DATA(o))) == 0);
}

static inline int
ent_gaussian_unitp(Lisp_Object o)
{
	return (!ent_gaussian_zerop(o) &&
		(bigz_fits_long_p(bigg_re(XBIGG_DATA(o))) &&
		 (bigz_to_long(bigg_re(XBIGG_DATA(o))) == 0L ||
		  bigz_to_long(bigg_re(XBIGG_DATA(o))) == 1L ||
		  bigz_to_long(bigg_re(XBIGG_DATA(o))) == -1L)) &&
		(bigz_fits_long_p(bigg_im(XBIGG_DATA(o))) &&
		 (bigz_to_long(bigg_im(XBIGG_DATA(o))) == 0L ||
		  bigz_to_long(bigg_im(XBIGG_DATA(o))) == 1L ||
		  bigz_to_long(bigg_im(XBIGG_DATA(o))) == -1L)));
}

static inline Lisp_Object
ent_sum_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigg_add(ent_scratch_bigg, XBIGG_DATA(l), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_sum_BIGG_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(r);

	r = ent_lift(r, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(r));
	bigg_add(ent_scratch_bigg, XBIGG_DATA(l), ent_scratch_bigg);
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_sum_COMPARABLE_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(l);

	l = ent_lift(l, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(l));
	bigg_add(ent_scratch_bigg, ent_scratch_bigg, XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}

static inline Lisp_Object
ent_diff_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigg_sub(ent_scratch_bigg, XBIGG_DATA(l), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_diff_BIGG_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(r);

	r = ent_lift(r, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(r));
	bigg_sub(ent_scratch_bigg, XBIGG_DATA(l), ent_scratch_bigg);
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_diff_COMPARABLE_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(l);

	l = ent_lift(l, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(l));
	bigg_sub(ent_scratch_bigg, ent_scratch_bigg, XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}

static inline Lisp_Object
ent_neg_BIGG_T(Lisp_Object l)
{
	bigg_neg(ent_scratch_bigg, XBIGG_DATA(l));
	return make_bigg_bg(ent_scratch_bigg);
}

static inline Lisp_Object
ent_prod_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigg_mul(ent_scratch_bigg, XBIGG_DATA(l), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_prod_BIGG_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(r);

	r = ent_lift(r, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(r));
	bigg_mul(ent_scratch_bigg, XBIGG_DATA(l), ent_scratch_bigg);
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_prod_COMPARABLE_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(l);

	l = ent_lift(l, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(l));
	bigg_mul(ent_scratch_bigg, ent_scratch_bigg, XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}

static inline Lisp_Object
ent_div_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_gaussian_zerop(r)) {
		if (!ent_gaussian_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}
	bigg_div(ent_scratch_bigg, XBIGG_DATA(l), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_div_BIGG_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(r);

	if (ent_unrel_zerop(l)) {
		if (!ent_gaussian_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}

	r = ent_lift(r, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(r));
	bigg_div(ent_scratch_bigg, XBIGG_DATA(l), ent_scratch_bigg);
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_div_COMPARABLE_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(l);

	if (ent_gaussian_zerop(r)) {
		if (!ent_unrel_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}

	l = ent_lift(l, BIGZ_T, NULL);

	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(l));
	bigg_div(ent_scratch_bigg, ent_scratch_bigg, XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
static inline Lisp_Object
ent_quo_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object tmp_l, tmp_r;

	if (ent_gaussian_zerop(r)) {
		if (!ent_gaussian_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}

	bigc_set_prec(ent_scratch_bigc, internal_get_precision(Qnil));
	tmp_l = Fcoerce_number(l, Qbigc, Qnil);
	tmp_r = Fcoerce_number(r, Qbigc, Qnil);
	bigc_div(ent_scratch_bigc, XBIGC_DATA(tmp_l), XBIGC_DATA(tmp_r));
	return make_bigc_bc(ent_scratch_bigc);
}
static inline Lisp_Object
ent_quo_BIGG_T_COMPARABLE(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(r);

	if (ent_unrel_zerop(r)) {
		if (!ent_gaussian_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}

	l = ent_lift(l, BIGC_T, NULL);
	return ent_binop(ASE_BINARY_OP_QUO, l, r);
}
static inline Lisp_Object
ent_quo_COMPARABLE_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	CHECK_COMPARABLE(l);

	if (ent_gaussian_zerop(r)) {
		if (!ent_unrel_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}

	r = ent_lift(r, BIGC_T, NULL);
	return ent_binop(ASE_BINARY_OP_QUO, l, r);
}
#endif

static inline Lisp_Object
ent_inv_BIGG_T(Lisp_Object r)
{
	if (ent_gaussian_zerop(r)) {
		return make_indef(COMPLEX_INFINITY);
	}
	bigg_div(ent_scratch_bigg,
		 XBIGG_DATA(Qent_gaussian_one), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_rem_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_gaussian_zerop(r)) {
		return make_bigg(0, 0);
	}
	bigg_mod(ent_scratch_bigg, XBIGG_DATA(l), XBIGG_DATA(r));
	return make_bigg_bg(ent_scratch_bigg);
}
static inline Lisp_Object
ent_pow_BIGG_T_integer(Lisp_Object l, Lisp_Object r)
{
	long unsigned int expo = 0UL;

	if (INTP(r)) {
		expo = ent_int(r);
	} else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, r);
	} else {
		Fsignal(Qdomain_error, r);
	}
	bigg_pow(ent_scratch_bigg, XBIGG_DATA(l), expo);
	return make_bigg_bg(ent_scratch_bigg);
}

/* relations */
static inline int
ent_eq_bigg(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(bigg_re(XBIGG_DATA(l)), bigg_re(XBIGG_DATA(r))) &&
		bigz_eql(bigg_im(XBIGG_DATA(l)), bigg_im(XBIGG_DATA(r))));
}

static inline int
ent_ne_bigg(Lisp_Object l, Lisp_Object r)
{
	return !(bigz_eql(bigg_re(XBIGG_DATA(l)), bigg_re(XBIGG_DATA(r))) &&
		 bigz_eql(bigg_im(XBIGG_DATA(l)), bigg_im(XBIGG_DATA(r))));
}

#if 0
static inline Lisp_Object
ent_vallt_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	bigg_norm(ent_scratch_bigz, XBIGG_DATA(l));
	bigg_norm(b2, XBIGG_DATA(r));
	result = bigz_lt(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valgt_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	bigg_norm(ent_scratch_bigz, XBIGG_DATA(l));
	bigg_norm(b2, XBIGG_DATA(r));
	result = bigz_gt(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valeq_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	bigg_norm(ent_scratch_bigz, XBIGG_DATA(l));
	bigg_norm(b2, XBIGG_DATA(r));
	result = bigz_eql(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valne_BIGG_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	bigg_norm(ent_scratch_bigz, XBIGG_DATA(l));
	bigg_norm(b2, XBIGG_DATA(r));
	result = bigz_eql(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qnil : Qt;
}
#endif


static Lisp_Object
ent_lift_all_BIGG_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	number = ent_lift(number, BIGZ_T, NULL);
	bigg_set_bigz(ent_scratch_bigg, XBIGZ_DATA(number));
	return make_bigg_bg(ent_scratch_bigg);
}

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
static Lisp_Object
ent_lift_BIGC_T_BIGG_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	Lisp_Object re, im;

	re = Freal_part(number);
	re = ent_lift(re, BIGZ_T, NULL);
	im = Fimaginary_part(number);
	im = ent_lift(im, BIGZ_T, NULL);

	return make_bigg_bz(XBIGZ_DATA(re), XBIGZ_DATA(im));
}
#endif


static inline void
ent_gaussian_nullary_optable_init(void)
{
	Qent_gaussian_zero = make_bigg(0L, 0L);
	Qent_gaussian_one = make_bigg(1L, 0L);
	staticpro(&Qent_gaussian_zero);
	staticpro(&Qent_gaussian_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, BIGG_T, Qent_gaussian_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, BIGG_T, Qent_gaussian_one);
}

static inline void
ent_gaussian_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, BIGG_T, ent_neg_BIGG_T);
	ent_unop_register(ASE_UNARY_OP_INV, BIGG_T, ent_inv_BIGG_T);
}

static inline void
ent_gaussian_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, BIGG_T, ent_sum_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, INT_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, BIGZ_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, BIGQ_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGQ_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, BIGF_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGF_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, BIGFR_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGFR_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGG_T, FLOAT_T, ent_sum_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, BIGG_T, ent_sum_COMPARABLE_BIGG_T);
#endif
	/* diffs */
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, BIGG_T, ent_diff_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, INT_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, BIGZ_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, BIGQ_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGQ_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, BIGF_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGF_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, BIGFR_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGFR_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGG_T, FLOAT_T, ent_diff_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, BIGG_T, ent_diff_COMPARABLE_BIGG_T);
#endif
	/* prods */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, BIGG_T, ent_prod_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, INT_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, BIGZ_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, BIGQ_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGQ_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, BIGF_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGF_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, BIGFR_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGFR_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGG_T, FLOAT_T, ent_prod_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, BIGG_T, ent_prod_COMPARABLE_BIGG_T);
#endif

	/* divisions and quotients */
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, BIGG_T, ent_div_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, INT_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, BIGZ_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, BIGQ_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGQ_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, BIGF_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGF_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, BIGFR_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGFR_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGG_T, FLOAT_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGG_T, ent_quo_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGZ_T, ent_quo_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGG_T, ent_quo_COMPARABLE_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGQ_T, ent_quo_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGG_T, ent_quo_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGF_T, ent_quo_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGG_T, ent_quo_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGFR_T, ent_quo_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGG_T, ent_quo_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, FLOAT_T, ent_quo_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGG_T, ent_quo_COMPARABLE_BIGG_T);
#endif
#else  /* !HAVE_MPC */
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGG_T, ent_div_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGZ_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGQ_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGQ_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGF_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGF_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, BIGFR_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGFR_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGG_T, FLOAT_T, ent_div_BIGG_T_COMPARABLE);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, BIGG_T, ent_div_COMPARABLE_BIGG_T);
#endif
#endif
	ent_binop_register(ASE_BINARY_OP_REM,
			   BIGG_T, BIGG_T, ent_rem_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   BIGG_T, BIGG_T, ent_rem_BIGG_T);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGG_T, INT_T, ent_pow_BIGG_T_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   BIGG_T, BIGZ_T, ent_pow_BIGG_T_integer);
}

static inline void
ent_gaussian_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, BIGG_T, ent_gaussian_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, BIGG_T, ent_gaussian_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, BIGG_T, ent_gaussian_unitp);
}

static inline void
ent_gaussian_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    BIGG_T, BIGG_T, ent_eq_bigg);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    BIGG_T, BIGG_T, ent_ne_bigg);
}

static inline void
ent_gaussian_lifttable_init(void)
{
	ent_lift_register(INT_T, BIGG_T, ent_lift_all_BIGG_T);
	ent_lift_register(BIGZ_T, BIGC_T, ent_lift_all_BIGG_T);
#if defined HAVE_MPQ && defined WITH_GMP
	ent_lift_register(BIGQ_T, BIGG_T, ent_lift_all_BIGG_T);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	ent_lift_register(BIGF_T, BIGG_T, ent_lift_all_BIGG_T);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	ent_lift_register(BIGFR_T, BIGG_T, ent_lift_all_BIGG_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_lift_register(FLOAT_T, BIGG_T, ent_lift_all_BIGG_T);
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	ent_lift_register(BIGC_T, BIGG_T, ent_lift_BIGC_T_BIGG_T);
#endif
}

void init_optables_BIGG_T(void)
{
	ent_gaussian_nullary_optable_init();
	ent_gaussian_unary_optable_init();
	ent_gaussian_binary_optable_init();
	ent_gaussian_unary_reltable_init();
	ent_gaussian_binary_reltable_init();
	ent_gaussian_lifttable_init();
}

void init_ent_gaussian(void)
{
	bigg_init(ent_scratch_bigg);
}

void syms_of_ent_gaussian(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigg);

	DEFSUBR(Fmake_bigg);
}

void vars_of_ent_gaussian(void)
{
	Fprovide(intern("bigg"));
	Fprovide(intern("gaussian"));
}
