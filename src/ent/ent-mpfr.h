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


#ifndef INCLUDED_number_mpfr_h_
#define INCLUDED_number_mpfr_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include <mpfr.h>

#include <ent/ent-indef.h>

typedef mpfr_t bigfr;
typedef mp_prec_t bigfr_prec;
typedef mp_rnd_t bigfr_rnd;

extern Lisp_Object Qbigfrp;

extern Lisp_Object Veuler;
extern Lisp_Object Veuler_mascheroni;
extern Lisp_Object Vpi;


typedef struct Lisp_Bigfr Lisp_Bigfr;
struct Lisp_Bigfr
{
	struct lrecord_header lheader;
	bigfr data;
};

DECLARE_LRECORD(bigfr, Lisp_Bigfr);
#define XBIGFR(x) XRECORD(x, bigfr, Lisp_Bigfr)
#define wrap_bigfr(p) wrap_object(p)
#define BIGFRP(x) RECORDP(x, bigfr)
#define CHECK_BIGFR(x) CHECK_RECORD(x, bigfr)
#define CONCHECK_BIGFR(x) CONCHECK_RECORD(x, bigfr)

#define bigfr_data(f) ((f)->data)
#define XBIGFR_DATA(x) bigfr_data(XBIGFR(x))
#define XBIGFR_GET_PREC(x) bigfr_get_prec(XBIGFR_DATA(x))
#define XBIGFR_SET_PREC(x,p) bigfr_set_prec(XBIGFR_DATA(x), p)

#define BIGFR_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_bigfr_bfr(XBIGFR_DATA(f));		\
	bigfr_##op(XBIGFR_DATA(retval), XBIGFR_DATA(f));		\
	return retval;							\
} while (0)

#define BIGFR_ARITH_RETURN1(f,op,arg) do				\
{									\
	Lisp_Object retval = make_bigfr_bfr(XBIGFR_DATA(f));		\
	bigfr_##op(XBIGFR_DATA(retval), XBIGFR_DATA(f), arg);		\
	return retval;							\
} while (0)

#define BIGFR_INIT_PREC(f, prec) do					\
{									\
	bigfr_init_prec(f, internal_get_precision(prec));		\
} while (0)

extern bigfr ent_scratch_bigfr;


/******************************** Bigfrs ********************************/

/***** Bigfr: basic functions *****/
#define bigfr_init(f)                mpfr_init(f)
#define bigfr_init_prec(f,prec)      mpfr_init2(f, prec)
#define bigfr_fini(f)                mpfr_clear(f)
#define bigfr_hashcode(f)            mpfr_get_ui(f, GMP_RNDN)
#define bigfr_sign(f)                mpfr_sgn(f)
#define bigfr_get_prec(f)            mpfr_get_prec(f)
#define bigfr_set_prec(f, prec)      mpfr_set_prec(f, prec)
#define bigfr_set_default_prec(prec) mpfr_set_default_prec(prec)
#define bigfr_get_default_prec()     mpfr_get_default_prec()

/***** Bigfr: conversions *****/
extern Bufbyte *bigfr_to_string (bigfr, int);
#define bigfr_to_int(f)              ((int)mpfr_get_si(f), GMP_RNDN)
#define bigfr_to_uint(f)             ((unsigned int)mpfr_get_ui(f), GMP_RNDN)
#define bigfr_to_long(f)             mpfr_get_si(f, GMP_RNDN)
#define bigfr_to_ulong(f)            mpfr_get_ui(f, GMP_RNDN)
#if fpfloat_double_p
#define bigfr_to_fpfloat(f)		mpfr_get_d(f, GMP_RNDN)
#elif fpfloat_long_double_p
#define bigfr_to_fpfloat(f)		mpfr_get_ld(f, GMP_RNDN)
#endif

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
#define bigz_set_bigfr(b,f)          mpfr_get_z(b, f, GMP_RNDN)
#endif
#if defined HAVE_MPQ && defined WITH_GMP
#define bigq_set_bigfr(q, f)         mpfr_get_q(q, f, GMP_RNDN);
#endif
#if defined HAVE_MPF && defined WITH_GMP
#define bigf_set_bigfr(f, fr)        mpfr_get_f(f, fr, GMP_RNDN);
#endif

/***** Bigfr: conversion predicates *****/
#define bigfr_fits_int(f)            mpfr_fits_sint_p(f, GMP_RNDN)
#define bigfr_fits_uint(f)           mpfr_fits_uint_p(f, GMP_RNDN)
#define bigfr_fits_long(f)           mpfr_fits_slong_p(f, GMP_RNDN)
#define bigfr_fits_ulong(f)          mpfr_fits_ulong_p(f, GMP_RNDN)

/***** Bigfr: converting assignments *****/
#define bigfr_set(f1,f2)             mpfr_set(f1, f2, GMP_RNDN)
#define bigfr_set_string(f,str,base) mpfr_set_str(f, str, base, GMP_RNDN)
#define bigfr_set_long(f,l)          mpfr_set_si(f, l, GMP_RNDN)
#define bigfr_set_ulong(f,l)         mpfr_set_ui(f, l, GMP_RNDN)
#if fpfloat_double_p
#define bigfr_set_fpfloat(f, d)		mpfr_set_d(f, d, GMP_RNDN)
#elif fpfloat_long_double_p
#define bigfr_set_fpfloat(f, d)		mpfr_set_ld(f, d, GMP_RNDN)
#endif
#define bigfr_set_bigz(f,b)          mpfr_set_z(f, b, GMP_RNDN)
#define bigfr_set_bigq(f,r)          mpfr_set_q(f, r, GMP_RNDN)
#define bigfr_set_bigf(f,r)          mpfr_set_f(f, r, GMP_RNDN)

#define bigfr_set_pinf(f)            mpfr_set_inf(f, 1)
#define bigfr_set_ninf(f)            mpfr_set_inf(f, -1)
#define bigfr_set_nan(f)             mpfr_set_nan(f)

#define bigfr_inf_p(f)               mpfr_inf_p(f)
#define bigfr_nan_p(f)               mpfr_nan_p(f)

/***** Bigfr: comparisons *****/
#define bigfr_cmp(f1,f2)             mpfr_cmp(f1, f2)
#define bigfr_lt(f1,f2)              mpfr_less_p(f1, f2)
#define bigfr_le(f1,f2)              mpfr_lessequal_p(f1, f2)
#define bigfr_eq(f1,f2)			mpfr_equal_p(f1, f2)
#define bigfr_ge(f1,f2)              mpfr_greaterequal_p(f1, f2)
#define bigfr_gt(f1,f2)              mpfr_greater_p(f1, f2)
#define bigfr_ne(f1, f2)             mpfr_lessgreater_p(f1, f2)
#define bigfr_nc(f1, f2)             mpfr_unordered_p(f1, f2)

/***** Bigfr: arithmetic *****/
#define bigfr_neg(f,f2)              mpfr_neg(f, f2, GMP_RNDN)
#define bigfr_abs(f,f2)              mpfr_abs(f, f2, GMP_RNDN)
#define bigfr_add(res,f1,f2)         mpfr_add(res, f1, f2, GMP_RNDN)
#define bigfr_sub(res,f1,f2)         mpfr_sub(res, f1, f2, GMP_RNDN)
#define bigfr_mul(res,f1,f2)         mpfr_mul(res, f1, f2, GMP_RNDN)
#define bigfr_div(res,f1,f2)         mpfr_div(res, f1, f2, GMP_RNDN)

#define bigfr_rint(res,f)            mpfr_rint(res, f, GMP_RNDN)
#define bigfr_ceil(res,f)            mpfr_ceil(res, f)
#define bigfr_floor(res,f)           mpfr_floor(res, f)
#define bigfr_trunc(res,f)           mpfr_trunc(res, f)
#define bigfr_sqrt(res,f)            mpfr_sqrt(res, f, GMP_RNDN)
#define bigfr_sqrt_ui(res,f)         mpfr_sqrt_ui(res, f, GMP_RNDN)
#define bigfr_cbrt(res,f)            mpfr_cbrt(res, f, GMP_RNDN)
#define bigfr_root(res,f,rad)        mpfr_root(res, f, rad, GMP_RNDN)
#define bigfr_pow(res,f,exp)         mpfr_pow_ui(res, f, exp, GMP_RNDN)

/* Advanced functions */
#define bigfr_exp(res, f)            mpfr_exp(res, f, GMP_RNDN);
#define bigfr_exp2(res, f)           mpfr_exp2(res, f, GMP_RNDN);
#define bigfr_exp10(res, f)          mpfr_exp10(res, f, GMP_RNDN);

#define bigfr_log(res, f)            mpfr_log(res, f, GMP_RNDN);
#define bigfr_log2(res, f)           mpfr_log2(res, f, GMP_RNDN);
#define bigfr_log10(res, f)          mpfr_log10(res, f, GMP_RNDN);

#define bigfr_erf(res, f)            mpfr_erf(res, f, GMP_RNDN);
#define bigfr_erfc(res, f)           mpfr_erfc(res, f, GMP_RNDN);
#define bigfr_lgamma(res, f)         mpfr_lngamma(res, f, GMP_RNDN);

#define bigfr_cos(res, f)	     mpfr_cos(res, f, GMP_RNDN);
#define bigfr_sin(res, f)	     mpfr_sin(res, f, GMP_RNDN);
#define bigfr_tan(res, f)	     mpfr_tan(res, f, GMP_RNDN);
#define bigfr_sec(res, f)	     mpfr_sec(res, f, GMP_RNDN);
#define bigfr_csc(res, f)	     mpfr_csc(res, f, GMP_RNDN);
#define bigfr_cot(res, f)	     mpfr_cot(res, f, GMP_RNDN);

#define bigfr_acos(res, f)	     mpfr_acos(res, f, GMP_RNDN);
#define bigfr_asin(res, f)	     mpfr_asin(res, f, GMP_RNDN);
#define bigfr_atan(res, f)	     mpfr_atan(res, f, GMP_RNDN);
#define bigfr_atan2(res, f, g)	     mpfr_atan2(res, f, g, GMP_RNDN);

#define bigfr_cosh(res, f)	     mpfr_cosh(res, f, GMP_RNDN);
#define bigfr_sinh(res, f)	     mpfr_sinh(res, f, GMP_RNDN);
#define bigfr_tanh(res, f)	     mpfr_tanh(res, f, GMP_RNDN);
#define bigfr_sech(res, f)	     mpfr_sech(res, f, GMP_RNDN);
#define bigfr_csch(res, f)	     mpfr_csch(res, f, GMP_RNDN);
#define bigfr_coth(res, f)	     mpfr_coth(res, f, GMP_RNDN);

#define bigfr_acosh(res, f)	     mpfr_acosh(res, f, GMP_RNDN);
#define bigfr_asinh(res, f)	     mpfr_asinh(res, f, GMP_RNDN);
#define bigfr_atanh(res, f)	     mpfr_atanh(res, f, GMP_RNDN);

extern Lisp_Object read_bigfr_string(char*);
extern Lisp_Object make_bigfr(fpfloat, unsigned long);
#if defined HAVE_MPF && defined WITH_GMP
extern Lisp_Object make_bigfr_bf(bigf);
#endif
extern Lisp_Object make_bigfr_bfr(bigfr);
extern Lisp_Object make_indef_bfr(bigfr);
extern_inline Lisp_Object ent_mpfr_wipe_indef(bigfr);

extern_inline Lisp_Object
ent_mpfr_wipe_indef(bigfr n)
{
	if (!bigfr_nan_p(n) && !bigfr_inf_p(n))
		return make_bigfr_bfr(n);
	else if (bigfr_nan_p(n))
		return make_indef(NOT_A_NUMBER);
	else if (bigfr_inf_p(n)) {
		if (bigfr_sign(n) > 0)
			return make_indef(POS_INFINITY);
		else
			return make_indef(NEG_INFINITY);
	}
	return ent_nullop_zero(BIGFR_T);
}

extern void init_optables_BIGFR_T(void);
extern void init_ent_mpfr(void);
extern void vars_of_ent_mpfr(void);
extern void syms_of_ent_mpfr(void);

#endif /* INCLUDED_number_mpfr_h_ */
