/*
  ent-gmp.h -- Numeric types for SXEmacs
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


#ifndef INCLUDED_number_gmp_h_
#define INCLUDED_number_gmp_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include <gmp.h>

typedef mpz_t bigz;
typedef mpq_t bigq;
typedef mpf_t bigf;

extern bigz ent_scratch_bigz;
extern bigq ent_scratch_bigq;
extern bigf ent_scratch_bigf;


/********************************* Bigzs **********************************/
struct Lisp_Bigz {
	struct lrecord_header lheader;
	bigz data;
};
typedef struct Lisp_Bigz Lisp_Bigz;

DECLARE_LRECORD (bigz, Lisp_Bigz);
#define XBIGZ(x) XRECORD (x, bigz, Lisp_Bigz)
#define wrap_bigz(p) wrap_object (p)
#define BIGZP(x) RECORDP (x, bigz)
#define CHECK_BIGZ(x) CHECK_RECORD (x, bigz)
#define CONCHECK_BIGZ(x) CONCHECK_RECORD (x, bigz)

#define bigz_data(b) (b)->data
#define XBIGZ_DATA(x) bigz_data (XBIGZ (x))

extern Lisp_Object make_bigz(long);
extern Lisp_Object make_bigz_bz(bigz);


extern gmp_randstate_t random_state;

/***** Bigz: basic functions *****/
#define bigz_init(b)			mpz_init (b)
#define bigz_fini(b)			mpz_clear (b)
#define bigz_hashcode(b)		mpz_get_ui (b)
#define bigz_sign(b)			mpz_sgn (b)
#define bigz_evenp(b)			mpz_even_p (b)
#define bigz_oddp(b)			mpz_odd_p (b)

/***** Bigz: size *****/
#define bigz_fits_int_p(b)		mpz_fits_sint_p (b)
#define bigz_fits_uint_p(b)		mpz_fits_uint_p (b)
#define bigz_fits_long_p(b)		mpz_fits_slong_p (b)
#define bigz_fits_ulong_p(b)		mpz_fits_ulong_p (b)

/***** Bigz: conversions *****/
#define bigz_to_string(b, base)		mpz_get_str (NULL, base, b)
#define bigz_to_string2(t, b, base)	mpz_get_str(t, base, b)
#define bigz_to_int(b)			((EMACS_INT) mpz_get_si (b))
#define bigz_to_uint(b)			((EMACS_UINT) mpz_get_ui (b))
#define bigz_to_long(b)			mpz_get_si (b)
#define bigz_to_ulong(b)		mpz_get_ui (b)
#if fpfloat_double_p
#define bigz_to_fpfloat(b)		mpz_get_d(b)
#elif fpfloat_long_double_p
#define bigz_to_fpfloat(b)		(fpfloat)mpz_get_d(b)
#endif

/***** Bigz: converting assignments *****/
#define bigz_set(b1, b2)		mpz_set(b1, b2)
#define bigz_set_string(b, s, base)	mpz_set_str(b, s, base)
#define bigz_set_long(b, l)		mpz_set_si(b, l)
#define bigz_set_ulong(b, l)		mpz_set_ui(b, l)
#if fpfloat_double_p
#define bigz_set_fpfloat(b, f)		mpz_set_d(b, f)
#elif fpfloat_long_double_p
#define bigz_set_fpfloat(b, f)		mpz_set_d(b, (double)f)
#endif
#define bigz_set_bigq(b, r)		mpz_set_q(b, r)
#define bigz_set_bigf(b, f)		mpz_set_f(b, f)

/***** Bigz: comparisons *****/
#define bigz_cmp(b1,b2)               mpz_cmp (b1, b2)
#define bigz_lt(b1,b2)                (mpz_cmp (b1, b2) < 0)
#define bigz_le(b1,b2)                (mpz_cmp (b1, b2) <= 0)
#define bigz_eql(b1,b2)               (mpz_cmp (b1, b2) == 0)
#define bigz_ge(b1,b2)                (mpz_cmp (b1, b2) >= 0)
#define bigz_gt(b1,b2)                (mpz_cmp (b1, b2) > 0)

/***** Bigz: arithmetic *****/
#define bigz_neg(b,b2)			mpz_neg(b, b2)
#define bigz_abs(b,b2)			mpz_abs(b, b2)
#define bigz_add(b,b1,b2)		mpz_add(b, b1, b2)
#define bigz_sub(b,b1,b2)		mpz_sub(b, b1, b2)
#define bigz_mul(b,b1,b2)		mpz_mul(b, b1, b2)
#define bigz_divisible_p(b1,b2)		mpz_divisible_p(b1, b2)
#define bigz_div(b,b1,b2)		mpz_tdiv_q(b, b1, b2)
#define bigz_div_ui(b,b1,b2)		mpz_tdiv_q_ui(b, b1, b2)
#define bigz_ceil(b,b1,b2)		mpz_cdiv_q(b, b1, b2)
#define bigz_floor(b,b1,b2)		mpz_fdiv_q(b, b1, b2)
#define bigz_mod(b,b1,b2)		mpz_mod(b, b1, b2)
#define bigz_pow(res,b,pow)		mpz_pow_ui(res, b, pow)
#define bigz_gcd(res,b1,b2)		mpz_gcd(res, b1, b2)
#define bigz_lcm(res,b1,b2)		mpz_lcm(res, b1, b2)

/***** Bigz: bit manipulations *****/
#define bigz_and(res,b1,b2)           mpz_and (res, b1, b2)
#define bigz_ior(res,b1,b2)           mpz_ior (res, b1, b2)
#define bigz_xor(res,b1,b2)           mpz_xor (res, b1, b2)
#define bigz_not(res,b)               mpz_com (res, b)
#define bigz_setbit(b,bit)            mpz_setbit (b, bit)
#define bigz_clrbit(b,bit)            mpz_clrbit (b, bit)
#define bigz_testbit(b,bit)           mpz_tstbit (b, bit)
#define bigz_lshift(res,b,bits)       mpz_mul_2exp (res, b, bits)
#define bigz_rshift(res,b,bits)       mpz_fdiv_q_2exp (res, b, bits)

/***** Bigz: random numbers *****/
#define bigz_random_seed(seed)        gmp_randseed_ui (random_state, seed)
#define bigz_random(res,limit)        mpz_urandomm (res, random_state, limit)

extern Lisp_Object read_bigz_string(const char *cp, int base);
extern_inline Lisp_Object ent_mpz_downgrade_maybe(bigz);

extern_inline Lisp_Object
ent_mpz_downgrade_maybe(bigz n)
{
	if (!bigz_fits_int_p(n))
		return make_bigz_bz(n);
	else {
		EMACS_INT i = bigz_to_int(n);
		if (NUMBER_FITS_IN_AN_EMACS_INT(i))
			return make_int(i);
		else
			return make_bigz_bz(n);
	}
}


/************************** Rational Integer Fractions **********************/
struct Lisp_Bigq
{
	struct lrecord_header lheader;
	bigq data;
};
typedef struct Lisp_Bigq Lisp_Bigq;

DECLARE_LRECORD(bigq, Lisp_Bigq);
#define XBIGQ(x) XRECORD(x, bigq, Lisp_Bigq)
#define wrap_bigq(p) wrap_object (p)
#define BIGQP(x) RECORDP(x, bigq)
#define CHECK_BIGQ(x) CHECK_RECORD(x, bigq)
#define CONCHECK_BIGQ(x) CONCHECK_RECORD(x, bigq)

#define bigq_data(r) (r)->data

#define XBIGQ_DATA(r) bigq_data(XBIGQ(r))
#define XBIGQ_NUMERATOR(r) bigq_numerator(XBIGQ_DATA(r))
#define XBIGQ_DENOMINATOR(r) bigq_denominator(XBIGQ_DATA(r))

extern Lisp_Object make_bigq(long, unsigned long);
extern Lisp_Object make_bigq_bz(bigz, bigz);
extern Lisp_Object make_bigq_bq(bigq);


/***** Bigq: basic functions *****/
#define bigq_init(r)                   mpq_init (r)
#define bigq_fini(r)                   mpq_clear (r)
#define bigq_hashcode(r) \
  (mpz_get_ui (mpq_denref (r)) * mpz_get_ui (mpq_numref (r)))
#define bigq_sign(r)                   mpq_sgn (r)
#define bigq_numerator(r)              mpq_numref (r)
#define bigq_denominator(r)            mpq_denref (r)
#define bigq_canonicalize(r)           mpq_canonicalize (r)

/***** Bigq: conversions *****/
#define bigq_to_string(r,base)		mpq_get_str (NULL, base, r)
#define bigq_to_string2(t, b, base)	mpq_get_str(t, base, b)
#define bigq_to_int(r)                 ((EMACS_INT) (mpq_get_d (r)))
#define bigq_to_uint(r)                ((EMACS_UINT) (mpq_get_d (r)))
#define bigq_to_long(r)                ((long) (mpq_get_d (r)))
#define bigq_to_ulong(r)               ((unsigned long) (mpq_get_d (r)))
#if fpfloat_double_p
#define bigq_to_fpfloat(r)		mpq_get_d(r)
#elif fpfloat_long_double_p
#define bigq_to_fpfloat(r)		(fpfloat)mpq_get_d(r)
#endif

/***** Bigq: converting assignments *****/
#define bigq_set(r1, r2)		mpq_set(r1, r2)
#define bigq_set_string(r, s, base)	mpq_set_str(r, s, base)
#define bigq_set_long(r, l)		mpq_set_si(r, l, 1UL)
#define bigq_set_ulong(r, l)		mpq_set_ui(r, l, 1UL)
#if fpfloat_double_p
#define bigq_set_fpfloat(r, f)		mpq_set_d(r, f)
#elif fpfloat_long_double_p
#define bigq_set_fpfloat(r, f)		mpq_set_d(r, (double)f)
#endif
#define bigq_set_bigz(r, b)		mpq_set_z(r, b)
#define bigq_set_bigf(r, f)		mpq_set_f(r, f)
#define bigq_set_long_ulong(r,num,den)	mpq_set_si(r, num, den)
#define bigq_set_ulong_ulong(r,num,den)	mpq_set_ui(r, num, den)
/* FIXME: Why does this canonicalize, but the previous 2 don't? */
#define bigq_set_bigz_bigz(r,num,den) do {	\
    mpz_set (mpq_numref (r), num);		\
    mpz_set (mpq_denref (r), den);		\
    mpq_canonicalize (r);			\
  } while (0)

/***** Bigq: comparisons *****/
#define bigq_cmp(r1,r2)                mpq_cmp (r1, r2)
#define bigq_lt(r1,r2)                 (mpq_cmp (r1, r2) < 0)
#define bigq_le(r1,r2)                 (mpq_cmp (r1, r2) <= 0)
#define bigq_eql(r1,r2)                mpq_equal (r1, r2)
#define bigq_ge(r1,r2)                 (mpq_cmp (r1, r2) >= 0)
#define bigq_gt(r1,r2)                 (mpq_cmp (r1, r2) > 0)
#define bigq_ne(r1,r2)                 (!mpq_equal (r1, r2))

/***** Bigq: arithmetic *****/
#define bigq_neg(q,q2)                 mpq_neg (q, q2)
#define bigq_abs(q,q2)                 mpq_abs (q, q2)
#define bigq_inv(q,q2)                 mpq_inv (q, q2)
#define bigq_add(res,q1,q2)            mpq_add (res, q1, q2)
#define bigq_sub(res,q1,q2)            mpq_sub (res, q1, q2)
#define bigq_mul(res,q1,q2)            mpq_mul (res, q1, q2)
#define bigq_div(res,q1,q2)            mpq_div (res, q1, q2)

extern Lisp_Object read_bigq_string(char *cp);
extern_inline Lisp_Object ent_mpq_downgrade_maybe(bigq);

extern_inline Lisp_Object
ent_mpq_downgrade_maybe(bigq n)
{
	bigq_canonicalize(n);
	if (!bigz_fits_long_p(bigq_denominator(n)) ||
	    bigz_to_long(bigq_denominator(n)) != 1L) {
		return make_bigq_bq(n);
	} else {
		return ent_mpz_downgrade_maybe(bigq_numerator(n));
	}
}


/******************************** Bigfs *********************************/
struct Lisp_Bigf
{
	struct lrecord_header lheader;
	bigf data;
};
typedef struct Lisp_Bigf Lisp_Bigf;

DECLARE_LRECORD(bigf, Lisp_Bigf);
#define XBIGF(x) XRECORD(x, bigf, Lisp_Bigf)
#define wrap_bigf(p) wrap_object(p)
#define BIGFP(x) RECORDP(x, bigf)
#define CHECK_BIGF(x) CHECK_RECORD(x, bigf)
#define CONCHECK_BIGF(x) CONCHECK_RECORD(x, bigf)

#define bigf_data(f) ((f)->data)
#define XBIGF_DATA(x) bigf_data(XBIGF(x))
#define XBIGF_GET_PREC(x) bigf_get_prec(XBIGF_DATA(x))
#define XBIGF_SET_PREC(x,p) bigf_set_prec(XBIGF_DATA(x), p)

extern Lisp_Object make_bigf(fpfloat, unsigned long);
extern Lisp_Object make_bigf_bf(bigf);


/***** Bigf: basic functions *****/
#define bigf_init(f)                mpf_init (f)
#define bigf_init_prec(f,prec)      mpf_init2 (f, prec)
#define bigf_fini(f)                mpf_clear (f)
#define bigf_hashcode(f)            mpf_get_ui (f)
#define bigf_sign(f)                mpf_sgn (f)
#define bigf_get_prec(f)            mpf_get_prec (f)
#define bigf_set_prec(f, prec)      mpf_set_prec (f, prec)
#define bigf_set_default_prec(prec) mpf_set_default_prec(prec)
#define bigf_get_default_prec()     mpf_get_default_prec()

/***** Bigf: conversions *****/
extern Bufbyte *bigf_to_string (bigf f, int base);
#define bigf_to_int(f)              ((EMACS_INT) mpf_get_si (f))
#define bigf_to_uint(f)             ((EMACS_UINT) mpf_get_ui (f))
#define bigf_to_long(f)             mpf_get_si (f)
#define bigf_to_ulong(f)            mpf_get_ui (f)
#if fpfloat_double_p
#define bigf_to_fpfloat(f)		mpf_get_d(f)
#elif fpfloat_long_double_p
#define bigf_to_fpfloat(f)		(fpfloat)mpf_get_d(f)
#endif

/***** Bigf: converting assignments *****/
#define bigf_set(f1,f2)             mpf_set (f1, f2)
#define bigf_set_string(f,str,base) mpf_set_str (f, str, base)
#define bigf_set_long(f,l)          mpf_set_si (f, l)
#define bigf_set_ulong(f,l)         mpf_set_ui (f, l)
#if fpfloat_double_p
#define bigf_set_fpfloat(d, f)		mpf_set_d(d, f)
#elif fpfloat_long_double_p
#define bigf_set_fpfloat(d, f)		mpf_set_d(d, (double)f)
#endif
#define bigf_set_bigz(f, b)		mpf_set_z (f, b)
#define bigf_set_bigq(f, r)		mpf_set_q (f, r)

/***** Bigf: comparisons *****/
#define bigf_cmp(f1,f2)             mpf_cmp (f1, f2)
#define bigf_lt(f1,f2)              (mpf_cmp (f1, f2) < 0)
#define bigf_le(f1,f2)              (mpf_cmp (f1, f2) <= 0)
#define bigf_eq(f1,f2)			(mpf_cmp (f1, f2) == 0)
#define bigf_eq_bits(f1,f2,bits)	mpf_eq (f1, f2, bits)
#define bigf_ge(f1,f2)              (mpf_cmp (f1, f2) >= 0)
#define bigf_gt(f1,f2)              (mpf_cmp (f1, f2) > 0)

/***** Bigf: arithmetic *****/
#define bigf_neg(f,f2)              mpf_neg (f, f2)
#define bigf_abs(f,f2)              mpf_abs (f, f2)
#define bigf_add(res,f1,f2)         mpf_add (res, f1, f2)
#define bigf_sub(res,f1,f2)         mpf_sub (res, f1, f2)
#define bigf_mul(res,f1,f2)         mpf_mul (res, f1, f2)
#define bigf_div(res,f1,f2)         mpf_div (res, f1, f2)
#define bigf_ceil(res,f)            mpf_ceil (res, f)
#define bigf_floor(res,f)           mpf_floor (res, f)
#define bigf_trunc(res,f)           mpf_trunc (res, f)
#define bigf_sqrt(res,f)            mpf_sqrt (res, f)
#define bigf_pow(res,f,exp)         mpf_pow_ui (res, f, exp)

extern Lisp_Object read_bigf_string(char *cp);

extern void init_optables_BIGZ_T(void);
extern void init_optables_BIGQ_T(void);
extern void init_optables_BIGF_T(void);
extern void init_ent_mpz(void);
extern void init_ent_mpq(void);
extern void init_ent_mpf(void);
extern void syms_of_ent_mpz(void);
extern void syms_of_ent_mpq(void);
extern void syms_of_ent_mpf(void);
extern void vars_of_ent_mpz(void);
extern void vars_of_ent_mpq(void);
extern void vars_of_ent_mpf(void);

#endif /* INCLUDED_number_gmp_h_ */
