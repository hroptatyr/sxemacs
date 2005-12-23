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


/********************************* Bigzs **********************************/

#define HAVE_MPZ 1

extern gmp_randstate_t random_state;

/***** Bigz: basic functions *****/
#define bigz_init(b)                  mpz_init (b)
#define bigz_fini(b)                  mpz_clear (b)
#define bigz_hashcode(b)              mpz_get_ui (b)
#define bigz_sign(b)                  mpz_sgn (b)
#define bigz_evenp(b)                 mpz_even_p (b)
#define bigz_oddp(b)                  mpz_odd_p (b)

/***** Bigz: size *****/
#define bigz_fits_int_p(b)            mpz_fits_sint_p (b)
#define bigz_fits_uint_p(b)           mpz_fits_uint_p (b)
#define bigz_fits_long_p(b)           mpz_fits_slong_p (b)
#define bigz_fits_ulong_p(b)          mpz_fits_ulong_p (b)

/***** Bigz: conversions *****/
#define bigz_to_string(b,base)        mpz_get_str (NULL, base, b)
#define bigz_to_int(b)                ((int) mpz_get_si (b))
#define bigz_to_uint(b)               ((unsigned int) mpz_get_ui (b))
#define bigz_to_long(b)               mpz_get_si (b)
#define bigz_to_ulong(b)              mpz_get_ui (b)
#define bigz_to_double(b)             mpz_get_d (b)

/***** Bigz: converting assignments *****/
#define bigz_set(b1,b2)               mpz_set (b1, b2)
#define bigz_set_string(b,s,base)     mpz_set_str (b, s, base)
#define bigz_set_long(b,l)            mpz_set_si (b, l)
#define bigz_set_ulong(b,l)           mpz_set_ui (b, l)
#define bigz_set_double(b,f)          mpz_set_d (b, f)
#define bigz_set_bigq(b,r)            mpz_set_q (b, r)
#define bigz_set_bigf(b,f)            mpz_set_f (b, f)

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

extern Lisp_Object read_bigz_string(char *cp);


/********************************** Bigqs **********************************/

#define HAVE_MPQ 1

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
#define bigq_to_string(r,base)         mpq_get_str (NULL, base, r)
#define bigq_to_int(r)                 ((int) (mpq_get_d (r)))
#define bigq_to_uint(r)                ((unsigned int) (mpq_get_d (r)))
#define bigq_to_long(r)                ((long) (mpq_get_d (r)))
#define bigq_to_ulong(r)               ((unsigned long) (mpq_get_d (r)))
#define bigq_to_double(r)              mpq_get_d (r)

/***** Bigq: converting assignments *****/
#define bigq_set(r1,r2)                mpq_set (r1, r2)
#define bigq_set_string(r,s,base)      mpq_set_str (r, s, base)
#define bigq_set_long(r,l)             mpq_set_si (r, l, 1UL)
#define bigq_set_ulong(r,l)            mpq_set_ui (r, l, 1UL)
#define bigq_set_double(r,f)           mpq_set_d (r, f)
#define bigq_set_bigz(r,b)           mpq_set_z (r, b)
#define bigq_set_bigf(r,f)         mpq_set_f (r, f)
#define bigq_set_long_ulong(r,num,den)    mpq_set_si (r, num, den)
#define bigq_set_ulong_ulong(r,num,den)   mpq_set_ui (r, num, den)
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


/******************************** Bigfs *********************************/

#define HAVE_MPF 1

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
#define bigf_to_int(f)              ((int) mpf_get_si (f))
#define bigf_to_uint(f)             ((unsigned int) mpf_get_ui (f))
#define bigf_to_long(f)             mpf_get_si (f)
#define bigf_to_ulong(f)            mpf_get_ui (f)
#define bigf_to_double(f)           mpf_get_d (f)

/***** Bigf: converting assignments *****/
#define bigf_set(f1,f2)             mpf_set (f1, f2)
#define bigf_set_string(f,str,base) mpf_set_str (f, str, base)
#define bigf_set_long(f,l)          mpf_set_si (f, l)
#define bigf_set_ulong(f,l)         mpf_set_ui (f, l)
#define bigf_set_double(d,f)        mpf_set_d (d, f)
#define bigf_set_bigz(f,b)          mpf_set_z (f, b)
#define bigf_set_bigq(f,r)          mpf_set_q (f, r)

/***** Bigf: comparisons *****/
#define bigf_cmp(f1,f2)             mpf_cmp (f1, f2)
#define bigf_lt(f1,f2)              (mpf_cmp (f1, f2) < 0)
#define bigf_le(f1,f2)              (mpf_cmp (f1, f2) <= 0)
#define bigf_eql(f1,f2)             (mpf_cmp (f1, f2) == 0)
#define bigf_eql_bits(f1,f2,bits)   mpf_eq (f1, f2, bits)
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
extern void init_number_gmp(void);

#endif /* INCLUDED_number_gmp_h_ */
