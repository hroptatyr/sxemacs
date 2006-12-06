/*
  ent-mp.h -- Numeric types for SXEmacs -- The BSD MP library
  Copyright (C) 2004 Jerry James
  Copyright (C) 2006 Sebastian Freundt

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

#ifndef INCLUDED_number_mp_h_
#define INCLUDED_number_mp_h_

/* BSD MP libraries without MP_PREFIX define a function named pow in mp.h that
   has a different prototype from the one in math.h.  We don't use that
   function anyway, so we do this for safety purposes.  However, this means
   that number-mp.h must always be included before math.h. */
#define pow mp_pow
#include <mp.h>
#undef pow

#ifdef MP_PREFIX
#define MP_GCD   mp_gcd
#define MP_ITOM  mp_itom
#define MP_MADD  mp_madd
#define MP_MCMP  mp_mcmp
#define MP_MDIV  mp_mdiv
#define MP_MFREE mp_mfree
#define MP_MSUB  mp_msub
#define MP_MULT  mp_mult
#define MP_SDIV  mp_sdiv
#ifdef HAVE_MP_MOVE
#define MP_MOVE(x,y) mp_move (x, y)
#else
#define MP_MOVE(x,y) mp_madd (x, bignum_zero, y)
#endif
#else
#define MP_GCD   gcd
#define MP_ITOM  itom
#define MP_MADD  madd
#define MP_MCMP  mcmp
#define MP_MDIV  mdiv
#define MP_MFREE mfree
#define MP_MSUB  msub
#define MP_MULT  mult
#define MP_SDIV  sdiv
#ifdef HAVE_MP_MOVE
#define MP_MOVE(x,y) move (x, y)
#else
#define MP_MOVE(x,y) madd (x, bignum_zero, y)
#endif
#endif

typedef MINT *bigz;
extern bigz ent_scratch_bigz;


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

#define BIGZ_ARITH_RETURN(b,op) do				\
{								\
	Lisp_Object retval = make_bigz(0);			\
	bigz_##op(XBIGZ_DATA(retval), XBIGZ_DATA(b));		\
	return Fcanonicalize_number(retval);			\
} while (0)

#define BIGZ_ARITH_RETURN1(b,op,arg) do				\
{								\
	Lisp_Object retval = make_bigz(0);			\
	bigz_##op(XBIGZ_DATA(retval), XBIGZ_DATA(b), arg);	\
	return Fcanonicalize_number(retval);			\
} while (0)

extern Lisp_Object make_bigz(long);
extern Lisp_Object make_bigz_bz(bigz);


/********************************* Bignums **********************************/

#undef HAVE_MPZ
#define HAVE_MPZ 1

extern MINT *bigz_zero, *intern_bigz;
extern MINT *bigz_min_int, *bigz_max_int, *bigz_max_uint;
extern MINT *bigz_min_long, *bigz_max_long, *bigz_max_ulong;
extern short div_rem;

/***** Bignum: basic functions *****/
#define bigz_init(b)              (b = MP_ITOM(0))
#define bigz_fini(b)              MP_MFREE(b)
#define bigz_hashcode(b)          bigz_to_uint (b)
#define bigz_sign(b)              MP_MCMP (b, bigz_zero)
#define bigz_evenp(b)             (MP_SDIV (b, 2, intern_bigz, &div_rem), \
				   div_rem == 0)
#define bigz_oddp(b)              (MP_SDIV (b, 2, intern_bigz, &div_rem), \
				   div_rem != 0)

/***** Bignum: size *****/
#define bigz_fits_int_p(b)        (MP_MCMP (b, bigz_min_int) >= 0 && \
				   MP_MCMP (b, bigz_max_int) <= 0)
#define bigz_fits_uint_p(b)       (MP_MCMP (b, bigz_zero) >= 0 &&	\
				   MP_MCMP (b, bigz_max_uint) <= 0)
#define bigz_fits_long_p(b)       (MP_MCMP (b, bigz_min_long) >= 0 && \
				   MP_MCMP (b, bigz_max_long) <= 0)
#define bigz_fits_ulong_p(b)      (MP_MCMP (b, bigz_zero) >= 0 &&	\
				   MP_MCMP (b, bigz_max_ulong) <= 0)

/***** Bignum: conversions *****/
extern char *bigz_to_string(bigz, int);
extern int bigz_to_int(bigz);
extern unsigned int bigz_to_uint(bigz);
extern long bigz_to_long(bigz);
extern unsigned long bigz_to_ulong(bigz);
extern fpfloat bigz_to_fpfloat(bigz);

/***** Bignum: converting assignments *****/
#define bigz_set(b1, b2)          MP_MOVE (b2, b1)
extern int bigz_set_string(bigz, const char *, int);
extern void bigz_set_long(bigz, long);
extern void bigz_set_ulong(bigz, unsigned long);
extern void bigz_set_fpfloat(bigz, fpfloat);

/***** Bignum: comparisons *****/
#define bigz_cmp(b1,b2)           MP_MCMP (b1, b2)
#define bigz_lt(b1,b2)            (MP_MCMP (b1, b2) < 0)
#define bigz_le(b1,b2)            (MP_MCMP (b1, b2) <= 0)
#define bigz_eql(b1,b2)           (MP_MCMP (b1, b2) == 0)
#define bigz_ge(b1,b2)            (MP_MCMP (b1, b2) >= 0)
#define bigz_gt(b1,b2)            (MP_MCMP (b1, b2) > 0)

/***** Bignum: arithmetic *****/
#define bigz_neg(b,b2)            MP_MSUB (bigz_zero, b2, b)
#define bigz_abs(b,b2)            (MP_MCMP (b2, bigz_zero) < 0	\
				     ? MP_MSUB (bigz_zero, b2, b)	\
				     : MP_MADD (bigz_zero, b2, b))
#define bigz_add(b,b1,b2)         MP_MADD (b1, b2, b)
#define bigz_sub(b,b1,b2)         MP_MSUB (b1, b2, b)
#define bigz_mul(b,b1,b2)         MP_MULT (b1, b2, b)
extern int bigz_divisible_p(bigz, bigz);
#define bigz_div(b,b1,b2)         MP_MDIV (b1, b2, b, intern_bigz)
extern void bigz_ceil(bigz, bigz, bigz);
extern void bigz_floor(bigz, bigz, bigz);
#define bigz_mod(b,b1,b2)         MP_MDIV (b1, b2, intern_bigz, b)
extern void bigz_pow(bigz, bigz, unsigned long);
#define bigz_gcd(res,b1,b2)       MP_GCD (b1, b2, res)
extern void bigz_lcm(bigz, bigz, bigz);

/***** Bignum: bit manipulations *****/
extern void bigz_and(bigz, bigz, bigz);
extern void bigz_ior(bigz, bigz, bigz);
extern void bigz_xor(bigz, bigz, bigz);
extern void bigz_not(bigz, bigz);
extern void bigz_setbit(bigz, unsigned long);
extern void bigz_clrbit(bigz, unsigned long);
extern int bigz_testbit(bigz, unsigned long);
extern void bigz_lshift(bigz, bigz, unsigned long);
extern void bigz_rshift(bigz, bigz, unsigned long);

/***** Bignum: random numbers *****/
extern void bigz_random_seed(unsigned long);
extern void bigz_random(bigz, bigz);


extern void init_optables_BIGZ_T(void);
extern void init_ent_mpz(void);
extern void syms_of_ent_mpz(void);
extern void vars_of_ent_mpz(void);

#endif /* INCLUDED_number_mp_h_ */
