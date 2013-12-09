/*
  ent-mpc.h -- Numeric types for SXEmacs
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


#ifndef INCLUDED_number_mpc_h_
#define INCLUDED_number_mpc_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#ifdef HAVE_MPFR_H
# include "mpfr.h"
#endif
#include "mpc.h"


typedef mpc_t bigc;


struct Lisp_Bigc
{
	struct lrecord_header lheader;
	bigc data;
};
typedef struct Lisp_Bigc Lisp_Bigc;

DECLARE_LRECORD(bigc, Lisp_Bigc);
#define XBIGC(x) XRECORD(x, bigc, Lisp_Bigc)
#define wrap_bigc(p) wrap_object(p)
#define BIGCP(x) RECORDP(x, bigc)
#define CHECK_BIGC(x) CHECK_RECORD(x, bigc)
#define CONCHECK_BIGC(x) CONCHECK_RECORD(x, bigc)

#define bigc_data(f) ((f)->data)
#define XBIGC_DATA(x) bigc_data(XBIGC(x))
#define XBIGC_GET_PREC(x) bigc_get_prec(XBIGC_DATA(x))
#define XBIGC_SET_PREC(x,p) bigc_set_prec(XBIGC_DATA(x), p)

#define XBIGC_RE(x) bigc_re(XBIGC_DATA(x))
#define XBIGC_IM(x) bigc_im(XBIGC_DATA(x))

#define BIGC_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_bigc_bc(XBIGC_DATA(f));		\
	bigc_##op(XBIGC_DATA(retval), XBIGC_DATA(f));			\
	return retval;							\
} while (0)

#define BIGC_ARITH_RETURN1(f,op,arg) do					\
{									\
	Lisp_Object retval = make_bigc_bc(XBIGC_DATA(f));		\
	bigc_##op(XBIGC_DATA(retval), XBIGC_DATA(f), arg);		\
	return retval;							\
} while (0)

#define BIGC_INIT_PREC(f, prec) do					\
{									\
	bigc_init_prec(f, internal_get_precision(prec));		\
} while (0)

extern Lisp_Object make_bigc(fpfloat, fpfloat, unsigned long);
extern Lisp_Object make_bigc_bfr(bigfr, bigfr, unsigned long);
extern Lisp_Object make_bigc_bc(bigc);

extern bigc ent_scratch_bigc;


/********************************* Bigcs ********************************/


/***** Bigc: basic functions *****/
#if defined(HAVE_MPC_INIT) && HAVE_MPC_INIT
#define bigc_init(f)                mpc_init(f)
#else
#define bigc_init(f)                mpc_init2((f),internal_get_precision(Qnil))
#endif
#define bigc_init_prec(f,prec)      mpc_init2((f), (prec))
#define bigc_init_2prec(f,p1,p2)    mpc_init3((f), (p1), (p2))
#define bigc_fini(f)                mpc_clear(f)
#define bigc_hashcode(f)            (bigfr_hashcode(bigc_re(f)) ^ \
				     bigfr_hashcode(bigc_im(f)))
#define bigc_get_prec(f)            max(bigfr_get_prec(bigc_re(f)), \
					bigfr_get_prec(bigc_im(f)))
#define bigc_set_prec(f, prec)      mpc_set_prec((f), (prec))
#define bigc_set_default_prec(prec) mpc_set_default_prec((prec))
#define bigc_get_default_prec()     mpc_get_default_prec()

/***** Bigc: conversions *****/
extern Bufbyte *bigc_to_string(bigc, int);

/***** Bigc: converting assignments *****/
#if ! defined(HAVE_MPC_SET_UI_FR) || ! HAVE_MPC_SET_UI_FR
#if defined(MPC_SET_X_Y)
int mpc_set_ui_fr (mpc_t rop, unsigned long int re, mpfr_t im, mpc_rnd_t rnd);
#else
#error Cannot derived mpc_set_ui_fr without MPC_SET_X_Y!
#endif
#endif

#define bigc_set(f1, f2)		mpc_set(f1, f2, GMP_RNDN)
#define bigc_set_long(f, l)		mpc_set_si(f, l, GMP_RNDN)
#define bigc_set_ulong(f, l)		mpc_set_ui(f, l, GMP_RNDN)
#if fpfloat_double_p
#define bigc_set_fpfloat(f, d)		mpc_set_d(f, d, GMP_RNDN)
#define bigc_set_fpfloat_fpfloat(f, d1, d2)	\
	mpc_set_d_d(f, d1, d2, GMP_RNDN)
#elif fpfloat_long_double_p
#define bigc_set_fpfloat(f, d)		mpc_set_d(f, (double)d, GMP_RNDN)
#define bigc_set_fpfloat_fpfloat(f, d1, d2)	\
	mpc_set_d_d(f, (double)d1, (double)d2, GMP_RNDN)
#endif
#define bigc_set_ulong_ulong(f,u1,u2)   mpc_set_ui_ui(f, u1, u2, GMP_RNDN)
#define bigc_set_long_long(f, l1, l2)   mpc_set_si_si(f, l1, l2, GMP_RNDN)
#define bigc_set_bigfr(c, bf) do					\
{									\
	mpc_set_ui_ui(c, 0UL, 0UL, GMP_RNDN);				\
	mpc_add_fr(c, c, bf, GMP_RNDN);					\
} while (0)
#define bigc_set_bigfr_bigfr(c, bf1, bf2) do				\
{									\
	mpc_set_ui_fr(c, 0UL, bf2, GMP_RNDN);				\
	mpc_add_fr(c, c, bf1, GMP_RNDN);				\
} while (0)

#if defined mpc_realref
#define bigc_re(z)                  mpc_realref(z)
#define bigc_im(z)                  mpc_imagref(z)
#else
#define bigc_re(z)                  MPC_RE(z)
#define bigc_im(z)                  MPC_IM(z)
#endif

/***** Bigc: comparisons *****/
#define bigc_cmp(f1,f2)             mpc_cmp(f1, f2)
#define bigc_eq(f1,f2)							\
	(bigfr_eq(bigc_re(f1), bigc_re(f2)) &&				\
	 bigfr_eq(bigc_im(f1), bigc_im(f2)))

/***** Bigc: arithmetic *****/
#define bigc_neg(f,f2)              mpc_neg(f, f2, GMP_RNDN)
#define bigc_abs(f,f2)              mpc_abs(f, f2, GMP_RNDN)
#define bigc_norm(fr,c)             mpc_norm(fr, c, GMP_RNDN)
#define bigc_conj(c,c2)             mpc_conj(c, c2, GMP_RNDN)
#define bigc_add(res,f1,f2)         mpc_add(res, f1, f2, GMP_RNDN)
#define bigc_sub(res,f1,f2)         mpc_sub(res, f1, f2, GMP_RNDN)
#define bigc_mul(res,f1,f2)         mpc_mul(res, f1, f2, GMP_RNDN)
#define bigc_div(res,f1,f2)         mpc_div(res, f1, f2, GMP_RNDN)

#define bigc_sqrt(res,f)            mpc_sqrt(res, f, GMP_RNDN)

extern void bigc_pow(bigc, bigc, unsigned long);

/* Advanced functions */
#define bigc_exp(res, f)            mpc_exp(res, f, GMP_RNDN);

/* indefinite handling */
extern int bigc_nan_p(bigc);
extern int bigc_inf_p(bigc);

extern Lisp_Object read_bigc_string(char*);

extern void init_optables_BIGC_T(void);
extern void init_ent_mpc(void);
extern void syms_of_ent_mpc(void);
extern void vars_of_ent_mpc(void);

#endif /* INCLUDED_number_mpc_h_ */
