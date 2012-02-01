/*
  ent-pseumpc.h -- Numeric types for SXEmacs
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


#ifndef INCLUDED_number_pseumpc_h_
#define INCLUDED_number_pseumpc_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#ifdef HAVE_MPFR_H
# include "mpfr.h"
#endif

struct bigc
{
	bigfr real;
	bigfr imag;
};
typedef struct bigc bigc[1];


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


/******************************** Bigcs ********************************/

/***** Bigc: basic functions *****/
extern void bigc_init(bigc);
extern void bigc_init_prec(bigc, unsigned long);
extern void bigc_init_2prec(bigc, unsigned long, unsigned long);
extern void bigc_fini(bigc);
#define bigc_hashcode(f)		(bigfr_hashcode(bigc_re(f)) ^ \
					 bigfr_hashcode(bigc_im(f)))
#define bigc_get_prec(f)		max(bigfr_get_prec(bigc_re(f)), \
					    bigfr_get_prec(bigc_im(f)))
extern void bigc_set_prec(bigc, unsigned long);
#define bigc_set_default_prec(prec)
#define bigc_get_default_prec()		bigfr_get_default_prec()

/***** Bigc: conversions *****/
extern Bufbyte *bigc_to_string(bigc, int);

/***** Bigc: converting assignments *****/
extern void bigc_set(bigc, bigc);
extern void bigc_set_long(bigc, long);
extern void bigc_set_long_long(bigc, long, long);
extern void bigc_set_ulong(bigc, unsigned long);
extern void bigc_set_ulong_ulong(bigc, unsigned long, unsigned long);
extern void bigc_set_fpfloat(bigc, fpfloat);
extern void bigc_set_fpfloat_fpfloat(bigc, fpfloat, fpfloat);
extern void bigc_set_bigfr(bigc, bigfr);
extern void bigc_set_bigfr_bigfr(bigc, bigfr, bigfr);

#define bigc_re(z)			((z)->real)
#define bigc_im(z)			((z)->imag)

/***** Bigc: comparisons *****/
extern int bigc_eq(bigc, bigc);
#define bigc_cmp(f1,f2)             (bigc_eq(f1,f2) ? 0 : 1)

/***** Bigc: arithmetic *****/
extern void bigc_neg(bigc, bigc);
extern void bigc_abs(bigfr, bigc);
extern void bigc_norm(bigfr, bigc);
extern void bigc_conj(bigc, bigc);
extern void bigc_add(bigc, bigc, bigc);
extern void bigc_sub(bigc, bigc, bigc);
extern void bigc_mul(bigc, bigc, bigc);
extern void bigc_div(bigc, bigc, bigc);
#if defined HAVE_MPZ && defined WITH_GMP
extern void bigc_pow(bigc, bigc, unsigned long);
#endif

extern void bigc_sqrt(bigc, bigc);

/* Advanced functions */
#define bigc_exp(res, f)

/* indefinite handling */
extern int bigc_nan_p(bigc);
extern int bigc_inf_p(bigc);

extern Lisp_Object read_bigc_string(char*);

extern void init_optables_BIGC_T(void);
extern void init_ent_mpc(void);
extern void syms_of_ent_mpc(void);
extern void vars_of_ent_mpc(void);

#endif /* INCLUDED_number_pseumpc_h_ */
