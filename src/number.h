/*
  number.h -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005 Sebastian Freundt

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

#ifndef INCLUDED_number_h_
#define INCLUDED_number_h_

/* The following types are always defined in the same manner,
   we have some inclusions of the categories (worlds) they live in.
   These inclusions, unlike in XEmacs, are mathematically inspired,
   so here we go:

   Categories (types):
   ===========
   int       = whatever fits in the Lisp_Object type, ordinary C int
   bigz      = mpz rational integers
   integer   = int + bigz
   bigq      = mpq rational numbers

   ffelm     = finite field element

   padic     = p-adic number

   float     = ordinary C double
   bigf      = mpf big floats (= gmp reals)
   bigr      = mpfr reals

   bigc      = mpc complex number
   pseudoc   = mpfr + mpfr*i implementation of complex numbers

   gaussian  = gaussian number (mpz + mpz*i implementation)

   quatern   = quaternion number (which library does that?) - NOT YET

   octon     = octonion number (which library does that? lidia?) - NOT YET

   Category unions: 
   ================
   rational        = integer + bigq
   real            = float + bigf + bigr
                     (and everything else simulating real numbers)
   comparable      = rational + real
                     (and everything else that has a total order)
   complex         = bigc + gaussian
   algebraic       = rational + ffelm + padic + gaussian
   archimedean     = rational + real + complex
                     (and everything else with an archimedean valuation)
   non-archimidean = padic + ffelm

   number    = archimedean + non-archimedean + quatern + octon


   The top-level configure script should define the symbols
   HAVE_MPZ, HAVE_MPQ, HAVE_MPF, HAVE_MPFR and HAVE_MPC to indicate which 
   it provides.
   If some type is not defined by the library, this is what happens:

   - (provide 'bignum) and (provide 'bigz) if HAVE_MPZ
   - (provide 'ratio) and (provide 'bigq) if HAVE_MPQ
   - (provide 'bigfloat) and (provide 'bigf) if HAVE_MPF
   - (provide 'bigfr) if HAVE_MPFR
   - (provide 'bigc) if HAVE_MPC
   - (provide 'ecm) if HAVE_ECM
*/

/* Load the library definitions */
#ifdef WITH_GMP
#include "number-gmp.h"
#endif
#ifdef WITH_MPFR
#include "number-mpfr.h"
#endif
#ifdef WITH_MPC
#include "number-mpc.h"
#endif
#ifdef WITH_ECM
#include "number-ecm.h"
#endif
#ifdef WITH_MP
#include "number-mp.h"
#endif
/* now maybe include those pseudo implementations */
#ifdef HAVE_PSEUG
#include "ent-gaussian.h"
#endif
#ifdef HAVE_PSEUC
#include "ent-pseumpc.h"
#endif
#ifdef HAVE_QUATERN
#include "ent-quatern.h"
#endif
#ifdef HAVE_RESCLASS
#include "ent-resclass.h"
#endif

#include "ent-indef.h"

/* ordinary (small) integers */
#include "ent-int.h"
/* ordinary floats */
#ifdef LISP_FLOAT_TYPE
#include "ent-float.h"
#endif


/******************************** Errors ************************************/
extern Lisp_Object Qoperation_error, Qrelation_error, Qvaluation_error;


/******************************* Operations *********************************/
enum operations {
	SUM,
	DIFF,
	PROD,
	DIV,
	QUO,
	REM,
	MOD,
	POW,
	LIF,
	LT,
	GT,
	EQ,
	NE,
	VAL,
	VALLT,
	VALGT,
	VALEQ,
	VALNE,
	NUMBER_OF_OPS
};
typedef enum operations operation;


/************************* Big Rational Integers ****************************/
#ifdef HAVE_MPZ

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

extern bigz ent_scratch_bigz;
#else  /* !HAVE_MPZ */

#define BIGZP(x)         (0 && x)
#define CHECK_BIGZ(x)    dead_wrong_type_argument (Qbigzp, x)
#define CONCHECK_BIGZ(x) dead_wrong_type_argument (Qbigzp, x)
typedef void bigz;
#define make_bigz(l)     This SXEmacs does not support bigzs
#define make_bigz_bz(b)  This SXEmacs does not support bigzs

#endif	/* HAVE_MPZ */

extern Lisp_Object Qbigzp;
EXFUN(Fbigzp, 1);


/********************************* Integers *********************************/
extern Lisp_Object Qintegerp;

#define INTEGERP(x) (INTP(x) || BIGZP(x))
#define CHECK_INTEGER(x) do {			\
 if (!INTEGERP (x))				\
   dead_wrong_type_argument (Qintegerp, x);	\
 } while (0)
#define CONCHECK_INTEGER(x) do			\
{					\
	if (!INTEGERP (x))			\
		x = wrong_type_argument (Qintegerp, x);	\
}  while (0)

#ifdef HAVE_MPZ
#define make_integer(x)							\
	(NUMBER_FITS_IN_AN_EMACS_INT (x) ? make_int(x) : make_bigz(x))
#else
#define make_integer(x) make_int(x)
#endif

extern EMACS_INT Vmost_negative_int, Vmost_positive_int;
EXFUN(Fintegerp, 1);
EXFUN(Fevenp, 1);
EXFUN(Foddp, 1);


/************************** Rational Integer Fractions **********************/
#ifdef HAVE_MPQ

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

#define BIGQ_ARITH_RETURN(r,op) do			\
{							\
	Lisp_Object retval = make_bigq (0L, 1UL);	\
	bigq_##op (XBIGQ_DATA(retval), XBIGQ_DATA(r));	\
	return Fcanonicalize_number(retval);		\
} while (0)

#define BIGQ_ARITH_RETURN1(r,op,arg) do			\
{								\
	Lisp_Object retval = make_bigq(0L, 1UL);		\
	bigq_##op (XBIGQ_DATA(retval), XBIGQ_DATA(r), arg);	\
	return Fcanonicalize_number(retval);			\
} while (0)

extern Lisp_Object make_bigq(long, unsigned long);
extern Lisp_Object make_bigq_bz(bigz, bigz);
extern Lisp_Object make_bigq_bq(bigq);

extern bigq ent_scratch_bigq;
#else /* !HAVE_MPQ */

#define BIGQP(x)          (0 && x)
#define CHECK_BIGQ(x)     dead_wrong_type_argument(Qbigqp, x)
#define CONCHECK_BIGQ(x)  dead_wrong_type_argument(Qbigqp, x)
typedef void bigq;
#define make_bigq(n,d)    This SXEmacs does not support bigqs
#define make_bigq_bz(n,d) This SXEmacs does not support bigqs

#endif /* HAVE_MPQ */

extern Lisp_Object Qbigqp;
EXFUN(Fbigqp, 1);


/********************************* Rationals ********************************/
extern Lisp_Object Qrationalp;

#define RATIONALP(x) (INTEGERP(x) || BIGQP(x))
#define CHECK_RATIONAL(x) do {			\
 if (!RATIONALP (x))				\
   dead_wrong_type_argument (Qrationalp, x);	\
 } while (0)
#define CONCHECK_RATIONAL(x) do {		\
 if (!RATIONALP (x))				\
   x = wrong_type_argument (Qrationalp, x);	\
}  while (0)

EXFUN(Frationalp, 1);
EXFUN(Fnumerator, 1);
EXFUN(Fdenominator, 1);


/******************************** Bigfs ************************************/
#ifdef HAVE_MPF
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

#define BIGF_ARITH_RETURN(f,op) do				\
{								\
	Lisp_Object retval = make_bigf_bf(XBIGF_DATA(f));	\
	bigf_##op(XBIGF_DATA(retval), XBIGF_DATA(f));		\
	return retval;						\
} while (0)

#define BIGF_ARITH_RETURN1(f,op,arg) do				\
{								\
	Lisp_Object retval = make_bigf_bf(XBIGF_DATA(f));	\
	bigf_##op(XBIGF_DATA(retval), XBIGF_DATA(f), arg);	\
	return retval;						\
} while (0)

extern Lisp_Object make_bigf(double, unsigned long);
extern Lisp_Object make_bigf_bf(bigf);

extern bigf ent_scratch_bigf;
#else /* !HAVE_MPF */

#define BIGFP(x)         (0 && x)
#define CHECK_BIGF(x)    dead_wrong_type_argument (Qbigfp, x)
#define CONCHECK_BIGF(x) dead_wrong_type_argument (Qbigfp, x)
typedef void bigf;
#define make_bigf(f)     This SXEmacs does not support bigfloats
#define make_bigf_bf(f)  This SXEmacs does not support bigfloats

#endif /* HAVE_MPF */

extern Lisp_Object Qbigfp;
EXFUN(Fbigfp, 1);


/******************************** Bigfrs ***********************************/
#ifdef HAVE_MPFR
struct Lisp_Bigfr
{
	struct lrecord_header lheader;
	bigfr data;
};
typedef struct Lisp_Bigfr Lisp_Bigfr;

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

extern Lisp_Object make_bigfr(double, unsigned long);
extern Lisp_Object make_bigfr_bf(bigf);
extern Lisp_Object make_bigfr_bfr(bigfr);
extern Lisp_Object make_indef_bfr(bigfr);

extern bigfr ent_scratch_bigfr;
#else /* !HAVE_MPFR */

#define BIGFRP(x)         (0 && x)
#define CHECK_BIGFR(x)    dead_wrong_type_argument (Qbigfrp, x)
#define CONCHECK_BIGFR(x) dead_wrong_type_argument (Qbigfrp, x)
typedef void bigfr;
#define make_bigfr(f)     This SXEmacs does not support bigfloats
#define make_bigfr_bf(f)  This SXEmacs does not support bigfloats
#define make_bigfr_bfr(f) This SXEmacs does not support bigfloats

#endif /* HAVE_MPFR */

extern Lisp_Object Qbigfrp;
EXFUN(Fbigfrp, 1);


/*********************************** Reals **********************************/
extern Lisp_Object Qrealp;

#ifdef LISP_FLOAT_TYPE
#define REALP(x) (FLOATP(x) || BIGFP(x) || BIGFRP(x))
#else
#define REALP(x) (BIGFP(x) || BIGFRP(x))
#endif
#define CHECK_REAL(x) do {			\
 if (!REALP (x))				\
   dead_wrong_type_argument (Qrealp, x);	\
 } while (0)
#define CONCHECK_REAL(x) do {		\
 if (!REALP (x))				\
   x = wrong_type_argument (Qrealp, x);	\
}  while (0)

extern Lisp_Object Vread_real_as;
extern Lisp_Object Vmax_real_precision;
extern Lisp_Object Vdefault_real_precision;


/****************************** Comparables *********************************/
extern Lisp_Object Qcomparablep;

#define COMPARABLEP(x) (RATIONALP(x) || REALP(x) || COMPARABLE_INDEF_P(x))
#define CHECK_COMPARABLE(x) do {			\
 if (!COMPARABLEP(x))				\
   dead_wrong_type_argument(Qcomparablep, x);	\
 } while (0)
#define CONCHECK_COMPARABLE(x) do {		\
 if (!COMPARABLEP(x))				\
   x = wrong_type_argument(Qcomparablep, x);	\
}  while (0)


/********************************* Biggs ************************************/
#if defined(HAVE_PSEUG) && defined(HAVE_MPZ)
struct Lisp_Bigg
{
	struct lrecord_header lheader;
	bigg data;
};
typedef struct Lisp_Bigg Lisp_Bigg;

DECLARE_LRECORD(bigg, Lisp_Bigg);
#define XBIGG(x) XRECORD(x, bigg, Lisp_Bigg)
#define wrap_bigg(p) wrap_object(p)
#define BIGGP(x) RECORDP(x, bigg)
#define CHECK_BIGG(x) CHECK_RECORD(x, bigg)
#define CONCHECK_BIGG(x) CONCHECK_RECORD(x, bigg)

#define bigg_data(f) ((f)->data)
#define XBIGG_DATA(x) bigg_data(XBIGG(x))

#define XBIGG_RE(x) bigg_re(XBIGG_DATA(x))
#define XBIGG_IM(x) bigg_im(XBIGG_DATA(x))

#define BIGG_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_bigg(0L, 0L);				\
	bigg_##op(XBIGG_DATA(retval), XBIGG_DATA(f));			\
	return retval;							\
} while (0)

#define BIGG_ARITH_RETURN1(f,op,arg) do					\
{									\
	Lisp_Object retval = make_bigg(0L, 0L);				\
	bigg_##op(XBIGG_DATA(retval), XBIGG_DATA(f), arg);		\
	return retval;							\
} while (0)

extern Lisp_Object make_bigg(long, long);
extern Lisp_Object make_bigg_bz(bigz, bigz);
extern Lisp_Object make_bigg_bg(bigg);

extern bigg ent_scratch_bigg;
#else /* !HAVE_PSEUG */

#define BIGGP(x)         (0 && x)
#define CHECK_BIGG(x)    dead_wrong_type_argument(Qbiggp, x)
#define CONCHECK_BIGG(x) dead_wrong_type_argument(Qbiggp, x)
typedef void bigg;
#define make_bigg(f, g, u)	This SXEmacs does not support gaussian numbers
#define make_bigg_bz(f)		This SXEmacs does not support gaussian numbers
#define make_bigg_bg(f)		This SXEmacs does not support gaussian numbers

#endif /* HAVE_PSEUG */

extern Lisp_Object Qbiggp;
EXFUN(Fbiggp, 1);


/***************************** Bigcs ****************************************/
#ifdef HAVE_MPC
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

extern Lisp_Object make_bigc(double, double, unsigned long);
extern Lisp_Object make_bigc_bfr(bigfr, bigfr, unsigned long);
extern Lisp_Object make_bigc_bc(bigc);

extern bigc ent_scratch_bigc;
#else /* !HAVE_MPFR */

#define BIGCP(x)         (0 && x)
#define CHECK_BIGC(x)    dead_wrong_type_argument(Qbigcp, x)
#define CONCHECK_BIGC(x) dead_wrong_type_argument(Qbigcp, x)
typedef void bigc;
#define make_bigc(f, g, u)	This SXEmacs does not support complex numbers
#define make_bigc_bfr(f)	This SXEmacs does not support complex numbers
#define make_bigc_bc(f)		This SXEmacs does not support complex numbers

#endif /* HAVE_MPC */

extern Lisp_Object Qbigcp;
EXFUN(Fbigcp, 1);


/******************************* Complex Nums *******************************/
extern Lisp_Object Qcomplexp;

#define COMPLEXP(x) (BIGCP(x) || BIGGP(x) || INFINITE_POINT_P(x))
#define CHECK_COMPLEX(x) do {			\
 if (!COMPLEXP (x))				\
   dead_wrong_type_argument(Qcomplexp, x);	\
 } while (0)
#define CONCHECK_COMPLEX(x) do {		\
 if (!COMPLEXP (x))				\
   x = wrong_type_argument(Qcomplexp, x);	\
}  while (0)

EXFUN(Freal_part, 1);
EXFUN(Fimaginary_part, 1);


/********************************* Quaterns *********************************/
#if defined(HAVE_QUATERN) && defined(HAVE_MPZ)
struct Lisp_Quatern
{
	struct lrecord_header lheader;
	quatern data;
};
typedef struct Lisp_Quatern Lisp_Quatern;

DECLARE_LRECORD(quatern, Lisp_Quatern);
#define XQUATERN(x) XRECORD(x, quatern, Lisp_Quatern)
#define wrap_quatern(p) wrap_object(p)
#define QUATERNP(x) RECORDP(x, quatern)
#define CHECK_QUATERN(x) CHECK_RECORD(x, quatern)
#define CONCHECK_QUATERN(x) CONCHECK_RECORD(x, quatern)

#define quatern_data(f) ((f)->data)
#define XQUATERN_DATA(x) quatern_data(XQUATERN(x))

#define XQUATERN_Z(x) quatern_z(XQUATERN_DATA(x))
#define XQUATERN_I(x) quatern_i(XQUATERN_DATA(x))
#define XQUATERN_J(x) quatern_j(XQUATERN_DATA(x))
#define XQUATERN_K(x) quatern_k(XQUATERN_DATA(x))

#define QUATERN_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_quatern(0L, 0L);			\
	quatern_##op(XQUATERN_DATA(retval), XQUATERN_DATA(f));		\
	return retval;							\
} while (0)

#define QUATERN_ARITH_RETURN1(f,op,arg) do				\
{									\
	Lisp_Object retval = make_quatern(0L, 0L);			\
	quatern_##op(XQUATERN_DATA(retval), XQUATERN_DATA(f), arg);	\
	return retval;							\
} while (0)

extern Lisp_Object make_quatern(long, long, long, long);
extern Lisp_Object make_quatern_bz(bigz, bigz, bigz, bigz);
extern Lisp_Object make_quatern_qu(quatern);

extern quatern ent_scratch_quatern;
#else /* !HAVE_QUATERN */

#define QUATERNP(x)         (0 && x)
#define CHECK_QUATERN(x)    dead_wrong_type_argument(Qquaternp, x)
#define CONCHECK_QUATERN(x) dead_wrong_type_argument(Qquaternp, x)
typedef void quatern;
#define make_quatern(f, g, u)	This SXEmacs does not support quaternions
#define make_quatern_bz(f)	This SXEmacs does not support quaternions
#define make_quatern_bg(f)	This SXEmacs does not support quaternions

#endif /* HAVE_QUATERN */

extern Lisp_Object Qquaternp;
EXFUN(Fquaternp, 1);


/***************************** Residue Class Rings **************************/
#ifdef HAVE_RESCLASS
struct Lisp_Resc_Rng
{
	struct lrecord_header lheader;
	resc_rng data;
};
typedef struct Lisp_Resc_Rng Lisp_Resc_Rng;

struct Lisp_Resc_Elm
{
	struct lrecord_header lheader;
	resc_elm data;
	Lisp_Object ring;
};
typedef struct Lisp_Resc_Elm Lisp_Resc_Elm;

DECLARE_LRECORD(resc_rng, Lisp_Resc_Rng);
DECLARE_LRECORD(resc_elm, Lisp_Resc_Elm);
#define XRESC_RNG(x) XRECORD(x, resc_rng, Lisp_Resc_Rng)
#define XSETRES_RNG(x, p) XSETRECORD(x, p, resc_rng)
#define wrap_resc_rng(p) wrap_object(p)
#define RESC_RNGP(x) RECORDP(x, resc_rng)
#define CHECK_RESC_RNG(x) CHECK_RECORD(x, resc_rng)
#define CONCHECK_RESC_RNG(x) CONCHECK_RECORD(x, resc_rng)

#define XRESC_ELM(x) XRECORD(x, resc_elm, Lisp_Resc_Elm)
#define wrap_resc_elm(p) wrap_object(p)
#define RESC_ELMP(x) RECORDP(x, resc_elm)
#define CHECK_RESC_ELM(x) CHECK_RECORD(x, resc_elm)
#define CONCHECK_RESC_ELM(x) CONCHECK_RECORD(x, resc_elm)

#define resc_rng_data(f) ((f)->data)
#define XRESC_RNG_DATA(x) resc_rng_data(XRESC_RNG(x))

#define resc_elm_data(f) ((f)->data)
#define resc_elm_ring(f) ((f)->ring)
#define XRESC_ELM_DATA(x) resc_elm_data(XRESC_ELM(x))
#define XRESC_ELM_RING(x) resc_elm_ring(XRESC_ELM(x))

#if 0
#define RESC_ELM_ARITH_RETURN(f,op) do					\
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
#endif	/* 0 */

extern Lisp_Object make_resc_rng(unsigned long);
extern Lisp_Object make_resc_rng_bz(bigz);
extern Lisp_Object make_resc_elm(long, Lisp_Object);
extern Lisp_Object make_resc_elm_bz(bigz, Lisp_Object);

#else /* !HAVE_RESCLASS */

#define RESC_RNGP(x)         (0 && x)
#define CHECK_RESC_RNG(x)    dead_wrong_type_argument(Qresc_rngp, x)
#define CONCHECK_RESC_RNG(x) dead_wrong_type_argument(Qresc_rngp, x)
#define RESC_ELMP(x)         (0 && x)
#define CHECK_RESC_ELM(x)    dead_wrong_type_argument(Qresc_elmp, x)
#define CONCHECK_RESC_ELM(x) dead_wrong_type_argument(Qresc_elmp, x)
typedef void resc_rng;
typedef void resc_elm;
#define make_resc_rng(l)	This SXEmacs does not support residue classes
#define make_resc_rng_bz(b)	This SXEmacs does not support residue classes
#define make_resc_elm(l,lo)	This SXEmacs does not support residue classes
#define make_resc_elm_bz(b,lo)	This SXEmacs does not support residue classes

#endif /* HAVE_RESCLASS */

extern Lisp_Object Qresc_rngp;
extern Lisp_Object Qresc_elmp;
EXFUN(Fresc_rng_p, 1);
EXFUN(Fresc_elm_p, 1);
extern Lisp_Object internal_coerce_to_RESC_ELM_T(Lisp_Object, Lisp_Object);


/******************************* Archimedeans *******************************/
extern Lisp_Object Qarchimedeanp;

#define ARCHIMEDEANP(x)							\
  (RATIONALP(x) || REALP(x) || COMPARABLE_INDEF_P(x) ||			\
   COMPLEXP(x) || QUATERNP(x))
#define CHECK_ARCHIMEDEAN(x) do {					\
 if (!ARCHIMEDEANP (x))							\
   dead_wrong_type_argument (Qarchimedeanp, x);				\
 } while (0)
#define CONCHECK_ARCHIMEDEAN(x) do {					\
 if (!ARCHIMEDEANP (x))							\
   x = wrong_type_argument (Qarchimedeanp, x);				\
}  while (0)


/***************************** Non-Archimedeans ******************************/
extern Lisp_Object Qnonarchimedeanp;

#define NONARCHIMEDEANP(x) (RESC_ELMP(x))  /* PADICP(x) */
#define CHECK_NONARCHIMEDEAN(x) do {			\
 if (!NONARCHIMEDEANP (x))				\
   dead_wrong_type_argument (Qnonarchimedeanp, x);	\
 } while (0)
#define CONCHECK_NONARCHIMEDEAN(x) do {		\
 if (!NONARCHIMEDEANP (x))				\
   x = wrong_type_argument (Qnonarchimedeanp, x);	\
}  while (0)


/****************************** Indefinities ********************************/
extern Lisp_Object Qindefinitep;
extern Lisp_Object Qinfinityp;

struct Lisp_Indef
{
	struct lrecord_header lheader;
	indef data;
};
typedef struct Lisp_Indef Lisp_Indef;

DECLARE_LRECORD(indef, Lisp_Indef);
#define XINDEF(x) XRECORD(x, indef, Lisp_Indef)
#define XSETINDEF(x, p) XSETRECORD(x, p, indef)
#define wrap_indef(p) wrap_object(p)
#define INDEFP(x) RECORDP(x, indef)
#define CHECK_INDEF(x) CHECK_RECORD(x, indef)
#define CONCHECK_INDEF(x) CONCHECK_RECORD(x, indef)

#define indef_data(f) ((f)->data)
#define XINDEF_DATA(x) indef_data(XINDEF(x))

#define INFINITYP(x) 							\
(INDEFP(x) && (XINDEF_DATA(x) < END_OF_INFINITIES))
#define COMPARABLE_INDEF_P(x) 						\
(INFINITYP(x) && (XINDEF_DATA(x) < END_OF_COMPARABLE_INFINITIES))
#define INFINITE_POINT_P(x) 						\
(INFINITYP(x) && (XINDEF_DATA(x) > END_OF_COMPARABLE_INFINITIES))

extern Lisp_Object make_indef_internal(indef);
extern Lisp_Object make_indef(indef);


/********************************* Numbers **********************************/
extern Lisp_Object Qnumberp;

#define NUMBERP(x) (ARCHIMEDEANP(x) || NONARCHIMEDEANP(x))
#define CHECK_NUMBER(x) do {			\
  if (!NUMBERP (x))				\
    dead_wrong_type_argument (Qnumberp, x);	\
} while (0)
#define CONCHECK_NUMBER(x) do {			\
  if (!NUMBERP (x))				\
    x = wrong_type_argument (Qnumberp, x);	\
} while (0)

EXFUN (Fcanonicalize_number, 1);
EXFUN (Fcoerce_number, 3);

enum number_type {
	INT_T,
	BIGZ_T,
	BIGQ_T,
	FLOAT_T,
	BIGF_T,
	BIGFR_T,
	BIGG_T,
        BIGC_T,
        QUATERN_T,
        OCTON_T,
	INDEF_T,
	RESC_ELM_T,
	IMPOSSIBLE,
	NUMBER_OF_TYPES
};
typedef enum number_type number_type;

extern Lisp_Object ent_normalise_number(Lisp_Object);
extern unsigned long ent_normalise_precision(unsigned long);
extern enum number_type get_number_type(Lisp_Object);
extern enum number_type promote_args(Lisp_Object *, Lisp_Object *);
extern unsigned long internal_get_precision(Lisp_Object);


/**************************** Auxiliary Categories **************************/
EXFUN(Fzerop, 1);
EXFUN(Fonep, 1);
EXFUN(Fzero, 1);
EXFUN(Fone, 1);


/**************************** Categorical Function Table ********************/

/* these tables hold functions according to their input signature and are
   grouped by their operation
*/

#define NOT NUMBER_OF_TYPES
/* table for the plus operation (binary +) */
extern Lisp_Object (*ent_optable_sum[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the difference operation (binary -) */
extern Lisp_Object (*ent_optable_diff[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the times operation (binary *) */
extern Lisp_Object (*ent_optable_prod[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the divide operation (binary /) */
extern Lisp_Object (*ent_optable_div[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the quotient operation (constructor actually) */
extern Lisp_Object (*ent_optable_quo[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the remainder operation (constructor actually) */
extern Lisp_Object (*ent_optable_rem[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the modulo operation (constructor actually) */
extern Lisp_Object (*ent_optable_mod[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for the power operation */
extern Lisp_Object (*ent_optable_pow[NOT][NOT])(Lisp_Object, Lisp_Object);

/* table for < order */
extern Lisp_Object (*ent_optable_lt[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for > order */
extern Lisp_Object (*ent_optable_gt[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for = order */
extern Lisp_Object (*ent_optable_eq[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for /= order */
extern Lisp_Object (*ent_optable_ne[NOT][NOT])(Lisp_Object, Lisp_Object);

/* table for valuation-< order */
extern Lisp_Object (*ent_optable_vallt[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for valuation-> order */
extern Lisp_Object (*ent_optable_valgt[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for valuation-= order */
extern Lisp_Object (*ent_optable_valeq[NOT][NOT])(Lisp_Object, Lisp_Object);
/* table for valuation-/= order */
extern Lisp_Object (*ent_optable_valne[NOT][NOT])(Lisp_Object, Lisp_Object);

/* unary tables */
/* table for the negate operation (unary -) */
extern Lisp_Object (*ent_optable_neg[NOT])(Lisp_Object);
/* table for the invert operation (unary /) */
extern Lisp_Object (*ent_optable_inv[NOT])(Lisp_Object);

/* table for lifts from one domain to another (coercion) */
extern Lisp_Object (*ent_optable_lift[NOT][NOT])(Lisp_Object, unsigned long);
#undef NOT


#endif /* INCLUDED_number_h_ */
