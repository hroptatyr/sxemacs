/*
  ent.h -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005, 2006, 2007 Sebastian Freundt

  XEmacs Author:  Jerry James
  Author: Sebastian Freundt
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


#ifndef INCLUDED_ent_h_
#define INCLUDED_ent_h_

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

   float     = ordinary C double or long double
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

#include "ent-optable.h"
#include "ent-nullary-op.h"
#include "ent-unary-op.h"
#include "ent-binary-op.h"
#include "ent-unary-rel.h"
#include "ent-binary-rel.h"
#include "ent-lift.h"

/* ordinary (small) integers */
#include "ent-int.h"
/* ordinary floats */
#ifdef HAVE_FPFLOAT
#include "ent-float.h"
#endif

#include "ent-indef.h"

/* Load the library definitions */
#if defined HAVE_GMP && defined WITH_GMP
# include "ent-gmp.h"
#elif defined HAVE_BSDMP && defined WITH_MP
# include "ent-mp.h"
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
# include "ent-mpfr.h"
#endif
#if defined HAVE_MPC && defined WITH_MPC
# include "ent-mpc.h"
#elif defined HAVE_PSEUC && defined WITH_PSEUC 
# include "ent-pseumpc.h"
#endif
#if defined HAVE_ECM && defined WITH_ECM
# include "ent-ecm.h"
#endif

/* now maybe include those pseudo implementations */
#if defined HAVE_PSEUG && defined WITH_PSEUG
# include "ent-gaussian.h"
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
# include "ent-quatern.h"
#endif


/* debugging stuff */
#ifdef ALL_DEBUG_FLAGS
#undef ENT_DEBUG_FLAG
#define ENT_DEBUG_FLAG
#endif

#define __ENT_DEBUG__(args...)		fprintf(stderr, "ENT " args)
#ifndef ENT_DEBUG_FLAG
#define ENT_DEBUG(args...)
#else
#define ENT_DEBUG(args...)		__ENT_DEBUG__(args)
#endif
#define ENT_DEBUG_OP(args...)		ENT_DEBUG("[operation]: " args)
#define ENT_CRITICAL(args...)		__ENT_DEBUG__("CRITICAL: " args)


/******************************** Errors ************************************/
extern Lisp_Object Qoperation_error, Qrelation_error, Qvaluation_error;


/************************* Big Rational Integers ****************************/
extern Lisp_Object Qbigzp;
EXFUN(Fbigzp, 1);

#if !defined HAVE_MPZ || !(defined WITH_GMP || defined WITH_MP)
#define BIGZP(x)		(0 && x)
#define CHECK_BIGZ(x)		dead_wrong_type_argument(Qbigzp, x)
#define CONCHECK_BIGZ(x)	dead_wrong_type_argument(Qbigzp, x)
typedef void bigz;
#endif	/* !HAVE_MPZ */


/********************************* Integers *********************************/
extern Lisp_Object Qintegerp;

EXFUN(Fintegerp, 1);
EXFUN(Fevenp, 1);
EXFUN(Foddp, 1);

#define INTEGERP(x) (INTP(x) || BIGZP(x))
#define CHECK_INTEGER(x)					\
	do {							\
		if (!INTEGERP(x))				\
			dead_wrong_type_argument(Qintegerp, x);	\
	} while (0)
#define CONCHECK_INTEGER(x)					\
	do {							\
		if (!INTEGERP (x))				\
			x = wrong_type_argument (Qintegerp, x);	\
	}  while (0)


/************************** Rational Integer Fractions **********************/
extern Lisp_Object Qbigqp;
EXFUN(Fbigqp, 1);

#if !defined HAVE_MPQ || !defined WITH_GMP
#define BIGQP(x)		(0 && x)
#define CHECK_BIGQ(x)		dead_wrong_type_argument(Qbigqp, x)
#define CONCHECK_BIGQ(x)	dead_wrong_type_argument(Qbigqp, x)
typedef void bigq;
#endif /* !HAVE_MPQ */


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
#if !defined HAVE_MPF || !defined WITH_GMP
#define BIGFP(x)		(0 && x)
#define CHECK_BIGF(x)		dead_wrong_type_argument(Qbigfp, x)
#define CONCHECK_BIGF(x)	dead_wrong_type_argument(Qbigfp, x)
typedef void bigf;
#endif /* !HAVE_MPF */

extern Lisp_Object Qbigfp;
EXFUN(Fbigfp, 1);


/******************************** Bigfrs ***********************************/
extern Lisp_Object Qbigfrp;
EXFUN(Fbigfrp, 1);

#if !defined HAVE_MPFR || !defined WITH_MPFR
#define BIGFRP(x)		(0 && x)
#define CHECK_BIGFR(x)		dead_wrong_type_argument(Qbigfrp, x)
#define CONCHECK_BIGFR(x)	dead_wrong_type_argument(Qbigfrp, x)
typedef void bigfr;
#endif	/* !HAVE_MPFR */


/******************************* Floats *************************************/
extern Lisp_Object Qfloatp;
EXFUN(Ffloatp, 1);

#if !defined(HAVE_FPFLOAT)
#define FLOATP(x)		(0 && x)
#define CHECK_FLOAT(x)		dead_wrong_type_argument(Qfloatp, x)
#define CONCHECK_FLOAT(x)	dead_wrong_type_argument(Qfloatp, x)
typedef void fpfloat;
#endif	/* !HAVE_FPFLOAT */

#define INT_OR_FLOATP(x)	(INTP(x) || FLOATP(x))
#define CHECK_INT_OR_FLOAT(x)					\
	do {							\
		if (!INT_OR_FLOATP (x))				\
			dead_wrong_type_argument(Qnumberp, x);	\
	} while (0)

#define CONCHECK_INT_OR_FLOAT(x)				\
	do {							\
		if (!INT_OR_FLOATP (x))				\
			x = wrong_type_argument(Qnumberp, x);	\
	} while (0)


/*********************************** Reals **********************************/
extern Lisp_Object Qrealp;

#ifdef HAVE_FPFLOAT
#define REALP(x) (FLOATP(x) || BIGFP(x) || BIGFRP(x))
#else
#define REALP(x) (BIGFP(x) || BIGFRP(x))
#endif
#define CHECK_REAL(x)						\
	do {							\
		if (!REALP(x))					\
			dead_wrong_type_argument(Qrealp, x);	\
	} while (0)
#define CONCHECK_REAL(x)					\
	do {							\
		if (!REALP(x))					\
			x = wrong_type_argument(Qrealp, x);	\
	}  while (0)

extern Lisp_Object Vread_real_as;
extern Fixnum max_real_precision;
extern Fixnum default_real_precision;


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
extern Lisp_Object Qbiggp;
EXFUN(Fbiggp, 1);

#if !defined HAVE_PSEUG || !defined WITH_PSEUG
#define BIGGP(x)		(0 && x)
#define CHECK_BIGG(x)		dead_wrong_type_argument(Qbiggp, x)
#define CONCHECK_BIGG(x)	dead_wrong_type_argument(Qbiggp, x)
typedef void bigg;
#endif /* HAVE_PSEUG */


/***************************** Bigcs ****************************************/
extern Lisp_Object Qbigcp;
EXFUN(Fbigcp, 1);

#if !(defined HAVE_MPC && defined WITH_MPC ||		\
      defined HAVE_PSEUC && defined WITH_PSEUC)
#define BIGCP(x)		(0 && x)
#define CHECK_BIGC(x)		dead_wrong_type_argument(Qbigcp, x)
#define CONCHECK_BIGC(x)	dead_wrong_type_argument(Qbigcp, x)
typedef void bigc;
#endif


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
extern Lisp_Object Qquaternp;
EXFUN(Fquaternp, 1);

#if !defined HAVE_QUATERN || !defined WITH_QUATERN
#define QUATERNP(x)		(0 && x)
#define CHECK_QUATERN(x)	dead_wrong_type_argument(Qquaternp, x)
#define CONCHECK_QUATERN(x)	dead_wrong_type_argument(Qquaternp, x)
typedef void quatern;
#endif /* HAVE_QUATERN */


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

#define NONARCHIMEDEANP(x) (x != x)  /* RESC_ELMP(x) || PADICP(x) */
#define CHECK_NONARCHIMEDEAN(x)						\
	do {								\
		if (!NONARCHIMEDEANP (x))				\
			dead_wrong_type_argument (Qnonarchimedeanp, x);	\
	} while (0)
#define CONCHECK_NONARCHIMEDEAN(x)					\
	do {								\
		if (!NONARCHIMEDEANP (x))				\
			x = wrong_type_argument (Qnonarchimedeanp, x);	\
	}  while (0)


/****************************** Indefinities ********************************/
extern Lisp_Object Qindefinitep;
extern Lisp_Object Qinfinityp;
EXFUN(Findefinitep, 1);
EXFUN(Finfinityp, 1);


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

EXFUN(Fcanonicalize_number, 1);
EXFUN(Fcoerce_number, 3);

extern unsigned long ent_normalise_precision(unsigned long);
extern unsigned long internal_get_precision(Lisp_Object);

/* parser hook for resclass objects */
extern int(*ase_resc_rng_pred_f)(const char *cp);
extern Lisp_Object(*ase_resc_rng_f)(char *cp);
extern int(*ase_resc_elm_pred_f)(const char *cp);
extern Lisp_Object(*ase_resc_elm_f)(char *cp);
/* parser hook for perms */
extern Lisp_Object(*ase_permutation_f)(Lisp_Object);


/**************************** Auxiliary Categories **************************/
EXFUN(Fzerop, 1);
EXFUN(Fonep, 1);
EXFUN(Fzero, 1);
EXFUN(Fone, 1);


/**************************** Categorical Function Table ********************/

/* tentative stuff */
extern dllist_t ase_empty_sets;

/* these tables hold functions according to their input signature and are
   grouped by their operation
*/

extern void syms_of_ent(void);
extern void vars_of_ent(void);
extern void init_ent(void);

#endif /* INCLUDED_ent_h_ */
