/*
  number.c -- Numeric types for SXEmacs
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

#include <config.h>
#include <limits.h>
#include "lisp.h"

#include "number.h"

Lisp_Object Qrationalp, Qrealp, Qcomparablep;
Lisp_Object Qarchimedeanp, Qnonarchimedeanp;
Lisp_Object Qcomplexp, Qgaussianp;
Lisp_Object Qinfinityp, Qindefinitep;

/* errors */
Lisp_Object Qoperation_error, Qrelation_error, Qvaluation_error;

Lisp_Object Vread_real_as;
Lisp_Object Vdefault_real_precision;
Lisp_Object Vmax_real_precision;
EMACS_INT Vmost_negative_int, Vmost_positive_int;
static Lisp_Object Qunsupported_type;
static int number_initialized;

#define	PREC_D2B_CONST		((double)3.321928094887362)
#define	PREC_B2D_CONST		((double)0.301029995663981)

#define	PREC_D2B_UP(x)		((unsigned long)(PREC_D2B_CONST*(x))+1)
#define	PREC_D2B_DOWN(x)	((unsigned long)(PREC_D2B_CONST*(x)))
#define	PREC_B2D_UP(x)		((unsigned long)(PREC_B2D_CONST*(x))+1)
#define	PREC_B2D_DOWN(x)	((unsigned long)(PREC_B2D_CONST*(x)))

#define PREC_MIN		PREC_D2B_UP(1)



/************************* Big Rational Integers ****************************/
#ifdef HAVE_MPZ
static void
bigz_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *bstr;

	bstr = (Bufbyte*)bigz_to_string(XBIGZ_DATA(obj), 10);
	write_c_string((char *)bstr, printcharfun);
	free(bstr);
	bstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigz_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigz_eql(XBIGZ_DATA(obj1), XBIGZ_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigz_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)bigz_hashcode(XBIGZ_DATA(obj));

	/* less warnings */
	if (depth);
}

static const struct lrecord_description bigz_description[] = {
        { XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigz, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigz", bigz,
				    NULL, bigz_print, NULL,
				    bigz_equal, bigz_hash,
				    bigz_description, Lisp_Bigz);

/* reserve a scratch bigz object, which can be used by everyone here */
bigz ent_scratch_bigz;
#endif	/* HAVE_MPZ */

Lisp_Object Qbigzp;
Lisp_Object Qbignump;		/* to be compatible to XE 21.5 */

DEFUN ("bignump", Fbignump, 1, 1, 0, /*
Return t if OBJECT is a bignum, nil otherwise.
				     */
       (object))
{
	return BIGZP (object) ? Qt : Qnil;
}

DEFUN ("bigzp", Fbigzp, 1, 1, 0, /*
Return t if OBJECT is a bigz, nil otherwise.
				 */
       (object))
{
	return BIGZP (object) ? Qt : Qnil;
}


/********************************* Integers *********************************/
/*  To remember: integers are the union of all integer-like types.          */
DEFUN ("integerp", Fintegerp, 1, 1, 0, /*
Return t if OBJECT is an integer, nil otherwise.
				       */
       (object))
{
	return INTEGERP(object) ? Qt : Qnil;
}

DEFUN ("evenp", Fevenp, 1, 1, 0, /*
Return t if INTEGER is even, nil otherwise.
				 */
       (integer))
{
	if (INDEFP(integer))
		return Qnil;

	CONCHECK_INTEGER(integer);
#ifdef HAVE_MPZ
	if (BIGZP(integer))
		return bigz_evenp(XBIGZ_DATA(integer)) ? Qt : Qnil;
#endif
	if (INTP(integer))
		return (XTYPE(integer) == Lisp_Type_Int_Even) ? Qt : Qnil;

	/* big else case */
	return Qnil;
}

DEFUN ("oddp", Foddp, 1, 1, 0, /*
Return t if INTEGER is odd, nil otherwise.
			       */
       (integer))
{
	if (INDEFP(integer))
		return Qnil;

	CONCHECK_INTEGER (integer);
#ifdef HAVE_MPZ
	if (BIGZP(integer))
		return bigz_oddp(XBIGZ_DATA(integer)) ? Qt : Qnil;
#endif
	if (INTP(integer))
		return (XTYPE(integer) == Lisp_Type_Int_Odd) ? Qt : Qnil;

	/* big else case */
	return Qnil;
}


/************************** Rational Integer Fractions **********************/
/* bigq objects are derived from quotients of bigz objects.                 */
/* In XE 21.5 bigq is called ratio.                                         */
#ifdef HAVE_MPQ
static void
bigq_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *rstr;

	rstr = (Bufbyte*)bigq_to_string(XBIGQ_DATA(obj), 10);
	write_c_string((char *)rstr, printcharfun);
	free(rstr);
	rstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigq_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigq_eql(XBIGQ_DATA(obj1), XBIGQ_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigq_hash (Lisp_Object obj, int depth)
{
	return bigq_hashcode(XBIGQ_DATA(obj));

	/* less warnings */
	if (depth);
}

static const struct lrecord_description bigq_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof (Lisp_Bigq, data) },
 	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigq", bigq,
				    NULL, bigq_print, NULL,
				    bigq_equal, bigq_hash,
				    bigq_description, Lisp_Bigq);

/* scratch bigq object */
bigq ent_scratch_bigq;
#endif /* HAVE_MPQ */

Lisp_Object Qbigqp;
Lisp_Object Qratiop;		/* to be compatible to XE 21.5 */

DEFUN ("ratiop", Fratiop, 1, 1, 0, /*
Return t if OBJECT is a ratio, nil otherwise.
				   */
       (object))
{
	return BIGQP(object) ? Qt : Qnil;
}

DEFUN ("bigqp", Fbigqp, 1, 1, 0, /*
Return t if OBJECT is a bigq, nil otherwise.
				   */
       (object))
{
	return BIGQP(object) ? Qt : Qnil;
}


/********************************* Rationals ********************************/
DEFUN ("rationalp", Frationalp, 1, 1, 0, /*
Return t if OBJECT is a rational (i.e. a rational integer or a rational
quotient), nil otherwise.
				       */
       (object))
{
	return RATIONALP(object) ? Qt : Qnil;
}

DEFUN ("numerator", Fnumerator, 1, 1, 0, /*
Return the numerator of the canonical form of RATIONAL.
If RATIONAL is an integer, RATIONAL is returned.
					 */
       (rational))
{
	CONCHECK_RATIONAL(rational);
#ifdef HAVE_MPQ
	return BIGQP(rational)
		? make_bigz_bz(XBIGQ_NUMERATOR(rational))
		: rational;
#else
	return rational;
#endif
}

DEFUN ("denominator", Fdenominator, 1, 1, 0, /*
Return the denominator of the canonical form of RATIONAL.
If RATIONAL is an integer, 1 is returned.
					     */
       (rational))
{
	CONCHECK_RATIONAL(rational);
#ifdef HAVE_MPQ
	return BIGQP(rational)
		? make_bigz_bz(XBIGQ_DENOMINATOR(rational))
		: make_int(1);
#else
	return make_int(1);
#endif
}


/********************************** Bigfs ***********************************/
#ifdef HAVE_MPF
static void
bigf_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = bigf_to_string(XBIGF_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigf_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigf_eql(XBIGF_DATA(obj1), XBIGF_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigf_hash (Lisp_Object obj, int depth)
{
	return bigf_hashcode(XBIGF_DATA(obj));

	/* less warnings */
	if (depth);
}

static const struct lrecord_description bigf_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigf, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigf", bigf,
				    NULL, bigf_print, NULL,
				    bigf_equal, bigf_hash,
				    bigf_description, Lisp_Bigf);


DEFUN ("bigf-get-precision", Fbigf_get_precision, 1, 1, 0, /*
Return the precision of bigf F as an integer.
							   */
       (f))
{
	CHECK_BIGF(f);
	return make_integer((signed long)XBIGF_GET_PREC(f));
}

DEFUN ("bigf-set-precision", Fbigf_set_precision, 2, 2, 0, /*
Set the precision of F, a bigf, to PRECISION, a nonnegative integer.
The new precision of F is returned.  Note that the return value may differ
from PRECISION if the underlying library is unable to support exactly
PRECISION bits of precision.
								   */
       (f, precision))
{
	unsigned long prec;

	CHECK_BIGF(f);
	if (INTP(precision)) {
		prec = (XINT(precision) <= 0)
			? 1UL : (unsigned long)XINT(precision);
	}
#ifdef HAVE_MPZ
	else if (BIGZP(precision)) {
		prec = bigz_fits_ulong_p(XBIGZ_DATA(precision))
			? bigz_to_ulong(XBIGZ_DATA(precision))
			: UINT_MAX;
	}
#endif	/* HAVE_MPZ */
	else {
		dead_wrong_type_argument(Qintegerp, f);
		return Qnil;
	}

	XBIGF_SET_PREC(f, prec);
	return Fbigf_get_precision(f);
}

/* scratch bigf object */
bigf ent_scratch_bigf;
#endif /* HAVE_MPF */

Lisp_Object Qbigfp;
Lisp_Object Qbigfloatp;		/* to be compatible to XE 21.5 */

DEFUN ("bigfloatp", Fbigfloatp, 1, 1, 0, /*
Return t if OBJECT is a bigfloat, nil otherwise.
					 */
       (object))
{
	return BIGFP(object) ? Qt : Qnil;
}

DEFUN ("bigfp", Fbigfp, 1, 1, 0, /*
Return t if OBJECT is a bigf, nil otherwise.
					 */
       (object))
{
	return BIGFP(object) ? Qt : Qnil;
}


/********************************* Floats ***********************************/
#ifdef LISP_FLOAT_TYPE
/* I wanted to define the lrecord implementation here, but that breaks at
 * Steve's site, so ... :(
 */
#endif


/********************************** Bigfrs **********************************/
#ifdef HAVE_MPFR

static void
bigfr_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = bigfr_to_string(XBIGFR_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigfr_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigfr_eql(XBIGFR_DATA(obj1), XBIGFR_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigfr_hash (Lisp_Object obj, int depth)
{
	return bigfr_hashcode(XBIGFR_DATA(obj));

	/* less warnings */
	if (depth);
}

static Lisp_Object
bigfr_mark (Lisp_Object obj)
{
	return Qnil;

	/* less warnings */
	if (obj == Qnil);
}

static void
bigfr_finalise (void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing MPFR objects",Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description bigfr_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigfr, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigfr", bigfr,
				    bigfr_mark, bigfr_print, bigfr_finalise,
				    bigfr_equal, bigfr_hash,
				    bigfr_description, Lisp_Bigfr);


DEFUN ("bigfr-get-precision", Fbigfr_get_precision, 1, 1, 0, /*
Return the precision of bigfr F as an integer.
							   */
       (f))
{
	CHECK_BIGFR(f);
	return make_integer((signed long)XBIGFR_GET_PREC(f));
}

#ifndef MPFR_PREC_MIN
#define MPFR_PREC_MIN 2UL
#endif

DEFUN ("bigfr-set-precision", Fbigfr_set_precision, 2, 2, 0, /*
Set the precision of F, a bigfr, to PRECISION, a nonnegative integer.
The new precision of F is returned.  Note that the return value may differ
from PRECISION if the underlying library is unable to support exactly
PRECISION bits of precision.
								   */
       (f, precision))
{
	unsigned long prec;

	CHECK_BIGFR(f);
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
		dead_wrong_type_argument(Qintegerp, f);
		return Qnil;
	}

	XBIGFR_SET_PREC(f, prec);
	return Fbigfr_get_precision(f);
}

bigfr ent_scratch_bigfr;
#else  /* !HAVE_MPFR */

#ifndef MPFR_PREC_MIN
#define MPFR_PREC_MIN 2UL
#endif
#ifndef MPFR_PREC_MAX
#define MPFR_PREC_MAX 1024UL
#endif


#endif /* HAVE_MPFR */

Lisp_Object Qbigfrp;

DEFUN ("bigfrp", Fbigfrp, 1, 1, 0, /*
Return t if OBJECT is a bigfr, nil otherwise.
					 */
       (object))
{
	return BIGFRP(object) ? Qt : Qnil;
}


/********************************** Reals ***********************************/
DEFUN ("realp", Frealp, 1, 1, 0, /*
Return t if OBJECT is a real, nil otherwise.
				 */
       (object))
{
	return REALP(object) ? Qt : Qnil;
}

static int
default_real_precision_changed (Lisp_Object sym, Lisp_Object *val,
				Lisp_Object in_object,
				int flags)
{
	unsigned long prec;

	CONCHECK_INTEGER(*val);
	prec = internal_get_precision(*val);
#ifdef HAVE_MPF
	if (prec != 0UL)
		bigf_set_default_prec(prec);
#endif
#ifdef HAVE_MPFR
	if (prec != 0UL)
		bigfr_set_default_prec(prec);
#endif
	return 0;

	/* less warnings */
	if (sym == Qnil);
	if (in_object == Qnil);
	if (flags);
}

DEFUN("real", Freal, 1, 2, 0,	/*
Return the real number numerically equal to NUMBER with
respect to the variable `read-real-as'.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision.
				 */
      (number, precision))
{
	if (Vread_real_as == Qbigfr) {
#ifdef HAVE_MPFR
		return Fcoerce_number(number, Qbigfr, precision);
#else  /* !HAVE_MPFR */
		;
#endif	/* HAVE_MPFR */
	}

	if (Vread_real_as == Qbigf) {
#ifdef HAVE_MPF
		return Fcoerce_number(number, Qbigf, precision);
#else  /* !HAVE_MPF */
		;
#endif	/* HAVE_MPF */
	}

        /* fallback to 'float */
	return Fcoerce_number(number, Qfloat, precision);
}


/******************************** Comparables *******************************/
DEFUN ("comparablep", Fcomparablep, 1, 1, 0, /*
Return t if OBJECT is a comparable number, nil otherwise.

We call a number comparable if there exists a total (archimedean)
order on the underlying structure.
					   */
       (object))
{
	return COMPARABLEP(object) ? Qt : Qnil;
}



/********************************** Biggs ***********************************/
#if defined(HAVE_PSEUG) && defined(HAVE_MPZ)

static void
bigg_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = bigg_to_string(XBIGG_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigg_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigg_eql(XBIGG_DATA(obj1), XBIGG_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigg_hash (Lisp_Object obj, int depth)
{
	return bigg_hashcode(XBIGG_DATA(obj));

	/* less warnings */
	if (depth);
}

static Lisp_Object
bigg_mark (Lisp_Object obj)
{
	return Qnil;

	/* less warnings */
	if (obj == Qnil);
}

static void
bigg_finalise (void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing "
			 "pseudo-gaussian objects",Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description bigg_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigg, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigg", bigg,
				    bigg_mark, bigg_print, bigg_finalise,
				    bigg_equal, bigg_hash,
				    bigg_description, Lisp_Bigg);

DEFUN ("make-bigg", Fmake_bigg, 2, 2, 0, /*
Return the Gaussian number whose rational component is REAL-PART
and whose imaginary component is IMAGINARY-PART.
					 */
       (real_part, imaginary_part))
{
	CHECK_COMPARABLE(real_part);
	CHECK_COMPARABLE(imaginary_part);

	return make_bigg_bz(
		XBIGZ_DATA(Fcoerce_number(real_part,
					  Qbigz, Qnil)),
		XBIGZ_DATA(Fcoerce_number(imaginary_part,
					  Qbigz, Qnil)));
}

bigg ent_scratch_bigg;
#endif /* HAVE_PSEUG */

Lisp_Object Qbiggp;

DEFUN ("biggp", Fbiggp, 1, 1, 0, /*
Return t if OBJECT is a bigg (a gaussian number), nil otherwise.
				 */
       (object))
{
	return BIGGP(object) ? Qt : Qnil;
}


/********************************** Bigcs ***********************************/
#ifdef HAVE_MPC

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

bigc ent_scratch_bigc;
#endif /* HAVE_MPC */

Lisp_Object Qbigcp;

DEFUN ("bigcp", Fbigcp, 1, 1, 0, /*
Return t if OBJECT is a bigc, nil otherwise.
					 */
       (object))
{
	return BIGCP(object) ? Qt : Qnil;
}


/******************************* Complex nums *******************************/
DEFUN ("complexp", Fcomplexp, 1, 1, 0, /*
Return t if OBJECT is a complex number (i.e. either a bigc
or a bigg), nil otherwise.
				       */
       (object))
{
	return COMPLEXP(object) ? Qt : Qnil;
}



/********************************** Quaterns ********************************/
#if defined(HAVE_QUATERN) && defined(HAVE_MPZ)

static void
quatern_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = quatern_to_string(XQUATERN_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
quatern_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return quatern_eql(XQUATERN_DATA(obj1), XQUATERN_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
quatern_hash (Lisp_Object obj, int depth)
{
	return quatern_hashcode(XQUATERN_DATA(obj));

	/* less warnings */
	if (depth);
}

static Lisp_Object
quatern_mark (Lisp_Object obj)
{
	return Qnil;

	/* less warnings */
	if (obj == Qnil);
}

static void
quatern_finalise (void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing "
			 "quaternionic objects", Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description quatern_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Quatern, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("quatern", quatern,
				    quatern_mark, quatern_print,
				    quatern_finalise,
				    quatern_equal, quatern_hash,
				    quatern_description, Lisp_Quatern);

quatern ent_scratch_quatern;
#endif /* HAVE_QUATERN */

Lisp_Object Qquaternp;

DEFUN ("quaternp", Fquaternp, 1, 1, 0, /*
Return t if OBJECT is a quaternion, nil otherwise.
				       */
       (object))
{
	return QUATERNP(object) ? Qt : Qnil;
}



/***************************** Residue Class Rings **************************/
#ifdef HAVE_RESCLASS

static void
resc_rng_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr;
	int flen;

	fstr = (Bufbyte*)resc_rng_to_string(XRESC_RNG_DATA(obj), 10);
	flen = strlen((char*)fstr);

	write_c_string("Z/", printcharfun);
	write_c_string((char*)fstr, printcharfun);
	write_c_string("Z", printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}
static void
resc_elm_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *fstr = (Bufbyte*)resc_elm_to_string(XRESC_ELM_DATA(obj), 10);
	Bufbyte *rstr = (Bufbyte*)resc_rng_to_string(
		XRESC_RNG_DATA(XRESC_ELM_RING(obj)), 10);

	write_c_string((char*)fstr, printcharfun);
	write_c_string("+", printcharfun);
	write_c_string((char*)rstr, printcharfun);
	write_c_string("Z", printcharfun);
	free(fstr);
	fstr = (Bufbyte *)NULL;
	free(rstr);
	rstr = (Bufbyte *)NULL;

	/* stupid compiler */
	if (escapeflag);
}

static int
resc_rng_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	/* less warnings */
	if (depth);
	if (NILP(obj1));
	if (NILP(obj2));

	return 0; /* bigg_eql(XBIGG_DATA(obj1), XBIGG_DATA(obj2)); */
}
static int
resc_elm_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	/* less warnings */
	if (depth);
	if (NILP(obj1));
	if (NILP(obj2));

	return 0; /* bigg_eql(XBIGG_DATA(obj1), XBIGG_DATA(obj2)); */
}

static unsigned long
resc_rng_hash(Lisp_Object obj, int depth)
{
	return resc_rng_hashcode(XRESC_RNG_DATA(obj));

	/* less warnings */
	if (depth);
}
static unsigned long
resc_elm_hash(Lisp_Object obj, int depth)
{
	return resc_elm_hashcode(XRESC_ELM_DATA(obj));

	/* less warnings */
	if (depth);
}

static Lisp_Object
resc_rng_mark(Lisp_Object obj)
{
	return Qnil;

	/* less warnings */
	if (obj == Qnil);
}
static Lisp_Object
resc_elm_mark(Lisp_Object obj)
{
	mark_object(XRESC_ELM_RING(obj));

	return XRESC_ELM_RING(obj);
}

static void
resc_rng_finalise(void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing "
			 "residue class objects",Qt);

	/* less warnings */
	if (header);
}
static void
resc_elm_finalise(void *header, int for_disksave)
{
	if (for_disksave)
		signal_simple_error
			("Can't dump an emacs containing "
			 "residue class objects",Qt);

	/* less warnings */
	if (header);
}

static const struct lrecord_description resc_rng_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Resc_Rng, data) },
	{ XD_END }
};
static const struct lrecord_description resc_elm_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Resc_Elm, data) },
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Resc_Elm, ring) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("resc_rng", resc_rng,
				    resc_rng_mark, resc_rng_print,
				    resc_rng_finalise,
				    resc_rng_equal, resc_rng_hash,
				    resc_rng_description, Lisp_Resc_Rng);
DEFINE_BASIC_LRECORD_IMPLEMENTATION("resc_elm", resc_elm,
				    resc_elm_mark, resc_elm_print,
				    resc_elm_finalise,
				    resc_elm_equal, resc_elm_hash,
				    resc_elm_description, Lisp_Resc_Elm);

#endif /* HAVE_RESCLASS */

Lisp_Object Qresc_rngp;
Lisp_Object Qresc_elmp;

DEFUN ("residue-class-ring-p", Fresidue_class_ring_p, 1, 1, 0, /*
Return t if OBJECT is a residue class ring, nil otherwise.
							       */
       (object))
{
	return RESC_RNGP(object) ? Qt : Qnil;
}

DEFUN ("residue-class-p", Fresidue_class_p, 1, 1, 0, /*
Return t if OBJECT is a residue class, nil otherwise.
						     */
       (object))
{
	return RESC_ELMP(object) ? Qt : Qnil;
}


/******************************* Archimedeans *******************************/
DEFUN ("archimedeanp", Farchimedeanp, 1, 1, 0, /*
Return t if OBJECT is a number with an archimedean valuation, nil otherwise.
					       */
       (object))
{
	return ARCHIMEDEANP(object) ? Qt : Qnil;
}


/***************************** Non-Archimedeans *****************************/
DEFUN ("nonarchimedeanp", Fnonarchimedeanp, 1, 1, 0, /*
Return t if OBJECT is a number with a non-archimedean valuation, nil
otherwise.
					       */
       (object))
{
	return NONARCHIMEDEANP(object) ? Qt : Qnil;
}


/******************************** Indefinite Symbols ************************/

static void
indef_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *istr = indef_to_string(XINDEF_DATA(obj));
	write_c_string((char*)istr, printcharfun);
	free(istr);
	istr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
indef_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return (XINDEF_DATA(obj1) == XINDEF_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
indef_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)XINDEF_DATA(obj);

	/* less warnings */
	if (depth);
}

static const struct lrecord_description indef_description[] = {
	{ XD_INT, offsetof(Lisp_Indef, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("indef", indef,
				    NULL, indef_print, NULL,
				    indef_equal, indef_hash,
				    indef_description, Lisp_Indef);

DEFUN("indefinitep", Findefinitep, 1, 1, 0, /*
Return t if OBJECT is an indefinite symbol, nil otherwise.
					    */
      (object))
{
	return INDEFP(object) ? Qt : Qnil;
}

DEFUN ("infinityp", Finfinityp, 1, 1, 0, /*
Return t if OBJECT is a form of infinity, nil otherwise.
					 */
       (object))
{
	return INFINITYP(object) ? Qt : Qnil;
}



/********************************* Numbers **********************************/
DEFUN ("canonicalize-number", Fcanonicalize_number, 1, 1, 0, /*
Return the canonical form of NUMBER.
							     */
       (number))
{
	/* The tests should go in order from larger, more expressive, or more
	   complex types to smaller, less expressive, or simpler types so that a
	   number can cascade all the way down to the simplest type if
	   appropriate. */
#ifdef HAVE_MPQ
	if (BIGQP(number) &&
	    bigz_fits_long_p(XBIGQ_DENOMINATOR(number)) &&
	    bigz_to_long(XBIGQ_DENOMINATOR(number)) == 1L)
		number = make_bigz_bz(XBIGQ_NUMERATOR(number));
	/* though that is mathematically wrong :( */
#endif
#ifdef HAVE_MPZ
	if (BIGZP(number) && bigz_fits_int_p(XBIGZ_DATA(number))) {
		EMACS_INT n = bigz_to_int(XBIGZ_DATA(number));
		if (NUMBER_FITS_IN_AN_EMACS_INT(n))
			number = make_int(n);
	}
#endif
#ifdef HAVE_MPFR
	if (BIGFRP(number) && bigfr_nan_p(XBIGFR_DATA(number)))
		number = make_indef(NOT_A_NUMBER);
	if (BIGFRP(number) && bigfr_inf_p(XBIGFR_DATA(number))) {
		if (bigfr_sign(XBIGFR_DATA(number)) > 0)
			number = make_indef(POS_INFINITY);
		else
			number = make_indef(NEG_INFINITY);
	}
#endif
	return number;
}

enum number_type
get_number_type (Lisp_Object arg)
{
	if (INTP(arg) || CHARP(arg) || MARKERP(arg))
		return INT_T;
#ifdef HAVE_MPZ
	if (BIGZP(arg))
		return BIGZ_T;
#endif
#ifdef HAVE_MPQ
	if (BIGQP(arg))
		return BIGQ_T;
#endif
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(arg))
		return FLOAT_T;
#endif
#ifdef HAVE_MPF
	if (BIGFP(arg))
		return BIGF_T;
#endif
#ifdef HAVE_MPFR
	if (BIGFRP(arg)) {
		if (!bigfr_nan_p(XBIGFR_DATA(arg)) &&
		    !bigfr_inf_p(XBIGFR_DATA(arg)))
			return BIGFR_T;
		else
			return INDEF_T;
	}
#endif
#ifdef HAVE_PSEUG
	if (BIGGP(arg))
		return BIGG_T;
#endif
#ifdef HAVE_MPC
	if (BIGCP(arg)) {
		if (!bigc_nan_p(XBIGC_DATA(arg)) &&
		    !bigc_inf_p(XBIGC_DATA(arg)))
			return BIGC_T;
		else
			return INDEF_T;
	}
#endif
#ifdef HAVE_RESCLASS
	if (RESC_ELMP(arg))
		return RESC_ELM_T;
#endif
#ifdef HAVE_QUATERN
	if (QUATERNP(arg))
		return QUATERN_T;
#endif

	if (INDEFP(arg))
		return INDEF_T;

	/* Catch unintentional bad uses of this function */
	abort();
	/* NOTREACHED */
	return INT_T;
}

Lisp_Object ent_normalise_number(Lisp_Object number)
{
	if (CHARP(number))
		return make_int(XCHAR(number));
	else if (MARKERP(number))
		return make_int(marker_position(number));

	CHECK_NUMBER(number);
	return number;
}

unsigned long ent_normalise_precision(unsigned long precision)
{
	/* MPFR will slaughter us when we pass a precision < MPFR_PREC_MIN */
	if (precision < MPFR_PREC_MIN)
		return XUINT(Vdefault_real_precision);
	if (precision > MPFR_PREC_MAX)
		return XUINT(Vmax_real_precision);

	return precision;
}

static Lisp_Object
ent_lift_trivial(Lisp_Object number, unsigned long precision)
{
	if (precision);

	return number;
}


/* Convert NUMBER to type TYPE.  If TYPE is BIGF_T then use the indicated
   PRECISION; otherwise, PRECISION is ignored. */
static Lisp_Object
internal_coerce_number (Lisp_Object number,
			enum number_type type,
			unsigned long precision)
{
	number_type nt;

	nt = get_number_type(number);

	return ent_optable_lift[nt][type](number, precision);
}


unsigned long
internal_get_precision(Lisp_Object precision)
{
	unsigned long susp_prec = 0;

	if (NILP(precision) && INTP(Vdefault_real_precision)) {
		susp_prec = XUINT(Vdefault_real_precision);
	} else if (NILP(precision) && BIGZP(Vdefault_real_precision)) {
#ifdef HAVE_MPZ
		if (!bigz_fits_ulong_p(XBIGZ_DATA(Vdefault_real_precision)))
			susp_prec = internal_get_precision(Vmax_real_precision);
		else
			susp_prec = bigz_to_ulong(
				XBIGZ_DATA(Vdefault_real_precision));
#endif
	} else if (INTP(precision)) {
		susp_prec = XUINT(precision);
#ifdef HAVE_MPZ
	} else if (BIGZP(precision)) {
		if (!bigz_fits_ulong_p(XBIGZ_DATA(precision)))
			susp_prec = internal_get_precision(Vmax_real_precision);
		else
			susp_prec = bigz_to_ulong(XBIGZ_DATA(precision));
#endif
	} else {
		susp_prec = XUINT(Vdefault_real_precision);
	}

	/* final comparison */
	if (susp_prec < MPFR_PREC_MIN)
		return MPFR_PREC_MIN;
	else if (susp_prec > MPFR_PREC_MAX)
		return MPFR_PREC_MAX;
	else
		return susp_prec;
}

DEFUN("coerce-number", Fcoerce_number, 2, 3, 0, /*
Convert NUMBER to the indicated type, possibly losing information.
See `coerce'.

TYPE is one of the symbols:
- 'fixnum or 'int     to convert to built-in integers
- 'bigz or 'bignum    to convert to bigz integers
- 'integer            to convert to the most suitable type out of
                      'bigz or 'int

- 'bigq or 'ratio     to convert to bigq fractions
- 'rational           to convert to the most suitable type out of
                      'bigq, 'bigz or 'int

- 'float              to convert to built-in floats
- 'bigf or 'bigfloat  to convert to bigf floats
- 'bigfr              to convert to bigfr floats
- 'real               to convert to the type indicated by 
                      `read-real-as' with a fallback to 'float

- 'bigg               to convert to a Gaussian
- 'bigc               to convert to a bigc complex number

NOTE: Not all of these types may be supported.

PRECISION is the number of bits of precision to use when converting to
reals; it is ignored otherwise.  If nil, the default precision is used.

Note that some conversions lose information.  No error is signaled in such
cases; the information is silently lost.
						 */
       (number, type, precision))
{
	CHECK_SYMBOL(type);
	if (EQ(type, Qint) || EQ(type, Qfixnum))
		return internal_coerce_number(number, INT_T, 0UL);
	else if (EQ(type, Qinteger)) {
		/* If bignums are available, we always convert to one first,
		   then downgrade to a int if possible. */
#ifdef HAVE_MPZ
		return Fcanonicalize_number(
			internal_coerce_number(number, BIGZ_T, 0UL));
#else
		return internal_coerce_number(number, INT_T, 0UL);
#endif	/* HAVE_MPZ */
	}
#ifdef HAVE_MPZ
	else if (EQ(type, Qbigz) || EQ(type, Qbignum)) {
		/* always convert to bigz */
		return internal_coerce_number(number, BIGZ_T, 0UL);
	}
#endif
#ifdef HAVE_MPQ
	else if (EQ(type, Qrational)) {
		/* convert to bigq and canonicalise */
		return Fcanonicalize_number(
			internal_coerce_number(number, BIGQ_T, 0UL));

	} else if (EQ(type, Qbigq) || EQ(type, Qratio)) {
		/* always convert to bigq */
		return internal_coerce_number(number, BIGQ_T, 0UL);
	}
#endif	/* HAVE_MPQ */
#ifdef LISP_FLOAT_TYPE
	else if (EQ(type, Qfloat))
		return internal_coerce_number(number, FLOAT_T, 0UL);
#endif
#ifdef HAVE_MPF
	else if (EQ(type, Qbigf) || EQ(type, Qbigfloat)) {
		unsigned long prec;
		prec = internal_get_precision(precision);
		return internal_coerce_number(number, BIGF_T, prec);
	}
#endif /* HAVE_MPF */
#ifdef HAVE_MPFR
	else if (EQ(type, Qbigfr)) { 
		unsigned long prec;
		prec = internal_get_precision(precision);
		return internal_coerce_number(number, BIGFR_T, prec);
	}
#endif /* HAVE_MPFR */
	else if (EQ(type, Qreal)) {
		/* respect `read-real-as' */
		unsigned long prec;
		prec = internal_get_precision(precision);
		if (0);
#ifdef HAVE_MPF
		else if(Vread_real_as == Qbigf)
			return internal_coerce_number(number, BIGF_T, prec);
#endif
#ifdef HAVE_MPFR
		else if (Vread_real_as == Qbigfr)
			return internal_coerce_number(number, BIGFR_T, prec);
#endif
		else 
			return internal_coerce_number(number, FLOAT_T, prec);
	}
#if defined(HAVE_PSEUG)
	else if (EQ(type, Qbigg)) { /* || EQ(type, Qcomplex)) { */
		return internal_coerce_number(number, BIGG_T, 0UL);
	}
#endif	/* HAVE_PSEUG */
#ifdef HAVE_MPC
	else if (EQ(type, Qbigc)) { /* || EQ(type, Qcomplex)) { */
		unsigned long prec;
		prec = internal_get_precision(precision);
		return internal_coerce_number(number, BIGC_T, prec);
	}
#endif /* HAVE_MPFR */

	Fsignal(Qunsupported_type, Qnil);
	/* NOTREACHED */
	return Qnil;
}


/************************ Auxiliary Categories **************************/
DEFUN("zerop", Fzerop, 1, 1, 0,	/*
Return t if NUMBER is a zero.
				*/
      (number))
{
	Lisp_Object result;

	if (!NUMBERP(number))
		return Qnil;

	switch (get_number_type(number)) {
	case INT_T:
		result = XINT(number) == 0 ? Qt : Qnil;
		break;
#ifdef HAVE_MPZ
	case BIGZ_T:
		result = bigz_sign(XBIGZ_DATA(number)) == 0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPQ
	case BIGQ_T:
		result = bigq_sign(XBIGQ_DATA(number)) == 0 ? Qt : Qnil;
		break;
#endif
#ifdef LISP_FLOAT_TYPE
	case FLOAT_T:
		result = XFLOAT_DATA(number) == 0.0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPF
	case BIGF_T:
		result = bigf_sign(XBIGF_DATA(number)) == 0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPFR
	case BIGFR_T:
		result = mpfr_zero_p(XBIGFR_DATA(number)) ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_PSEUG
	case BIGG_T:
		result = ((bigz_sign(bigg_re(XBIGG_DATA(number))) == 0) &&
			  (bigz_sign(bigg_im(XBIGG_DATA(number))) == 0))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPC
	case BIGC_T:
		result = (mpfr_zero_p(bigc_re(XBIGC_DATA(number))) &&
			  mpfr_zero_p(bigc_im(XBIGC_DATA(number))))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_QUATERN
	case QUATERN_T:
		result = ((bigz_sign(quatern_z(XQUATERN_DATA(number))) == 0) &&
			  (bigz_sign(quatern_i(XQUATERN_DATA(number))) == 0) &&
			  (bigz_sign(quatern_j(XQUATERN_DATA(number))) == 0) &&
			  (bigz_sign(quatern_k(XQUATERN_DATA(number))) == 0))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = bigz_sign(XRESC_ELM_DATA(number)) == 0 ? Qt : Qnil;
		break;
#endif
	case INDEF_T:
	default:
		result = Qnil;
		break;
	}
	return result;
}

DEFUN("zero", Fzero, 1, 1, 0, /*
Return the zero of the world NUMBER lives in.
			       */
       (number))
{
	Lisp_Object result;
	unsigned long prec;

#ifdef HAVE_RESCLASS
	/* allow residue class rings */
	if (RESC_RNGP(number))
		return make_resc_elm(0L, number);
#endif

	CHECK_NUMBER(number);

	switch (get_number_type(number)) {
	case INDEF_T:
	case INT_T:
		result = make_int(0);
		break;
#ifdef HAVE_MPZ
	case BIGZ_T:
		result = make_bigz(0L);
		break;
#endif
#ifdef HAVE_MPQ
	case BIGQ_T:
		result = make_bigq(0L, 1UL);
		break;
#endif
#ifdef LISP_FLOAT_TYPE
	case FLOAT_T:
		result = make_float(0.0);
		break;
#endif
#ifdef HAVE_MPF
	case BIGF_T:
		prec = internal_get_precision(Qnil);
		result = make_bigf(0.0, prec);
		break;
#endif
#ifdef HAVE_MPFR
	case BIGFR_T:
		prec = internal_get_precision(Qnil);
		result = make_bigfr(0.0, prec);
		break;
#endif
#ifdef HAVE_PSEUG
	case BIGG_T:
		result = make_bigg(0L, 0L);
		break;
#endif
#ifdef HAVE_MPC
	case BIGC_T:
		prec = internal_get_precision(Qnil);
		result = make_bigc(0.0, 0.0, prec);
		break;
#endif
#ifdef HAVE_QUATERN
	case QUATERN_T:
		result = make_quatern(0L, 0L, 0L, 0L);
		break;
#endif
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = make_resc_elm(0L, XRESC_ELM_RING(number));
		break;
#endif
	default:
		result = wrong_type_argument(Qunsupported_type, number);
		break;
	}

	if (prec);
	return result;
}

DEFUN("onep", Fonep, 1, 1, 0,	/*
Return t if NUMBER is a one.
				*/
      (number))
{
	Lisp_Object result;

	if (!NUMBERP(number))
		return Qnil;

	switch (get_number_type(number)) {
	case INT_T:
		result = XINT(number) == 1 ? Qt : Qnil;
		break;
#ifdef HAVE_MPZ
	case BIGZ_T:
		result = bigz_to_long(XBIGZ_DATA(number)) == 1L ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPQ
	case BIGQ_T:
		result = (bigz_to_long(bigq_numerator(
					       XBIGQ_DATA(number))) == 1L &&
			  bigz_to_long(bigq_denominator(
					       XBIGQ_DATA(number))) == 1L)
			? Qt : Qnil;
		break;
#endif
#ifdef LISP_FLOAT_TYPE
	case FLOAT_T:
		result = XFLOAT_DATA(number) == 1.0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPF
	case BIGF_T:
		result = bigf_to_double(XBIGF_DATA(number)) == 1.0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPFR
	case BIGFR_T:
		result = (mpfr_integer_p(XBIGFR_DATA(number)) &&
			  bigfr_to_long(XBIGFR_DATA(number)) == 1L) ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_PSEUG
	case BIGG_T:
		result = ((bigz_to_long(bigg_re(XBIGG_DATA(number))) == 1L) &&
			  (bigz_to_long(bigg_im(XBIGG_DATA(number))) == 0L))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPC
	case BIGC_T:
		result = ((mpfr_integer_p(bigc_re(XBIGC_DATA(number)))) &&
			  (bigfr_to_long(bigc_re(XBIGC_DATA(number))) == 1L) &&
			  (mpfr_zero_p(bigc_im(XBIGC_DATA(number)))))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_QUATERN
	case QUATERN_T:
		result = ((bigz_to_long(
				   quatern_z(XQUATERN_DATA(number))) == 1L) &&
			  (bigz_to_long(
				   quatern_i(XQUATERN_DATA(number))) == 0L) &&
			  (bigz_to_long(
				   quatern_j(XQUATERN_DATA(number))) == 0L) &&
			  (bigz_to_long(
				   quatern_k(XQUATERN_DATA(number))) == 0L))
			? Qt : Qnil;
		break;
#endif
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = bigz_to_long(XRESC_ELM_DATA(number)) == 1L ? Qt : Qnil;
		break;
#endif
	case INDEF_T:
	default:
		result = Qnil;
		break;
	}
	return result;
}

DEFUN("one", Fone, 1, 1, 0, /*
Return the one of the world NUMBER lives in.
			    */
       (number))
{
	Lisp_Object result;
	unsigned long prec;

#ifdef HAVE_RESCLASS
	/* allow residue class rings */
	if (RESC_RNGP(number))
		return make_resc_elm(1L, number);
#endif

	CHECK_NUMBER(number);

	switch (get_number_type(number)) {
	case INDEF_T:
	case INT_T:
		result = make_int(1);
		break;
#ifdef HAVE_MPZ
	case BIGZ_T:
		result = make_bigz(1L);
		break;
#endif
#ifdef HAVE_MPQ
	case BIGQ_T:
		result = make_bigq(1L, 1UL);
		break;
#endif
#ifdef LISP_FLOAT_TYPE
	case FLOAT_T:
		result = make_float(1.0);
		break;
#endif
#ifdef HAVE_MPF
	case BIGF_T:
		prec = internal_get_precision(Qnil);
		result = make_bigf(1.0, prec);
		break;
#endif
#ifdef HAVE_MPFR
	case BIGFR_T:
		prec = internal_get_precision(Qnil);
		result = make_bigfr(1.0, prec);
		break;
#endif
#ifdef HAVE_PSEUG
	case BIGG_T:
		result = make_bigg(1L, 0L);
		break;
#endif
#ifdef HAVE_MPC
	case BIGC_T:
		prec = internal_get_precision(Qnil);
		result = make_bigc(1.0, 0.0, prec);
		break;
#endif
#ifdef HAVE_QUATERN
	case QUATERN_T:
		result = make_quatern(1L, 0L, 0L, 0L);
		break;
#endif
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = make_resc_elm(1L, XRESC_ELM_RING(number));
		break;
#endif
	default:
		result = wrong_type_argument(Qunsupported_type, number);
		break;
	}

	if (prec);
	return result;
}


void
syms_of_number (void)
{
#ifdef HAVE_MPZ
	INIT_LRECORD_IMPLEMENTATION(bigz);
#endif
#ifdef HAVE_MPQ
	INIT_LRECORD_IMPLEMENTATION(bigq);
#endif
#ifdef HAVE_MPF
	INIT_LRECORD_IMPLEMENTATION(bigf);
#endif
#ifdef LISP_FLOAT_TYPE
	INIT_LRECORD_IMPLEMENTATION(float);
#endif
#ifdef HAVE_MPFR
	INIT_LRECORD_IMPLEMENTATION(bigfr);
#endif
#ifdef HAVE_MPC
	INIT_LRECORD_IMPLEMENTATION(bigc);
#endif
#ifdef HAVE_PSEUG
	INIT_LRECORD_IMPLEMENTATION(bigg);
#endif
#ifdef HAVE_QUATERN
	INIT_LRECORD_IMPLEMENTATION(quatern);
#endif
#ifdef HAVE_RESCLASS
	INIT_LRECORD_IMPLEMENTATION(resc_rng);
	INIT_LRECORD_IMPLEMENTATION(resc_elm);
#endif
	INIT_LRECORD_IMPLEMENTATION(indef);

	/* Type predicates */
	DEFSYMBOL(Qbignump);
	DEFSYMBOL(Qbigzp);
	DEFSYMBOL(Qratiop);
	DEFSYMBOL(Qbigqp);

	DEFSYMBOL(Qrationalp);

	DEFSYMBOL(Qbigfloatp);
	DEFSYMBOL(Qbigfp);
	DEFSYMBOL(Qbigfrp);
	DEFSYMBOL(Qrealp);
	DEFSYMBOL(Qcomparablep);

	DEFSYMBOL(Qbiggp);
	DEFSYMBOL(Qbigcp);
	DEFSYMBOL(Qquaternp);

	DEFSYMBOL(Qinfinityp);
	DEFSYMBOL(Qindefinitep);

	DEFSYMBOL(Qarchimedeanp);
	DEFSYMBOL(Qnonarchimedeanp);

	/* some error categories */
	DEFERROR(Qoperation_error,
		 "Operation undefined over domain", Qarith_error);
	DEFERROR(Qrelation_error,
		 "Relation undefined over domain", Qarith_error);
	DEFERROR(Qvaluation_error,
		 "Valuation undefined over domain", Qarith_error);

	/* Functions */
	DEFSUBR(Fbigzp);
	DEFSUBR(Fbignump);
	DEFSUBR(Fintegerp);
	DEFSUBR(Fevenp);
	DEFSUBR(Foddp);

	DEFSUBR(Fratiop);
	DEFSUBR(Fbigqp);
	DEFSUBR(Fnumerator);
	DEFSUBR(Fdenominator);

	DEFSUBR(Frationalp);

	DEFSUBR(Fbigfp);
	DEFSUBR(Fbigfloatp);
#ifdef HAVE_MPF
	DEFSUBR(Fbigf_get_precision);
	DEFSUBR(Fbigf_set_precision);
#endif

	DEFSUBR(Fbigfrp);
#ifdef HAVE_MPFR
	DEFSUBR(Fbigfr_get_precision);
	DEFSUBR(Fbigfr_set_precision);
#endif

	DEFSUBR(Frealp);
	DEFSUBR(Freal);
	DEFSUBR(Fcomparablep);

	DEFSUBR(Fbiggp);
#if defined(HAVE_PSEUG) && defined(HAVE_MPZ)
	DEFSUBR(Fmake_bigg);
#endif

	DEFSUBR(Fbigcp);
#ifdef HAVE_MPC
	DEFSUBR(Fbigc_get_precision);
	DEFSUBR(Fbigc_set_precision);
	DEFSUBR(Fmake_bigc);
#endif

	DEFSUBR(Fcomplexp);

	DEFSUBR(Fquaternp);
#if defined(HAVE_QUATERN) && defined(HAVE_MPZ)
	syms_of_ent_quatern();
#endif

	DEFSUBR(Fresidue_class_ring_p);
	DEFSUBR(Fresidue_class_p);
#ifdef HAVE_RESCLASS
	syms_of_ent_resclass();
#endif

	DEFSUBR(Farchimedeanp);
	DEFSUBR(Fnonarchimedeanp);

	DEFSUBR(Finfinityp);
	DEFSUBR(Findefinitep);

	DEFSUBR(Fcanonicalize_number);
	DEFSUBR(Fcoerce_number);

	DEFSUBR(Fzerop);
	DEFSUBR(Fzero);
	DEFSUBR(Fonep);
	DEFSUBR(Fone);

	/* Errors */
	DEFERROR_STANDARD(Qunsupported_type, Qwrong_type_argument);
}

void
vars_of_number (void)
{
	Vmost_negative_int = EMACS_INT_MIN;
	DEFVAR_CONST_INT("most-negative-fixnum", &Vmost_negative_int /*
The (ordinary) integer closest in value to negative infinity.
								   */);

	Vmost_positive_int = EMACS_INT_MAX;
	DEFVAR_CONST_INT("most-positive-fixnum", &Vmost_positive_int /*
The (ordinary) integer closest in value to positive infinity.
								  */);

	Fprovide(intern("number-types"));
	Fprovide(intern("ent"));
#ifdef HAVE_MPZ
	Fprovide(intern("bignum"));
	Fprovide(intern("bigz"));
#endif
#ifdef HAVE_MPQ
	Fprovide(intern("ratio"));
	Fprovide(intern("bigq"));
#endif
#ifdef HAVE_MPF
	Fprovide(intern("bigfloat"));
	Fprovide(intern("bigf"));
#endif
#ifdef LISP_FLOAT_TYPE
	Fprovide(intern("lisp-float-type"));
#endif
#ifdef HAVE_MPFR
	Fprovide(intern("bigfr"));
#endif
#if defined(HAVE_MPC) || defined(HAVE_PSEUC)
	Fprovide(intern("bigc"));
#endif
#ifdef HAVE_PSEUG
	Fprovide(intern("bigg"));
	Fprovide(intern("gaussian"));
#endif
#ifdef HAVE_QUATERN
	Fprovide(intern("quatern"));
#endif
#ifdef HAVE_RESCLASS
	Fprovide(intern("resclass"));
#endif

	Vmax_real_precision = make_int(EMACS_INT_MAX);

	DEFVAR_CONST_LISP("max-real-precision", &Vmax_real_precision /*
The maximum number of bits of precision a bigf or bigfr can have.
This is determined by the underlying library used to implement
arbitrary-precision floats.
*/);

	DEFVAR_LISP("read-real-as", &Vread_real_as /*
*Indicate how real numbers should be read.
If set to `nil' or 'float, reals are always converted to floats.
If set to 'bigf or 'bigfr, reals are read as MPF floats or MPFR 
floats respectively.
*/);
	Vread_real_as = Qfloat;

	Vdefault_real_precision = make_int(128);
	DEFVAR_LISP_MAGIC("default-real-precision",
			  &Vdefault_real_precision, /*
*The default floating-point precision for newly created
floating point values.
This should be an unsigned integer no greater than
`maximum-real-precision' to create external floats
with the indicated precision.

This variable is effective only when `read-real-as'
is set to a float type which supports setting a
precision.
						    */
			  default_real_precision_changed);

#ifdef HAVE_MPF
	bigf_set_default_prec(128UL);
#endif
#ifdef HAVE_MPFR
	bigfr_set_default_prec(128UL);
#endif


/* Now define +infinity and -infinity, complex-infinity and not-a-number */
	Vnot_a_number = Qnil;
	Vpinfinity = Qnil;
	Vninfinity = Qnil;
	Vcomplex_infinity = Qnil;

	DEFVAR_CONST_LISP("not-a-number", &Vnot_a_number /*
Not a number.
*/);
	DEFVAR_CONST_LISP("+infinity", &Vpinfinity /*
Positive infinity.
*/);
	DEFVAR_CONST_LISP("-infinity", &Vninfinity /*
Negative infinity.
*/);
	DEFVAR_CONST_LISP("complex-infinity", &Vcomplex_infinity /*
The infinitely distant point in the complex plane.
*/);

#ifdef HAVE_MPFR
	/* define pi and e */

	/* just some dummy values atm, to make the dumper smile */
	Veuler = make_int(1L);
	Vpi = make_int(1L);

	DEFVAR_CONST_LISP("euler", &Veuler /*
The value of the Euler constant e (2.7182818...).
*/);
	DEFVAR_CONST_LISP("pi", &Vpi /*
The value of pi (3.1415926...).
*/);
#endif	/* HAVE_MPFR */
}

/******************************* op tables ****************************/

#define NOT NUMBER_OF_TYPES
Lisp_Object (*ent_optable_sum[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_diff[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_neg[NOT])(Lisp_Object);
Lisp_Object (*ent_optable_prod[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_div[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_inv[NOT])(Lisp_Object);
Lisp_Object (*ent_optable_quo[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_rem[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_mod[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_pow[NOT][NOT])(Lisp_Object, Lisp_Object);

Lisp_Object (*ent_optable_lt[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_gt[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_eq[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_ne[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_vallt[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_valgt[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_valeq[NOT][NOT])(Lisp_Object, Lisp_Object);
Lisp_Object (*ent_optable_valne[NOT][NOT])(Lisp_Object, Lisp_Object);

Lisp_Object (*ent_optable_lift[NOT][NOT])(Lisp_Object, unsigned long);
#undef NOT

static Lisp_Object ent_sum_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_diff_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_prod_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_div_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_quo_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_rem_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_mod_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_pow_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_neg_undefined(Lisp_Object l)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l));
	return Qnil;
}
static Lisp_Object ent_inv_undefined(Lisp_Object l)
{
	Fsignal(Qoperation_error, Qnil);
	if (NILP(l));
	return Qnil;
}

static Lisp_Object ent_lt_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qrelation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_gt_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qrelation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_eq_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qrelation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_ne_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qrelation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_vallt_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qvaluation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_valgt_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qvaluation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_valeq_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qvaluation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}
static Lisp_Object ent_valne_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qvaluation_error, Qnil);
	if (NILP(l) || NILP(r));
	return Qnil;
}

static Lisp_Object
ent_lift_undefined(Lisp_Object l, unsigned long precision)
{
	Fsignal(Qdomain_error, Qnil);
	if (NILP(l));
	return Qnil;
}

static void initialise_operation_tables(void)
{
	unsigned int i, j;

	for (i = 0; i < NUMBER_OF_TYPES; i++) {
		for (j = 0; j < NUMBER_OF_TYPES; j++) {
			ent_optable_sum[i][j] = ent_sum_undefined;
			ent_optable_diff[i][j] = ent_diff_undefined;
			ent_optable_prod[i][j] = ent_prod_undefined;
			ent_optable_div[i][j] = ent_div_undefined;
			ent_optable_quo[i][j] = ent_quo_undefined;
			ent_optable_rem[i][j] = ent_rem_undefined;
			ent_optable_mod[i][j] = ent_mod_undefined;
			ent_optable_pow[i][j] = ent_pow_undefined;

			ent_optable_lt[i][j] = ent_lt_undefined;
			ent_optable_gt[i][j] = ent_gt_undefined;
			ent_optable_eq[i][j] = ent_eq_undefined;
			ent_optable_ne[i][j] = ent_ne_undefined;
			ent_optable_vallt[i][j] = ent_vallt_undefined;
			ent_optable_valgt[i][j] = ent_valgt_undefined;
			ent_optable_valeq[i][j] = ent_valeq_undefined;
			ent_optable_valne[i][j] = ent_valne_undefined;

			if (i == j)
				ent_optable_lift[i][j] = ent_lift_trivial;
			else
				ent_optable_lift[i][j] = ent_lift_undefined;
		}
		ent_optable_neg[i] = ent_neg_undefined;
		ent_optable_inv[i] = ent_inv_undefined;
	}
}

void init_number(void)
{
	initialise_operation_tables();

	init_optables_INT_T();
	init_optables_FLOAT_T();

#ifdef WITH_GMP
	init_optables_BIGZ_T();
	init_optables_BIGQ_T();
	init_optables_BIGF_T();
#endif
#ifdef WITH_MPFR
	init_optables_BIGFR_T();
#endif
#ifdef HAVE_MPC
	init_optables_BIGC_T();
#endif
#ifdef HAVE_PSEUG
	init_optables_BIGG_T();
#endif
#ifdef HAVE_QUATERN
	init_optables_QUATERN_T();
#endif
#ifdef HAVE_RESCLASS
	init_optables_RESC_ELM_T();
#endif
#ifdef WITH_NUMBER_TYPES
	init_optables_INDEF_T();
#endif

	if (!number_initialized)
	{
		number_initialized = 1;

		init_ent_int();
		init_ent_float();

#ifdef WITH_GMP
		init_number_gmp();
		bigz_init(ent_scratch_bigz);
		bigq_init(ent_scratch_bigq);
		bigf_init(ent_scratch_bigf);
#endif
#ifdef WITH_MPFR
		init_number_mpfr();
		bigfr_init(ent_scratch_bigfr);
#endif
#ifdef WITH_MP
		init_number_mp();
#endif
#ifdef HAVE_MPC
		init_number_mpc();
		bigc_init(ent_scratch_bigc);
#endif
#ifdef HAVE_PSEUG
		init_number_gaussian();
		bigg_init(ent_scratch_bigg);
#endif
#ifdef HAVE_QUATERN
		init_ent_quatern();
		quatern_init(ent_scratch_quatern);
#endif
#ifdef HAVE_RESCLASS
		init_ent_resclass();
#endif
#ifdef WITH_NUMBER_TYPES
		init_ent_indef();
#endif

	}

}
