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

static void initialise_coerce_table(void);

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

DEFUN("make-residue-class-ring", Fmake_residue_class_ring, 1, 1, 0, /*
Return a residue class ring of size MODULUS (>= 2).
								    */
       (modulus))
{
	CHECK_INTEGER(modulus);
	if (NILP(Fnonnegativep(modulus)))
		error("cannot create ring with negative modulus");
	if (!NILP(Fzerop(modulus)))
		error("cannot create ring of size 0");
	if (!NILP(Fonep(modulus)))
		error("ring is identical to Z");

	if (INTP(modulus))
		return make_resc_rng(XINT(modulus));
	else if (BIGZP(modulus))
		return make_resc_rng_bz(XBIGZ_DATA(modulus));
	else
		error("cannot create ring");
}

DEFUN("make-residue-class", Fmake_residue_class, 2, 2, 0, /*
Return the residue class of ELEMENT in RING.
							  */
      (element, ring))
{
	CHECK_RESC_RNG(ring);
	CHECK_INTEGER(element);

	if (INTP(element))
		return make_resc_elm(XINT(element), ring);
	else if (BIGZP(element))
		return make_resc_elm_bz(XBIGZ_DATA(element), ring);
	else
		error("cannot create class");
}

DEFUN("residue-class-ring", Fresidue_class_ring, 1, 1, 0, /*
Return the parental residue class ring (the world) of RESCLASS.
							  */
      (resclass))
{
	CHECK_RESC_ELM(resclass);

	return XRESC_ELM_RING(resclass);
}

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
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Indef, data) },
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
	if (BIGZP(number) && bigz_fits_int_p(XBIGZ_DATA(number)))	{
		int n = bigz_to_int(XBIGZ_DATA(number));
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
	if (INTP(arg))
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

	if (INDEFP(arg))
		return INDEF_T;

	/* Catch unintentional bad uses of this function */
	abort();
	/* NOTREACHED */
	return INT_T;
}


static Lisp_Object
internal_coerce_to_INT_T (Lisp_Object number, unsigned long precision)
{
	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);

	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return number;

	case BIGZ_T:
#ifdef HAVE_MPZ
		return make_int(bigz_to_long(XBIGZ_DATA(number)));
#else
		abort();
#endif

	case BIGQ_T: {
#ifdef HAVE_MPQ
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);

		bigz_div(bz,
			 XBIGQ_NUMERATOR(number),
			 XBIGQ_DENOMINATOR(number));
		result = make_int(bigz_to_long(bz));

		bigz_fini(bz);
		return result;
#else
		abort();
#endif
	}

	case FLOAT_T:
#ifdef LISP_FLOAT_TYPE
		return Ftruncate(number);
#else
		abort();
#endif

	case BIGF_T:
#ifdef HAVE_MPF
		return make_int(bigf_to_long(XBIGF_DATA(number)));
#else
		abort();
#endif

	case BIGFR_T:
#ifdef HAVE_MPFR
		/* warn about coercions of indefinite symbols */
		if (bigfr_inf_p(XBIGFR_DATA(number)))
			error("cannot coerce infinity symbol to int");
		if (bigfr_nan_p(XBIGFR_DATA(number)))
			error("cannot coerce not-a-number symbol to int");

		return make_int(bigfr_to_long(XBIGFR_DATA(number)));
#else
		abort();
#endif

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);

	}
	abort ();
	/* make our compiler smile */
	precision = 0UL;
	/* NOTREACHED */
	return Qzero;
}

static Lisp_Object
internal_coerce_to_BIGZ_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_MPZ
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);

	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return make_bigz(XREALINT(number));

	case BIGZ_T:
		return number;

	case BIGQ_T: {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);

		bigz_div(bz,
			 XBIGQ_NUMERATOR(number),
			 XBIGQ_DENOMINATOR(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
	}

	case FLOAT_T: {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);

		bigz_set_double(bz, XFLOAT_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
	}

	case BIGF_T: {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);
			
		bigz_set_bigf(bz, XBIGF_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
	}

	case BIGFR_T: {
#ifdef HAVE_MPFR
		bigz bz;
		Lisp_Object result;

		/* warn about coercions of indefinite symbols */
		if (bigfr_inf_p(XBIGFR_DATA(number)))
			error("cannot coerce infinity symbol to bigz");
		if (bigfr_nan_p(XBIGFR_DATA(number)))
			error("cannot coerce not-a-number symbol to bigz");

		bigz_init(bz);
			
		bigz_set_bigfr(bz, XBIGFR_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
#else
		abort();
		break;
#endif
	}

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);
	}

	abort();
	/* make our compiler smile */
	precision = 0UL;
	return Qzero;
#endif	/* HAVE_MPZ */
}

static Lisp_Object
internal_coerce_to_BIGQ_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_MPQ
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);

	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return make_bigq(XREALINT(number), 1UL);

	case BIGZ_T: {
		bigz bz;
		Lisp_Object result;
		bigz_init(bz);

		bigz_set_long(bz, 1L);
		result = make_bigq_bz(XBIGZ_DATA(number), bz);

		bigz_fini(bz);
		return result;
	}

	case BIGQ_T:
		return number;

	case FLOAT_T: {
		bigq bq;
		Lisp_Object result;

		bigq_init(bq);
		bigq_set_double(bq, XFLOAT_DATA(number));

		result = make_bigq_bq(bq);

		bigq_fini(bq);
		return result;
	}

	case BIGF_T: {
		bigq bq;
		Lisp_Object result;

		bigq_init(bq);
			
		bigq_set_bigf(bq, XBIGF_DATA(number));
		result = make_bigq_bq(bq);

		bigq_fini(bq);
		return result;
	}

	case BIGFR_T: {
#ifdef HAVE_MPFR
		bigq bq;
		bigf bf;
		Lisp_Object result;

		/* warn about coercions of indefinite symbols */
		if (bigfr_inf_p(XBIGFR_DATA(number)))
			error("cannot coerce infinity symbol to bigq");
		if (bigfr_nan_p(XBIGFR_DATA(number)))
			error("cannot coerce not-a-number symbol to bigq");

		bigq_init(bq);
		bigf_init_prec(bf, XBIGFR_GET_PREC(number));
			
		/* bigq_set_bigfr does not exist, so we have to go via bigf */
		bigf_set_bigfr(bf, XBIGFR_DATA(number));
		bigq_set_bigf(bq, bf);
		result = make_bigq_bq(bq);

		bigf_fini(bf);
		bigq_fini(bq);
		return result;
#else
		abort();
		break;
#endif
	}

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);
	}
	abort();
	/* make our compiler smile */
	precision = 0UL;
	return Qzero;
#endif	/* HAVE_MPQ */
}

static Lisp_Object
internal_coerce_to_FLOAT_T (Lisp_Object number, unsigned long precision)
{
#ifndef LISP_FLOAT_TYPE
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);

	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return make_float(XREALINT(number));

	case BIGZ_T:
#ifdef HAVE_MPZ
		return make_float(bigz_to_double(XBIGZ_DATA(number)));
#else
		abort();
#endif

	case BIGQ_T:
#ifdef HAVE_MPQ
		return make_float(bigq_to_double(XBIGQ_DATA(number)));
#else
		abort();
#endif

	case FLOAT_T:
		return number;

	case BIGF_T:
#ifdef HAVE_MPF
		return make_float(bigf_to_double(XBIGF_DATA(number)));
#else
		abort();
#endif

	case BIGFR_T:
#ifdef HAVE_MPFR
		return make_float(bigfr_to_double(XBIGFR_DATA(number)));
#else
		abort();
		break;
#endif

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);
	}
	abort();
	/* make our compiler smile */
	precision = 0UL;
	return Qzero;
#endif	/* LISP_FLOAT_TYPE */
}

static Lisp_Object
internal_coerce_to_BIGF_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_MPF
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);

	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return make_bigf(XREALINT(number), precision);

	case BIGZ_T: {
		bigf bf;
		Lisp_Object result;

		bigf_init_prec(bf, precision);

		bigf_set_bigz(bf, XBIGZ_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}

	case BIGQ_T: {
		bigf bf;
		Lisp_Object result;

		bigf_init_prec(bf, precision);

		bigf_set_bigq(bf, XBIGQ_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}

	case FLOAT_T: {
		bigf bf;
		Lisp_Object result;

		bigf_init_prec(bf, precision);

		bigf_set_double(bf, XFLOAT_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}

	case BIGF_T:
		return number;

	case BIGFR_T: {
#ifdef HAVE_MPFR
		bigf bf;
		Lisp_Object result;

		/* warn about coercions of indefinite symbols */
		if (bigfr_inf_p(XBIGFR_DATA(number)))
			error("cannot coerce infinity symbol to bigf");
		if (bigfr_nan_p(XBIGFR_DATA(number)))
			error("cannot coerce not-a-number symbol to bigf");

		bigf_init_prec(bf, XBIGFR_GET_PREC(number));
			
		bigf_set_bigfr(bf, XBIGFR_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
#else
		abort();
		break;
#endif
	}

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);
	}
	abort();
	return Qzero;
#endif	/* HAVE_MPF */
}

static Lisp_Object
internal_coerce_to_BIGFR_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_MPFR
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);
	current_type = get_number_type(number);

	/* MPFR will slaughter us when we pass a precision < MPFR_PREC_MIN */
	if (precision < MPFR_PREC_MIN)
		precision = XUINT(Vdefault_real_precision);
	if (precision > MPFR_PREC_MAX)
		precision = XUINT(Vmax_real_precision);

	switch (current_type) {
	case INT_T:
		return make_bigfr(XREALINT(number), precision);

	case BIGZ_T: {
		bigfr bf;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);

		bigfr_set_bigz(bf, XBIGZ_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}

	case BIGQ_T: {
		bigfr bf;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);

		bigfr_set_bigq(bf, XBIGQ_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}

	case FLOAT_T: {
		bigfr bf;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);

		bigfr_set_double(bf, XFLOAT_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}

	case BIGF_T: {
		bigfr bf;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);

		bigfr_set_bigf(bf, XBIGF_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}

	case BIGFR_T:
		return number;

	/* now these types are not coercible */
	case BIGG_T:
	case BIGC_T:
		return wrong_type_argument(Qcomparablep, number);

	case INDEF_T:
		if (COMPARABLE_INDEF_P(number))
			return number;
		else
			return wrong_type_argument(Qcomparablep, number);
	}
	abort();
	return Qzero;
#endif	/* HAVE_MPFR */
}

static Lisp_Object
internal_coerce_to_BIGG_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_PSEUG
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);
	current_type = get_number_type(number);

	/* MPFR will slaughter us when we pass a precision < MPFR_PREC_MIN */
	if (precision < MPFR_PREC_MIN)
		precision = XUINT(Vdefault_real_precision);
	if (precision > MPFR_PREC_MAX)
		precision = XUINT(Vmax_real_precision);

	switch (current_type) {
	case INT_T:
	case BIGZ_T:
	case BIGQ_T:
	case FLOAT_T:
	case BIGF_T:
	case BIGFR_T: {
		bigg bg;
		Lisp_Object result, l_bz;

#ifdef HAVE_MPFR
		/* warn about coercions of indefinite symbols */
		if (current_type == BIGFR_T) {
			if (bigfr_inf_p(XBIGFR_DATA(number)))
				error("cannot coerce infinity symbol "
				      "to bigg");
			if (bigfr_nan_p(XBIGFR_DATA(number)))
				error("cannot coerce not-a-number symbol "
				      "to bigg");
		}
#endif

		bigg_init(bg);
		l_bz = internal_coerce_to_BIGZ_T(number, 0UL);

		bigg_set_bigz(bg, XBIGZ_DATA(l_bz));
		result = make_bigg_bg(bg);

		bigg_fini(bg);
		return result;
	}

	case BIGG_T:
		return number;

	case BIGC_T: {
#ifdef HAVE_MPC
		bigg bg;
		Lisp_Object result, l_bz_re, l_bz_im;

		/* warn about coercions of indefinite symbols */
		if (bigc_inf_p(XBIGC_DATA(number)))
			error("cannot coerce infinity symbol to bigg");
		if (bigc_nan_p(XBIGC_DATA(number)))
			error("cannot coerce not-a-number symbol to bigg");

		bigg_init(bg);

		l_bz_re = internal_coerce_to_BIGZ_T(
			Freal_part(number), 0UL);
		l_bz_im = internal_coerce_to_BIGZ_T(
			Fimaginary_part(number), 0UL);

		bigg_set_bigz_bigz(bg,
				   XBIGZ_DATA(l_bz_re),
				   XBIGZ_DATA(l_bz_im));
		result = make_bigg_bg(bg);

		bigg_fini(bg);
		return result;
#else
		abort();
		break;
#endif
	}

	case INDEF_T:
		if (INFINITYP(number))
			return make_indef(COMPLEX_INFINITY);
		else
			return wrong_type_argument(Qinfinityp, number);

	}
	abort();
	return Qzero;
#endif	/* HAVE_PSEUG */
}

static Lisp_Object
internal_coerce_to_BIGC_T (Lisp_Object number, unsigned long precision)
{
#ifndef HAVE_MPC
	abort();

	if (NILP(number) && precision);
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);
	current_type = get_number_type(number);

	/* MPFR will slaughter us when we pass a precision < MPFR_PREC_MIN */
	if (precision < MPFR_PREC_MIN)
		precision = XUINT(Vdefault_real_precision);
	if (precision > MPFR_PREC_MAX)
		precision = XUINT(Vmax_real_precision);

	switch (current_type) {
	case INT_T: {
		bigc bc;
		Lisp_Object result;

		bigc_init_prec(bc, precision);

		bigc_set_long(bc, XINT(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
	}

	case BIGZ_T: {
		bigfr bf;
		bigc bc;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);
		bigc_init_prec(bc, precision);

		bigfr_set_bigz(bf, XBIGZ_DATA(number));
		bigc_set_bigfr(bc, bf);
		result = make_bigc_bc(bc);

		bigfr_fini(bf);
		bigc_fini(bc);
		return result;
	}

	case BIGQ_T: {
		bigfr bf;
		bigc bc;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);
		bigc_init_prec(bc, precision);

		bigfr_set_bigq(bf, XBIGQ_DATA(number));
		bigc_set_bigfr(bc, bf);
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		bigfr_fini(bf);
		return result;
	}

	case FLOAT_T: {
		bigc bc;
		Lisp_Object result;

		bigc_init_prec(bc, precision);

		bigc_set_double(bc, XFLOAT_DATA(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
	}

	case BIGF_T: {
		bigfr bf;
		bigc bc;
		Lisp_Object result;

		bigfr_init_prec(bf, precision);
		bigc_init_prec(bc, precision);

		bigfr_set_bigf(bf, XBIGF_DATA(number));
		bigc_set_bigfr(bc, bf);
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		bigfr_fini(bf);
		return result;
	}

	case BIGFR_T: {
#ifdef HAVE_MPFR
		bigc bc;
		Lisp_Object result;

		bigc_init_prec(bc, precision);

		bigc_set_bigfr(bc, XBIGFR_DATA(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
#else
		abort();
		break;
#endif
	}

	case BIGG_T: {
		bigc bc;
		Lisp_Object result, l_bfr_intg, l_bfr_imag;

		bigc_init_prec(bc, precision);
		l_bfr_intg = internal_coerce_to_BIGFR_T(
			make_bigz_bz(bigg_re(XBIGG_DATA(number))),
			precision);
		l_bfr_imag = internal_coerce_to_BIGFR_T(
			make_bigz_bz(bigg_im(XBIGG_DATA(number))),
			precision);

		bigc_set_bigfr_bigfr(bc,
				     XBIGFR_DATA(l_bfr_intg),
				     XBIGFR_DATA(l_bfr_imag));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
	}

	case BIGC_T:
		return number;

	case INDEF_T:
		if (INFINITYP(number))
			return number;
		else
			return wrong_type_argument(Qinfinityp, number);
	}
	abort();
	return Qzero;
#endif	/* HAVE_MPC */
}

static Lisp_Object
internal_coerce_to_INDEF_T (Lisp_Object number, unsigned long precision)
{
	return number;

	if (precision);
}

/* this is not static as it is special in that we have no chance to derive the
   domain from the type, it has to be passed as arg therefore
*/
Lisp_Object
internal_coerce_to_RESC_ELM_T (Lisp_Object number, Lisp_Object domain)
{
#ifndef HAVE_RESCLASS
	abort();

	if (NILP(number) && NILP(domain));
	return Qnil;
#else

	enum number_type current_type;

	if (CHARP(number))
		number = make_int(XCHAR(number));
	else if (MARKERP(number))
		number = make_int(marker_position(number));

	CHECK_NUMBER(number);
	CHECK_RESC_RNG(domain);
	current_type = get_number_type(number);

	switch (current_type) {
	case INT_T:
		return make_resc_elm(XREALINT(number), domain);

	case BIGZ_T:
		return make_resc_elm_bz(XBIGZ_DATA(number), domain);

	default:
		Fsignal(Qdomain_error, Qnil);
		break;
	}
	return Qnil;
#endif
}

/* Convert NUMBER to type TYPE.  If TYPE is BIGF_T then use the indicated
   PRECISION; otherwise, PRECISION is ignored. */
static Lisp_Object
internal_coerce_number (Lisp_Object number,
			enum number_type type,
			unsigned long precision)
{
	switch (type) {
	case INT_T:
		return internal_coerce_to_INT_T(number, precision);
	case BIGZ_T:
		return internal_coerce_to_BIGZ_T(number, precision);
	case BIGQ_T:
		return internal_coerce_to_BIGQ_T(number, precision);
	case FLOAT_T:
		return internal_coerce_to_FLOAT_T(number, precision);
	case BIGF_T:
		return internal_coerce_to_BIGF_T(number, precision);
	case BIGFR_T:
		return internal_coerce_to_BIGFR_T(number, precision);
	case BIGG_T:
		return internal_coerce_to_BIGG_T(number, precision);
	case BIGC_T:
		return internal_coerce_to_BIGC_T(number, precision);
	case INDEF_T:
		return internal_coerce_to_INDEF_T(number, precision);
	case RESC_ELM_T:
		error("cannot coerce number without specifiying domain");
		return Qzero;
	}
	abort();
	return Qzero;
}


/* This function promotes its arguments as necessary to make them both the
 * same type.  It destructively modifies its arguments to do so.  Characters
 * and markers are ALWAYS converted to integers.
 */
static enum number_type ent_coerce_table[NUMBER_OF_TYPES][NUMBER_OF_TYPES];

static void
initialise_coerce_table()
{
	enum number_type type;

	/* any operation is involutionary */
	for (type = INT_T; type < NUMBER_OF_TYPES; type++)
		ent_coerce_table[type][type] = type;

	/* mapping INT_Ts */
	ent_coerce_table[INT_T][BIGZ_T]		= BIGZ_T;
	ent_coerce_table[INT_T][BIGQ_T]		= BIGQ_T;
	ent_coerce_table[INT_T][FLOAT_T]	= FLOAT_T;
	ent_coerce_table[INT_T][BIGF_T]		= BIGF_T;
	ent_coerce_table[INT_T][BIGFR_T]	= BIGFR_T;
	ent_coerce_table[INT_T][BIGG_T]		= BIGG_T;
	ent_coerce_table[INT_T][BIGC_T]		= BIGC_T;
	ent_coerce_table[INT_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[INT_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[INT_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[INT_T][RESC_ELM_T]	= RESC_ELM_T;

	/* mapping BIGZ_Ts */
	ent_coerce_table[BIGZ_T][INT_T]		= BIGZ_T;
	ent_coerce_table[BIGZ_T][BIGQ_T]	= BIGQ_T;
	ent_coerce_table[BIGZ_T][FLOAT_T]	= FLOAT_T;
	ent_coerce_table[BIGZ_T][BIGF_T]	= BIGF_T;
	ent_coerce_table[BIGZ_T][BIGFR_T]	= BIGFR_T;
	ent_coerce_table[BIGZ_T][BIGG_T]	= BIGG_T;
	ent_coerce_table[BIGZ_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[BIGZ_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGZ_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGZ_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGZ_T][RESC_ELM_T]	= RESC_ELM_T;

	/* mapping BIGQ_Ts */
	ent_coerce_table[BIGQ_T][INT_T]		= BIGQ_T;
	ent_coerce_table[BIGQ_T][BIGZ_T]	= BIGQ_T;
	ent_coerce_table[BIGQ_T][FLOAT_T]	= FLOAT_T;
	ent_coerce_table[BIGQ_T][BIGF_T]	= BIGF_T;
	ent_coerce_table[BIGQ_T][BIGFR_T]	= BIGFR_T;
	ent_coerce_table[BIGQ_T][BIGG_T]	= BIGC_T;
	ent_coerce_table[BIGQ_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[BIGQ_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGQ_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGQ_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGQ_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping FLOAT_Ts */
	ent_coerce_table[FLOAT_T][INT_T]	= FLOAT_T;
	ent_coerce_table[FLOAT_T][BIGZ_T]	= FLOAT_T;
	ent_coerce_table[FLOAT_T][BIGQ_T]	= FLOAT_T;
	ent_coerce_table[FLOAT_T][BIGF_T]	= BIGF_T;
	ent_coerce_table[FLOAT_T][BIGFR_T]	= BIGFR_T;
	ent_coerce_table[FLOAT_T][BIGG_T]	= BIGC_T;
	ent_coerce_table[FLOAT_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[FLOAT_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[FLOAT_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[FLOAT_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[FLOAT_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping BIGF_Ts */
	ent_coerce_table[BIGF_T][INT_T]		= BIGF_T;
	ent_coerce_table[BIGF_T][BIGZ_T]	= BIGF_T;
	ent_coerce_table[BIGF_T][BIGQ_T]	= BIGF_T;
	ent_coerce_table[BIGF_T][FLOAT_T]	= BIGF_T;
	ent_coerce_table[BIGF_T][BIGFR_T]	= BIGFR_T;
	ent_coerce_table[BIGF_T][BIGG_T]	= BIGC_T;
	ent_coerce_table[BIGF_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[BIGF_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGF_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGF_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGF_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping BIGFR_Ts */
	ent_coerce_table[BIGFR_T][INT_T]	= BIGFR_T;
	ent_coerce_table[BIGFR_T][BIGZ_T]	= BIGFR_T;
	ent_coerce_table[BIGFR_T][BIGQ_T]	= BIGFR_T;
	ent_coerce_table[BIGFR_T][FLOAT_T]	= BIGFR_T;
	ent_coerce_table[BIGFR_T][BIGF_T]	= BIGFR_T;
	ent_coerce_table[BIGFR_T][BIGG_T]	= BIGC_T;
	ent_coerce_table[BIGFR_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[BIGFR_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGFR_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGFR_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGFR_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping BIGG_Ts */
	ent_coerce_table[BIGG_T][INT_T]		= BIGG_T;
	ent_coerce_table[BIGG_T][BIGZ_T]	= BIGG_T;
	ent_coerce_table[BIGG_T][BIGQ_T]	= BIGC_T;
	ent_coerce_table[BIGG_T][FLOAT_T]	= BIGC_T;
	ent_coerce_table[BIGG_T][BIGF_T]	= BIGC_T;
	ent_coerce_table[BIGG_T][BIGFR_T]	= BIGC_T;
	ent_coerce_table[BIGG_T][BIGC_T]	= BIGC_T;
	ent_coerce_table[BIGG_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGG_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGG_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGG_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping BIGC_Ts */
	ent_coerce_table[BIGC_T][INT_T]		= BIGC_T;
	ent_coerce_table[BIGC_T][BIGZ_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][BIGQ_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][FLOAT_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][BIGF_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][BIGFR_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][BIGG_T]	= BIGC_T;
	ent_coerce_table[BIGC_T][QUATERN_T]	= QUATERN_T;
	ent_coerce_table[BIGC_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[BIGC_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[BIGC_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping QUATERN_Ts */
	ent_coerce_table[QUATERN_T][INT_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGZ_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGQ_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][FLOAT_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGF_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGFR_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGG_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][BIGC_T]	= QUATERN_T;
	ent_coerce_table[QUATERN_T][OCTON_T]	= OCTON_T;
	ent_coerce_table[QUATERN_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[QUATERN_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping OCTON_Ts */
	ent_coerce_table[OCTON_T][INT_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGZ_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGQ_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][FLOAT_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGF_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGFR_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGG_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][BIGC_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][QUATERN_T]	= OCTON_T;
	ent_coerce_table[OCTON_T][INDEF_T]	= INDEF_T;
	ent_coerce_table[OCTON_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping INDEFS */
	ent_coerce_table[INDEF_T][INT_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGZ_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGQ_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][FLOAT_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGF_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGFR_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGG_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][BIGC_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][QUATERN_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][OCTON_T]	= INDEF_T;
	ent_coerce_table[INDEF_T][RESC_ELM_T]	= IMPOSSIBLE;

	/* mapping RESCLASS ELMS */
	ent_coerce_table[RESC_ELM_T][INT_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGZ_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGQ_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][FLOAT_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGF_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGFR_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGG_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][BIGC_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][QUATERN_T]	= IMPOSSIBLE;
	ent_coerce_table[RESC_ELM_T][OCTON_T]	= IMPOSSIBLE;
}

enum number_type
promote_args (Lisp_Object *arg1, Lisp_Object *arg2)
{
	enum number_type type1, type2, typelcm;

	if (CHARP(*arg1))
		*arg1 = make_int(XCHAR(*arg1));
	else if (MARKERP(*arg1))
		*arg1 = make_int(marker_position(*arg1));
	if (CHARP(*arg2))
		*arg2 = make_int(XCHAR(*arg2));
	else if (MARKERP(*arg2))
		*arg2 = make_int(marker_position(*arg2));

	CHECK_NUMBER(*arg1);
	CHECK_NUMBER(*arg2);

	type1 = get_number_type(*arg1);
	type2 = get_number_type(*arg2);

	typelcm = ent_coerce_table[type1][type2];

	if (typelcm == IMPOSSIBLE)
		return typelcm;

#ifdef HAVE_RESCLASS
	if (typelcm == RESC_ELM_T) {
		/* now we have to dig a little, since RESC_ELMs can be defined
		   over different rings, which makes it hard to promote them
		*/
		if (type1 == RESC_ELM_T && type2 == RESC_ELM_T &&
		    !bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(*arg1)),
			      XRESC_RNG_DATA(XRESC_ELM_RING(*arg2))))
			return IMPOSSIBLE;

		if (typelcm != type1)
			*arg1 = internal_coerce_to_RESC_ELM_T(
				*arg1, XRESC_ELM_RING(*arg2));
		if (typelcm != type2)
			*arg2 = internal_coerce_to_RESC_ELM_T(
				*arg2, XRESC_ELM_RING(*arg1));

		return typelcm;
	}
#endif

	if (typelcm != type1)
		*arg1 = internal_coerce_number(*arg1, typelcm, 0UL);
	if (typelcm != type2)
		*arg2 = internal_coerce_number(*arg2, typelcm, 0UL);

	return typelcm;
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

	CHECK_NUMBER(number);

	switch (get_number_type(number)) {
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
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = make_resc_elm(0L, XRESC_ELM_RING(number));
		break;
#endif
	case INDEF_T:
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

	CHECK_NUMBER(number);

	switch (get_number_type(number)) {
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
#ifdef HAVE_RESCLASS
	case RESC_ELM_T:
		result = make_resc_elm(1L, XRESC_ELM_RING(number));
		break;
#endif
	case INDEF_T:
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
#ifdef HAVE_MPFR
	INIT_LRECORD_IMPLEMENTATION(bigfr);
#endif
#ifdef HAVE_MPC
	INIT_LRECORD_IMPLEMENTATION(bigc);
#endif
#ifdef HAVE_PSEUG
	INIT_LRECORD_IMPLEMENTATION(bigg);
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

	DEFSUBR(Fresidue_class_ring_p);
	DEFSUBR(Fresidue_class_p);
#ifdef HAVE_RESCLASS
	DEFSUBR(Fmake_residue_class_ring);
	DEFSUBR(Fmake_residue_class);
	DEFSUBR(Fresidue_class_ring);
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
#ifdef HAVE_MPFR
	Fprovide(intern("bigfr"));
#endif
#if defined(HAVE_MPC) || defined(HAVE_PSEUC)
	Fprovide(intern("bigc"));
#endif
#ifdef HAVE_PSEUG
	Fprovide(intern("bigg"));
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
	Vnot_a_number = Qnil;
	Vpinfinity = Qnil;
	Vninfinity = Qnil;
	Vcomplex_infinity = Qnil;

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
		}
		ent_optable_neg[i] = ent_neg_undefined;
		ent_optable_inv[i] = ent_inv_undefined;
	}
}

void init_number(void)
{
	initialise_coerce_table();

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
#ifdef HAVE_RESCLASS
		init_ent_resclass();
#endif
#ifdef WITH_NUMBER_TYPES
		init_ent_indef();
#endif

	}

}
