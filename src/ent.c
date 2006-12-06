/*
  ent.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005, 2006 Sebastian Freundt

  XEmacs Author:  Jerry James
  Author: Sebastian Freundt
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

#include "ent.h"

Lisp_Object Qrationalp, Qrealp, Qcomparablep;
Lisp_Object Qarchimedeanp, Qnonarchimedeanp;
Lisp_Object Qcomplexp, Qgaussianp;

/* errors */
Lisp_Object Qoperation_error, Qrelation_error, Qvaluation_error;

Lisp_Object Vread_real_as;
Fixnum default_real_precision;
Fixnum max_real_precision;
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
#ifdef HAVE_FPFLOAT
/* I wanted to define the lrecord implementation here, but that breaks at
 * Steve's site, so ... :(
 */
#endif


/********************************** Bigfrs **********************************/

#ifndef MPFR_PREC_MIN
#define MPFR_PREC_MIN 2UL
#endif
#ifndef MPFR_PREC_MAX
#define MPFR_PREC_MAX 1024UL
#endif

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
Lisp_Object Qbiggp;

DEFUN ("biggp", Fbiggp, 1, 1, 0, /*
Return t if OBJECT is a bigg (a gaussian number), nil otherwise.
				 */
       (object))
{
	return BIGGP(object) ? Qt : Qnil;
}


/********************************** Bigcs ***********************************/
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
Lisp_Object Qquaternp;

DEFUN ("quaternp", Fquaternp, 1, 1, 0, /*
Return t if OBJECT is a quaternion, nil otherwise.
				       */
       (object))
{
	return QUATERNP(object) ? Qt : Qnil;
}



/***************************** Residue Class Rings **************************/
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
Lisp_Object Qinfinityp, Qindefinitep;

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
#ifdef HAVE_FPFLOAT
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
		return default_real_precision;
	if (precision > MPFR_PREC_MAX)
		return max_real_precision;

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

	if (NILP(precision) && default_real_precision > 0) {
		susp_prec = default_real_precision;
	} else if (INTP(precision)) {
		susp_prec = XUINT(precision);
#ifdef HAVE_MPZ
	} else if (BIGZP(precision)) {
		if (!bigz_fits_ulong_p(XBIGZ_DATA(precision)))
			susp_prec = max_real_precision;
		else
			susp_prec = bigz_to_ulong(XBIGZ_DATA(precision));
#endif
	} else {
		susp_prec = default_real_precision;
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

- 'quatern            to convert to a Quaternion

NOTE: Not all of these types may be supported.

PRECISION is the number of bits of precision to use when converting to
reals; it is ignored otherwise.  If nil, the default precision is used.

Note that some conversions lose information.  No error is signaled in such
cases; the information is silently lost.
						 */
       (number, type, precision))
{
	CHECK_SYMBOL(type);
	ent_normalise_number(number);

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
#ifdef HAVE_FPFLOAT
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
#endif /* HAVE_MPC */
#ifdef HAVE_QUATERN
	else if (EQ(type, Qquatern)) {
		unsigned long prec;
		prec = internal_get_precision(precision);
		return internal_coerce_number(number, QUATERN_T, prec);
	}
#endif /* HAVE_QUATERN */

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
#ifdef HAVE_FPFLOAT
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
#ifdef HAVE_FPFLOAT
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
#ifdef HAVE_FPFLOAT
	case FLOAT_T:
		result = XFLOAT_DATA(number) == 1.0 ? Qt : Qnil;
		break;
#endif
#ifdef HAVE_MPF
	case BIGF_T:
		result = bigf_to_fpfloat(XBIGF_DATA(number)) == 1.0 ? Qt : Qnil;
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
#ifdef HAVE_FPFLOAT
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


void syms_of_ent(void)
{
	syms_of_ent_int();
	syms_of_ent_indef();

#ifdef HAVE_MPZ
	syms_of_ent_mpz();
#endif
#ifdef HAVE_MPQ
	syms_of_ent_mpq();
#endif
#ifdef HAVE_MPF
	syms_of_ent_mpf();
#endif
#ifdef HAVE_FPFLOAT
	syms_of_ent_float();
#endif
#ifdef HAVE_MPFR
	syms_of_ent_mpfr();
#endif
#ifdef HAVE_MPC
	syms_of_ent_mpc();
#endif
#ifdef HAVE_PSEUG
	syms_of_ent_gaussian();
#endif
#ifdef HAVE_QUATERN
	syms_of_ent_quatern();
#endif
#ifdef HAVE_RESCLASS
	syms_of_ent_resclass();
#endif

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

	defsymbol(&Qresc_rngp, "residue-class-ring-p");
	defsymbol(&Qresc_elmp, "residue-class-p");

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

	DEFSUBR(Fbigfrp);

	DEFSUBR(Frealp);
	DEFSUBR(Freal);
	DEFSUBR(Fcomparablep);

	DEFSUBR(Fbiggp);

	DEFSUBR(Fbigcp);
	DEFSUBR(Fcomplexp);

	DEFSUBR(Fquaternp);

	DEFSUBR(Fresidue_class_ring_p);
	DEFSUBR(Fresidue_class_p);

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

void vars_of_ent(void)
{
	Fprovide(intern("number-types"));
	Fprovide(intern("ent"));

	vars_of_ent_int();
	vars_of_ent_indef();

#ifdef HAVE_MPZ
	vars_of_ent_mpz();
#endif
#ifdef HAVE_MPQ
	vars_of_ent_mpq();
#endif
#ifdef HAVE_MPF
	vars_of_ent_mpf();
#endif
#ifdef HAVE_FPFLOAT
	vars_of_ent_float();
#endif
#ifdef HAVE_MPFR
	vars_of_ent_mpfr();
#endif
#if defined(HAVE_MPC) || defined(HAVE_PSEUC)
	vars_of_ent_mpc();
#endif
#ifdef HAVE_PSEUG
	vars_of_ent_gaussian();
#endif
#ifdef HAVE_QUATERN
	vars_of_ent_quatern();
#endif
#ifdef HAVE_RESCLASS
	vars_of_ent_resclass();
#endif

	max_real_precision = EMACS_INT_MAX;

	DEFVAR_CONST_INT("max-real-precision", &max_real_precision /*
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

	default_real_precision = 128;
	DEFVAR_INT_MAGIC("default-real-precision",
			 &default_real_precision, /*
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

void init_ent(void)
{
	initialise_operation_tables();

	init_optables_INDEF_T();
	init_optables_INT_T();
#ifdef HAVE_FPFLOAT
	init_optables_FLOAT_T();
#endif
#ifdef HAVE_MPZ
	init_optables_BIGZ_T();
#endif
#ifdef HAVE_MPQ
	init_optables_BIGQ_T();
#endif
#ifdef HAVE_MPF
	init_optables_BIGF_T();
#endif
#ifdef HAVE_MPFR
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

	if (!number_initialized)
	{
		number_initialized = 1;

		init_ent_int();
		init_ent_indef();
#ifdef HAVE_FPFLOAT
		init_ent_float();
#endif
#ifdef HAVE_MPZ
		init_ent_mpz();
#endif
#ifdef HAVE_MPQ
		init_ent_mpq();
#endif
#ifdef HAVE_MPF
		init_ent_mpf();
#endif
#ifdef HAVE_MPFR
		init_ent_mpfr();
#endif
#ifdef HAVE_MPC
		init_ent_mpc();
#endif
#ifdef HAVE_PSEUG
		init_ent_gaussian();
#endif
#ifdef HAVE_QUATERN
		init_ent_quatern();
#endif
#ifdef HAVE_RESCLASS
		init_ent_resclass();
#endif
	}

}
