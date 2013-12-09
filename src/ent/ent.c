/*
  ent.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James
  Copyright (C) 2004, 2005, 2006 Sebastian Freundt

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


#include <config.h>
#include <limits.h>
#include "lisp.h"
#include "dynacat.h"

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
Lisp_Object Qoptable_index;

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
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
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
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
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
#if defined HAVE_MPQ && defined WITH_GMP
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
#if defined HAVE_MPQ && defined WITH_GMP
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
default_real_precision_changed (Lisp_Object SXE_UNUSED(sym), Lisp_Object *val,
				Lisp_Object SXE_UNUSED(in_object),
				int SXE_UNUSED(flags))
{
	unsigned long prec;

	CONCHECK_INTEGER(*val);
	prec = internal_get_precision(*val);
#if defined HAVE_MPF && defined WITH_GMP
	if (prec != 0UL)
		bigf_set_default_prec(prec);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	if (prec != 0UL)
		bigfr_set_default_prec(prec);
#endif
	return 0;
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
#if defined HAVE_MPFR && defined WITH_MPFR
		return Fcoerce_number(number, Qbigfr, precision);
#else  /* !HAVE_MPFR */
		;
#endif	/* HAVE_MPFR */
	}

	if (Vread_real_as == Qbigf) {
#if defined HAVE_MPF && defined WITH_GMP
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
Return the canonical form of NUMBER.  DEPRECATED FUNCTION.
*/
       (number))
{
	/* The tests should go in order from larger, more expressive, or more
	   complex types to smaller, less expressive, or simpler types so that a
	   number can cascade all the way down to the simplest type if
	   appropriate. */
#if defined HAVE_MPQ && defined WITH_GMP
	if (BIGQP(number))
		return ent_mpq_downgrade_maybe(XBIGQ_DATA(number));
#endif
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (BIGZP(number))
		return ent_mpz_downgrade_maybe(XBIGZ_DATA(number));
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number))
		return ent_mpfr_wipe_indef(XBIGFR_DATA(number));
#endif
	return number;
}

/* new ase optable magic */
static dllist_t ase_optable_freelist;

static inline void
ase_optable_freelist_init(void)
{
	long int i;
	ase_optable_freelist = make_noseeum_dllist();
	for (i = 0; i < lrecord_first_ent_type; i++) {
		dllist_append(ase_optable_freelist, (void*)i);
	}
}

int ase_optable_add(Lisp_Object typesym)
{
	/* TYPESYM should be a symbol as used in dynacats */
	long int *foo = dllist_pop_car(ase_optable_freelist);
	long int idx = (long int)foo;
	Fput(typesym, Qoptable_index, make_int(idx));
	return idx;
}

void ase_optable_del(Lisp_Object typesym)
{
	/* TYPESYM should be a symbol as used in dynacats */
	long int idx = (long int)ase_optable_index_typesym(typesym);
	dllist_append(ase_optable_freelist, (void*)idx);
	return;
}

int ase_optable_index(Lisp_Object arg)
{
	switch ((unsigned int)XTYPE(arg)) {
	case Lisp_Type_Record: {
		enum lrecord_type type =
			XRECORD_LHEADER_IMPLEMENTATION(arg)->lrecord_type_index;

		switch ((unsigned int)type) {
		case lrecord_type_marker:
			return INT_T;
		case lrecord_type_dynacat:
			assert(SYMBOLP(XDYNACAT_TYPE(arg)));
			/* must be an dynacat */
			/* now we've got two options, either compute a
			 * hash-value from the symbol's address
			 * or store a cookie in the plist of the symbol
			 * for the moment, we prefer the latter option
			 */
			return ase_optable_index_typesym(XDYNACAT_TYPE(arg));
		default:
			return type;
		}
	}
	default:
		return INT_T;
	}
	return -1; /* Should not reach here */
}

int
ase_optable_index_typesym(Lisp_Object typesym)
{
	Lisp_Object idx = Fget(typesym, Qoptable_index, Qnil);
	assert(INTEGERP(idx));
	return XINT(idx);
}

/* categorial subtleties */
dllist_t ase_empty_sets = 0;
Lisp_Object Qase_empty_sets;


#if 0
inline Lisp_Object
ent_normalise_number(Lisp_Object number)
{
	if (CHARP(number))
		return make_int(XCHAR(number));
	else if (MARKERP(number))
		return make_int(marker_position(number));

	return number;
}
#endif

unsigned long ent_normalise_precision(unsigned long precision)
{
	/* MPFR will slaughter us when we pass a precision < MPFR_PREC_MIN */
	if (precision < MPFR_PREC_MIN)
		return default_real_precision;
	if (precision > MPFR_PREC_MAX)
		return max_real_precision;

	return precision;
}

/* Convert NUMBER to type TYPE.  If TYPE is BIGF_T then use the indicated
   PRECISION; otherwise, PRECISION is ignored. */
static Lisp_Object
internal_coerce_number (Lisp_Object o,
			ase_object_type_t type,
			unsigned long precision)
{
	struct ent_lift_args_s la;

	la.precision = ent_normalise_precision(precision);

	return ent_lift(o, type, &la);
}


unsigned long
internal_get_precision(Lisp_Object precision)
{
	unsigned long susp_prec = 0;

	if (NILP(precision) && default_real_precision > 0) {
		susp_prec = default_real_precision;
	} else if (INTP(precision)) {
		susp_prec = XUINT(precision);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
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
	struct ent_lift_args_s la;

	CHECK_SYMBOL(type);

	if (EQ(type, Qint) || EQ(type, Qfixnum))
		return internal_coerce_number(number, INT_T, 0UL);
	else if (EQ(type, Qinteger)) {
		/* If bignums are available, we always convert to one first,
		   then downgrade to a int if possible. */
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		return Fcanonicalize_number(
			ent_lift(number, BIGZ_T, NULL));
#else
		return ent_lift(number, INT_T, NULL);
#endif	/* HAVE_MPZ */
	}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	else if (EQ(type, Qbigz) || EQ(type, Qbignum)) {
		/* always convert to bigz */
		return ent_lift(number, BIGZ_T, NULL);
	}
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	else if (EQ(type, Qrational)) {
		/* convert to bigq and canonicalise */
		return Fcanonicalize_number(
			ent_lift(number, BIGQ_T, NULL));

	} else if (EQ(type, Qbigq) || EQ(type, Qratio)) {
		/* always convert to bigq */
		return ent_lift(number, BIGQ_T, NULL);
	}
#endif	/* HAVE_MPQ */
#ifdef HAVE_FPFLOAT
	else if (EQ(type, Qfloat))
		return ent_lift(number, FLOAT_T, NULL);
#endif
#if defined HAVE_MPF && defined WITH_GMP
	else if (EQ(type, Qbigf) || EQ(type, Qbigfloat)) {
		la.precision = internal_get_precision(precision);
		return ent_lift(number, BIGF_T, &la);
	}
#endif /* HAVE_MPF */
#if defined HAVE_MPFR && defined WITH_MPFR
	else if (EQ(type, Qbigfr)) {
		la.precision = internal_get_precision(precision);
		return ent_lift(number, BIGFR_T, &la);
	}
#endif /* HAVE_MPFR */
	else if (EQ(type, Qreal)) {
		/* respect `read-real-as' */
		la.precision = internal_get_precision(precision);
		if (0);
#if defined HAVE_MPF && defined WITH_GMP
		else if(Vread_real_as == Qbigf)
			return ent_lift(number, BIGF_T, &la);
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
		else if (Vread_real_as == Qbigfr)
			return ent_lift(number, BIGFR_T, &la);
#endif
		else
			return ent_lift(number, FLOAT_T, &la);
	}
#if defined(HAVE_PSEUG) && defined WITH_PSEUG
	else if (EQ(type, Qbigg)) { /* || EQ(type, Qcomplex)) { */
		return ent_lift(number, BIGG_T, NULL);
	}
#endif	/* HAVE_PSEUG */
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	else if (EQ(type, Qbigc)) { /* || EQ(type, Qcomplex)) { */
		la.precision = internal_get_precision(precision);
		return ent_lift(number, BIGC_T, &la);
	}
#endif /* HAVE_MPC */
#if defined HAVE_QUATERN && defined WITH_QUATERN
	else if (EQ(type, Qquatern)) {
		la.precision = internal_get_precision(precision);
		return ent_lift(number, QUATERN_T, &la);
	}
#endif /* HAVE_QUATERN */

	Fsignal(Qunsupported_type, Qnil);
	/* NOTREACHED */
	return Qnil;
}


/************************ Auxiliary Stuff **************************/

DEFUN("dump-ase-types", Fdump_ase_types, 0, 0, 0, /*
*/
      ())
{
	ENT_CRITICAL("int:64\n");
	ENT_CRITICAL("bigz:%d\n", lrecord_type_bigz);
	ENT_CRITICAL("bigq:%d\n", lrecord_type_bigq);
	ENT_CRITICAL("bigf:%d\n", lrecord_type_bigf);
	ENT_CRITICAL("bigfr:%d\n", lrecord_type_bigfr);
	ENT_CRITICAL("float:%d\n", lrecord_type_float);
	ENT_CRITICAL("bigg:%d\n", lrecord_type_bigg);
	ENT_CRITICAL("bigc:%d\n", lrecord_type_bigc);
	ENT_CRITICAL("quatern:%d\n", lrecord_type_quatern);
	ENT_CRITICAL("indef:%d\n", lrecord_type_indef);

	ENT_CRITICAL("last:%d\n", lrecord_type_last_built_in_type);

	return Qt;
}


/******************************* op tables ****************************/

static inline void
initialise_operation_tables(void)
{
	/* new optable magic */
	ase_optable_freelist_init();
	ase_nullary_optable_init();
	ase_unary_optable_init();
	ase_binary_optable_init();
	ase_unary_reltable_init();
	ase_binary_reltable_init();
	ase_lifttable_init();
}

void init_ent_optables(void)
{
	initialise_operation_tables();

	init_optables_INDEF_T();
	init_optables_INT_T();
#ifdef HAVE_FPFLOAT
	init_optables_FLOAT_T();
#endif
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	init_optables_BIGZ_T();
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	init_optables_BIGQ_T();
#endif
#if defined HAVE_MPF && defined WITH_GMP
	init_optables_BIGF_T();
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	init_optables_BIGFR_T();
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	init_optables_BIGC_T();
#endif
#if defined HAVE_PSEUG && defined WITH_PSEUG
	init_optables_BIGG_T();
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
	init_optables_QUATERN_T();
#endif
}


void syms_of_ent(void)
{
	syms_of_ent_int();
	syms_of_ent_indef();

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	syms_of_ent_mpz();
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	syms_of_ent_mpq();
#endif
#if defined HAVE_MPF && defined WITH_GMP
	syms_of_ent_mpf();
#endif
#ifdef HAVE_FPFLOAT
	syms_of_ent_float();
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	syms_of_ent_mpfr();
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	syms_of_ent_mpc();
#endif
#if defined HAVE_PSEUG && defined WITH_PSEUG
	syms_of_ent_gaussian();
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
	syms_of_ent_quatern();
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

	DEFSUBR(Farchimedeanp);
	DEFSUBR(Fnonarchimedeanp);

	DEFSUBR(Finfinityp);
	DEFSUBR(Findefinitep);

	DEFSUBR(Fcanonicalize_number);
	DEFSUBR(Fcoerce_number);

	DEFSUBR(Fdump_ase_types);

	DEFSYMBOL(Qoptable_index);

	/* Errors */
	DEFERROR_STANDARD(Qunsupported_type, Qwrong_type_argument);

	/* Operation Tables */
	init_ent_optables();
	syms_of_ent_nullary_op();
	syms_of_ent_unary_op();
	syms_of_ent_binary_op();
	syms_of_ent_unary_rel();
	syms_of_ent_binary_rel();
	syms_of_ent_lift();
}

void vars_of_ent(void)
{
	Fprovide(intern("number-types"));
	Fprovide(intern("ent"));

	vars_of_ent_int();
	vars_of_ent_indef();

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	vars_of_ent_mpz();
#endif
#if defined HAVE_MPQ && defined WITH_GMP
	vars_of_ent_mpq();
#endif
#if defined HAVE_MPF && defined WITH_GMP
	vars_of_ent_mpf();
#endif
#ifdef HAVE_FPFLOAT
	vars_of_ent_float();
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
	vars_of_ent_mpfr();
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined(HAVE_PSEUC) && defined WITH_PSEUC
	vars_of_ent_mpc();
#endif
#if defined HAVE_PSEUG && defined WITH_PSEUG
	vars_of_ent_gaussian();
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
	vars_of_ent_quatern();
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

	vars_of_ent_nullary_op();
	vars_of_ent_unary_op();
	vars_of_ent_binary_op();
	vars_of_ent_unary_rel();
	vars_of_ent_binary_rel();
	vars_of_ent_lift();
}

void init_ent(void)
{
	init_ent_optables();

	if (!number_initialized) {
		number_initialized = 1;

		init_ent_int();
		init_ent_indef();
#ifdef HAVE_FPFLOAT
		init_ent_float();
#endif
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		init_ent_mpz();
#endif
#if defined HAVE_MPQ && defined WITH_GMP
		init_ent_mpq();
#endif
#if defined HAVE_MPF && defined WITH_GMP
		init_ent_mpf();
#endif
#if defined HAVE_MPFR && defined WITH_MPFR
		init_ent_mpfr();
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
		init_ent_mpc();
#endif
#if defined HAVE_PSEUG && defined WITH_PSEUG
		init_ent_gaussian();
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
		init_ent_quatern();
#endif
	}

	/* promote our empty sets */
	ase_empty_sets = make_noseeum_dllist();
}

/* ent.c ends here */
