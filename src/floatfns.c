/* Primitive operations on floating point for SXEmacs Lisp interpreter.
   Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30. */

/* ANSI C requires only these float functions:
   acos, asin, atan, atan2, ceil, cos, cosh, exp, fabs, floor, fmod,
   frexp, ldexp, log, log10, modf, pow, sin, sinh, sqrt, tan, tanh.

   Define HAVE_INVERSE_HYPERBOLIC if you have acosh, asinh, and atanh.
   Define HAVE_CBRT if you have cbrt().
   Define HAVE_RINT if you have rint().
   If you don't define these, then the appropriate routines will be simulated.

   Define HAVE_MATHERR if on a system supporting the SysV matherr() callback.
   (This should happen automatically.)

   Define FLOAT_CHECK_ERRNO if the float library routines set errno.
   This has no effect if HAVE_MATHERR is defined.

   Define FLOAT_CATCH_SIGILL if the float library routines signal SIGILL.
   (What systems actually do this?  Let me know. -jwz)

   Define FLOAT_CHECK_DOMAIN if the float library doesn't handle errors by
   either setting errno, or signalling SIGFPE/SIGILL.  Otherwise, domain and
   range checking will happen before calling the float routines.  This has
   no effect if HAVE_MATHERR is defined (since matherr will be called when
   a domain error occurs).
 */

#include <config.h>
#include "lisp.h"
#include "syssignal.h"

#ifdef LISP_FLOAT_TYPE

/* Need to define a differentiating symbol -- see sysfloat.h */
#define THIS_FILENAME floatfns
#include "sysfloat.h"

/* The code uses emacs_rint, so that it works to undefine HAVE_RINT
   if `rint' exists but does not work right.  */
#ifdef HAVE_RINT
#define emacs_rint rint
#else
static double emacs_rint(double x)
{
	double r = floor(x + 0.5);
	double diff = fabs(r - x);
	/* Round to even and correct for any roundoff errors.  */
	if (diff >= 0.5 && (diff > 0.5 || r != 2.0 * floor(r / 2.0)))
		r += r < x ? 1.0 : -1.0;
	return r;
}
#endif

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */
static int in_float;

/* If an argument is out of range for a mathematical function,
   here is the actual argument value to use in the error message.  */
static Lisp_Object float_error_arg, float_error_arg2;
static const char *float_error_fn_name;

/* Evaluate the floating point expression D, recording NUM
   as the original argument for error messages.
   D is normally an assignment expression.
   Handle errors which may result in signals or may set errno.

   Note that float_error may be declared to return void, so you can't
   just cast the zero after the colon to (SIGTYPE) to make the types
   check properly.  */
#ifdef FLOAT_CHECK_ERRNO
#define IN_FLOAT(d, name, num)				\
  do {							\
    float_error_arg = num;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    if (errno != 0) in_float_error ();			\
  } while (0)
#define IN_FLOAT2(d, name, num, num2)			\
  do {							\
    float_error_arg = num;				\
    float_error_arg2 = num2;				\
    float_error_fn_name = name;				\
    in_float = 2; errno = 0; (d); in_float = 0;		\
    if (errno != 0) in_float_error ();			\
  } while (0)
#else
#define IN_FLOAT(d, name, num) (in_float = 1, (d), in_float = 0)
#define IN_FLOAT2(d, name, num, num2) (in_float = 2, (d), in_float = 0)
#endif

#define arith_error(op,arg) \
  Fsignal (Qarith_error, list2 (build_string (op), arg))
#define range_error(op,arg) \
  Fsignal (Qrange_error, list2 (build_string (op), arg))
#define range_error2(op,a1,a2) \
  Fsignal (Qrange_error, list3 (build_string (op), a1, a2))
#define domain_error(op,arg) \
  Fsignal (Qdomain_error, list2 (build_string (op), arg))
#define domain_error2(op,a1,a2) \
  Fsignal (Qdomain_error, list3 (build_string (op), a1, a2))

/* Convert float to Lisp Integer if it fits, else signal a range
   error using the given arguments.
   If numbers from multi-prec libraries are available, range errors
   are never signaled.
*/
static Lisp_Object
float_to_int(double x, const char *name, Lisp_Object num, Lisp_Object num2)
{
#ifdef HAVE_MPZ
	Lisp_Object result;
	bigz bz;
	bigz_init(bz);

	bigz_set_double(bz, x);
	result = Fcanonicalize_number(make_bigz_bz(bz));

	bigz_fini(bz);
	return result;

	if (name || NILP(num) || NILP(num2));
	return Qnil;
#else  /* !HAVE_MPZ */
	REGISTER EMACS_INT result = (EMACS_INT) x;

	if (result > EMACS_INT_MAX || result < EMACS_INT_MIN) {
		if (!UNBOUNDP(num2))
			range_error2(name, num, num2);
		else
			range_error(name, num);
	}
	return make_int(result);
#endif	/* HAVE_MPZ */
}

static void in_float_error(void)
{
	switch (errno) {
	case 0:
		break;
	case EDOM:
		if (in_float == 2)
			domain_error2(float_error_fn_name, float_error_arg,
				      float_error_arg2);
		else
			domain_error(float_error_fn_name, float_error_arg);
		break;
	case ERANGE:
		range_error(float_error_fn_name, float_error_arg);
		break;
	default:
		arith_error(float_error_fn_name, float_error_arg);
		break;
	}
}

static Lisp_Object mark_float(Lisp_Object obj)
{
	return Qnil;

	if (obj);
}

static int float_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return (extract_float(obj1) == extract_float(obj2));

	if (depth);
}

static unsigned long float_hash(Lisp_Object obj, int depth)
{
	/* mod the value down to 32-bit range */
	/* #### change for 64-bit machines */
	return (unsigned long)fmod(extract_float(obj), 4e9);

	if (depth);
}

static const struct lrecord_description float_description[] = {
	{XD_END}
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("float", float,
				    mark_float, print_float, 0, float_equal,
				    float_hash, float_description, Lisp_Float);

/* Extract a Lisp number as a `double', or signal an error.  */

double extract_float(Lisp_Object num)
{
	if (FLOATP(num))
		return XFLOAT_DATA(num);

	if (INTP(num))
		return (double)XINT(num);

#ifdef HAVE_MPZ
	if (BIGZP(num))
		return bigz_to_double(XBIGZ_DATA(num));
#endif

#ifdef HAVE_MPQ
	if (BIGQP(num))
		return bigq_to_double(XBIGQ_DATA(num));
#endif

#ifdef HAVE_MPF
	if (BIGFP(num))
		return bigf_to_double(XBIGF_DATA(num));
#endif

#ifdef HAVE_MPFR
	if (BIGFRP(num))
		return bigfr_to_double(XBIGFR_DATA(num));
#endif

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(num))
		return extract_float(wrong_type_argument(Qnumberp, num));
#endif

#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
	return extract_float(wrong_type_argument(Qcomparablep, num));
#else
	return extract_float(wrong_type_argument(Qnumberp, num));
#endif	/* HAVE_MPC */
}
#endif  /* LISP_FLOAT_TYPE */

/* Trig functions.  */

#ifdef HAVE_MPFR
#define MPFR_TRIG_FUN(op) do						\
{									\
	bigfr bfr;							\
	Lisp_Object result;						\
	Lisp_Object bfrnumber;						\
									\
	if (INDEFP(number))						\
		return make_indef(NOT_A_NUMBER);			\
									\
	BIGFR_INIT_PREC(bfr, precision);				\
									\
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);		\
	bigfr_##op(bfr, XBIGFR_DATA(bfrnumber));			\
	result = make_bigfr_bfr(bfr);					\
									\
	bigfr_fini(bfr);						\
	return result;							\
} while (0)
#endif

#if defined(HAVE_MPFR) || defined(LISP_FLOAT_TYPE)

DEFUN("acos", Facos, 1, 2, 0,	/*
Return the inverse cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(acos);

#else  /* !HAVE_MPFR */
	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d > 1.0 || d < -1.0)
		domain_error("acos", number);
#endif
	IN_FLOAT(d = acos(d), "acos", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("asin", Fasin, 1, 2, 0,	/*
Return the inverse sine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(asin);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d > 1.0 || d < -1.0)
		domain_error("asin", number);
#endif
	IN_FLOAT(d = asin(d), "asin", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("atan", Fatan, 1, 3, 0,	/*
Return the inverse tangent of NUMBER.
If optional second argument NUMBER2 is provided,
return atan2 (NUMBER, NUMBER2).
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, number2, precision))
{
#ifdef HAVE_MPFR
	bigfr bfr;
	Lisp_Object result;

	if (NILP(number2)) {
		Lisp_Object bfrnumber;

		if (INDEFP(number))
			return make_indef(NOT_A_NUMBER);

		BIGFR_INIT_PREC(bfr, precision);
		bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
		bigfr_atan(bfr, XBIGFR_DATA(bfrnumber));
		result = make_bigfr_bfr(bfr);
	} else {
		Lisp_Object bfrn1;
		Lisp_Object bfrn2;

		if (INDEFP(number))
			return make_indef(NOT_A_NUMBER);
		if (INFINITYP(number2))
			return Qzero;
		if (INDEFP(number2))
			return make_indef(NOT_A_NUMBER);

		BIGFR_INIT_PREC(bfr, precision);
		bfrn1 = Fcoerce_number(number, Qbigfr, Qnil);
		bfrn2 = Fcoerce_number(number2, Qbigfr, Qnil);
		bigfr_atan2(bfr, XBIGFR_DATA(bfrn1), XBIGFR_DATA(bfrn2));
		result = make_bigfr_bfr(bfr);
	}

	bigfr_fini(bfr);
	return result;

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	if (NILP(number2))
		IN_FLOAT(d = atan(d), "atan", number);
	else {
		double d2;

#ifdef WITH_NUMBER_TYPES
		if (INFINITYP(number2))
			return Qzero;
		if (INDEFP(number2))
			return make_indef(NOT_A_NUMBER);
#endif

		d2 = extract_float(number2);
#ifdef FLOAT_CHECK_DOMAIN
		if (d == 0.0 && d2 == 0.0)
			domain_error2("atan", number, number2);
#endif
		IN_FLOAT2(d = atan2(d, d2), "atan", number, number2);
	}
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("cos", Fcos, 1, 2, 0,	/*
Return the cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(cos);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = cos(d), "cos", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("sin", Fsin, 1, 2, 0,	/*
Return the sine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(sin);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = sin(d), "sin", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("tan", Ftan, 1, 2, 0,	/*
Return the tangent of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(tan);

#else  /* !HAVE_MPFR */

	double d, c;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	c = cos(d);
#ifdef FLOAT_CHECK_DOMAIN
	if (c == 0.0)
		domain_error("tan", number);
#endif
	IN_FLOAT(d = (sin(d) / c), "tan", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#ifdef HAVE_MPFR
DEFUN("sec", Fsec, 1, 2, 0,	/*
Return the secant of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(sec);
}

DEFUN("csc", Fcsc, 1, 2, 0,	/*
Return the cosecant of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(csc);
}

DEFUN("cot", Fcot, 1, 2, 0,	/*
Return the cotangent of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(cot);
}
#endif  /* HAVE_MPFR */

#endif  /* HAVE_MPFR || LISP_FLOAT_TYPE (trig functions) */

/* Bessel functions */
#if 0				/* Leave these out unless we find there's a reason for them.  */
/* #ifdef LISP_FLOAT_TYPE */

DEFUN("bessel-j0", Fbessel_j0, 1, 1, 0,	/*
Return the bessel function j0 of NUMBER.
					 */
      (number))
{
	double d = extract_float(number);
	IN_FLOAT(d = j0(d), "bessel-j0", number);
	return make_float(d);
}

DEFUN("bessel-j1", Fbessel_j1, 1, 1, 0,	/*
Return the bessel function j1 of NUMBER.
					 */
      (number))
{
	double d = extract_float(number);
	IN_FLOAT(d = j1(d), "bessel-j1", number);
	return make_float(d);
}

DEFUN("bessel-jn", Fbessel_jn, 2, 2, 0,	/*
Return the order N bessel function output jn of NUMBER.
The first number (the order) is truncated to an integer.
					 */
      (number1, number2))
{
	int i1 = extract_float(number1);
	double f2 = extract_float(number2);

	IN_FLOAT(f2 = jn(i1, f2), "bessel-jn", number1);
	return make_float(f2);
}

DEFUN("bessel-y0", Fbessel_y0, 1, 1, 0,	/*
Return the bessel function y0 of NUMBER.
					 */
      (number))
{
	double d = extract_float(number);
	IN_FLOAT(d = y0(d), "bessel-y0", number);
	return make_float(d);
}

DEFUN("bessel-y1", Fbessel_y1, 1, 1, 0,	/*
Return the bessel function y1 of NUMBER.
					 */
      (number))
{
	double d = extract_float(number);
	IN_FLOAT(d = y1(d), "bessel-y0", number);
	return make_float(d);
}

DEFUN("bessel-yn", Fbessel_yn, 2, 2, 0,	/*
Return the order N bessel function output yn of NUMBER.
The first number (the order) is truncated to an integer.
					 */
      (number1, number2))
{
	int i1 = extract_float(number1);
	double f2 = extract_float(number2);

	IN_FLOAT(f2 = yn(i1, f2), "bessel-yn", number1);
	return make_float(f2);
}

#endif				/* 0 (bessel functions) */


/* Error functions. */
#if defined(HAVE_MPFR) || defined(LISP_FLOAT_TYPE)
DEFUN("erf", Ferf, 1, 2, 0,	/*
Return the mathematical error function of NUMBER.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(erf);

#else  /* !HAVE_MPFR */
	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = erf(d), "erf", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("erfc", Ferfc, 1, 2, 0,	/*
Return the complementary error function of NUMBER.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(erfc);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = erfc(d), "erfc", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("log-gamma", Flog_gamma, 1, 2, 0,	/*
Return the log gamma of NUMBER.
					 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(lgamma);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = lgamma(d), "log-gamma", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}
#endif	/* LISP_FLOAT_TYPE || HAVE_MPFR */


/* Root and Log functions. */

#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
DEFUN("exp", Fexp, 1, 2, 0,	/*
Return the exponential base e of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
/* Attention, somehow the precision must be large enough to make the result
 * fit, otherwise this is a good memory test :)
 */
#if defined(HAVE_MPFR) || defined(HAVE_MPC)

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return Fcoerce_number(Qzero, Qbigfr, precision);
		else
			return number;
	}

	if (COMPARABLEP(number)) {
#ifdef HAVE_MPFR
		bigfr bfr;
		Lisp_Object result;
		Lisp_Object bfrnumber;

		BIGFR_INIT_PREC(bfr, precision);

		bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
		bigfr_exp(bfr, XBIGFR_DATA(bfrnumber));
		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;

#endif	/* HAVE_MPFR */
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		bigc bc;
		Lisp_Object result;

		BIGC_INIT_PREC(bc, precision);

		bigc_exp(bc, XBIGC_DATA(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
#endif	/* HAVE_MPC */
	}

	return wrong_type_argument(Qnumberp, number);
#else  /* !HAVE_MPFR && !HAVE_MPC */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return Fcoerce_number(Qzero, Qfloat, precision);
		else
			return number;
	}
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d > 709.7827)	/* Assume IEEE doubles here */
		range_error("exp", number);
	else if (d < -709.0)
		return make_float(0.0);
	else
#endif
		IN_FLOAT(d = exp(d), "exp", number);
	if (NILP(precision));
	return make_float(d);
#endif	/* HAVE_MPFR */
}
#endif  /* LISP_FLOAT_TYPE || HAVE_MPFR */

#if defined(HAVE_MPFR) || defined(WITH_NUMBER_TYPES)
DEFUN("2^", Fexp2, 1, 2, 0,	/*
Return the exponential of NUMBER to 2 power.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision in float computations.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR
	Lisp_Object bfrnumber;
	bigfr bfr;
	Lisp_Object result;
#endif
#ifdef HAVE_MPZ
	if (INTP(number))
		return Fpow(make_int(2),number);
#endif
	if (INDEFP(number))
		return ent_optable_pow[INT_T][INDEF_T](make_int(2), number);

#ifdef HAVE_MPFR
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	BIGFR_INIT_PREC(bfr, precision);

	bigfr_exp2(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;
#endif
	/* fallback */
	if (NILP(precision));
	return Qnil;
}

DEFUN("10^", Fexp10, 1, 2, 0,	/*
Return the exponential of NUMBER to 10 power.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision in float computations.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR
	Lisp_Object bfrnumber;
	bigfr bfr;
	Lisp_Object result;
#endif
#ifdef HAVE_MPZ
	if (INTP(number))
		return Fpow(make_int(10),number);
#endif
	if (INDEFP(number))
		return ent_optable_pow[INT_T][INDEF_T](make_int(10), number);

#ifdef HAVE_MPFR
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	BIGFR_INIT_PREC(bfr, precision);

	bigfr_exp10(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;
#endif
	/* fallback */
	if (NILP(precision));
	return Qnil;
}
#endif	/* HAVE_MPZ || HAVE_MPFR */

#if 0
/* we attempt to define this as alias for ^ */
DEFUN("expt", Fexpt, 2, 2, 0,	/*
Return the exponential NUMBER1 ** NUMBER2.
				 */
      (number1, number2))
{
#ifdef HAVE_MPZ
	if (INTEGERP(number1) && INTP(number2)) {
		return Fpow(number1, number2);
	}
#endif	/* HAVE_MPZ */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number1) || INDEFP(number2))
		return ent_optable_pow
			[get_number_type(number1)][get_number_type(number2)](
				number1, number2);
#endif

#ifdef HAVE_PSEUG
	if (BIGGP(number1) && INTP(number2)) {
		return Fpow(number1, number2);
	}
#endif

	if (INTP(number1)) {
		number_type nt;
		nt = get_number_type(number2);
		return ent_optable_pow[INT_T][nt](number1, number2);
	}

#if defined(HAVE_MPF)
	if (BIGFP(number1)) {
		number_type nt;
		nt = get_number_type(number2);
		return ent_optable_pow[BIGF_T][nt](number1, number2);
	}
#endif	/* HAVE_MPF */

#if defined(HAVE_MPFR)
	if (BIGFRP(number1)) {
		number_type nt;
		nt = get_number_type(number2);
		return ent_optable_pow[BIGFR_T][nt](number1, number2);
	}
#endif

#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number1)) {
		number_type nt;
		nt = get_number_type(number2);
		return ent_optable_pow[FLOAT_T][nt](number1, number2);
	}
#endif	/* LISP_FLOAT_TYPE */

	return Qzero;
}
#endif

#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
DEFUN("log", Flog, 1, 3, 0,	/*
Return the natural logarithm of NUMBER.
If second optional argument BASE is given, return the logarithm of
NUMBER using that base.
If third optional argument PRECISION is given, use its value
(an integer) as precision.
				 */
      (number, base, precision))
{
#ifdef HAVE_MPFR
	Lisp_Object bfrnumber;
	bigfr bfr;
	Lisp_Object result;

	if (!NILP(base)) {
		Lisp_Object *qargs = alloca_array(Lisp_Object, 2);
		qargs[0] = Flog(number, Qnil, precision);
		if (INDEFP(qargs[0]))
			return qargs[0];
		qargs[1] = Flog(base, Qnil, precision);
		return Fquo(2, qargs);
	}

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	BIGFR_INIT_PREC(bfr, precision);

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d <= 0.0)
		domain_error2("log", number, base);
#endif
	if (NILP(base))
		IN_FLOAT(d = log(d), "log", number);
	else {
		double b = extract_float(base);
#ifdef FLOAT_CHECK_DOMAIN
		if (b <= 0.0 || b == 1.0)
			domain_error2("log", number, base);
#endif
		if (b == 10.0)
			IN_FLOAT2(d = log10(d), "log", number, base);
		else
			IN_FLOAT2(d = (log(d) / log(b)), "log", number, base);
	}
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("log10", Flog10, 1, 2, 0,	/*
Return the logarithm base 10 of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR
	Lisp_Object bfrnumber;
	bigfr bfr;
	Lisp_Object result;

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	BIGFR_INIT_PREC(bfr, precision);

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log10(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d <= 0.0)
		domain_error("log10", number);
#endif
	IN_FLOAT(d = log10(d), "log10", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#ifdef HAVE_MPFR
DEFUN("log2", Flog2, 1, 2, 0,	/*
Return the logarithm base 2 of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
				 */
      (number, precision))
{
	Lisp_Object bfrnumber;
	bigfr bfr;
	Lisp_Object result;

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	BIGFR_INIT_PREC(bfr, precision);

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log2(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;
}
#endif	/* HAVE_MPFR */


DEFUN("sqrt", Fsqrt, 1, 2, 0,	/*
Return the square root of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
				 */
      (number, precision))
{
#if defined(HAVE_MPFR) || defined(HAVE_MPC)

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(COMPLEX_INFINITY);
		else
			return number;
	}

	if (COMPARABLEP(number)) {
#ifdef HAVE_MPFR
		bigfr bfr;
		Lisp_Object result;

		BIGFR_INIT_PREC(bfr, precision);

		if (NATNUMP(number))
			bigfr_sqrt_ui(bfr, (unsigned long)XUINT(number));
		else if (BIGZP(number) &&
			 bigz_fits_ulong_p(XBIGZ_DATA(number)) &&
			 bigz_sign(XBIGZ_DATA(number)) >= 0) {
			bigfr_sqrt_ui(bfr,
				      (unsigned long)bigz_to_ulong(
					      XBIGZ_DATA(number)));
		} else if (!NILP(Fnonnegativep(number))) {
			Lisp_Object bfrnumber;
			bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
			bigfr_sqrt(bfr, XBIGFR_DATA(bfrnumber));
		} else {
#ifdef HAVE_MPC
			Lisp_Object bcnumber;
			bigc bc;
			BIGC_INIT_PREC(bc, precision);
			bcnumber = Fcoerce_number(number, Qbigc, Qnil);
			bigc_sqrt(bc, XBIGC_DATA(bcnumber));
			result = make_bigc_bc(bc);
			bigc_fini(bc);
			bigfr_fini(bfr);
			return result;
#else  /* !HAVE_MPC */
			Lisp_Object bfrnumber;
			bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
			bigfr_sqrt(bfr, XBIGFR_DATA(bfrnumber));
#endif	/* HAVE_MPC */
		}
		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;
#endif	/* HAVE_MPFR */
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		bigc bc;
		Lisp_Object result;

		BIGC_INIT_PREC(bc, precision);

		bigc_sqrt(bc, XBIGC_DATA(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
#endif	/* HAVE_MPC */
	} 

	if (NILP(precision));
	return wrong_type_argument(Qnumberp, number);

#else  /* !HAVE_MPFR && !HAVE_MPC */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(COMPLEX_INFINITY);
		else
			return number;
	}
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d < 0.0)
		domain_error("sqrt", number);
#endif
	IN_FLOAT(d = sqrt(d), "sqrt", number);
	if (NILP(precision));
	return make_float(d);
#endif	/* HAVE_MPFR */
}

DEFUN("cube-root", Fcube_root, 1, 2, 0,	/*
Return the cube root of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
					 */
      (number, precision))
{
#ifdef HAVE_MPFR
	bigfr bfr;
	Lisp_Object bfrnumber;
	Lisp_Object result;

	if (INDEFP(number))
		return number;

	BIGFR_INIT_PREC(bfr, precision);

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_cbrt(bfr, XBIGFR_DATA(bfrnumber));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif

	d = extract_float(number);
#ifdef HAVE_CBRT
	IN_FLOAT(d = cbrt(d), "cube-root", number);
#else
	if (d >= 0.0)
		IN_FLOAT(d = pow(d, 1.0 / 3.0), "cube-root", number);
	else
		IN_FLOAT(d = -pow(-d, 1.0 / 3.0), "cube-root", number);
#endif
	if (NILP(precision));
	return make_float(d);
#endif	/* HAVE_MPFR */
}
#endif  /* LISP_FLOAT_TYPE || MPFR */


#ifdef HAVE_MPFR
DEFUN("root", Froot, 2, 3, 0,	/*
Return the RADIX-th root of NUMBER.
If third optional argument PRECISION is given, use its value
(an integer) as precision.
      */
      (number, radix, precision))
{
	bigfr bfr;
	Lisp_Object bfrnumber;
	Lisp_Object result;

	if (!NATNUMP(radix)) {
		dead_wrong_type_argument(Qnatnump, radix);
		return Qnil;
	}

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(COMPLEX_INFINITY);
		else
			return number;
	}

	BIGFR_INIT_PREC(bfr, precision);

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_root(bfr, XBIGFR_DATA(bfrnumber), XUINT(radix));
	result = make_bigfr_bfr(bfr);

	bigfr_fini(bfr);
	return result;
}
#endif  /* HAVE_MPFR */


/* (Inverse) hyperbolic trig functions. */
#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)

DEFUN("acosh", Facosh, 1, 2, 0,	/*
Return the inverse hyperbolic cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(acosh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d < 1.0)
		domain_error("acosh", number);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
	IN_FLOAT(d = acosh(d), "acosh", number);
#else
	IN_FLOAT(d = log(d + sqrt(d * d - 1.0)), "acosh", number);
#endif
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("asinh", Fasinh, 1, 2, 0,	/*
Return the inverse hyperbolic sine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(asinh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef HAVE_INVERSE_HYPERBOLIC
	IN_FLOAT(d = asinh(d), "asinh", number);
#else
	IN_FLOAT(d = log(d + sqrt(d * d + 1.0)), "asinh", number);
#endif
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("atanh", Fatanh, 1, 2, 0,	/*
Return the inverse hyperbolic tangent of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(atanh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d >= 1.0 || d <= -1.0)
		domain_error("atanh", number);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
	IN_FLOAT(d = atanh(d), "atanh", number);
#else
	IN_FLOAT(d = 0.5 * log((1.0 + d) / (1.0 - d)), "atanh", number);
#endif
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("cosh", Fcosh, 1, 2, 0,	/*
Return the hyperbolic cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(cosh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d > 710.0 || d < -710.0)
		range_error("cosh", number);
#endif
	IN_FLOAT(d = cosh(d), "cosh", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("sinh", Fsinh, 1, 2, 0,	/*
Return the hyperbolic sine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(sinh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
#ifdef FLOAT_CHECK_DOMAIN
	if (d > 710.0 || d < -710.0)
		range_error("sinh", number);
#endif
	IN_FLOAT(d = sinh(d), "sinh", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MFPR */
}

DEFUN("tanh", Ftanh, 1, 2, 0,	/*
Return the hyperbolic tangent of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
#ifdef HAVE_MPFR

	MPFR_TRIG_FUN(tanh);

#else  /* !HAVE_MPFR */

	double d;

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);
#endif

	d = extract_float(number);
	IN_FLOAT(d = tanh(d), "tanh", number);
	return make_float(d);

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#ifdef HAVE_MPFR

DEFUN("sech", Fsech, 1, 2, 0,	/*
Return the hyperbolic secant of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(sech);
}

DEFUN("csch", Fcsch, 1, 2, 0,	/*
Return the hyperbolic cosecant of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(csch);
}

DEFUN("coth", Fcoth, 1, 2, 0,	/*
Return the hyperbolic cotangent of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
				 */
      (number, precision))
{
	MPFR_TRIG_FUN(coth);
}
#endif	/* HAVE_MPFR */

#endif  /* HAVE_MPFR || LISP_FLOAT_TYPE (inverse trig functions) */


/* Rounding functions */

DEFUN("abs", Fabs, 1, 1, 0,	/*
Return the absolute value of NUMBER.
				 */
      (number))
{
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number)) {
		IN_FLOAT(number = make_float(fabs(XFLOAT_DATA(number))),
			 "abs", number);
		return number;
	}
#endif				/* LISP_FLOAT_TYPE */

	if (INTP(number)) {
#ifdef HAVE_MPZ
		/* The most negative Lisp int will overflow */
		return (XINT(number) >= 0)
			? number : make_integer(-XINT(number));
#else  /* !HAVE_MPZ */
		return (XINT(number) >= 0) ? number : make_int(-XINT(number));
#endif	/* HAVE_MPZ */
	}

#ifdef HAVE_MPZ
	if (BIGZP(number)) {
		bigz bz;
		Lisp_Object result;

		if (bigz_sign(XBIGZ_DATA(number)) >= 0)
			return number;

		bigz_init(bz);

		bigz_abs(bz, XBIGZ_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
	}
#endif	/* HAVE_MPZ */

#ifdef HAVE_MPQ
	if (BIGQP(number)) {
		bigq bq;
		Lisp_Object result;

		if (bigq_sign(XBIGQ_DATA(number)) >= 0)
			return number;

		bigq_init(bq);

		bigq_abs(bq, XBIGQ_DATA(number));
		result = make_bigq_bq(bq);

		bigq_fini(bq);
		return result;
	}
#endif	/* HAVE_MPQ */

#ifdef HAVE_MPF
	if (BIGFP(number)) {
		bigf bf;
		Lisp_Object result;

		if (bigf_sign(XBIGF_DATA (number)) >= 0)
			return number;

		bigf_init_prec(bf, XBIGF_GET_PREC(number));

		bigf_abs(bf, XBIGF_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	if (BIGFRP(number)) {
		bigfr bfr;
		Lisp_Object result;

		if (bigfr_sign(XBIGFR_DATA (number)) >= 0)
			return number;

		bigfr_init_prec(bfr, XBIGFR_GET_PREC(number));

		bigfr_abs(bfr, XBIGFR_DATA(number));
		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;
	}
#endif	/* HAVE_MPFR */

#if defined(HAVE_PSEUG) && defined(HAVE_MPFR)
	if (BIGGP(number)) {
		bigfr bfr;
		Lisp_Object result;

		bigfr_init_prec(bfr, internal_get_precision(Qnil));

		bigg_abs(bfr, XBIGG_DATA(number));
		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;
	}
#endif	/* HAVE_PSEUG && HAVE_MPFR */

#ifdef HAVE_MPC
	if (BIGCP(number)) {
		bigfr bfr;
		Lisp_Object result;

		bigfr_init_prec(bfr, XBIGC_GET_PREC(number));

		if (bigc_nan_p(XBIGC_DATA(number)))
			bigfr_set_nan(bfr);
		else if (bigc_inf_p(XBIGC_DATA(number)))
			bigfr_set_pinf(bfr);
		else
			bigc_abs(bfr, XBIGC_DATA(number));

		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;
	}
#endif	/* HAVE_PSEUG */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(POS_INFINITY);
		else
			return number;
	}
#endif	/* WITH_NUMBER_TYPES */

	return Fabs(wrong_type_argument(Qnumberp, number));
}

#if defined(LISP_FLOAT_TYPE)
DEFUN("float", Ffloat, 1, 1, 0,	/*
Return the floating point number numerically equal to NUMBER.
				 */
      (number))
{
/* Just create the float in order of preference */
#if defined(WITH_NUMBER_TYPES)

	return Fcoerce_number(number, Qfloat, Qnil);

#else  /* !WITH_NUMBER_TYPES, create an ordinary float */

	if (INTP(number))
		return make_float((double)XINT(number));

	if (FLOATP(number))	/* give 'em the same float back */
		return number;

	return Ffloat(wrong_type_argument(Qnumberp, number));
#endif	/* WITH_NUMBER_TYPES */
}
#endif  /* LISP_FLOAT_TYPE */

#ifdef LISP_FLOAT_TYPE
DEFUN("logb", Flogb, 1, 1, 0,	/*
Return largest integer <= the base 2 log of the magnitude of NUMBER.
This is the same as the exponent of a float.
				 */
      (number))
{
	double f = extract_float(number);

	if (f == 0.0)
		return make_int(EMACS_INT_MIN);
#ifdef HAVE_LOGB
	{
		Lisp_Object val;
		IN_FLOAT(val = make_int((EMACS_INT) logb(f)), "logb", number);
		return val;
	}
#else
#ifdef HAVE_FREXP
	{
		int exqp;
		IN_FLOAT(frexp(f, &exqp), "logb", number);
		return make_int(exqp - 1);
	}
#else
	{
		int i;
		double d;
		EMACS_INT val;
		if (f < 0.0)
			f = -f;
		val = -1;
		while (f < 0.5) {
			for (i = 1, d = 0.5; d * d >= f; i += i)
				d *= d;
			f /= d;
			val -= i;
		}
		while (f >= 1.0) {
			for (i = 1, d = 2.0; d * d <= f; i += i)
				d *= d;
			f /= d;
			val += i;
		}
		return make_int(val);
	}
#endif				/* ! HAVE_FREXP */
#endif				/* ! HAVE_LOGB */
}
#endif				/* LISP_FLOAT_TYPE */

DEFUN("ceiling", Fceiling, 1, 1, 0,	/*
Return the smallest integer no less than NUMBER.  (Round toward +inf.)
					 */
      (number))
{
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number)) {
		double d;
		IN_FLOAT((d = ceil(XFLOAT_DATA(number))), "ceiling", number);
		return (float_to_int(d, "ceiling", number, Qunbound));
	}
#endif				/* LISP_FLOAT_TYPE */

#ifdef HAVE_MPZ
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
 	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#if defined(HAVE_MPQ) && defined(HAVE_MPZ)
	if (BIGQP(number)) {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);
		
		bigz_ceil(bz, XBIGQ_NUMERATOR(number),
			  XBIGQ_DENOMINATOR(number));
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		return result;
	}
#endif

#ifdef HAVE_MPF
	else if (BIGFP(number)) {
		Lisp_Object result;
		bigf bf;
#ifdef HAVE_MPZ
		bigz bz;
#endif
		bigf_init(bf);
#ifdef HAVE_MPZ
		bigz_init(bz);
		
		bigf_ceil(bf, XBIGF_DATA(number));
		bigz_set_bigf(bz, bf);
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		bigf_fini(bf);
		return result;
#else  /* !HAVE_MPZ */
		bigf_ceil(bf, XBIGF_DATA(number));
		result = make_int((EMACS_INT)bigf_to_long(bf));
		bigf_fini(bf);
		return result;
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	else if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;
#ifdef HAVE_MPZ
		bigz bz;
#endif
		bigfr_init(bf);
#ifdef HAVE_MPZ
		bigz_init(bz);
		
		bigfr_ceil(bf, XBIGFR_DATA(number));
		bigz_set_bigfr(bz, bf);
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		bigfr_fini(bf);
		return result;
#else  /* !HAVE_MPZ */
		bigfr_ceil(bf, XBIGFR_DATA(number));
		result = make_int((EMACS_INT)bigfr_to_long(bf));
		bigfr_fini(bf);
		return result;
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
	return Fceiling(wrong_type_argument(Qcomparablep, number));
#else  /* !HAVE_MPC */
	return Fceiling(wrong_type_argument(Qnumberp, number));
#endif  /* HAVE_MPC */
}

DEFUN("floor", Ffloor, 1, 2, 0,	/*
Return the largest integer no greater than NUMBER.  (Round towards -inf.)
With optional second argument DIVISOR, return the largest integer no
greater than NUMBER/DIVISOR.
				 */
      (number, divisor))
{
#ifdef WITH_NUMBER_TYPES
	CHECK_COMPARABLE(number);
	if (NILP(divisor)) {
		if (FLOATP(number)) {
			double d;
			IN_FLOAT((d = floor(XFLOAT_DATA(number))),
				 "floor", number);
			return (float_to_int(d, "floor", number, Qunbound));
		}
#if defined(HAVE_MPQ) && defined(HAVE_MPZ)
		else if (BIGQP(number)) {
			bigz bz;
			Lisp_Object result;

			bigz_init(bz);
			
			bigz_floor(bz, XBIGQ_NUMERATOR(number),
				   XBIGQ_DENOMINATOR(number));
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			return result;
		}
#endif

#ifdef HAVE_MPF
		else if (BIGFP(number)) {
			Lisp_Object result;
			bigf bf;
#ifdef HAVE_MPZ
			bigz bz;
#endif
			bigf_init(bf);
#ifdef HAVE_MPZ
			bigz_init(bz);

			bigf_floor(bf, XBIGF_DATA(number));
			bigz_set_bigf(bz, bf);
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			bigf_fini(bf);
			return result;
#else  /* !HAVE_MPZ */
			bigf_floor(bf, XBIGF_DATA(number));
			result = make_int((EMACS_INT)bigf_to_long(bf));
			bigf_fini(bf);
			return result;
#endif	/* HAVE_MPZ */
		}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
		else if (BIGFRP(number)) {
			Lisp_Object result;
			bigfr bf;
#ifdef HAVE_MPZ
			bigz bz;
#endif
			bigfr_init(bf);
#ifdef HAVE_MPZ
			bigz_init(bz);
		
			bigfr_floor(bf, XBIGFR_DATA(number));
			bigz_set_bigfr(bz, bf);
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			bigfr_fini(bf);
			return result;
#else  /* !HAVE_MPZ */
			bigfr_floor(bf, XBIGFR_DATA(number));
			result = make_int((EMACS_INT)bigfr_to_long(bf));
			bigfr_fini(bf);
			return result;
#endif	/* HAVE_MPZ */
		}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
		else if (INDEFP(number))
			return number;
#endif	/* WITH_NUMBER_TYPES */

		else
			return number;

	} else {		/* !NILP(divisor) */

		CHECK_COMPARABLE(divisor);
		switch (promote_args(&number, &divisor)) {
		case INT_T: {
			EMACS_INT i1 = XREALINT (number);
			EMACS_INT i2 = XREALINT (divisor);

			if (i2 == 0)
				Fsignal(Qarith_error, Qnil);

			/* With C's /, the result is implementation-defined
			   if either operand is negative, so use only
			   nonnegative operands.  */
			i1 = (i2 < 0
			      ? (i1 <= 0  ?  -i1 / -i2  :  -1 - ((i1 - 1) / -i2))
			      : (i1 < 0  ?  -1 - ((-1 - i1) / i2)  :  i1 / i2));

			return make_int(i1);
		}
#ifdef HAVE_MPZ
		case BIGZ_T: {
			bigz bz;
			Lisp_Object result;

			if (bigz_sign(XBIGZ_DATA(divisor)) == 0)
				Fsignal(Qarith_error, Qnil);

			bigz_init(bz);

			bigz_floor(bz, XBIGZ_DATA(number), XBIGZ_DATA(divisor));
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			return result;
		}
#endif
#if defined(HAVE_MPQ) && defined(HAVE_MPZ)
		case BIGQ_T: {
			bigz bz;
			bigq bq;
			Lisp_Object result;

			if (bigq_sign(XBIGQ_DATA(divisor)) == 0)
				Fsignal(Qarith_error, Qnil);

			bigz_init(bz);
			bigq_init(bq);

			bigq_div(bq, XBIGQ_DATA(number), XBIGQ_DATA(divisor));
			bigz_floor(bz, bigq_numerator(bq),
				   bigq_denominator(bq));
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			bigq_fini(bq);
			return result;
		}
#endif

#ifdef HAVE_MPF
		case BIGF_T: {
			Lisp_Object result;
			bigf bf;
#ifdef HAVE_MPZ
			bigz bz;
#endif
			if (bigf_sign(XBIGF_DATA(divisor)) == 0)
				Fsignal(Qarith_error, Qnil);

			bigf_init(bf);
#ifdef HAVE_MPZ
			bigz_init(bz);

			bigf_div(bf, XBIGF_DATA(number), XBIGF_DATA(divisor));
			bigf_floor(bf, bf);
			bigz_set_bigf(bz, bf);
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			bigf_fini(bf);
			return result;
#else  /* !HAVE_MPZ */
			bigf_div(bf, XBIGF_DATA(number), XBIGF_DATA(divisor));
			bigf_floor(bf, bf);
			result = make_int((EMACS_INT)bigf_to_long(bf));

			bigf_fini(bf);
			return result;
#endif	/* HAVE_MPZ */
		}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
		case BIGFR_T: {
			Lisp_Object result;
			bigfr bf;
#ifdef HAVE_MPZ
			bigz bz;
#endif

			if (bigfr_sign(XBIGFR_DATA(divisor)) == 0)
				Fsignal(Qarith_error, Qnil);

			bigfr_init(bf);
#ifdef HAVE_MPZ
			bigz_init(bz);

			bigfr_div(bf, XBIGFR_DATA(number),
				  XBIGFR_DATA(divisor));
			bigfr_floor(bf, bf);
			bigz_set_bigfr(bz, bf);
			result = Fcanonicalize_number(make_bigz_bz(bz));

			bigz_fini(bz);
			bigfr_fini(bf);
			return result;
#else  /* !HAVE_MPZ */
			bigfr_div(bf, XBIGFR_DATA(number),
				  XBIGFR_DATA(divisor));
			bigfr_floor(bf, bf);
			result = make_int((EMACS_INT)bigfr_to_long(bf));

			bigfr_fini(bf);
			return result;
#endif	/* HAVE_MPZ */
		}
#endif	/* HAVE_MPF */
		case INDEF_T:
			return ent_optable_div[INDEF_T][INDEF_T](
				number, divisor);
			break;

#if defined(LISP_FLOAT_TYPE)
		case FLOAT_T:
		default: {
			double f1 = extract_float(number);
			double f2 = extract_float(divisor);
	    
			if (f2 == 0.0)
				Fsignal(Qarith_error, Qnil);
	    
			IN_FLOAT2 (f1 = floor (f1 / f2), "floor",
				   number, divisor);
			return float_to_int (f1, "floor", number, divisor);
		}
#endif	/* LISP_FLOAT_TYPE */
		}
	}

#else /* !WITH_NUMBER_TYPES */

	CHECK_INT_OR_FLOAT(number);

	if (!NILP(divisor)) {
		EMACS_INT i1, i2;

		CHECK_INT_OR_FLOAT(divisor);

#ifdef LISP_FLOAT_TYPE
		if (FLOATP(number) || FLOATP(divisor)) {
			double f1 = extract_float(number);
			double f2 = extract_float(divisor);

			if (f2 == 0)
				Fsignal(Qarith_error, Qnil);

			IN_FLOAT2(f1 =
				  floor(f1 / f2), "floor", number, divisor);
			return float_to_int(f1, "floor", number, divisor);
		}
#endif				/* LISP_FLOAT_TYPE */

		i1 = XINT(number);
		i2 = XINT(divisor);

		if (i2 == 0)
			Fsignal(Qarith_error, Qnil);

		/* With C's /, the result is implementation-defined if either operand
		   is negative, so use only nonnegative operands.  */
		i1 = (i2 < 0 ? (i1 <= 0 ? -i1 / -i2 : -1 - ((i1 - 1) / -i2))
		      : (i1 < 0 ? -1 - ((-1 - i1) / i2) : i1 / i2));

		return (make_int(i1));
	}
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number)) {
		double d;
		IN_FLOAT((d = floor(XFLOAT_DATA(number))), "floor", number);
		return (float_to_int(d, "floor", number, Qunbound));
	}
#endif				/* LISP_FLOAT_TYPE */

	return number;
#endif	/* WITH_NUMBER_TYPES */
}

DEFUN("round", Fround, 1, 1, 0,	/*
Return the nearest integer to NUMBER.
				 */
      (number))
{
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number)) {
		double d;
		/* Screw the prevailing rounding mode.  */
		IN_FLOAT((d =
			  emacs_rint(XFLOAT_DATA(number))), "round", number);
		return (float_to_int(d, "round", number, Qunbound));
	}
#endif				/* LISP_FLOAT_TYPE */

#ifdef HAVE_MPZ
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#if defined(HAVE_MPQ) && defined(HAVE_MPZ)
	else if (BIGQP(number)) {
		bigz b1, b2;
		Lisp_Object result;

		bigz_init(b1);
		bigz_init(b2);
		
		if (bigz_divisible_p(XBIGQ_NUMERATOR(number),
				     XBIGQ_DENOMINATOR(number))) {
			bigz_div(b1, XBIGQ_NUMERATOR(number),
				 XBIGQ_DENOMINATOR(number));
		} else {
			bigz_add(b2, XBIGQ_NUMERATOR(number),
				 XBIGQ_DENOMINATOR(number));
			bigz_div(b1, b2, XBIGQ_DENOMINATOR(number));
		}
		result = Fcanonicalize_number(make_bigz_bz(b1));

		bigz_fini(b1);
		bigz_fini(b2);

		return result;
	}
#endif	/* HAVE_MPQ && HAVE_MPZ */

#ifdef HAVE_MPF
	else if (BIGFP(number)) {
		warn_when_safe(Qbigf, Qnotice,
			       "rounding number of type 'bigf (mpf-floats)"
			       "not yet implemented");
		return number;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	else if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;
#ifdef HAVE_MPZ
		bigz bz;
#endif
		bigfr_init(bf);
#ifdef HAVE_MPZ
		bigz_init(bz);

		bigfr_rint(bf, XBIGFR_DATA(number));
		bigz_set_bigfr(bz, bf);
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		bigfr_fini(bf);
		return result;
#else  /* !HAVE_MPZ */
		bigfr_rint(bf, XBIGFR_DATA(number));
		result = make_int((EMACS_INT)bigfr_to_long(bf));

		bigfr_fini(bf);
		return result;
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	else if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
	return Fround(wrong_type_argument(Qcomparablep, number));
#else  /* !HAVE_MPC */
	return Fround(wrong_type_argument(Qnumberp, number));
#endif	/* HAVE_MPC */
}

DEFUN("truncate", Ftruncate, 1, 1, 0,	/*
Truncate a floating point number to an integer.
Rounds the value toward zero.
					 */
      (number))
{
#ifdef LISP_FLOAT_TYPE
	if (FLOATP(number))
		return float_to_int(XFLOAT_DATA(number), "truncate", number,
				    Qunbound);
#endif				/* LISP_FLOAT_TYPE */

#ifdef HAVE_MPZ
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#ifdef HAVE_MPQ
	else if (BIGQP(number)) {
		bigz bz;
		Lisp_Object result;

		bigz_init(bz);
		
		bigz_div(bz, XBIGQ_NUMERATOR(number),
			 XBIGQ_DENOMINATOR(number));
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		return result;
	}
#endif

#ifdef HAVE_MPF
	else if (BIGFP(number)) {
		Lisp_Object result;
		bigf bf;
#ifdef HAVE_MPZ
		bigz bz;
#endif
		bigf_init(bf);
#ifdef HAVE_MPZ
		bigz_init(bz);

		bigf_trunc(bf, XBIGF_DATA(number));
		bigz_set_bigf(bz, bf);
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		bigf_fini(bf);
		return result;
#else  /* !HAVE_MPZ */
		bigf_trunc(bf, XBIGF_DATA(number));
		result = make_int((EMACS_INT)bigf_to_long(bf));

		bigf_fini(bf);
		return result;
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	else if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;
#ifdef HAVE_MPZ
		bigz bz;
#endif
		bigfr_init(bf);
#ifdef HAVE_MPZ
		bigz_init(bz);

		bigfr_trunc(bf, XBIGFR_DATA(number));
		bigz_set_bigfr(bz, bf);
		result = Fcanonicalize_number(make_bigz_bz(bz));

		bigz_fini(bz);
		bigfr_fini(bf);
		return result;
#else  /* !HAVE_MPZ */
		bigfr_trunc(bf, XBIGFR_DATA(number));
		result = make_int((EMACS_INT)bigfr_to_long(bf));

		bigfr_fini(bf);
		return result;
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	else if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
	return Ftruncate(wrong_type_argument(Qcomparablep, number));
#else  /* !HAVE_MPC */
	return Ftruncate(wrong_type_argument(Qnumberp, number));
#endif	/* HAVE_MPC */
}


/* misc complex functions */
#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
DEFUN("conjugate", Fconjugate, 1, 1, 0,	/*
Return the \(canonical\) conjugate of NUMBER.
If NUMBER is a comparable, just return NUMBER.
					*/
      (number))
{
	if (COMPARABLEP(number)) {
		return number;
#ifdef HAVE_PSEUG
	} else if (BIGGP(number)) {
		bigg bg;
		Lisp_Object result;
		bigg_init(bg);

		bigg_conj(bg, XBIGG_DATA(number));
		result = make_bigg_bg(bg);

		bigg_fini(bg);
		return result;
#endif
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		bigc bc;
		Lisp_Object result;
		bigc_init_prec(bc, XBIGC_GET_PREC(number));

		bigc_conj(bc, XBIGC_DATA(number));
		result = make_bigc_bc(bc);

		bigc_fini(bc);
		return result;
#endif
	} else if (INDEFP(number)) {
		return number;
	}

	/* what should the rest do? */
	return Fconjugate(wrong_type_argument(Qnumberp, number));
}

DEFUN("canonical-norm", Fcanonical_norm, 1, 1, 0,	/*
Return the canonical norm of NUMBER.
							*/
      (number))
{
	if (INDEFP(number)) {
		if (INFINITYP(number))
			return make_indef(POS_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(number)) {
		return Fabs(number);
#ifdef HAVE_PSEUG
	} else if (BIGGP(number)) {
		bigz bz;
		Lisp_Object result;
		bigz_init(bz);

		bigg_norm(bz, XBIGG_DATA(number));
		result = make_bigz_bz(bz);

		bigz_fini(bz);
		return result;
#endif
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		bigfr bfr;
		Lisp_Object result;
		bigfr_init_prec(bfr, XBIGC_GET_PREC(number));

		if (bigc_nan_p(XBIGC_DATA(number)))
			bigfr_set_nan(bfr);
		else if (bigc_inf_p(XBIGC_DATA(number)))
			bigfr_set_pinf(bfr);
		else
			bigc_norm(bfr, XBIGC_DATA(number));

		result = make_bigfr_bfr(bfr);

		bigfr_fini(bfr);
		return result;
#endif
	} 

	/* what should the rest do? */
	return Fcanonical_norm(wrong_type_argument(Qnumberp, number));
}

DEFUN("real-part", Freal_part, 1, 1, 0,	/*
Return the real part of NUMBER.
					*/
      (number))
{
	if (INDEFP(number)) {
		if (COMPARABLE_INDEF_P(number))
			return number;
		else if (INFINITYP(number))
			return make_indef(POS_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(number)) {
		return number;
#ifdef HAVE_PSEUG
	} else if (BIGGP(number)) {
		return make_bigz_bz(bigg_re(XBIGG_DATA(number)));
#endif
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		return make_bigfr_bfr(bigc_re(XBIGC_DATA(number)));
#endif
	}

	/* what should the rest do? */
	return Freal_part(wrong_type_argument(Qnumberp, number));
}

DEFUN("imaginary-part", Fimaginary_part, 1, 1, 0,	/*
Return the imaginary part of NUMBER.
If NUMBER is a comparable, 0 is returned.
							*/
      (number))
{
	if (INDEFP(number)) {
		if (COMPARABLE_INDEF_P(number))
			return Qzero;
		else if (INFINITYP(number))
			return make_indef(POS_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	} else if (RATIONALP(number)) {
		return make_int(0);
#ifdef HAVE_MPFR
	} else if (REALP(number)) {
		return make_bigfr(0.0, 0UL);
#endif
#ifdef HAVE_PSEUG
	} else if (BIGGP(number)) {
		return make_bigz_bz(bigg_im(XBIGG_DATA(number)));
#endif
#ifdef HAVE_MPC
	} else if (BIGCP(number)) {
		return make_bigfr_bfr(bigc_im(XBIGC_DATA(number)));
#endif
	}

	/* what should the rest do? */
	return Fimaginary_part(wrong_type_argument(Qnumberp, number));
}
#endif	/* HAVE_MPC || HAVE_PSEUG */


/* Float-rounding functions. */
#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR) || defined(HAVE_MPF)

DEFUN("fceiling", Ffceiling, 1, 1, 0,	/*
Return the smallest integer no less than NUMBER, as a float.
\(Round toward +inf.\)
					 */
      (number))
{
        double d;

#ifdef HAVE_MPF
	if (BIGFP(number)) {
		Lisp_Object result;
		bigf bf;

		bigf_init_prec(bf, XBIGF_GET_PREC(number));

		bigf_ceil(bf, XBIGF_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;

		bigfr_init_prec(bf, XBIGFR_GET_PREC(number));
		
		bigfr_ceil(bf, XBIGFR_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

	d = extract_float(number);
	IN_FLOAT(d = ceil(d), "fceiling", number);
	return make_float(d);

}

DEFUN("ffloor", Fffloor, 1, 1, 0,	/*
Return the largest integer no greater than NUMBER, as a float.
\(Round towards -inf.\)
					 */
      (number))
{
        double d;
#ifdef HAVE_MPF
	if (BIGFP(number)) {
		Lisp_Object result;
		bigf bf;

		bigf_init_prec(bf, XBIGF_GET_PREC(number));

		bigf_floor(bf, XBIGF_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;

		bigfr_init_prec(bf, XBIGFR_GET_PREC(number));
		
		bigfr_floor(bf, XBIGFR_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

	d = extract_float(number);
	IN_FLOAT(d = floor(d), "ffloor", number);
	return make_float(d);
}

DEFUN("fround", Ffround, 1, 1, 0,	/*
Return the nearest integer to NUMBER, as a float.
					 */
      (number))
{
        double d;
#ifdef HAVE_MPF
	if (BIGFP(number)) {
		warn_when_safe(Qbigf, Qnotice,
			       "rounding number of type 'bigf (mpf-floats)"
			       "not yet implemented");
		return number;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;

		bigfr_init_prec(bf, XBIGFR_GET_PREC(number));
		
		bigfr_rint(bf, XBIGFR_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

        d = extract_float(number);
	IN_FLOAT(d = emacs_rint(d), "fround", number);
	return make_float(d);
}

DEFUN("ftruncate", Fftruncate, 1, 1, 0,	/*
Truncate a floating point number to an integral float value.
Rounds the value toward zero.
					 */
      (number))
{
        double d;
#ifdef HAVE_MPF
	if (BIGFP(number)) {
		Lisp_Object result;
		bigf bf;

		bigf_init_prec(bf, XBIGF_GET_PREC(number));

		bigf_trunc(bf, XBIGF_DATA(number));
		result = make_bigf_bf(bf);

		bigf_fini(bf);
		return result;
	}
#endif	/* HAVE_MPF */

#ifdef HAVE_MPFR
	if (BIGFRP(number)) {
		Lisp_Object result;
		bigfr bf;

		bigfr_init_prec(bf, XBIGFR_GET_PREC(number));
		
		bigfr_trunc(bf, XBIGFR_DATA(number));
		result = make_bigfr_bfr(bf);

		bigfr_fini(bf);
		return result;
	}
#endif	/* HAVE_MPFR */

#ifdef WITH_NUMBER_TYPES
	if (INDEFP(number))
		return number;
#endif	/* WITH_NUMBER_TYPES */

        d = extract_float(number);
	if (d >= 0.0)
		IN_FLOAT(d = floor(d), "ftruncate", number);
	else
		IN_FLOAT(d = ceil(d), "ftruncate", number);
	return make_float(d);
}
#endif  /* HAVE_MPF(R) || LISP_FLOAT_TYPE (float-rounding functions) */


#ifdef LISP_FLOAT_TYPE
#ifdef FLOAT_CATCH_SIGILL
static SIGTYPE float_error(int signo)
{
	if (!in_float)
		fatal_error_signal(signo);

	EMACS_REESTABLISH_SIGNAL(signo, arith_error);
	EMACS_UNBLOCK_SIGNAL(signo);

	in_float = 0;

	/* Was Fsignal(), but it just doesn't make sense for an error
	   occurring inside a signal handler to be restartable, considering
	   that anything could happen when the error is signaled and trapped
	   and considering the asynchronous nature of signal handlers. */
	signal_error(Qarith_error, list1(float_error_arg));
}

/* Another idea was to replace the library function `infnan'
   where SIGILL is signaled.  */

#endif				/* FLOAT_CATCH_SIGILL */

/* In C++, it is impossible to determine what type matherr expects
   without some more configure magic.
   We shouldn't be using matherr anyways - it's a non-standard SYSVism. */
#if defined (HAVE_MATHERR) && !defined(__cplusplus)
int matherr(struct exception *x)
{
	Lisp_Object args;
	if (!in_float)
		/* Not called from emacs-lisp float routines; do the default thing. */
		return 0;

	/* if (!strcmp (x->name, "pow")) x->name = "expt"; */

	args = Fcons(build_string(x->name),
		     Fcons(make_float(x->arg1), ((in_float == 2)
						 ? Fcons(make_float(x->arg2),
							 Qnil)
						 : Qnil)));
	switch (x->type) {
	case DOMAIN:
		Fsignal(Qdomain_error, args);
		break;
	case SING:
		Fsignal(Qsingularity_error, args);
		break;
	case OVERFLOW:
		Fsignal(Qoverflow_error, args);
		break;
	case UNDERFLOW:
		Fsignal(Qunderflow_error, args);
		break;
	default:
		Fsignal(Qarith_error, args);
		break;
	}
	return 1;		/* don't set errno or print a message */
}
#endif				/* HAVE_MATHERR */
#endif				/* LISP_FLOAT_TYPE */

void init_floatfns_very_early(void)
{
#ifdef LISP_FLOAT_TYPE
# ifdef FLOAT_CATCH_SIGILL
	signal(SIGILL, float_error);
# endif
	in_float = 0;
#endif				/* LISP_FLOAT_TYPE */
}

void syms_of_floatfns(void)
{
	INIT_LRECORD_IMPLEMENTATION(float);

	/* Trig functions.  */

#if defined(LISP_FLOAT_TYPE) || HAVE_MPFR
	DEFSUBR(Facos);
	DEFSUBR(Fasin);
	DEFSUBR(Fatan);
	DEFSUBR(Fcos);
	DEFSUBR(Fsin);
	DEFSUBR(Ftan);
#endif  /* LISP_FLOAT_TYPE || HAVE_MPFR*/
#ifdef HAVE_MPFR
	DEFSUBR(Fsec);
	DEFSUBR(Fcsc);
	DEFSUBR(Fcot);
#endif

	/* Bessel functions */

#if 0
	DEFSUBR(Fbessel_y0);
	DEFSUBR(Fbessel_y1);
	DEFSUBR(Fbessel_yn);
	DEFSUBR(Fbessel_j0);
	DEFSUBR(Fbessel_j1);
	DEFSUBR(Fbessel_jn);
#endif				/* 0 */

	/* Error functions. */

#if 1
#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
	DEFSUBR(Ferf);
	DEFSUBR(Ferfc);
	DEFSUBR(Flog_gamma);
#endif
#endif				/* 0 */

	/* Root and Log functions. */

#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
	DEFSUBR(Fexp);
#endif  /* LISP_FLOAT_TYPE || HAVE_MPFR */
#if defined(WITH_NUMBER_TYPES) || defined(HAVE_MPFR)
	DEFSUBR(Fexp2);
	DEFSUBR(Fexp10);
#endif	/* HAVE_MPZ || HAVE_MPFR */
#if 0
	DEFSUBR(Fexpt);
#endif
#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
	DEFSUBR(Flog);
#ifdef HAVE_MPFR
	DEFSUBR(Flog2);
#endif	/* HAVE_MPFR */
	DEFSUBR(Flog10);
	DEFSUBR(Fsqrt);
	DEFSUBR(Fcube_root);
#ifdef HAVE_MPFR
	DEFSUBR(Froot);
#endif
#endif  /* LISP_FLOAT_TYPE || HAVE_MPFR*/

	/* Inverse trig functions. */

#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPFR)
	DEFSUBR(Facosh);
	DEFSUBR(Fasinh);
	DEFSUBR(Fatanh);
	DEFSUBR(Fcosh);
	DEFSUBR(Fsinh);
	DEFSUBR(Ftanh);
#endif  /* LISP_FLOAT_TYPE || HAVE_MPFR */
#ifdef HAVE_MPFR
	DEFSUBR(Fsech);
	DEFSUBR(Fcsch);
	DEFSUBR(Fcoth);
#endif	/* HAVE_MPFR */

	/* Rounding functions */

	DEFSUBR(Fabs);
#ifdef LISP_FLOAT_TYPE
	DEFSUBR(Ffloat);
	DEFSUBR(Flogb);
#endif				/* LISP_FLOAT_TYPE */
	DEFSUBR(Fceiling);
	DEFSUBR(Ffloor);
	DEFSUBR(Fround);
	DEFSUBR(Ftruncate);

	/* misc complex functions */
#if defined(HAVE_MPC) || defined(HAVE_PSEUG)
	DEFSUBR(Fconjugate);
	DEFSUBR(Fcanonical_norm);
	DEFSUBR(Freal_part);
	DEFSUBR(Fimaginary_part);
#endif	/* HAVE_MPC */

	/* Float-rounding functions. */

#if defined(LISP_FLOAT_TYPE) || defined(HAVE_MPF) || defined(HAVE_MPFR)
	DEFSUBR(Ffceiling);
	DEFSUBR(Fffloor);
	DEFSUBR(Ffround);
	DEFSUBR(Fftruncate);
#endif  /* LISP_FLOAT_TYPE || HAVE_MPF(R) */
}

void vars_of_floatfns(void)
{
#ifdef LISP_FLOAT_TYPE
	Fprovide(intern("lisp-float-type"));
#endif
}
