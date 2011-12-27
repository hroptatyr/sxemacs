/* Primitive operations on floating point for SXEmacs Lisp interpreter.
   Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

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

#ifdef HAVE_FPFLOAT

/* The code uses emacs_rint, so that it works to undefine HAVE_RINT
   if `rint' exists but does not work right.  */
#ifdef HAVE_RINT
#define emacs_rint rint
#else
static fpfloat emacs_rint(fpfloat x)
{
	fpfloat r = floor(x + 0.5);
	fpfloat diff = fabs(r - x);
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
float_to_int(fpfloat x, const char *name, Lisp_Object num, Lisp_Object num2)
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	bigz_set_fpfloat(ent_scratch_bigz, x);
	return ent_mpz_downgrade_maybe(ent_scratch_bigz);

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


#endif  /* HAVE_FPFLOAT */

/* Trig functions.  */

#if defined HAVE_MPFR && defined WITH_MPFR
#define MPFR_TRIG_FUN(op) do						\
{									\
	Lisp_Object bfrnumber;						\
									\
	if (INDEFP(number))						\
		return make_indef(NOT_A_NUMBER);			\
									\
	bigfr_set_prec(ent_scratch_bigfr,				\
		       internal_get_precision(precision));		\
									\
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);		\
	bigfr_##op(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));		\
	return make_bigfr_bfr(ent_scratch_bigfr);			\
} while (0)
#endif

#if defined(HAVE_MPFR) && defined WITH_MPFR || defined(HAVE_FPFLOAT)

DEFUN("acos", Facos, 1, 2, 0,	/*
Return the inverse cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(acos);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = acos(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;
#endif	/* HAVE_MPFR */
}

DEFUN("asin", Fasin, 1, 2, 0,	/*
Return the inverse sine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(asin);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = asin(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object result;

	if (NILP(number2)) {
		Lisp_Object bfrnumber;

		if (INDEFP(number))
			return make_indef(NOT_A_NUMBER);

		bigfr_set_prec(ent_scratch_bigfr,
			       internal_get_precision(precision));
		bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
		bigfr_atan(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
		result = make_bigfr_bfr(ent_scratch_bigfr);
	} else {
		Lisp_Object bfrn1;
		Lisp_Object bfrn2;

		if (INDEFP(number))
			return make_indef(NOT_A_NUMBER);
		if (INFINITYP(number2))
			return Qzero;
		if (INDEFP(number2))
			return make_indef(NOT_A_NUMBER);

		bigfr_set_prec(ent_scratch_bigfr,
			       internal_get_precision(precision));
		bfrn1 = Fcoerce_number(number, Qbigfr, Qnil);
		bfrn2 = Fcoerce_number(number2, Qbigfr, Qnil);
		bigfr_atan2(ent_scratch_bigfr,
			    XBIGFR_DATA(bfrn1),
			    XBIGFR_DATA(bfrn2));
		result = make_bigfr_bfr(ent_scratch_bigfr);
	}

	return result;

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (INDEFP(number))
		return make_indef(NOT_A_NUMBER);

	if (NILP(number2) && FLOATP(number)) {
		fpfloat d;
		d = atan(XFLOAT_DATA(number));
		return make_float(d);
	} else if (FLOATP(number)) {
		number = ent_lift(number2, FLOAT_T, NULL);

		if (FLOATP(number2)) {
			fpfloat d;
			d = atan2(XFLOAT_DATA(number), XFLOAT_DATA(number2));
			return make_float(d);
		} else if (INFINITYP(number2)) {
			return Qzero;
		} else if (INDEFP(number2)) {
			return make_indef(NOT_A_NUMBER);
		}
	}

	/* Just signal here, I'm not in the mood to distinguish cases here */
	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(cos);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = cos(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(sin);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = sin(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(tan);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = XFLOAT_DATA(number);
		d = sin(d) / cos(d);
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#if defined HAVE_MPFR && defined WITH_MPFR
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

#endif  /* HAVE_MPFR || HAVE_FPFLOAT (trig functions) */

/* Bessel functions */
#if 0				/* Leave these out unless we find there's a reason for them.  */
/* #ifdef HAVE_FPFLOAT */

DEFUN("bessel-j0", Fbessel_j0, 1, 1, 0,	/*
Return the bessel function j0 of NUMBER.
*/
      (number))
{
	fpfloat d = extract_float(number);
	IN_FLOAT(d = j0(d), "bessel-j0", number);
	return make_float(d);
}

DEFUN("bessel-j1", Fbessel_j1, 1, 1, 0,	/*
Return the bessel function j1 of NUMBER.
*/
      (number))
{
	fpfloat d = extract_float(number);
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
	fpfloat f2 = extract_float(number2);

	IN_FLOAT(f2 = jn(i1, f2), "bessel-jn", number1);
	return make_float(f2);
}

DEFUN("bessel-y0", Fbessel_y0, 1, 1, 0,	/*
Return the bessel function y0 of NUMBER.
*/
      (number))
{
	fpfloat d = extract_float(number);
	IN_FLOAT(d = y0(d), "bessel-y0", number);
	return make_float(d);
}

DEFUN("bessel-y1", Fbessel_y1, 1, 1, 0,	/*
Return the bessel function y1 of NUMBER.
*/
      (number))
{
	fpfloat d = extract_float(number);
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
	fpfloat f2 = extract_float(number2);

	IN_FLOAT(f2 = yn(i1, f2), "bessel-yn", number1);
	return make_float(f2);
}

#endif				/* 0 (bessel functions) */


/* Error functions. */
#if defined(HAVE_MPFR) && defined WITH_MPFR || defined(HAVE_FPFLOAT)
DEFUN("erf", Ferf, 1, 2, 0,	/*
Return the mathematical error function of NUMBER.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(erf);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = erf(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("erfc", Ferfc, 1, 2, 0,	/*
Return the complementary error function of NUMBER.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(erfc);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = erfc(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("log-gamma", Flog_gamma, 1, 2, 0,	/*
Return the log gamma of NUMBER.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(lgamma);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = lgamma(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}
#endif	/* HAVE_FPFLOAT || HAVE_MPFR */


/* Root and Log functions. */

#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
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
#if defined(HAVE_MPFR) && defined WITH_MPFR ||	\
	defined(HAVE_MPC) && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return Fcoerce_number(Qzero, Qbigfr, precision);
		else
			return number;
	}

	if (COMPARABLEP(number)) {
#if defined HAVE_MPFR && defined WITH_MPFR
		Lisp_Object bfrnumber;

		bigfr_set_prec(ent_scratch_bigfr,
			       internal_get_precision(precision));

		bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
		bigfr_exp(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
		return make_bigfr_bfr(ent_scratch_bigfr);

#endif	/* HAVE_MPFR */
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	} else if (BIGCP(number)) {
		bigc_set_prec(ent_scratch_bigc,
			      internal_get_precision(precision));

		bigc_exp(ent_scratch_bigc, XBIGC_DATA(number));
		return make_bigc_bc(ent_scratch_bigc);
#endif	/* HAVE_MPC */
	}

	return wrong_type_argument(Qnumberp, number);
#else  /* !HAVE_MPFR && !HAVE_MPC */
	if (INDEFP(number)) {
		goto indefcase;
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = exp(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
	indefcase:
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return Fcoerce_number(Qzero, Qfloat, precision);
		else
			return number;
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}
#endif  /* HAVE_FPFLOAT || HAVE_MPFR */

DEFUN("2^", Fexp2, 1, 2, 0,	/*
Return the exponential of NUMBER to 2 power.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision in float computations.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object bfrnumber;
#endif
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (INTP(number))
		return _ent_binop(ASE_BINARY_OP_POW,
				  INT_T, make_int(2), INT_T, number);
#endif
	if (INDEFP(number))
		return _ent_binop(ASE_BINARY_OP_POW,
				  INT_T, make_int(2), INDEF_T, number);

#if defined HAVE_MPFR && defined WITH_MPFR
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bigfr_exp2(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);
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
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object bfrnumber;
#endif
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (INTP(number))
		return _ent_binop(ASE_BINARY_OP_POW,
				  INT_T, make_int(10), INT_T, number);
#endif
	if (INDEFP(number))
		return _ent_binop(ASE_BINARY_OP_POW,
				  INT_T, make_int(10), INDEF_T, number);

#if defined HAVE_MPFR && defined WITH_MPFR
	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bigfr_exp10(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);
#endif
	/* fallback */
	if (NILP(precision));
	return Qnil;
}

#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
DEFUN("log", Flog, 1, 3, 0,	/*
Return the natural logarithm of NUMBER.
If second optional argument BASE is given, return the logarithm of
NUMBER using that base.
If third optional argument PRECISION is given, use its value
(an integer) as precision.
*/
      (number, base, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object bfrnumber;

	if (!NILP(base)) {
		Lisp_Object _logn, _logb;
		_logn = Flog(number, Qnil, precision);
		if (UNLIKELY(INDEFP(_logn))) {
			return _logn;
		}
		_logb = Flog(base, Qnil, precision);
		return ent_binop(ASE_BINARY_OP_QUO, _logn, _logb);
	}

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY) {
			return number;
		} else if (XINDEF_DATA(number) == NEG_INFINITY) {
			return make_indef(NOT_A_NUMBER);
		} else {
			return number;
		}
	}

	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		goto indefcase;
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = log(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
	indefcase:
		if (XINDEF_DATA(number) == POS_INFINITY) {
			return number;
		} else if (XINDEF_DATA(number) == NEG_INFINITY) {
			return make_indef(NOT_A_NUMBER);
		} else {
			return number;
		}
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object bfrnumber;

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log10(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		goto indefcase;
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = log10(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
	indefcase:
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#if defined HAVE_MPFR && defined WITH_MPFR
DEFUN("log2", Flog2, 1, 2, 0,	/*
Return the logarithm base 2 of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
*/
      (number, precision))
{
	Lisp_Object bfrnumber;

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(NOT_A_NUMBER);
		else
			return number;
	}

	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_log2(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif	/* HAVE_MPFR */


DEFUN("sqrt", Fsqrt, 1, 2, 0,	/*
Return the square root of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
*/
      (number, precision))
{
#if defined(HAVE_MPFR) && defined WITH_MPFR ||	\
	defined(HAVE_MPC) && defined WITH_MPC ||	\
	defined(HAVE_PSEUC) && defined WITH_PSEUC

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(COMPLEX_INFINITY);
		else
			return number;
	}

	if (COMPARABLEP(number)) {
#if defined HAVE_MPFR && defined WITH_MPFR
		bigfr_set_prec(ent_scratch_bigfr,
			       internal_get_precision(precision));

		if (NATNUMP(number))
			bigfr_sqrt_ui(ent_scratch_bigfr,
				      (unsigned long)XUINT(number));
		else if (BIGZP(number) &&
			 bigz_fits_ulong_p(XBIGZ_DATA(number)) &&
			 bigz_sign(XBIGZ_DATA(number)) >= 0) {
			bigfr_sqrt_ui(ent_scratch_bigfr,
				      (unsigned long)bigz_to_ulong(
					      XBIGZ_DATA(number)));
		} else if (!NILP(Fnonnegativep(number))) {
			Lisp_Object bfrnumber;
			bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
			bigfr_sqrt(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
		} else {
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
			Lisp_Object bcnumber;
			bigc_set_prec(ent_scratch_bigc,
				      internal_get_precision(precision));
			bcnumber = Fcoerce_number(number, Qbigc, precision);
			bigc_sqrt(ent_scratch_bigc, XBIGC_DATA(bcnumber));
			return make_bigc_bc(ent_scratch_bigc);
#else  /* !HAVE_MPC */
			Lisp_Object bfrnumber;
			bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
			bigfr_sqrt(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
#endif	/* HAVE_MPC */
		}
		return make_bigfr_bfr(ent_scratch_bigfr);
#endif	/* HAVE_MPFR */
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	} else if (BIGCP(number) || BIGGP(number)) {
		Lisp_Object bcnumber;
		bigc_set_prec(ent_scratch_bigc,
			      internal_get_precision(precision));

		bcnumber = Fcoerce_number(number, Qbigc, precision);
		bigc_sqrt(ent_scratch_bigc, XBIGC_DATA(bcnumber));
		return make_bigc_bc(ent_scratch_bigc);
#endif	/* HAVE_MPC */
	} 

	if (NILP(precision));
	return wrong_type_argument(Qnumberp, number);

#else  /* !HAVE_MPFR && !HAVE_MPC */
	if (INDEFP(number)) {
		goto indefcase;
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = sqrt(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
	indefcase:
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(COMPLEX_INFINITY);
		else
			return number;
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

DEFUN("cube-root", Fcube_root, 1, 2, 0,	/*
Return the cube root of NUMBER.
If second optional argument PRECISION is given, use its value
(an integer) as precision.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR
	Lisp_Object bfrnumber;

	if (INDEFP(number))
		return number;

	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_cbrt(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber));
	return make_bigfr_bfr(ent_scratch_bigfr);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		goto indefcase;
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
#ifdef HAVE_CBRT
		d = cbrt(XFLOAT_DATA(number));
#else
		d = XFLOAT_DATA(number);
		if (d >= 0.0)
			d = pow(d, 1.0 / 3.0);
		else
			d = -pow(-d, 1.0 / 3.0);
#endif
		return make_float(d);
	} else if (INDEFP(number)) {
	indefcase:
		return number;
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}
#endif  /* HAVE_FPFLOAT || MPFR */


#if defined HAVE_MPFR && defined WITH_MPFR
DEFUN("root", Froot, 2, 3, 0,	/*
Return the RADIX-th root of NUMBER.
If third optional argument PRECISION is given, use its value
(an integer) as precision.
*/
      (number, radix, precision))
{
	Lisp_Object bfrnumber;

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

	bigfr_set_prec(ent_scratch_bigfr,
		       internal_get_precision(precision));

	bfrnumber = Fcoerce_number(number, Qbigfr, Qnil);
	bigfr_root(ent_scratch_bigfr, XBIGFR_DATA(bfrnumber), XUINT(radix));
	return make_bigfr_bfr(ent_scratch_bigfr);
}
#endif  /* HAVE_MPFR */


/* (Inverse) hyperbolic trig functions. */
#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR

DEFUN("acosh", Facosh, 1, 2, 0,	/*
Return the inverse hyperbolic cosine of NUMBER.
If optional argument PRECISION is non-nil, its value
(an integer) is used as precision.
*/
      (number, precision))
{
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(acosh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d = XFLOAT_DATA(number);
#ifdef HAVE_INVERSE_HYPERBOLIC
		d = acosh(d);
#else
		d = log(d + sqrt(d * d - 1.0));
#endif
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(asinh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d = XFLOAT_DATA(number);
#ifdef HAVE_INVERSE_HYPERBOLIC
		d = acosh(d);
#else
		d = log(d + sqrt(d * d + 1.0));
#endif
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(atanh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d = XFLOAT_DATA(number);
#ifdef HAVE_INVERSE_HYPERBOLIC
		d = atanh(d);
#else
		d = 0.5 * log((1.0 + d) / (1.0 - d));
#endif
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(cosh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = cosh(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(sinh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d;
		d = sinh(XFLOAT_DATA(number));
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

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
#if defined HAVE_MPFR && defined WITH_MPFR

	MPFR_TRIG_FUN(tanh);

#else  /* !HAVE_MPFR */
	if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		fpfloat d = XFLOAT_DATA(number);
		d = tanh(d);
		return make_float(d);
	} else if (INDEFP(number)) {
		return make_indef(NOT_A_NUMBER);
	}

	Fsignal(Qarith_error, list1(number));
	return Qnil;

	if (NILP(precision));
#endif	/* HAVE_MPFR */
}

#if defined HAVE_MPFR && defined WITH_MPFR

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

#endif  /* HAVE_MPFR || HAVE_FPFLOAT (inverse trig functions) */


/* Rounding functions */

DEFUN("abs", Fabs, 1, 1, 0,	/*
Return the absolute value of NUMBER.
*/
      (number))
{
#ifdef HAVE_FPFLOAT
	if (FLOATP(number)) {
		return make_float(fabs(XFLOAT_DATA(number)));
	}
#endif				/* HAVE_FPFLOAT */

	if (INTP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		/* The most negative Lisp int will overflow */
		return (XINT(number) >= 0)
			? number : make_integer(-XINT(number));
#else  /* !HAVE_MPZ */
		return (XINT(number) >= 0) ? number : make_int(-XINT(number));
#endif	/* HAVE_MPZ */
	}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (BIGZP(number)) {
		if (bigz_sign(XBIGZ_DATA(number)) >= 0)
			return number;

		bigz_abs(ent_scratch_bigz, XBIGZ_DATA(number));
		return make_bigz_bz(ent_scratch_bigz);
	}
#endif	/* HAVE_MPZ */

#if defined HAVE_MPQ && defined WITH_GMP
	if (BIGQP(number)) {
		if (bigq_sign(XBIGQ_DATA(number)) >= 0)
			return number;

		bigq_abs(ent_scratch_bigq, XBIGQ_DATA(number));
		return make_bigq_bq(ent_scratch_bigq);
	}
#endif	/* HAVE_MPQ */

#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		if (bigf_sign(XBIGF_DATA (number)) >= 0)
			return number;

		bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(number));

		bigf_abs(ent_scratch_bigf, XBIGF_DATA(number));
		return make_bigf_bf(ent_scratch_bigf);
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		if (bigfr_sign(XBIGFR_DATA (number)) >= 0)
			return number;

		bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(number));

		bigfr_abs(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_MPFR */

#if defined(HAVE_PSEUG) && defined WITH_PSEUG && defined(HAVE_MPFR)
	if (BIGGP(number)) {
		bigfr_set_prec(ent_scratch_bigfr,
			       internal_get_precision(Qnil));

		bigg_abs(ent_scratch_bigfr, XBIGG_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_PSEUG && HAVE_MPFR */

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	if (BIGCP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGC_GET_PREC(number));

		if (bigc_nan_p(XBIGC_DATA(number)))
			return make_indef(NOT_A_NUMBER);
		else if (bigc_inf_p(XBIGC_DATA(number)))
			return make_indef(POS_INFINITY);
		else
			bigc_abs(ent_scratch_bigfr, XBIGC_DATA(number));

		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_PSEUG */

	if (INDEFP(number)) {
		if (XINDEF_DATA(number) == POS_INFINITY)
			return number;
		else if (XINDEF_DATA(number) == NEG_INFINITY)
			return make_indef(POS_INFINITY);
		else
			return number;
	}

	return Fabs(wrong_type_argument(Qnumberp, number));
}

#if defined(HAVE_FPFLOAT)
/* fuck fuck fuck, I want this in number.el */
DEFUN("float", Ffloat, 1, 1, 0,	/*
Return the floating point number numerically equal to NUMBER.
*/
      (number))
{
	/* Just create the float in order of preference */
	return Fcoerce_number(number, Qfloat, Qnil);
}
#endif  /* HAVE_FPFLOAT */

#ifdef HAVE_FPFLOAT
DEFUN("logb", Flogb, 1, 1, 0,	/*
Return largest integer <= the base 2 log of the magnitude of NUMBER.
This is the same as the exponent of a float.
*/
      (number))
{
	fpfloat f = extract_float(number);

	if (f == 0.0)
		return make_int(EMACS_INT_MIN);
#ifdef HAVE_LOGB
	{
		fpfloat _lb = logb(f);
		Lisp_Object val;
		IN_FLOAT(val = make_int((EMACS_INT)_lb), "logb", number);
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
		fpfloat d;
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
#endif				/* HAVE_FPFLOAT */

DEFUN("ceiling", Fceiling, 1, 1, 0,	/*
Return the smallest integer no less than NUMBER.  (Round toward +inf.)
*/
      (number))
{
#ifdef HAVE_FPFLOAT
	if (FLOATP(number)) {
		fpfloat d;
		d = ceil(XFLOAT_DATA(number));
		return (float_to_int(d, "ceiling", number, Qunbound));
	}
#endif				/* HAVE_FPFLOAT */

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
 	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#if defined(HAVE_MPQ) && defined(HAVE_MPZ) && defined WITH_GMP
	if (BIGQP(number)) {
		bigz_ceil(ent_scratch_bigz,
			  XBIGQ_NUMERATOR(number),
			  XBIGQ_DENOMINATOR(number));
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
	}
#endif

#if defined HAVE_MPF && defined WITH_GMP
	else if (BIGFP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigf_ceil(ent_scratch_bigf, XBIGF_DATA(number));
		bigz_set_bigf(ent_scratch_bigz, ent_scratch_bigf);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		bigf_ceil(ent_scratch_bigf, XBIGF_DATA(number));
		return make_int((EMACS_INT)bigf_to_long(ent_scratch_bigf));
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	else if (BIGFRP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigfr_ceil(ent_scratch_bigfr, XBIGFR_DATA(number));
		bigz_set_bigfr(ent_scratch_bigz, ent_scratch_bigfr);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		bigfr_ceil(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_int((EMACS_INT)bigfr_to_long(ent_scratch_bigfr));
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

	if (INDEFP(number))
		return number;

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC ||	\
	defined HAVE_PSEUG && defined WITH_PSEUG
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
	ase_object_type_t ntquo;
	Lisp_Object quo;

	CHECK_COMPARABLE(number);
	if (NILP(divisor)) {
		return Ffloor(number, make_int(1L));

	} 

	/* !NILP(divisor) */

	CHECK_COMPARABLE(divisor);

	if (INTEGERP(number) && INTEGERP(divisor)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		/* this is the optimised version, since
		 * bigz_floor always takes two arguments
		 */
		number = ent_lift(number, BIGZ_T, NULL);
		divisor = ent_lift(divisor, BIGZ_T, NULL);

		bigz_floor(ent_scratch_bigz,
			   XBIGZ_DATA(number),
			   XBIGZ_DATA(divisor));
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else
		number = ent_lift(number, FLOAT_T, NULL);
		divisor = ent_lift(divisor, FLOAT_T, NULL);
#endif
	}

	quo = ent_binop(ASE_BINARY_OP_QUO, number, divisor);
	ntquo = ase_optable_index(quo);

	switch (ntquo) {
	case INT_T:		/* trivial */
	case BIGZ_T:
	case INDEF_T:
		return quo;
		break;
	case FLOAT_T: {
		fpfloat d;
		IN_FLOAT((d = floor(XFLOAT_DATA(quo))), "floor", quo);
		return (float_to_int(d, "floor", quo, Qunbound));
	}
	case BIGQ_T:
#if defined(HAVE_MPQ) && defined(HAVE_MPZ) && defined WITH_GMP
		bigz_floor(ent_scratch_bigz,
			   XBIGQ_NUMERATOR(quo), XBIGQ_DENOMINATOR(quo));
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
		break;
#else
		return quo;
#endif
	case BIGF_T:
#if defined HAVE_MPF && defined WITH_GMP
		bigf_floor(ent_scratch_bigf, XBIGF_DATA(quo));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigz_set_bigf(ent_scratch_bigz, ent_scratch_bigf);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		return make_int(
			(EMACS_INT)bigf_to_long(ent_scratch_bigf));
#endif	/* HAVE_MPZ */
		break;
#endif	/* HAVE_MPF */

	case BIGFR_T:
#if defined HAVE_MPFR && defined WITH_MPFR
		bigfr_floor(ent_scratch_bigfr, XBIGFR_DATA(quo));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigz_set_bigfr(ent_scratch_bigz, ent_scratch_bigfr);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		return make_int(
			(EMACS_INT)bigfr_to_long(ent_scratch_bigfr));
#endif	/* HAVE_MPZ */
		break;
#endif	/* HAVE_MPFR */

	default:
		return quo;
	}

	return Fsignal(Qdomain_error, Qnil);
}

DEFUN("round", Fround, 1, 1, 0, /*
Return the nearest integer to NUMBER.

NUMBER has to have an archimedian valuation, #'round returns the
integer z for which | number - z | is minimal.
*/
      (number))
{
#ifdef HAVE_FPFLOAT
	if (FLOATP(number)) {
		fpfloat d;
		/* Screw the prevailing rounding mode.  */
		d = emacs_rint(XFLOAT_DATA(number));
		return (float_to_int(d, "round", number, Qunbound));
	}
#endif				/* HAVE_FPFLOAT */

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#if defined(HAVE_MPQ) && defined(HAVE_MPZ) && defined WITH_GMP
	else if (BIGQP(number)) {
		/* first off, let's create the division, remainder as well */
		/* fuck ugly? */
		mpz_tdiv_qr(ent_scratch_bigz,
			    bigq_numerator(ent_scratch_bigq),
			    XBIGQ_NUMERATOR(number),
			    XBIGQ_DENOMINATOR(number));

		/* <- denom(number) * 2 */
		mpz_mul_2exp(bigq_numerator(ent_scratch_bigq),
			     bigq_numerator(ent_scratch_bigq), 1);

		/* check if we had to add one */
		if (mpz_cmpabs(bigq_numerator(ent_scratch_bigq),
			       XBIGQ_DENOMINATOR(number)) >= 0) {
			/* >= ceil(denom(number) / 2) */
			if (mpz_sgn(bigq_numerator(ent_scratch_bigq)) > 0) {
				mpz_add_ui(ent_scratch_bigz,
					   ent_scratch_bigz, 1UL);
			} else {
				mpz_sub_ui(ent_scratch_bigz,
					   ent_scratch_bigz, 1UL);
			}
		}
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
	}
#endif	/* HAVE_MPQ && HAVE_MPZ */

#if defined HAVE_MPF && defined WITH_GMP
	else if (BIGFP(number)) {
		warn_when_safe(Qbigf, Qnotice,
			       "rounding number of type 'bigf (mpf-floats)"
			       "not yet implemented");
		return number;
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	else if (BIGFRP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigfr_rint(ent_scratch_bigfr, XBIGFR_DATA(number));
		bigz_set_bigfr(ent_scratch_bigz, ent_scratch_bigfr);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		bigfr_rint(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_int((EMACS_INT)bigfr_to_long(ent_scratch_bigfr));
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

	else if (INDEFP(number))
		return number;

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC ||	\
	defined HAVE_PSEUG && defined WITH_PSEUG
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
#ifdef HAVE_FPFLOAT
	if (FLOATP(number))
		return float_to_int(XFLOAT_DATA(number), "truncate", number,
				    Qunbound);
#endif				/* HAVE_FPFLOAT */

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (INTEGERP(number))
#else  /* !HAVE_MPZ */
	if (INTP(number))
#endif	/* HAVE_MPZ */
		return number;

#if defined HAVE_MPQ && defined WITH_GMP
	else if (BIGQP(number)) {
		bigz_div(ent_scratch_bigz,
			 XBIGQ_NUMERATOR(number),
			 XBIGQ_DENOMINATOR(number));
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
	}
#endif

#if defined HAVE_MPF && defined WITH_GMP
	else if (BIGFP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigf_trunc(ent_scratch_bigf, XBIGF_DATA(number));
		bigz_set_bigf(ent_scratch_bigz, ent_scratch_bigf);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		bigf_trunc(ent_scratch_bigf, XBIGF_DATA(number));
		return make_int((EMACS_INT)bigf_to_long(ent_scratch_bigf));
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	else if (BIGFRP(number)) {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		bigfr_trunc(ent_scratch_bigfr, XBIGFR_DATA(number));
		bigz_set_bigfr(ent_scratch_bigz, ent_scratch_bigfr);
		return ent_mpz_downgrade_maybe(ent_scratch_bigz);
#else  /* !HAVE_MPZ */
		bigfr_trunc(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_int((EMACS_INT)bigfr_to_long(ent_scratch_bigfr));
#endif	/* HAVE_MPZ */
	}
#endif	/* HAVE_MPFR */

	else if (INDEFP(number))
		return number;

#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC ||	\
	defined HAVE_PSEUG && defined WITH_PSEUG
	return Ftruncate(wrong_type_argument(Qcomparablep, number));
#else  /* !HAVE_MPC */
	return Ftruncate(wrong_type_argument(Qnumberp, number));
#endif	/* HAVE_MPC */
}

<<<<<<< HEAD
=======
DEFUN("almost=", Falmost_eq, 2, 3, 0,	/*
Return t if NUMBER1 is almost equal to NUMBER2.

Optional argument THRES can be used to specify the threshold,
float-epsilon by default.
*/
      (number1, number2, thres))
{
#if defined HAVE_FPFLOAT
	if (NILP(thres)) {
		thres = Vfloat_epsilon;
	}
	CHECK_FLOAT(thres);

	if (FLOATP(number1) && FLOATP(number2)) {
		fpfloat n1 = XFLOAT_DATA(number1);
		fpfloat n2 = XFLOAT_DATA(number2);
		fpfloat thr = XFLOAT_DATA(thres);
		fpfloat d;
		if (n1 >= n2) {
			d = n1 - n2;
		} else {
			d = n2 - n1;
		}
		return d < thr ? Qt : Qnil;
	}
#endif	/* HAVE_FPFLOAT */
	return ent_binrel(ASE_BINARY_REL_EQUALP, number1, number2) ? Qt : Qnil;
}

DEFUN("almost/=", Falmost_neq, 2, 3, 0,	/*
Return t if NUMBER1 is clearly different from NUMBER2.

Optional argument THRES can be used to specify the threshold,
float-epsilon by default.
*/
      (number1, number2, thres))
{
#if defined HAVE_FPFLOAT
	if (NILP(thres)) {
		thres = Vfloat_epsilon;
	}
	CHECK_FLOAT(thres);

	if (FLOATP(number1) && FLOATP(number2)) {
		fpfloat n1 = XFLOAT_DATA(number1);
		fpfloat n2 = XFLOAT_DATA(number2);
		fpfloat thr = XFLOAT_DATA(thres);
		fpfloat d;
		if (n1 >= n2) {
			d = n1 - n2;
		} else {
			d = n2 - n1;
		}
		return d < thr ? Qnil : Qt;
	}
#endif	/* HAVE_FPFLOAT */
	return ent_binrel(ASE_BINARY_REL_NEQP, number1, number2) ? Qt : Qnil;
}

>>>>>>> master

/* misc complex functions */
DEFUN("conjugate", Fconjugate, 1, 1, 0,	/*
Return the \(canonical\) conjugate of NUMBER.
If NUMBER is a comparable, just return NUMBER.
*/
      (number))
{
	if (COMPARABLEP(number)) {
		return number;
#if defined HAVE_PSEUG && defined WITH_PSEUG
	} else if (BIGGP(number)) {
		bigg_conj(ent_scratch_bigg, XBIGG_DATA(number));
		return make_bigg_bg(ent_scratch_bigg);
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	} else if (BIGCP(number)) {
		bigc_set_prec(ent_scratch_bigc, XBIGC_GET_PREC(number));
		bigc_conj(ent_scratch_bigc, XBIGC_DATA(number));
		return make_bigc_bc(ent_scratch_bigc);
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
	} else if (QUATERNP(number)) {
		quatern_conj(ent_scratch_quatern, XQUATERN_DATA(number));
		return make_quatern_qu(ent_scratch_quatern);
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
#if defined HAVE_PSEUG && defined WITH_PSEUG
	} else if (BIGGP(number)) {
		bigg_norm(ent_scratch_bigz, XBIGG_DATA(number));
		return make_bigz_bz(ent_scratch_bigz);
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	} else if (BIGCP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGC_GET_PREC(number));
		bigc_norm(ent_scratch_bigfr, XBIGC_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
#endif
#if defined HAVE_QUATERN && defined WITH_QUATERN
	} else if (QUATERNP(number)) {
		quatern_norm(ent_scratch_bigz, XQUATERN_DATA(number));
		return make_bigz_bz(ent_scratch_bigz);
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
#if defined HAVE_PSEUG && defined WITH_PSEUG
	} else if (BIGGP(number)) {
		return make_bigz_bz(bigg_re(XBIGG_DATA(number)));
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
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
#if defined HAVE_MPFR && defined WITH_MPFR
	} else if (REALP(number)) {
		return make_bigfr(0.0, 0UL);
#endif
#if defined HAVE_PSEUG && defined WITH_PSEUG
	} else if (BIGGP(number)) {
		return make_bigz_bz(bigg_im(XBIGG_DATA(number)));
#endif
#if defined HAVE_MPC && defined WITH_MPC ||	\
	defined HAVE_PSEUC && defined WITH_PSEUC
	} else if (BIGCP(number)) {
		return make_bigfr_bfr(bigc_im(XBIGC_DATA(number)));
#endif
	}

	/* what should the rest do? */
	return Fimaginary_part(wrong_type_argument(Qnumberp, number));
}


/* Float-rounding functions. */
#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR ||	\
	defined(HAVE_MPF) && defined WITH_GMP

DEFUN("fceiling", Ffceiling, 1, 1, 0,	/*
Return the smallest integer no less than NUMBER, as a float.
\(Round toward +inf.\)
*/
      (number))
{
#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(number));

		bigf_ceil(ent_scratch_bigf, XBIGF_DATA(number));
		return make_bigf_bf(ent_scratch_bigf);
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(number));
		
		bigfr_ceil(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_MPFR */

	if (INDEFP(number))
		return number;

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number))
		return make_float(ceil(XFLOAT_DATA(number)));
	else
		return number;
}

DEFUN("ffloor", Fffloor, 1, 1, 0,	/*
Return the largest integer no greater than NUMBER, as a float.
\(Round towards -inf.\)
*/
      (number))
{
#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(number));

		bigf_floor(ent_scratch_bigf, XBIGF_DATA(number));
		return make_bigf_bf(ent_scratch_bigf);
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(number));
		
		bigfr_floor(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_MPFR */

	if (INDEFP(number))
		return number;

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number))
		return make_float(floor(XFLOAT_DATA(number)));
	else
		return number;
}

DEFUN("fround", Ffround, 1, 1, 0,	/*
Return the nearest integer to NUMBER, as a float.
*/
      (number))
{
#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		warn_when_safe(Qbigf, Qnotice,
			       "rounding number of type 'bigf (mpf-floats)"
			       "not yet implemented");
		return number;
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(number));
		
		bigfr_rint(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_MPFR */

	if (INDEFP(number))
		return number;

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number))
		return make_float(emacs_rint(XFLOAT_DATA(number)));
	else
		return number;
}

DEFUN("ftruncate", Fftruncate, 1, 1, 0,	/*
Truncate a floating point number to an integral float value.
Rounds the value toward zero.
*/
      (number))
{
        fpfloat d;
#if defined HAVE_MPF && defined WITH_GMP
	if (BIGFP(number)) {
		bigf_set_prec(ent_scratch_bigf, XBIGF_GET_PREC(number));

		bigf_trunc(ent_scratch_bigf, XBIGF_DATA(number));
		return make_bigf_bf(ent_scratch_bigf);
	}
#endif	/* HAVE_MPF */

#if defined HAVE_MPFR && defined WITH_MPFR
	if (BIGFRP(number)) {
		bigfr_set_prec(ent_scratch_bigfr, XBIGFR_GET_PREC(number));
		
		bigfr_trunc(ent_scratch_bigfr, XBIGFR_DATA(number));
		return make_bigfr_bfr(ent_scratch_bigfr);
	}
#endif	/* HAVE_MPFR */

	if (INDEFP(number))
		return number;

	number = ent_lift(number, FLOAT_T, NULL);

	if (FLOATP(number)) {
		d = XFLOAT_DATA(number);
		if (d >= 0.0)
			d = floor(d);
		else
			d = ceil(d);
		return make_float(d);
	} else {
		return number;
	}
}
#endif  /* HAVE_MPF(R) || HAVE_FPFLOAT (float-rounding functions) */


#ifdef HAVE_FPFLOAT
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
#endif				/* HAVE_FPFLOAT */

void init_floatfns_very_early(void)
{
#ifdef HAVE_FPFLOAT
# ifdef FLOAT_CATCH_SIGILL
	signal(SIGILL, float_error);
# endif
	in_float = 0;
#endif				/* HAVE_FPFLOAT */
}

void syms_of_floatfns(void)
{

	/* Trig functions.  */

#if defined(HAVE_FPFLOAT) || defined HAVE_MPFR && defined WITH_MPFR
	DEFSUBR(Facos);
	DEFSUBR(Fasin);
	DEFSUBR(Fatan);
	DEFSUBR(Fcos);
	DEFSUBR(Fsin);
	DEFSUBR(Ftan);
#endif  /* HAVE_FPFLOAT || HAVE_MPFR*/
#if defined HAVE_MPFR && defined WITH_MPFR
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
#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
	DEFSUBR(Ferf);
	DEFSUBR(Ferfc);
	DEFSUBR(Flog_gamma);
#endif
#endif				/* 0 */

	/* Root and Log functions. */

#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
	DEFSUBR(Fexp);
#endif  /* HAVE_FPFLOAT || HAVE_MPFR */
	DEFSUBR(Fexp2);
	DEFSUBR(Fexp10);
#if 0
	DEFSUBR(Fexpt);
#endif
#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
	DEFSUBR(Flog);
#if defined HAVE_MPFR && defined WITH_MPFR
	DEFSUBR(Flog2);
#endif	/* HAVE_MPFR */
	DEFSUBR(Flog10);
	DEFSUBR(Fsqrt);
	DEFSUBR(Fcube_root);
#if defined HAVE_MPFR && defined WITH_MPFR
	DEFSUBR(Froot);
#endif
#endif  /* HAVE_FPFLOAT || HAVE_MPFR*/

	/* Inverse trig functions. */

#if defined(HAVE_FPFLOAT) || defined(HAVE_MPFR) && defined WITH_MPFR
	DEFSUBR(Facosh);
	DEFSUBR(Fasinh);
	DEFSUBR(Fatanh);
	DEFSUBR(Fcosh);
	DEFSUBR(Fsinh);
	DEFSUBR(Ftanh);
#endif  /* HAVE_FPFLOAT || HAVE_MPFR */
#if defined HAVE_MPFR && defined WITH_MPFR
	DEFSUBR(Fsech);
	DEFSUBR(Fcsch);
	DEFSUBR(Fcoth);
#endif	/* HAVE_MPFR */

	/* Rounding functions */

	DEFSUBR(Fabs);
#ifdef HAVE_FPFLOAT
	DEFSUBR(Ffloat);
	DEFSUBR(Flogb);
#endif				/* HAVE_FPFLOAT */
	DEFSUBR(Fceiling);
	DEFSUBR(Ffloor);
	DEFSUBR(Fround);
	DEFSUBR(Ftruncate);
<<<<<<< HEAD
=======
	DEFSUBR(Falmost_eq);
	DEFSUBR(Falmost_neq);
>>>>>>> master

	/* misc complex functions */
	DEFSUBR(Fconjugate);
	DEFSUBR(Fcanonical_norm);
	DEFSUBR(Freal_part);
	DEFSUBR(Fimaginary_part);

	/* Float-rounding functions. */

#if defined(HAVE_FPFLOAT) || defined(HAVE_MPF) && defined WITH_GMP ||	\
	defined(HAVE_MPFR) && defined WITH_MPFR
	DEFSUBR(Ffceiling);
	DEFSUBR(Fffloor);
	DEFSUBR(Ffround);
	DEFSUBR(Fftruncate);
#endif  /* HAVE_FPFLOAT || HAVE_MPF(R) */
}

void vars_of_floatfns(void)
{
}
