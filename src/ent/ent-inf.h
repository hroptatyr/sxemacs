/*
  ent-inf.h -- Infinite and Nan Predicates
  Copyright (C) 2005, 2006 Sebastian Freundt
  Copyright (C) 2006 Nelson Ferreira

  Author:  Sebastian Freundt
	   Nelson Ferreira

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

#ifndef INCLUDED_ent_inf_h_
#define INCLUDED_ent_inf_h_

#if defined HAVE_MATH_H
# include <math.h>
#endif	/* HAVE_MATH_H */
#if defined HAVE_LIMITS_H
# include <limits.h>
#endif	/* HAVE_LIMITS_H */
#if defined HAVE_VALUES_H
# include <values.h>
#endif	/* HAVE_VALUES_H */

#if defined HAVE_STDBOOL_H
# include <stdbool.h>
#endif	/* HAVE_STDBOOL_H */

#if defined HAVE_IEEEFP_H
# include <ieeefp.h>
#endif /* HAVE_IEEEFP_H */

#if defined HAVE_MATHS_ISNAN || defined HAVE_ISNAN
# define sxe_isnan	isnan
#else  /* !HAVE_ISNAN */
/* tenative work around */
# define sxe_isnan(x)	((x) != (x))
# warning Your isnan() should be spelt isfscked().  Any ideas?
#endif	/* !HAVE_ISNAN */

#if defined HAVE_MATHS_ISINF || defined HAVE_ISINF
# define sxe_isinf	isinf
#elif defined HAVE_MATHS_FINITE || defined HAVE_FINITE
# define sxe_isinf(x)	(!finite(x))
#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
# define sxe_isinf(x)	(fpclassify(x) == FP_INFINITE)
#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define sxe_isinf(x)	(fpclass(x) == FP_PINF || fpclass(x) == FP_NINF)
#else /* anyone? */
# warning Your isinf() supply disgusts me.  How about grilling your box?
#endif	/* HAVE_ISINF */

#if defined HAVE_MATHS_SIGNBIT || defined HAVE_SIGNBIT
# define sxe_signbit	signbit
#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define sxe_signbit(x) (fpclass(x) == FP_NINF || fpclass(x) == FP_NDENORM || fpclass(x) == FP_NZERO || fpclass(x) == FP_NNORM)
#else  /* !HAVE_SIGNBIT */
# define sxe_signbit(x)	((x) < 0)
# warning Your signbit() computation is vile.  Consider scrapping your machine.
#endif	/* HAVE_SIGNBIT */

#if 0
/* just to change the order more easily */

#elif defined HAVE_MATHS_ISINF || defined HAVE_ISINF
/* this one next, as gcc seems to have optimised built-ins for this */
# define ENT_FLOAT_INF_P(_val)	(isinf(_val))

#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
# define ENT_FLOAT_INF_P(_val)	(fpclassify(_val) == FP_INFINITE)

#elif defined HAVE_MATHS_FINITE || defined HAVE_FINITE
/* this one next, as gcc seems to have optimised built-ins for this */
# define ENT_FLOAT_INF_P(_val)	(!finite(_val))

#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define ENT_FLOAT_INF_P(_val)	(fpclass(_val) == FP_PINF || fpclass(_val) == FP_NINF)

#elif defined(HAVE_MATHS_INFINITY) && 0
/* very ugly as INFINITY is actually of type float */
# define ENT_FLOAT_INF_P(_val)	((_val) == INFINITY || (_val) == -INFINITY)

#else
# define ENT_FLOAT_INF_P(_val)	(sxe_isinf(_val))
# warning infinity detection does not work
# warning assuming there are no infinities on your system
#endif

/* an inline variant to avoid multiple evaluation of X */
static inline bool __attribute__((always_inline))
ent_float_inf_p(fpfloat x)
{
	return ENT_FLOAT_INF_P(x);
}

#if 0
/* just to change the order more easily */

#elif defined HAVE_MATHS_ISINF || defined HAVE_ISINF
/* isinf() returns +1 if _val is infinite and >= 0 */
# define ENT_FLOAT_PINF_P(_val)	(isinf(_val) == 1)

#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
/* this became second choice as there is an additional signbit evaluation */
# define ENT_FLOAT_PINF_P(_val)						\
	(fpclassify(_val) == FP_INFINITE && sxe_signbit(_val) == 0)

#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define ENT_FLOAT_PINF_P(_val)	(fpclass(_val) == FP_PINF)

#elif defined(HAVE_MATHS_INFINITY) && 0
/* deprecated since types do not match */
# define ENT_FLOAT_PINF_P(_val)	((_val) == INFINITY)

#else
# define ENT_FLOAT_PINF_P(_val)	(sxe_isinf(_val) && sxe_signbit(_val) == 0)
#endif

/* a recommended inline variant to avoid multiple evaluation of X */
static inline bool __attribute__((always_inline))
ent_float_pinf_p(fpfloat x)
{
	return ENT_FLOAT_PINF_P(x);
}

#if 0
/* just so we can change the order easily */

#elif defined HAVE_MATHS_ISINF || defined HAVE_ISINF
/* isinf() is supposed to return -1 if _val is infinite and < 0 */
# define ENT_FLOAT_NINF_P(_val)	(isinf(_val) == -1)

#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
/* second choice, as isinf() seems to be more optimised */
# define ENT_FLOAT_NINF_P(_val)						\
	(fpclassify(_val) == FP_INFINITE && sxe_signbit(_val) != 0)

#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define ENT_FLOAT_NINF_P(_val)	(fpclass(_val) == FP_NINF)

#elif defined(HAVE_MATHS_INFINITY) && 0
/* type mismatch hence deprecated */
# define ENT_FLOAT_NINF_P(_val)	((_val) == -INFINITY)

#else
# define ENT_FLOAT_NINF_P(_val)	(sxe_isinf(_val) && sxe_signbit(_val) != 0)
#endif

/* a recommended inline variant to avoid multiple evaluation of X */
static inline bool __attribute__((always_inline))
ent_float_ninf_p(fpfloat x)
{
	return ENT_FLOAT_NINF_P(x);
}

#if 0
/* just so we can change the order easily */

#elif defined HAVE_MATHS_ISNAN || defined HAVE_ISNAN
/* preferred as gcc can optimise this one */
# define ENT_FLOAT_NAN_P(_val)	(isnan(_val))

#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
# define ENT_FLOAT_NAN_P(_val)	(fpclassify(_val) == FP_NAN)

#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define ENT_FLOAT_NAN_P(_val)	(fpclass(_val) == FP_QNAN || fpclass(_val) == FP_SNAN)

#elif defined(HAVE_MATHS_NAN) && 0
/* disabled because of a type mismatch */
# define ENT_FLOAT_NAN_P(_val)	((_val) == NAN)

#else
# define ENT_FLOAT_NAN_P(_val)	(sxe_isnan(_val))
# warning NAN detection possibly broken
# warning Hate mails please to your system vendor.
#endif

/* an inline variant to avoid multiple evaluation of X */
static inline bool __attribute__((always_inline))
ent_float_nan_p(fpfloat x)
{
	return ENT_FLOAT_NAN_P(x);
}

#if 0
/* to make it easy to change the order */

#elif defined HAVE_MATHS_FPCLASSIFY || defined HAVE_FPCLASSIFY
# define ENT_FLOAT_INDEFINITE_P(_val)	(fpclassify(_val) != FP_NORMAL)

#elif defined HAVE_MATHS_FPCLASS || defined HAVE_FPCLASS
# define ENT_FLOAT_INDEFINITE_P(_val)	(fpclass(_val) == FP_NINF || fpclass(_val) == FP_PINF || fpclass(_val) == FP_SNAN || fpclass(_val) == FP_SNAN)

#elif (defined HAVE_MATHS_ISNAN || defined HAVE_ISNAN) &&	\
	(defined HAVE_MATHS_ISINF || defined HAVE_ISINF || defined HAVE_MATHS_FINITE || defined HAVE_FINITE)
# define ENT_FLOAT_INDEFINITE_P(_val)	(isnan(_val) || isinf(_val))

#elif defined(HAVE_MATHS_NAN) && defined(HAVE_MATHS_INFINITY) && 0
/* severe type mismatch */
# define ENT_FLOAT_INDEFINITE_P(_val)	(((_val) == NAN)||((_val) == INFINITY))

#else
/* VERY expensive */
# define ENT_FLOAT_INDEFINITE_P(_val)				\
	(ENT_FLOAT_INF_P(_val) || ENT_FLOAT_NAN_P(_val))
# if ! ((defined(HAVE_MATHS_NAN) || defined(HAVE_MATHS_INFINITY)) &&	\
       (defined(HAVE_ISNAN) || defined(HAVE_ISINF)))
#  warning Indefinites detection possibly broken
#  warning Hate mails please to your system vendor.
# endif
#endif

/* an inline variant to avoid multiple evaluation of X, recommended */
static inline bool __attribute__((always_inline))
ent_float_indefinite_p(fpfloat x)
{
	return ENT_FLOAT_INDEFINITE_P(x);
}

#endif /* INCLUDED_ent_inf_h_ */
