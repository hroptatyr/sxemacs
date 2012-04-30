/*
  ent-float.h -- Fixed Precision Float Functions
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


#ifndef INCLUDED_ent_float_h_
#define INCLUDED_ent_float_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

/* debugging stuff */
#define ENT_DEBUG_FPFLOAT(args...)	ENT_DEBUG("[fpfloat]: " args)

#ifdef HAVE_MATHERR
/* Work around symbol conflict on Linux/glibc */
#ifndef DOMAIN
/* SysV matherr is not defined if _BSD_SOURCE is used, and on Linux X11 */
/* is compiled with _BSD_SOURCE which can also change the size of other */
/* types.  The configure test for matherr is broken. */
/* Bah.  Good riddance to bad rubbish. */
#undef HAVE_MATHERR
#endif
#endif

#ifdef NO_MATHERR
#undef HAVE_MATHERR
#endif

#ifdef HAVE_MATHERR
# ifdef FLOAT_CHECK_ERRNO
#  undef FLOAT_CHECK_ERRNO
# endif
# ifdef FLOAT_CHECK_DOMAIN
#  undef FLOAT_CHECK_DOMAIN
# endif
#endif

#ifndef NO_FLOAT_CHECK_ERRNO
#define FLOAT_CHECK_ERRNO
#endif

#if defined FLOAT_CHECK_ERRNO && defined HAVE_ERRNO_H
# include <errno.h>
#endif

#include <ent/ent-optable.h>

extern Lisp_Object Qfloat;

extern void print_float(Lisp_Object, Lisp_Object, int);
extern void float_to_string(char*, fpfloat, int);

/* Note: the 'unused_next_' field exists only to ensure that the
   `next' pointer fits within the structure, for the purposes of the
   free list.  This makes a difference in the unlikely case of
   sizeof(double) being smaller than sizeof(void *). */

typedef struct Lisp_Float Lisp_Float;
struct Lisp_Float {
	struct lrecord_header lheader;
	union {
		fpfloat fpf;
		struct Lisp_Float *unused_next_;
	} data;
};

DECLARE_LRECORD(float, Lisp_Float);
#define XFLOAT(x)		XRECORD(x, float, Lisp_Float)
#define XSETFLOAT(x, p)		XSETRECORD(x, p, float)
#define FLOATP(x)		RECORDP(x, float)
#define CHECK_FLOAT(x)		CHECK_RECORD(x, float)
#define CONCHECK_FLOAT(x)	CONCHECK_RECORD(x, float)

#define float_data(f)		((f)->data.fpf)
#define XFLOAT_DATA(x)		float_data(XFLOAT(x))
#define XFLOATINT(n)		extract_float(n)

extern Lisp_Object Vmost_positive_float;
extern Lisp_Object Vmost_negative_float;
extern Lisp_Object Vleast_positive_float;
extern Lisp_Object Vleast_negative_float;
extern Lisp_Object Vleast_positive_normalised_float;
extern Lisp_Object Vleast_negative_normalised_float;
extern Lisp_Object Vfloat_epsilon;
extern Fixnum max_float_print_size;

extern void init_optables_FLOAT_T(void);
extern void init_ent_float(void);
extern void syms_of_ent_float(void);
extern void vars_of_ent_float(void);
extern Lisp_Object make_float(fpfloat);

#include "ent-inf.h"
#include "ent-strflt.h"
#include "ent-lift.h"

#if fpfloat_long_double_p && defined(HAVE_MATHS_LDBL_MAX)
# define FPFLOAT_MAX	LDBL_MAX
#elif fpfloat_double_p && defined(HAVE_MATHS_DBL_MAX)
# define FPFLOAT_MAX	DBL_MAX
#elif fpfloat_double_p && defined(HAVE_MATHS_MAXDOUBLE)
# define FPFLOAT_MAX	MAXDOUBLE
#endif
#if fpfloat_long_double_p && defined(HAVE_MATHS_LDBL_MIN)
# define FPFLOAT_MIN	LDBL_MIN
#elif fpfloat_double_p && defined(HAVE_MATHS_DBL_MIN)
# define FPFLOAT_MIN	DBL_MIN
#elif fpfloat_double_p && defined(HAVE_MATHS_MINDOUBLE)
# define FPFLOAT_MAX	MINDOUBLE
#endif


extern Lisp_Object Qrange_error;
EXFUN(Fsignal, 2);

extern_inline fpfloat ent_float(Lisp_Object);
#define extract_float	ent_float

extern_inline fpfloat
ent_float(Lisp_Object num)
{
	num = ent_lift(num, FLOAT_T, NULL);
	if (FLOATP(num))
		return XFLOAT_DATA(num);
	else
		Fsignal(Qrange_error, num);
	return 0.0f;
}

#endif /* INCLUDED_ent_float_h_ */
