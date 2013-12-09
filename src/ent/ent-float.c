/*
  ent-float.c -- Ordinary Floats for SXEmacs
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


#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "ent.h"


Lisp_Object Vmost_positive_float;
Lisp_Object Vmost_negative_float;
Lisp_Object Vleast_positive_float;
Lisp_Object Vleast_negative_float;
Lisp_Object Vleast_positive_normalised_float;
Lisp_Object Vleast_negative_normalised_float;
Lisp_Object Vfloat_epsilon;

Fixnum max_float_print_size = 0;


static Lisp_Object
mark_float(Lisp_Object SXE_UNUSED(obj))
{
	return Qnil;
}

static inline int
float_equal(Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return (ent_float(obj1) == ent_float(obj2));
}

static inline unsigned long
float_hash(Lisp_Object obj, int SXE_UNUSED(depth))
{
#if 1
	fpfloat h = 22.0/7.0*ent_float(obj);
	union {fpfloat h; long unsigned int hash;} u;

	u.h = h;
	return (long unsigned int)u.hash;
#else
	/* mod the value down to 32-bit range */
	/* #### change for 64-bit machines */
	/* WHAT THE FUCK!?! */
	return (long unsigned int)fmod(ent_float(obj), 4e9);
#endif
}

static const struct lrecord_description float_description[] = {
	{XD_END}
};

void inline
print_float(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char pigbuf[350];	/* see comments in float_to_string */

	float_to_string(pigbuf, XFLOAT_DATA(obj), sizeof(pigbuf));
	write_c_string(pigbuf, printcharfun);
}

DEFINE_BASIC_LRECORD_IMPLEMENTATION("float", float,
				    mark_float, print_float, 0, float_equal,
				    float_hash, float_description, Lisp_Float);


static inline Lisp_Object
ent_sum_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) + XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_sum_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) + ent_int(r));
}
static inline Lisp_Object
ent_sum_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(ent_int(l) + XFLOAT_DATA(r));
}

static inline Lisp_Object
ent_diff_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) - XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_diff_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) - ent_int(r));
}
static inline Lisp_Object
ent_diff_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(ent_int(l) - XFLOAT_DATA(r));
}

static inline Lisp_Object
ent_neg_FLOAT_T(Lisp_Object l)
{
	return make_float(-XFLOAT_DATA(l));
}

static inline Lisp_Object
ent_prod_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) * XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_prod_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(ent_int(l) * XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_prod_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) * ent_int(r));
}

static inline Lisp_Object
ent_div_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0f) {
		if (XFLOAT_DATA(l) > 0.0f)
			return make_indef(POS_INFINITY);
		else if (XFLOAT_DATA(l) < 0.0f)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	return make_float(XFLOAT_DATA(l)/XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_div_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0f) {
		EMACS_INT rl = ent_int(l);
		if (rl > 0)
			return make_indef(POS_INFINITY);
		else if (rl < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	return make_float(ent_int(l)/XFLOAT_DATA(r));
}
static inline Lisp_Object
ent_div_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rr = ent_int(r);
	if (rr == 0) {
		if (XFLOAT_DATA(l) > 0.0f)
			return make_indef(POS_INFINITY);
		else if (XFLOAT_DATA(l) < 0.0f)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	return make_float(XFLOAT_DATA(l)/rr);
}

static inline Lisp_Object
ent_rem_FLOAT_T(Lisp_Object SXE_UNUSED(l), Lisp_Object SXE_UNUSED(r))
{
	return make_float(0.0);
}
static inline Lisp_Object
ent_rem_FLOAT_T_INT_T(Lisp_Object SXE_UNUSED(l), Lisp_Object SXE_UNUSED(r))
{
	return make_float(0.0);
}

static inline Lisp_Object
ent_mod_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	fpfloat rem;

	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(XFLOAT_DATA(l), XFLOAT_DATA(r));

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XFLOAT_DATA(r) < 0 ? rem > 0 : rem < 0)
		rem += XFLOAT_DATA(r);
	return make_float(rem);
}
static inline Lisp_Object
ent_mod_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	fpfloat rem;
	EMACS_INT rr = ent_int(r);

	if (rr == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(XFLOAT_DATA(l), rr);

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (rr < 0 ? rem > 0 : rem < 0)
		rem += rr;
	return make_float(rem);
}
static inline Lisp_Object
ent_mod_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	fpfloat rem;

	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(ent_int(l), XFLOAT_DATA(r));

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XFLOAT_DATA(r) < 0.0f ? rem > 0.0f : rem < 0.f)
		rem += XFLOAT_DATA(r);
	return make_float(rem);
}

static inline Lisp_Object
ent_inv_FLOAT_T(Lisp_Object l)
{
	if (XFLOAT_DATA(l) == 0.0f) {
		return make_indef(POS_INFINITY);
	}
	return make_float(1.0/XFLOAT_DATA(l));
}
static inline Lisp_Object
ent_pow_FLOAT_T_integer(Lisp_Object l, Lisp_Object r)
{
	fpfloat retval;
	fpfloat x = XFLOAT_DATA(l);
	EMACS_INT y = ent_int(r);

	if (y < 0) {
		if (x == 1.0)
			retval = 1.0;
		else if (x == -1.0)
			retval = (y & 1) ? -1.0 : 1.0;
		else
			retval = 0.0;
	} else {
		retval = 1.0;
		while (y > 0) {
			if (y & 1)
				retval *= x;
			x *= x;
			y = (EMACS_UINT) y >> 1;
		}
	}
	return make_float(retval);
}
static inline Lisp_Object
ent_pow_FLOAT_T_float(Lisp_Object l, Lisp_Object r)
{
	fpfloat f1 = ent_float(l);
	fpfloat f2 = ent_float(r);

	/* Really should check for overflow, too */
	if (f1 == 0.0 && f2 == 0.0)
		return make_float(1.0);
	else if (f1 == 0.0 && f2 < 0.0)
		Fsignal(Qarith_error, r);
	else if (f1 < 0 && f2 != floor(f2))
		Fsignal(Qdomain_error, r);
	else
		return make_float(pow(f1, f2));

	return make_float(0.0);
}

/* relations */
static inline int
ent_lt_float(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) < XFLOAT_DATA(r));
}
static inline int
ent_lt_int_float(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) < XFLOAT_DATA(r));
}
static inline int
ent_lt_float_int(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) < ent_int(r));
}

static inline int
ent_gt_float(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) > XFLOAT_DATA(r));
}
static inline int
ent_gt_float_int(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) > ent_int(r));
}
static inline int
ent_gt_int_float(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) > XFLOAT_DATA(r));
}

static inline int
ent_eq_float(Lisp_Object l, Lisp_Object r)
{
#if defined HAVE_CLEAN_FLOATOPS || 1	/* we wait until this breaks */
	return (XFLOAT_DATA(l) == XFLOAT_DATA(r));
#else
	fpfloat diff;

	diff = XFLOAT_DATA(l) - XFLOAT_DATA(r);

	if (diff == (fpfloat)0.0)
		return 1;
	else if (diff < (fpfloat)0.0 && diff > -XFLOAT_DATA(Vfloat_epsilon))
		return 1;
	else if (diff > (fpfloat)0.0 && diff < XFLOAT_DATA(Vfloat_epsilon))
		return 1;
	else
		return 0;
#endif
}
static inline int
ent_eq_int_float(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) == XFLOAT_DATA(r));
}
static inline int
ent_eq_float_int(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) == ent_int(r));
}

static inline int
ent_ne_float(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) != XFLOAT_DATA(r));
}
static inline int
ent_ne_int_float(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) != XFLOAT_DATA(r));
}
static inline int
ent_ne_float_int(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) != ent_int(r));
}


static inline Lisp_Object
ent_lift_INT_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	return make_float(ent_int(number));
}
static inline Lisp_Object
_ent_lift_INT_T_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	return make_float(ent_int(number));
}

static inline Lisp_Object
ent_lift_FLOAT_T_INT_T(Lisp_Object number, unsigned long precision)
{
	return Ftruncate(number);
}
static inline Lisp_Object
_ent_lift_FLOAT_T_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	return Ftruncate(number);
}

static inline int
ent_float_zerop(Lisp_Object l)
{
	return (XFLOAT_DATA(l) == 0.0f);
}

static inline int
ent_float_onep(Lisp_Object l)
{
	return (XFLOAT_DATA(l) == 1.0f);
}

static inline int
ent_float_unitp(Lisp_Object unused)
{
	return 1;
}


static ase_nullary_operation_f Qent_float_zero, Qent_float_one;
static inline void
ent_float_nullary_optable_init(void)
{
	Qent_float_zero = make_float(0.0f);
	Qent_float_one = make_float(1.0f);
	staticpro(&Qent_float_zero);
	staticpro(&Qent_float_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, FLOAT_T, Qent_float_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, FLOAT_T, Qent_float_one);
}

static inline void
ent_float_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, FLOAT_T, ent_neg_FLOAT_T);
	ent_unop_register(ASE_UNARY_OP_INV, FLOAT_T, ent_inv_FLOAT_T);
}

static inline void
ent_float_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, FLOAT_T, ent_sum_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   FLOAT_T, INT_T, ent_sum_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, FLOAT_T, ent_sum_INT_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, FLOAT_T, ent_diff_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   FLOAT_T, INT_T, ent_diff_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, FLOAT_T, ent_diff_INT_T_FLOAT_T);

	/* products */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, FLOAT_T, ent_prod_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   FLOAT_T, INT_T, ent_prod_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, FLOAT_T, ent_prod_INT_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, FLOAT_T, ent_div_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   FLOAT_T, INT_T, ent_div_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, FLOAT_T, ent_div_INT_T_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, FLOAT_T, ent_div_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   FLOAT_T, INT_T, ent_div_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, FLOAT_T, ent_div_INT_T_FLOAT_T);

	/* remainders */
	ent_binop_register(ASE_BINARY_OP_REM,
			   FLOAT_T, FLOAT_T, ent_rem_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   FLOAT_T, INT_T, ent_rem_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_REM,
			   INT_T, FLOAT_T, ent_rem_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   FLOAT_T, FLOAT_T, ent_mod_FLOAT_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   FLOAT_T, INT_T, ent_mod_FLOAT_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   INT_T, FLOAT_T, ent_mod_INT_T_FLOAT_T);
	/* powers */
	ent_binop_register(ASE_BINARY_OP_POW,
			   FLOAT_T, INT_T, ent_pow_FLOAT_T_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   FLOAT_T, FLOAT_T, ent_pow_FLOAT_T_float);
}

static inline void
ent_float_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, FLOAT_T, ent_float_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, FLOAT_T, ent_float_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, FLOAT_T, ent_float_unitp);
}

static inline void
ent_float_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, FLOAT_T, ent_lt_float);
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    FLOAT_T, INT_T, ent_lt_float_int);
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, FLOAT_T, ent_lt_int_float);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, FLOAT_T, ent_gt_float);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    FLOAT_T, INT_T, ent_gt_float_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, FLOAT_T, ent_gt_int_float);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, FLOAT_T, ent_eq_float);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    FLOAT_T, INT_T, ent_eq_float_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, FLOAT_T, ent_eq_int_float);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, FLOAT_T, ent_ne_float);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, FLOAT_T, ent_ne_int_float);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    FLOAT_T, INT_T, ent_ne_float_int);
}

static inline void
ent_float_lifttable_init(void)
{
	/* lift tables (coercion) */
	ent_lift_register(INT_T, FLOAT_T, _ent_lift_INT_T_FLOAT_T);
	ent_lift_register(FLOAT_T, INT_T, _ent_lift_FLOAT_T_INT_T);
	ent_lift_register(INDEF_T, FLOAT_T, ent_lift_INDEF_T_COMPARABLE);
}

void init_optables_FLOAT_T(void)
{
	ent_float_nullary_optable_init();
	ent_float_unary_optable_init();
	ent_float_binary_optable_init();
	ent_float_unary_reltable_init();
	ent_float_binary_reltable_init();
	ent_float_lifttable_init();
}


void init_ent_float(void)
{
}

void syms_of_ent_float(void)
{
	INIT_LRECORD_IMPLEMENTATION(float);
}

void vars_of_ent_float(void)
{
	fpfloat f = 0.0, fp = 0.0;

	f = 1.0;
	while ( (f > fp) &&
		(f = 2.0 * (fp = f)) &&
		! ENT_FLOAT_INDEFINITE_P(f) );

	DEFVAR_CONST_LISP("most-positive-float", &Vmost_positive_float /*
The float closest in value to +infinity.
								       */);
	Vmost_positive_float = make_float(fp);

	f = -1.0;
	while ( (f < fp) &&
		(f = 2.0 * (fp = f)) &&
		! ENT_FLOAT_INDEFINITE_P(f) );

	DEFVAR_CONST_LISP("most-negative-float", &Vmost_negative_float /*
The float closest in value to -infinity.
								       */);
	Vmost_negative_float = make_float(fp);

	{
		char tmp[] = "1.0";
	/* let's compute the array we need to print such a float */
#if fpfloat_double_p
		max_float_print_size = snprintf(tmp, sizeof(tmp), "%f", fp);
#elif fpfloat_long_double_p
		max_float_print_size = snprintf(tmp, sizeof(tmp), "%Lf", fp);
#endif
	}
	assert(max_float_print_size>0);
	max_float_print_size += 10;

	DEFVAR_CONST_INT("max-float-print-size", &max_float_print_size /*
The maximal string length of a printed float.
								       */);

	/* other stuff */
	f = 1.0;
	while ((f = (fp = f) / 2) != 0.0);
	DEFVAR_CONST_LISP("least-positive-float", &Vleast_positive_float /*
The float closest in value to +0.
								       */);
	Vleast_positive_float = make_float(fp);

	f = -1.0;
	while ((f = (fp = f) / 2) != -0.0);
	DEFVAR_CONST_LISP("least-negative-float", &Vleast_negative_float /*
The float closest in value to -0.
								       */);
	Vleast_negative_float = make_float(fp);

	for( f = fp = 1.0; (f /= 2) * 2 == fp && f != 0; fp = f );
	DEFVAR_CONST_LISP("least-positive-normalised-float",
			  &Vleast_positive_normalised_float /*
The float closest in value to +0 without rounding errors.
							    */);
	Vleast_positive_normalised_float = make_float(fp);

	for( f = fp = -1.0; ( f /= 2) * 2 == fp && f != 0; fp = f);
	DEFVAR_CONST_LISP("least-negative-normalised-float",
			  &Vleast_negative_normalised_float /*
The float closest in value to -0 without rounding errors.
							    */);
	Vleast_negative_normalised_float = make_float(fp);

	DEFVAR_CONST_LISP("float-epsilon", &Vfloat_epsilon /*
The least positive float which, added to 1, is still greater than 1.
							   */);
#if defined DBL_EPSILON
	Vfloat_epsilon = make_float(DBL_EPSILON);
#else  /* !DBL_EPSILON */
	f = 1.0;
	while ((f = (fp = f) / 2) + 1 != 1);
	Vfloat_epsilon = make_float(fp);
#endif	/* DBL_EPSILON */

	Fprovide(intern("fpfloat"));
	Fprovide(intern("lisp-float-type"));
}

/* ent-float.c ends here */
