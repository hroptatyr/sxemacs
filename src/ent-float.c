/*
  ent-float.c -- Ordinary Floats for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

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
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "ent-float.h"


Lisp_Object Vmost_positive_float;
Lisp_Object Vmost_negative_float;
Lisp_Object Vleast_positive_float;
Lisp_Object Vleast_negative_float;
Lisp_Object Vleast_positive_normalised_float;
Lisp_Object Vleast_negative_normalised_float;
Lisp_Object Vfloat_epsilon;

Fixnum max_float_print_size = 0;


static Lisp_Object
mark_float(Lisp_Object obj)
{
	return Qnil;

	if (obj);
}

static int
float_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return (extract_float(obj1) == extract_float(obj2));

	if (depth);
}

static unsigned long
float_hash(Lisp_Object obj, int depth)
{
	/* mod the value down to 32-bit range */
	/* #### change for 64-bit machines */
	return (unsigned long)fmod(extract_float(obj), 4e9);

	if (depth);
}

static const struct lrecord_description float_description[] = {
	{XD_END}
};

void print_float(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char pigbuf[350];	/* see comments in float_to_string */

	float_to_string(pigbuf, XFLOAT_DATA(obj));
	write_c_string(pigbuf, printcharfun);
}

DEFINE_BASIC_LRECORD_IMPLEMENTATION("float", float,
				    mark_float, print_float, 0, float_equal,
				    float_hash, float_description, Lisp_Float);


/* Extract a Lisp number as a `double', or signal an error.  */
fpfloat extract_float(Lisp_Object num)
{
	number_type nt;
	Lisp_Object tmp;

	nt = get_number_type(num);

	tmp = ent_optable_lift[nt][FLOAT_T](num, 0UL);
	if (FLOATP(tmp))
		return XFLOAT_DATA(tmp);
	else
		Fsignal(Qrange_error, tmp);
	return 0.0f;
}


static Lisp_Object ent_sum_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) + XFLOAT_DATA(r));
}
static Lisp_Object ent_sum_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) + XINT(r));
}
static Lisp_Object ent_sum_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XINT(l) + XFLOAT_DATA(r));
}

static Lisp_Object ent_diff_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) - XFLOAT_DATA(r));
}
static Lisp_Object ent_diff_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) - XINT(r));
}
static Lisp_Object ent_diff_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XINT(l) - XFLOAT_DATA(r));
}

static Lisp_Object ent_neg_FLOAT_T(Lisp_Object l)
{
	return make_float(-XFLOAT_DATA(l));
}

static Lisp_Object ent_prod_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) * XFLOAT_DATA(r));
}
static Lisp_Object ent_prod_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XREALINT(l) * XFLOAT_DATA(r));
}
static Lisp_Object ent_prod_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) * XREALINT(r));
}

static Lisp_Object ent_div_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_float(XFLOAT_DATA(l)/XFLOAT_DATA(r));
}
static Lisp_Object ent_div_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0f)
		Fsignal(Qarith_error, Qnil);
	return make_float(XREALINT(l)/XFLOAT_DATA(r));
}
static Lisp_Object ent_div_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_float(XFLOAT_DATA(l)/XREALINT(r));
}

static Lisp_Object ent_rem_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	return make_float(0.0);
}
static Lisp_Object ent_rem_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	return make_float(0.0);
}

static Lisp_Object ent_mod_FLOAT_T(Lisp_Object l, Lisp_Object r)
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
static Lisp_Object ent_mod_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	fpfloat rem;

	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(XFLOAT_DATA(l), XREALINT(r));

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XREALINT(r) < 0 ? rem > 0 : rem < 0)
		rem += XREALINT(r);
	return make_float(rem);
}
static Lisp_Object ent_mod_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	fpfloat rem;

	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(XREALINT(l), XFLOAT_DATA(r));

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XFLOAT_DATA(r) < 0 ? rem > 0 : rem < 0)
		rem += XFLOAT_DATA(r);
	return make_float(rem);
}

static Lisp_Object ent_inv_FLOAT_T(Lisp_Object l)
{
	if (XFLOAT_DATA(l) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_float(1.0/XFLOAT_DATA(l));
}
static Lisp_Object ent_pow_FLOAT_T_integer(Lisp_Object l, Lisp_Object r)
{
	fpfloat retval;
	fpfloat x = XFLOAT_DATA(l);
	EMACS_INT y = XINT(r);

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
static Lisp_Object ent_pow_FLOAT_T_float(Lisp_Object l, Lisp_Object r)
{
	fpfloat f1 = extract_float(l);
	fpfloat f2 = extract_float(r);

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
static Lisp_Object ent_lt_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (FLOATP(l) && FLOATP(r))
		return (XFLOAT_DATA(l) < XFLOAT_DATA(r)) ? Qt : Qnil;
	else
		Fsignal(Qrange_error, list2(l, r));
	return Qnil;
}
static Lisp_Object ent_lt_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) < XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_lt_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) < XREALINT(r))
		? Qt : Qnil;
}

static Lisp_Object ent_gt_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (FLOATP(l) && FLOATP(r))
		return (XFLOAT_DATA(l) > XFLOAT_DATA(r)) ? Qt : Qnil;
	else
		Fsignal(Qrange_error, list2(l, r));
	return Qnil;
}
static Lisp_Object ent_gt_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) > XREALINT(r))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) > XFLOAT_DATA(r))
		? Qt : Qnil;
}

static Lisp_Object ent_eq_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
#if HAVE_CLEAN_FLOATOPS || 1	/* we wait until this breaks */
	if (FLOATP(l) && FLOATP(r))
		return (XFLOAT_DATA(l) == XFLOAT_DATA(r)) ? Qt : Qnil;
	else
		Fsignal(Qrange_error, list2(l, r));
	return Qnil;
#else
	fpfloat diff;

	if (!FLOATP(l) || !FLOATP(r)) {
		Fsignal(Qrange_error, list2(l, r));
		return Qnil;
	}

	diff = XFLOAT_DATA(l) - XFLOAT_DATA(r);

	if (diff == (fpfloat)0.0)
		return Qt;
	else if (diff < (fpfloat)0.0 && diff > -XFLOAT_DATA(Vfloat_epsilon))
		return Qt;
	else if (diff > (fpfloat)0.0 && diff < XFLOAT_DATA(Vfloat_epsilon))
		return Qt;
	else
		return Qnil;
#endif
}
static Lisp_Object ent_eq_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) == XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) == XREALINT(r))
		? Qt : Qnil;
}

static Lisp_Object ent_ne_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) != XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_INT_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) != XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_FLOAT_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) != XREALINT(r))
		? Qt : Qnil;
}


static Lisp_Object
ent_lift_INT_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	number = ent_normalise_number(number);
	return make_float(XREALINT(number));
}

static Lisp_Object
ent_lift_FLOAT_T_INT_T(Lisp_Object number, unsigned long precision)
{
		return Ftruncate(number);
}


void init_optables_FLOAT_T(void)
{
	/* sums and diffs */
	ent_optable_sum[FLOAT_T][FLOAT_T] = ent_sum_FLOAT_T;
	ent_optable_sum[FLOAT_T][INT_T] = ent_sum_FLOAT_T_INT_T;
	ent_optable_sum[INT_T][FLOAT_T] = ent_sum_INT_T_FLOAT_T;
	ent_optable_diff[FLOAT_T][FLOAT_T] = ent_diff_FLOAT_T;
	ent_optable_diff[FLOAT_T][INT_T] = ent_diff_FLOAT_T_INT_T;
	ent_optable_diff[INT_T][FLOAT_T] = ent_diff_INT_T_FLOAT_T;

	ent_optable_neg[FLOAT_T] = ent_neg_FLOAT_T;

	/* prods and quos */
	ent_optable_prod[FLOAT_T][FLOAT_T] = ent_prod_FLOAT_T;
	ent_optable_prod[FLOAT_T][INT_T] = ent_prod_FLOAT_T_INT_T;
	ent_optable_prod[INT_T][FLOAT_T] = ent_prod_INT_T_FLOAT_T;
	ent_optable_div[FLOAT_T][FLOAT_T] = ent_div_FLOAT_T;
	ent_optable_div[FLOAT_T][INT_T] = ent_div_FLOAT_T_INT_T;
	ent_optable_div[INT_T][FLOAT_T] = ent_div_INT_T_FLOAT_T;
	ent_optable_inv[FLOAT_T] = ent_inv_FLOAT_T;
	ent_optable_quo[FLOAT_T][FLOAT_T] = ent_div_FLOAT_T;
	ent_optable_quo[FLOAT_T][INT_T] = ent_div_FLOAT_T_INT_T;
	ent_optable_quo[INT_T][FLOAT_T] = ent_div_INT_T_FLOAT_T;
	/* remainders */
	ent_optable_rem[FLOAT_T][FLOAT_T] = ent_rem_FLOAT_T;
	ent_optable_rem[FLOAT_T][INT_T] = ent_rem_FLOAT_T_INT_T;
	ent_optable_rem[INT_T][FLOAT_T] = ent_rem_FLOAT_T;
	ent_optable_mod[FLOAT_T][FLOAT_T] = ent_mod_FLOAT_T;
	ent_optable_mod[FLOAT_T][INT_T] = ent_mod_FLOAT_T_INT_T;
	ent_optable_mod[INT_T][FLOAT_T] = ent_mod_INT_T_FLOAT_T;
	/* powers */
	ent_optable_pow[FLOAT_T][INT_T] = ent_pow_FLOAT_T_integer;
	ent_optable_pow[FLOAT_T][FLOAT_T] = ent_pow_FLOAT_T_float;

	/* ordering tables */
	ent_optable_lt[FLOAT_T][FLOAT_T] = ent_lt_FLOAT_T;
	ent_optable_lt[FLOAT_T][INT_T] = ent_lt_FLOAT_T_INT_T;
	ent_optable_lt[INT_T][FLOAT_T] = ent_lt_INT_T_FLOAT_T;
	ent_optable_gt[FLOAT_T][FLOAT_T] = ent_gt_FLOAT_T;
	ent_optable_gt[FLOAT_T][INT_T] = ent_gt_FLOAT_T_INT_T;
	ent_optable_gt[INT_T][FLOAT_T] = ent_gt_INT_T_FLOAT_T;
	ent_optable_eq[FLOAT_T][FLOAT_T] = ent_eq_FLOAT_T;
	ent_optable_eq[FLOAT_T][INT_T] = ent_eq_FLOAT_T_INT_T;
	ent_optable_eq[INT_T][FLOAT_T] = ent_eq_INT_T_FLOAT_T;
	ent_optable_ne[FLOAT_T][FLOAT_T] = ent_ne_FLOAT_T;
	ent_optable_ne[INT_T][FLOAT_T] = ent_ne_INT_T_FLOAT_T;
	ent_optable_ne[FLOAT_T][INT_T] = ent_ne_FLOAT_T_INT_T;
	ent_optable_vallt[FLOAT_T][FLOAT_T] = ent_lt_FLOAT_T;
	ent_optable_valgt[FLOAT_T][FLOAT_T] = ent_gt_FLOAT_T;
	ent_optable_valeq[FLOAT_T][FLOAT_T] = ent_eq_FLOAT_T;
	ent_optable_valne[FLOAT_T][FLOAT_T] = ent_ne_FLOAT_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][FLOAT_T] = ent_lift_INT_T_FLOAT_T;
	ent_optable_lift[FLOAT_T][INT_T] = ent_lift_FLOAT_T_INT_T;
	ent_optable_lift[INDEF_T][FLOAT_T] = ent_lift_INDEF_T_COMPARABLE;
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
	fpfloat f = 0.0, fp = 0.0, fpp = 0.0;

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

	/* let's compute the array we need to print such a float */
#if fpfloat_double_p
	max_float_print_size = snprintf(NULL, 0, "%f", fp) + 10;
#elif fpfloat_long_double_p
	max_float_print_size = snprintf(NULL, 0, "%Lf", fp) + 10;
#endif

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

	f = 1.0;
	while ((f = (fp = f) / 2) * 2 == fp && f != 0);
	DEFVAR_CONST_LISP("least-positive-normalised-float",
			  &Vleast_positive_normalised_float /*
The float closest in value to +0 without rounding errors.
							    */);
	Vleast_positive_normalised_float = make_float(fp);

	f = -1.0;
	while ((f = (fp = f) / 2) * 2 == fp && f != 0);
	DEFVAR_CONST_LISP("least-negative-normalised-float",
			  &Vleast_negative_normalised_float /*
The float closest in value to -0 without rounding errors.
							    */);
	Vleast_negative_normalised_float = make_float(fp);

	f = 1.0;
	while ((f = (fp = f) / 2) + 1 != 1);
	DEFVAR_CONST_LISP("float-epsilon", &Vfloat_epsilon /*
The least positive float which, added to 1, is still greater than 1.
							   */);
	Vfloat_epsilon = make_float(fp);

	Fprovide(intern("fpfloat"));
	Fprovide(intern("lisp-float-type"));
}

/* ent-float.c ends here */
