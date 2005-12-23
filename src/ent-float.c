/*
  ent-float.c -- Ordinary Floats for SXEmacs
  Copyright (C) 2005 Sebastian Freundt

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



static Lisp_Object ent_sum_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) + XFLOAT_DATA(r));
}
static Lisp_Object ent_diff_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) - XFLOAT_DATA(r));
}
static Lisp_Object ent_neg_FLOAT_T(Lisp_Object l)
{
	return make_float(-XFLOAT_DATA(l));
}
static Lisp_Object ent_prod_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return make_float(XFLOAT_DATA(l) * XFLOAT_DATA(r));
}
static Lisp_Object ent_div_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_float(XFLOAT_DATA(l)/XFLOAT_DATA(r));
}
static Lisp_Object ent_rem_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	if (NILP(l));
	return make_float(0.0);
}
extern double fmod(double, double);
static Lisp_Object ent_mod_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	double rem;

	if (XFLOAT_DATA(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = fmod(XFLOAT_DATA(l), XFLOAT_DATA(r));

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
	double retval;
	double x = XFLOAT_DATA(l);
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
	double f1 = extract_float(l);
	double f2 = extract_float(r);

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
	return (XFLOAT_DATA(l) < XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) > XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) == XFLOAT_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return (XFLOAT_DATA(l) != XFLOAT_DATA(r))
		? Qt : Qnil;
}


void init_optables_FLOAT_T(void)
{
	ent_optable_sum[FLOAT_T][FLOAT_T] = ent_sum_FLOAT_T;
	ent_optable_diff[FLOAT_T][FLOAT_T] = ent_diff_FLOAT_T;
	ent_optable_neg[FLOAT_T] = ent_neg_FLOAT_T;
	ent_optable_prod[FLOAT_T][FLOAT_T] = ent_prod_FLOAT_T;
	ent_optable_div[FLOAT_T][FLOAT_T] = ent_div_FLOAT_T;
	ent_optable_inv[FLOAT_T] = ent_inv_FLOAT_T;
	ent_optable_quo[FLOAT_T][FLOAT_T] = ent_div_FLOAT_T;
	ent_optable_rem[FLOAT_T][FLOAT_T] = ent_rem_FLOAT_T;
	ent_optable_mod[FLOAT_T][FLOAT_T] = ent_mod_FLOAT_T;
	ent_optable_pow[FLOAT_T][INT_T] = ent_pow_FLOAT_T_integer;
	ent_optable_pow[FLOAT_T][FLOAT_T] = ent_pow_FLOAT_T_float;

	ent_optable_lt[FLOAT_T][FLOAT_T] = ent_lt_FLOAT_T;
	ent_optable_gt[FLOAT_T][FLOAT_T] = ent_gt_FLOAT_T;
	ent_optable_eq[FLOAT_T][FLOAT_T] = ent_eq_FLOAT_T;
	ent_optable_ne[FLOAT_T][FLOAT_T] = ent_ne_FLOAT_T;
	ent_optable_vallt[FLOAT_T][FLOAT_T] = ent_lt_FLOAT_T;
	ent_optable_valgt[FLOAT_T][FLOAT_T] = ent_gt_FLOAT_T;
	ent_optable_valeq[FLOAT_T][FLOAT_T] = ent_eq_FLOAT_T;
	ent_optable_valne[FLOAT_T][FLOAT_T] = ent_ne_FLOAT_T;
}


void init_ent_float(void)
{
}


/* ent-float.c ends here */
