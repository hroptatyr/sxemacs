/*
  ent-int.c -- Ordinary Integers for SXEmacs
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

#include "ent-int.h"



static Lisp_Object ent_sum_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_integer(XREALINT(l) + XREALINT(r));
}
static Lisp_Object ent_diff_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_integer(XREALINT(l) - XREALINT(r));
}
static Lisp_Object ent_neg_INT_T(Lisp_Object l)
{
	return make_integer(-XREALINT(l));
}
static Lisp_Object ent_prod_INT_T(Lisp_Object l, Lisp_Object r)
{
/* Due to potential overflow, we compute using MP */
#ifdef HAVE_MPZ
	bigz bz;
	Lisp_Object result;

	bigz_init(bz);

	bigz_set_long(ent_scratch_bigz, XREALINT(l));
	bigz_set_long(bz, XREALINT(r));
	bigz_mul(ent_scratch_bigz, ent_scratch_bigz, bz);
	result = Fcanonicalize_number(make_bigz_bz(ent_scratch_bigz));

	bigz_fini(bz);
	return result;
#else
	return make_integer(XREALINT(l) * XREALINT(r));
#endif
}
static Lisp_Object ent_div_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_integer(XREALINT(l)/XREALINT(r));
}
#ifdef HAVE_MPQ
static Lisp_Object ent_quo_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);

	if (XREALINT(r) < 0)
		return make_bigq(-XREALINT(l), (unsigned long)-XREALINT(r));
	else
		return make_bigq(XREALINT(l), (unsigned long)XREALINT(r));
	return Qdomain_error;
}
#endif
static Lisp_Object ent_inv_INT_T(Lisp_Object l)
{
	if (XREALINT(l) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_integer(1L/XREALINT(l));
}
static Lisp_Object ent_rem_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rem;

	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = XREALINT(l) % XREALINT(r);

	return make_int(rem);
}
static Lisp_Object ent_mod_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rem;

	if (XREALINT(r) == 0)
		Fsignal(Qarith_error, Qnil);
	rem = XREALINT(l) % XREALINT(r);

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (XREALINT(r) < 0 ? rem > 0 : rem < 0)
		rem += XREALINT(r);

	return make_int(rem);
}
static Lisp_Object ent_pow_INT_T_integer(Lisp_Object l, Lisp_Object r)
{
#ifdef HAVE_MPZ
	unsigned long expo;
	int sign;
	Lisp_Object result;

	if (NILP(Fnonnegativep(r))) {
		sign = -1;
		r = ent_optable_neg[get_number_type(r)](r);
	} else
		sign = 1;

	if (INTP(r))
		expo = XINT(r);
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, Qnil);
	} else
		Fsignal(Qdomain_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_pow(ent_scratch_bigz, ent_scratch_bigz, expo);

	result = make_bigz_bz(ent_scratch_bigz);

	if (sign == 1)
		return result;
	else if (sign == -1) {
		sign = bigz_to_int(XBIGZ_DATA(result));

		if (sign == 1)
			return make_bigz(1L);
		else if (sign == -1)
			return (expo & 1) ? make_bigz(-1L) : make_bigz(1L);
		else
			return make_bigz(0L);
	}

	/* not reached */
	Fsignal(Qarith_error, Qnil);
	return Qzero;

#else
	EMACS_INT retval;
	EMACS_INT x = XINT(l);
	EMACS_INT y = XINT(r);

	if (y < 0) {
		if (x == 1)
			retval = 1;
		else if (x == -1)
			retval = (y & 1) ? -1 : 1;
		else
			retval = 0;
	} else {
		retval = 1;
		while (y > 0) {
			if (y & 1)
				retval *= x;
			x *= x;
			y = (EMACS_UINT) y >> 1;
		}
	}
	return make_int(retval);
#endif
}

/* comparison relations */
static Lisp_Object ent_lt_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) < XREALINT(r)) ? Qt : Qnil;
}
static Lisp_Object ent_gt_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) > XREALINT(r)) ? Qt : Qnil;
}
static Lisp_Object ent_eq_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) == XREALINT(r)) ? Qt : Qnil;
}
static Lisp_Object ent_ne_INT_T(Lisp_Object l, Lisp_Object r)
{
	return (XREALINT(l) != XREALINT(r)) ? Qt : Qnil;
}


void init_optables_INT_T(void)
{
	ent_optable_sum[INT_T][INT_T] = ent_sum_INT_T;
	ent_optable_diff[INT_T][INT_T] = ent_diff_INT_T;
	ent_optable_neg[INT_T] = ent_neg_INT_T;
	ent_optable_prod[INT_T][INT_T] = ent_prod_INT_T;
	ent_optable_div[INT_T][INT_T] = ent_div_INT_T;
#ifdef HAVE_MPQ
	ent_optable_quo[INT_T][INT_T] = ent_quo_INT_T;
#else
	ent_optable_quo[INT_T][INT_T] = ent_div_INT_T;
#endif
	ent_optable_inv[INT_T] = ent_inv_INT_T;
	ent_optable_rem[INT_T][INT_T] = ent_rem_INT_T;
	ent_optable_mod[INT_T][INT_T] = ent_mod_INT_T;
	ent_optable_pow[INT_T][INT_T] = ent_pow_INT_T_integer;
#ifdef HAVE_MPZ
	ent_optable_pow[INT_T][BIGZ_T] = ent_pow_INT_T_integer;
#endif

	ent_optable_lt[INT_T][INT_T] = ent_lt_INT_T;
	ent_optable_gt[INT_T][INT_T] = ent_gt_INT_T;
	ent_optable_eq[INT_T][INT_T] = ent_eq_INT_T;
	ent_optable_ne[INT_T][INT_T] = ent_ne_INT_T;
	ent_optable_vallt[INT_T][INT_T] = ent_lt_INT_T;
	ent_optable_valgt[INT_T][INT_T] = ent_gt_INT_T;
	ent_optable_valeq[INT_T][INT_T] = ent_eq_INT_T;
	ent_optable_valne[INT_T][INT_T] = ent_ne_INT_T;
}

void init_ent_int(void)
{
}


/* ent-int.c ends here */
