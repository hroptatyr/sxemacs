/*
  ent-indef.c -- Indefinite symbols for SXEmacs
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

#include "ent-indef.h"

static int indef_eql(Lisp_Object, Lisp_Object);
static int indef_lt(Lisp_Object, Lisp_Object);
static int indef_gt(Lisp_Object, Lisp_Object);
static int indef_le(Lisp_Object, Lisp_Object);
static int indef_ge(Lisp_Object, Lisp_Object);
static Lisp_Object indef_negate(Lisp_Object);


Lisp_Object Vnot_a_number;
Lisp_Object Vpinfinity;
Lisp_Object Vninfinity;
Lisp_Object Vcomplex_infinity;


Bufbyte *indef_to_string(indef i)
{
	Bufbyte *str;

	switch (i) {
	case POS_INFINITY:
		str = xnew_array(Bufbyte, 10);
		strncpy((char*)str, "+infinity", 9);
		str[9] = '\0';
		break;
	case NEG_INFINITY:
		str = xnew_array(Bufbyte, 10);
		strncpy((char*)str, "-infinity", 9);
		str[9] = '\0';
		break;
	case NOT_A_NUMBER:
		str = xnew_array(Bufbyte, 13);
		strncpy((char*)str, "not-a-number", 12);
		str[12] = '\0';
		break;
	case COMPLEX_INFINITY:
		str = xnew_array(Bufbyte, 17);
		strncpy((char*)str, "complex-infinity", 16);
		str[16] = '\0';
		break;
	default:
		str = xnew_array(Bufbyte, 26);
		strncpy((char*)str, "unknown indefinite symbol", 25);
		str[25] = '\0';
	}

	return str;
}


static indef ent_optable_indef_sum[NUMBER_INDEFS][NUMBER_INDEFS];
static indef ent_optable_indef_diff[NUMBER_INDEFS][NUMBER_INDEFS];
static indef ent_optable_indef_prod[NUMBER_INDEFS][NUMBER_INDEFS];
static indef ent_optable_indef_div[NUMBER_INDEFS][NUMBER_INDEFS];
static indef ent_optable_indef_rem[NUMBER_INDEFS][NUMBER_INDEFS];
static indef ent_optable_indef_pow[NUMBER_INDEFS][NUMBER_INDEFS];

static void init_indef_table(void)
{
	indef i;

	/* initialise NOT_A_NUMBER stuff */
	for (i = 0; i < NUMBER_INDEFS; i++) {
		ent_optable_indef_sum[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_sum[i][NOT_A_NUMBER] = NOT_A_NUMBER;
		ent_optable_indef_diff[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_diff[i][NOT_A_NUMBER] = NOT_A_NUMBER;
		ent_optable_indef_prod[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_prod[i][NOT_A_NUMBER] = NOT_A_NUMBER;
		ent_optable_indef_sum[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_sum[i][NOT_A_NUMBER] = NOT_A_NUMBER;
	}

	/* Addition table */
	ent_optable_indef_sum[POS_INFINITY][POS_INFINITY] = POS_INFINITY;
	ent_optable_indef_sum[POS_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_sum[POS_INFINITY][COMPLEX_INFINITY] = NOT_A_NUMBER;

	ent_optable_indef_sum[NEG_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_sum[NEG_INFINITY][NEG_INFINITY] = NEG_INFINITY;
	ent_optable_indef_sum[NEG_INFINITY][COMPLEX_INFINITY] = NOT_A_NUMBER;

	ent_optable_indef_sum[COMPLEX_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_sum[COMPLEX_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_sum[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	/* Subtraction table */
	ent_optable_indef_diff[POS_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_diff[POS_INFINITY][NEG_INFINITY] = POS_INFINITY;
	ent_optable_indef_diff[POS_INFINITY][COMPLEX_INFINITY] = NOT_A_NUMBER;

	ent_optable_indef_diff[NEG_INFINITY][POS_INFINITY] = NEG_INFINITY;
	ent_optable_indef_diff[NEG_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_diff[NEG_INFINITY][COMPLEX_INFINITY] = NOT_A_NUMBER;

	ent_optable_indef_diff[COMPLEX_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_diff[COMPLEX_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_diff[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	/* Multiplication table */
	ent_optable_indef_prod[POS_INFINITY][POS_INFINITY] = POS_INFINITY;
	ent_optable_indef_prod[POS_INFINITY][NEG_INFINITY] = NEG_INFINITY;
	ent_optable_indef_prod[POS_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_prod[NEG_INFINITY][POS_INFINITY] = NEG_INFINITY;
	ent_optable_indef_prod[NEG_INFINITY][NEG_INFINITY] = POS_INFINITY;
	ent_optable_indef_prod[NEG_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_prod[COMPLEX_INFINITY][POS_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_prod[COMPLEX_INFINITY][NEG_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_prod[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	/* Division table */
	ent_optable_indef_div[POS_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_div[POS_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_div[POS_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_div[NEG_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_div[NEG_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_div[NEG_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_div[COMPLEX_INFINITY][POS_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_div[COMPLEX_INFINITY][NEG_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_div[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	/* Mod table */
	ent_optable_indef_rem[POS_INFINITY][POS_INFINITY] = POS_INFINITY;
	ent_optable_indef_rem[POS_INFINITY][NEG_INFINITY] = NEG_INFINITY;
	ent_optable_indef_rem[POS_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_rem[NEG_INFINITY][POS_INFINITY] = NEG_INFINITY;
	ent_optable_indef_rem[NEG_INFINITY][NEG_INFINITY] = POS_INFINITY;
	ent_optable_indef_rem[NEG_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_rem[COMPLEX_INFINITY][POS_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_rem[COMPLEX_INFINITY][NEG_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_rem[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	/* exponentiation table */
	ent_optable_indef_pow[POS_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_pow[POS_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_pow[POS_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_pow[NEG_INFINITY][POS_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_pow[NEG_INFINITY][NEG_INFINITY] = NOT_A_NUMBER;
	ent_optable_indef_pow[NEG_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

	ent_optable_indef_pow[COMPLEX_INFINITY][POS_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_pow[COMPLEX_INFINITY][NEG_INFINITY] =
		COMPLEX_INFINITY;
	ent_optable_indef_pow[COMPLEX_INFINITY][COMPLEX_INFINITY] =
		COMPLEX_INFINITY;

}

#define INDEF_AUTOOPERATE(op, l1, l2)					\
	make_indef(ent_optable_indef_##op[XINDEF_DATA(l1)][XINDEF_DATA(l2)])

static Lisp_Object indef_negate(Lisp_Object ind)
{
	if (XINDEF_DATA(ind) == POS_INFINITY) {
		return make_indef(NEG_INFINITY);
	} else if (XINDEF_DATA(ind) == NEG_INFINITY) {
		return make_indef(POS_INFINITY);
	} else if (XINDEF_DATA(ind) == COMPLEX_INFINITY) {
		return ind;
	} else {
		return make_indef(NOT_A_NUMBER);
	}
}

static Lisp_Object indef_sum(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(sum, l, r);

	if (INDEFP(l) && NUMBERP(r))
		return l;
	else if (NUMBERP(l) && INDEFP(r))
		return r;
	else
		return make_indef(NOT_A_NUMBER);
}
static Lisp_Object indef_diff(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(diff, l, r);

	if (INDEFP(l) && NUMBERP(r))
		return l;
	else if (NUMBERP(l) && INDEFP(r))
		return indef_negate(r);
	else
		return make_indef(NOT_A_NUMBER);
}
static Lisp_Object indef_prod(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(prod, l, r);

	if (INDEFP(l) && COMPARABLEP(r)) {
		if (!NILP(Fzerop(r)))
			return make_indef(NOT_A_NUMBER);
		else if (!NILP(Fnonnegativep(r)))
			return l;
		else
			return indef_negate(l);
	} else if (COMPARABLEP(l) && INDEFP(r)) {
		if (!NILP(Fzerop(l)))
			return make_indef(NOT_A_NUMBER);
		else if (!NILP(Fnonnegativep(l)))
			return r;
		else
			return indef_negate(r);
	} else if (INFINITYP(l) || INFINITYP(r)) {
		return make_indef(COMPLEX_INFINITY);
	} else {
		return make_indef(NOT_A_NUMBER);
	}
}
static Lisp_Object indef_div(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(div, l, r);

	if (INDEFP(l) && COMPARABLEP(r)) {
		if (!NILP(Fzerop(r)))
			return make_indef(NOT_A_NUMBER);
		else if (!NILP(Fnonnegativep(r)))
			return l;
		else
			return indef_negate(l);
	} else if (COMPARABLEP(l) && INDEFP(r)) {
		if (!COMPARABLE_INDEF_P(r))
			return r;
		else 
			return Qzero;
	} else if (INFINITYP(l) || INFINITYP(r)) {
		return make_indef(COMPLEX_INFINITY);
	} else {
		return make_indef(NOT_A_NUMBER);
	}
}
static Lisp_Object indef_rem(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(rem, l, r);

	if (INDEFP(l) && COMPARABLEP(r)) {
		return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(l) && INDEFP(r)) {
		return l;
	} else if (INFINITE_POINT_P(r)) {
		return l;
	} else
		return make_indef(NOT_A_NUMBER);
}
EXFUN(Feqlsign, MANY);
static Lisp_Object indef_pow(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(pow, l, r);

	if (INDEFP(l) && !NILP(Fzerop(r))) {
		return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLE_INDEF_P(l) && COMPARABLEP(r)) {
		if (!NILP(Fzerop(r)))
			return make_indef(NOT_A_NUMBER);
		else if (NILP(Fnonnegativep(r)))
			return Qzero;
		else if (XINDEF_DATA(l) == NEG_INFINITY &&
			 !NILP(Fevenp(r)))
			return make_indef(POS_INFINITY);
		else
			return l;
	} else if (INDEFP(l) && COMPARABLEP(r)) {
		return l;
	} else if (INFINITE_POINT_P(l)) {
		return make_indef(COMPLEX_INFINITY);
	} else if (INDEFP(l)) {
		return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(l) && COMPARABLE_INDEF_P(r)) {
		Lisp_Object *comp = alloca_array(Lisp_Object, 3);
		comp[0] = make_int(1);
		comp[1] = l;
		comp[2] = make_int(-1);
		if (!NILP(Feqlsign(2, comp)))	/* l == 1 */
			return l;
		else if (!NILP(Fgtr(3, comp)))	/* -1 < l < 1 */
			return make_int(0);
		else if (!NILP(Flss(2, comp))) {	/* 1 < l */
			if (XINDEF_DATA(r) == POS_INFINITY)
				return r;
			else
				return Qzero;
		} else
			return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(l) && INFINITYP(r)) {
		return make_indef(COMPLEX_INFINITY);
	} else
		return make_indef(NOT_A_NUMBER);
}

static Lisp_Object ent_lt_INDEF_T(Lisp_Object l, Lisp_Object r)
{
	return (indef_lt(l, r)) ? Qt : Qnil;
}
static Lisp_Object ent_gt_INDEF_T(Lisp_Object l, Lisp_Object r)
{
	return (indef_gt(l, r)) ? Qt : Qnil;
}
static Lisp_Object ent_eq_INDEF_T(Lisp_Object l, Lisp_Object r)
{
	return (indef_eql(l, r)) ? Qt : Qnil;
}
static Lisp_Object ent_ne_INDEF_T(Lisp_Object l, Lisp_Object r)
{
	return (indef_eql(l, r)) ? Qnil : Qt;
}

static int indef_eql(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1))
		return wrong_type_argument(Qcomparablep, l1);
	if (!COMPARABLEP(l2))
		return wrong_type_argument(Qcomparablep, l2);

	if (INDEFP(l1) && INDEFP(l2))
		return (XINDEF_DATA(l1) == XINDEF_DATA(l2));
	else
		return 0 == 1;
}

static int indef_lt(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1))
		return wrong_type_argument(Qcomparablep, l1);
	if (!COMPARABLEP(l2))
		return wrong_type_argument(Qcomparablep, l2);

	if (INDEFP(l1) && INDEFP(l2)) {
		/* only +infinity is not less than -infinity */
		if ((XINDEF_DATA(l1) == POS_INFINITY) &&
		    (XINDEF_DATA(l2) == NEG_INFINITY))
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l1)) {
		if (XINDEF_DATA(l2) == NEG_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l2)) {
		if (XINDEF_DATA(l1) == POS_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else
		return wrong_type_argument(Qindefinitep, l1);
}

static int indef_le(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1))
		return wrong_type_argument(Qcomparablep, l1);
	if (!COMPARABLEP(l2))
		return wrong_type_argument(Qcomparablep, l2);

	if (INDEFP(l1) && INDEFP(l2)) {
		/* only +infinity is not leq -infinity */
		if ((XINDEF_DATA(l1) == POS_INFINITY) &&
		    (XINDEF_DATA(l2) == NEG_INFINITY))
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l1)) {
		if (XINDEF_DATA(l2) == NEG_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l2)) {
		if (XINDEF_DATA(l1) == POS_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else
		return wrong_type_argument(Qindefinitep, l1);
}

static int indef_gt(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1))
		return wrong_type_argument(Qcomparablep, l1);
	if (!COMPARABLEP(l2))
		return wrong_type_argument(Qcomparablep, l2);

	if (INDEFP(l1) && INDEFP(l2)) {
		/* only -infinity is not greater than +infinity */
		if ((XINDEF_DATA(l1) == NEG_INFINITY) &&
		    (XINDEF_DATA(l2) == POS_INFINITY))
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l1)) {
		if (XINDEF_DATA(l2) == POS_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l2)) {
		if (XINDEF_DATA(l1) == NEG_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else
		return wrong_type_argument(Qindefinitep, l1);
}

static int indef_ge(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1))
		return wrong_type_argument(Qcomparablep, l1);
	if (!COMPARABLEP(l2))
		return wrong_type_argument(Qcomparablep, l2);

	if (INDEFP(l1) && INDEFP(l2)) {
		/* only -infinity is not geq +infinity */
		if ((XINDEF_DATA(l1) == NEG_INFINITY) &&
		    (XINDEF_DATA(l2) == POS_INFINITY))
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l1)) {
		if (XINDEF_DATA(l2) == POS_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else if (!INDEFP(l2)) {
		if (XINDEF_DATA(l1) == NEG_INFINITY)
			return (0 == 1);
		else
			return (0 == 0);
	} else
		return wrong_type_argument(Qindefinitep, l1);
}


void init_optables_INDEF_T(void)
{
	number_type i;

	for (i = 0; i < NUMBER_OF_TYPES; i++) {
		ent_optable_sum[i][INDEF_T] = indef_sum;
		ent_optable_sum[INDEF_T][i] = indef_sum;
		ent_optable_diff[i][INDEF_T] = indef_diff;
		ent_optable_diff[INDEF_T][i] = indef_diff;
		ent_optable_prod[i][INDEF_T] = indef_prod;
		ent_optable_prod[INDEF_T][i] = indef_prod;
		ent_optable_div[i][INDEF_T] = indef_div;
		ent_optable_div[INDEF_T][i] = indef_div;
		ent_optable_quo[i][INDEF_T] = indef_div;
		ent_optable_quo[INDEF_T][i] = indef_div;
		ent_optable_mod[i][INDEF_T] = indef_rem;
		ent_optable_mod[INDEF_T][i] = indef_rem;
		ent_optable_rem[i][INDEF_T] = indef_rem;
		ent_optable_rem[INDEF_T][i] = indef_rem;
		ent_optable_pow[i][INDEF_T] = indef_pow;
		ent_optable_pow[INDEF_T][i] = indef_pow;

		ent_optable_lt[INDEF_T][i] = ent_lt_INDEF_T;
		ent_optable_lt[i][INDEF_T] = ent_lt_INDEF_T;
		ent_optable_gt[INDEF_T][i] = ent_gt_INDEF_T;
		ent_optable_gt[i][INDEF_T] = ent_gt_INDEF_T;
		ent_optable_eq[INDEF_T][i] = ent_eq_INDEF_T;
		ent_optable_eq[i][INDEF_T] = ent_eq_INDEF_T;
		ent_optable_ne[INDEF_T][i] = ent_ne_INDEF_T;
		ent_optable_ne[i][INDEF_T] = ent_ne_INDEF_T;
		ent_optable_vallt[INDEF_T][i] = ent_lt_INDEF_T;
		ent_optable_vallt[i][INDEF_T] = ent_lt_INDEF_T;
		ent_optable_valgt[INDEF_T][i] = ent_gt_INDEF_T;
		ent_optable_valgt[i][INDEF_T] = ent_gt_INDEF_T;
		ent_optable_valeq[INDEF_T][i] = ent_eq_INDEF_T;
		ent_optable_valeq[i][INDEF_T] = ent_eq_INDEF_T;
		ent_optable_valne[INDEF_T][i] = ent_ne_INDEF_T;
		ent_optable_valne[i][INDEF_T] = ent_ne_INDEF_T;
	}
	ent_optable_neg[INDEF_T] = indef_negate;
}

void init_ent_indef(void)
{
	init_indef_table();

	if (initialized) {
		/* must be dumped in here, otherwise the pdumper barfs
		 * on 64-bit systems, dunno why that is
		 */
		Vnot_a_number = make_indef((indef)NOT_A_NUMBER);
		Vninfinity = make_indef((indef)NEG_INFINITY);
		Vpinfinity = make_indef((indef)POS_INFINITY);
		Vcomplex_infinity = make_indef((indef)COMPLEX_INFINITY);
	}
}


/* ent-indef.c ends here */
