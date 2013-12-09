/*
  ent-indef.c -- Indefinite symbols for SXEmacs
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

static inline int indef_eq(Lisp_Object, Lisp_Object);
static inline int indef_neq(Lisp_Object, Lisp_Object);
static int indef_lt(Lisp_Object, Lisp_Object);
static int indef_gt(Lisp_Object, Lisp_Object);
#if 0
static int indef_le(Lisp_Object, Lisp_Object);
static int indef_ge(Lisp_Object, Lisp_Object);
#endif
static Lisp_Object indef_negate(Lisp_Object);


Lisp_Object Vnot_a_number;
Lisp_Object Vpinfinity;
Lisp_Object Vninfinity;
Lisp_Object Vcomplex_infinity;


static void
indef_print (Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *istr = indef_to_string(XINDEF_DATA(obj));
	write_c_string((char*)istr, printcharfun);
	xfree(istr);
	istr = (Bufbyte *)NULL;
	return;
}

static int
indef_equal (Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return (XINDEF_DATA(obj1) == XINDEF_DATA(obj2));
}

static unsigned long
indef_hash (Lisp_Object obj, int SXE_UNUSED(depth))
{
	return (unsigned long)XINDEF_DATA(obj);
}

static const struct lrecord_description indef_description[] = {
	{ XD_INT, offsetof(Lisp_Indef, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("indef", indef,
				    NULL, indef_print, NULL,
				    indef_equal, indef_hash,
				    indef_description, Lisp_Indef);


Bufbyte *indef_to_string(indef i)
{
	Bufbyte *str;

	switch (i) {
	case POS_INFINITY:
		str = xnew_atomic_array(Bufbyte, 10);
		memcpy((char*)str, "+infinity\000", 10);
		break;
	case NEG_INFINITY:
		str = xnew_atomic_array(Bufbyte, 10);
		memcpy((char*)str, "-infinity\000", 10);
		break;
	case NOT_A_NUMBER:
		str = xnew_atomic_array(Bufbyte, 13);
		memcpy((char*)str, "not-a-number\000", 13);
		break;
	case COMPLEX_INFINITY:
		str = xnew_atomic_array(Bufbyte, 17);
		memcpy((char*)str, "complex-infinity\000", 17);
		break;
	case END_OF_COMPARABLE_INFINITIES:
	case END_OF_INFINITIES:
	case NUMBER_INDEFS:
	default:
		str = xnew_atomic_array(Bufbyte, 26);
		memcpy((char*)str, "unknown indefinite symbol\000", 26);
		break;
	}

	return str;
}

static Lisp_Object
ent_lift_indef(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	if (INFINITYP(number))
		return number;
	else
		signal_error(Qdomain_error, list1(number));

	return Qnil;
}

Lisp_Object
ent_lift_INDEF_T_COMPARABLE(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	if (COMPARABLE_INDEF_P(number))
		return number;
	else
		signal_error(Qdomain_error, list1(number));

	return Qnil;
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
		ent_optable_indef_div[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_div[i][NOT_A_NUMBER] = NOT_A_NUMBER;
		ent_optable_indef_rem[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_rem[i][NOT_A_NUMBER] = NOT_A_NUMBER;
		ent_optable_indef_pow[NOT_A_NUMBER][i] = NOT_A_NUMBER;
		ent_optable_indef_pow[i][NOT_A_NUMBER] = NOT_A_NUMBER;
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

static Lisp_Object
indef_sum(Lisp_Object l, Lisp_Object r)
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
static Lisp_Object
indef_diff(Lisp_Object l, Lisp_Object r)
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
static Lisp_Object
indef_prod(Lisp_Object l, Lisp_Object r)
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
static Lisp_Object
indef_div(Lisp_Object l, Lisp_Object r)
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
static Lisp_Object
indef_inv(Lisp_Object l)
{
	switch (XINDEF_DATA(l)) {
	case POS_INFINITY:
		return Qzero;
	case NEG_INFINITY:
		return Qzero;
	case COMPLEX_INFINITY:
	case NOT_A_NUMBER:
		return l;

	case END_OF_COMPARABLE_INFINITIES:
	case END_OF_INFINITIES:
	case NUMBER_INDEFS:
	default:
		abort();	/* punishment enough? */
		break;
	}
	return l;
}

static Lisp_Object
indef_rem(Lisp_Object l, Lisp_Object r)
{
	if (INDEFP(l) && INDEFP(r))
		return INDEF_AUTOOPERATE(rem, l, r);

	if (INDEFP(l) && COMPARABLEP(r)) {
		return make_indef(NOT_A_NUMBER);
	} else if (COMPARABLEP(l) && INFINITYP(r)) {
		return l;
	} else if (COMPARABLEP(l) && NOT_A_NUMBER_P(r)) {
		return r;
	} else if (INFINITE_POINT_P(r)) {
		return l;
	} else
		return make_indef(NOT_A_NUMBER);
}

static Lisp_Object
indef_pow(Lisp_Object l, Lisp_Object r)
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
		Lisp_Object comp[3] = {make_int(-1), l, make_int(1)};
		if (ent_unrel(ASE_UNARY_REL_ONEP, l))	/* l == 1 */
			return l;
		else if (ent_binrel_transitive_many(
				 ASE_BINARY_REL_LESSP, 3, comp))
			/* -1 < l < 1 */
			return make_int(0);
		else if (ent_binrel(ASE_BINARY_REL_LESSP, Qone, l)) {
			/* 1 < l */
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

static inline int
indef_eq(Lisp_Object l1, Lisp_Object l2)
{
	if (INDEFP(l1) && INDEFP(l2) &&
	    INFINITYP(l1) && INFINITYP(l2))
		return (XINDEF_DATA(l1) == XINDEF_DATA(l2));
	else if (!COMPARABLEP(l1) || !COMPARABLEP(l2)) {
		Fsignal(Qrelation_error, list2(l1, l2));
		return Qnil;
	} else
		return 0;
}

static inline int
indef_neq(Lisp_Object l1, Lisp_Object l2)
{
	return !(indef_eq(l1, l2));
}

static int
indef_lt(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1) || !COMPARABLEP(l2)) {
		Fsignal(Qrelation_error, list2(l1, l2));
		return Qnil;
	}

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

#if 0
static int
indef_le(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1) || !COMPARABLEP(l2)) {
		Fsignal(Qrelation_error, list2(l1, l2));
		return Qnil;
	}

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
#endif

static int
indef_gt(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1) || !COMPARABLEP(l2)) {
		Fsignal(Qrelation_error, list2(l1, l2));
		return Qnil;
	}

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

#if 0
static int
indef_ge(Lisp_Object l1, Lisp_Object l2)
{
	if (!COMPARABLEP(l1) || !COMPARABLEP(l2)) {
		Fsignal(Qrelation_error, list2(l1, l2));
		return Qnil;
	}

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
#endif

static inline int
ent_indef_neutralp(Lisp_Object unused)
{
	return 0;
}


static inline void
ent_indef_nullary_optable_init(void)
{
	ent_nullop_register(ASE_NULLARY_OP_ZERO, INDEF_T, Qzero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, INDEF_T, Qone);
}

static inline void
ent_indef_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, INDEF_T, indef_negate);
	ent_unop_register(ASE_UNARY_OP_INV, INDEF_T, indef_inv);
}

static inline void
ent_indef_binary_optable_init(void)
{
	ase_object_type_t i;

	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		ent_binop_register(ASE_BINARY_OP_SUM, i, INDEF_T, indef_sum);
		ent_binop_register(ASE_BINARY_OP_SUM, INDEF_T, i, indef_sum);
		ent_binop_register(ASE_BINARY_OP_DIFF, i, INDEF_T, indef_diff);
		ent_binop_register(ASE_BINARY_OP_DIFF, INDEF_T, i, indef_diff);
		ent_binop_register(ASE_BINARY_OP_PROD, i, INDEF_T, indef_prod);
		ent_binop_register(ASE_BINARY_OP_PROD, INDEF_T, i, indef_prod);
		ent_binop_register(ASE_BINARY_OP_DIV, i, INDEF_T, indef_div);
		ent_binop_register(ASE_BINARY_OP_DIV, INDEF_T, i, indef_div);
		ent_binop_register(ASE_BINARY_OP_QUO, i, INDEF_T, indef_div);
		ent_binop_register(ASE_BINARY_OP_QUO, INDEF_T, i, indef_div);
		ent_binop_register(ASE_BINARY_OP_REM, i, INDEF_T, indef_rem);
		ent_binop_register(ASE_BINARY_OP_REM, INDEF_T, i, indef_rem);
		ent_binop_register(ASE_BINARY_OP_POW, i, INDEF_T, indef_pow);
		ent_binop_register(ASE_BINARY_OP_POW, INDEF_T, i, indef_pow);
	}
}

static inline void
ent_indef_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, INDEF_T, ent_indef_neutralp);
	ent_unrel_register(ASE_UNARY_REL_ONEP, INDEF_T, ent_indef_neutralp);
}

static inline void
ent_indef_binary_reltable_init(void)
{
	ase_object_type_t idx = INDEF_T, i;

	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		ent_binrel_register(ASE_BINARY_REL_LESSP, i, idx, indef_lt);
		ent_binrel_register(ASE_BINARY_REL_LESSP, idx, i, indef_lt);
		ent_binrel_register(ASE_BINARY_REL_GREATERP, i, idx, indef_gt);
		ent_binrel_register(ASE_BINARY_REL_GREATERP, idx, i, indef_gt);
		ent_binrel_register(ASE_BINARY_REL_EQUALP, i, idx, indef_eq);
		ent_binrel_register(ASE_BINARY_REL_EQUALP, idx, i, indef_eq);
		ent_binrel_register(ASE_BINARY_REL_NEQP, i, idx, indef_neq);
		ent_binrel_register(ASE_BINARY_REL_NEQP, idx, i, indef_neq);
	}
}

static inline void
ent_indef_lifttable_init(void)
{
	ase_object_type_t i;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		ent_lift_register(INDEF_T, i, ent_lift_indef);
	}

}

void init_optables_INDEF_T(void)
{
	ent_indef_nullary_optable_init();
	ent_indef_unary_optable_init();
	ent_indef_binary_optable_init();
	ent_indef_unary_reltable_init();
	ent_indef_binary_reltable_init();
	ent_indef_lifttable_init();
}

void init_ent_indef(void)
{
	init_indef_table();
}

void syms_of_ent_indef(void)
{
	INIT_LRECORD_IMPLEMENTATION(indef);
}

void vars_of_ent_indef(void)
{
/* Now define +infinity and -infinity, complex-infinity and not-a-number */
	Vnot_a_number = make_indef_internal((indef)NOT_A_NUMBER);
	Vninfinity = make_indef_internal((indef)NEG_INFINITY);
	Vpinfinity = make_indef_internal((indef)POS_INFINITY);
	Vcomplex_infinity = make_indef_internal((indef)COMPLEX_INFINITY);

	DEFVAR_CONST_LISP("not-a-number", &Vnot_a_number /*
Not a number.
*/);
	DEFVAR_CONST_LISP("+infinity", &Vpinfinity /*
Positive infinity.
*/);
	DEFVAR_CONST_LISP("-infinity", &Vninfinity /*
Negative infinity.
*/);
	DEFVAR_CONST_LISP("complex-infinity", &Vcomplex_infinity /*
The infinitely distant point in the complex plane.
*/);

	staticpro(&Vnot_a_number);
	staticpro(&Vninfinity);
	staticpro(&Vpinfinity);
	staticpro(&Vcomplex_infinity);

	Fprovide(intern("indefinite"));
	Fprovide(intern("infinity"));
}

/* ent-indef.c ends here */
