/*
  ent-int.c -- Ordinary Integers for SXEmacs
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

EMACS_INT Vmost_negative_int, Vmost_positive_int;
Lisp_Object Qzero, Qone;


static inline int
ent_int_zerop(Lisp_Object l)
{
	return (ent_int(l) == 0);
}
static inline int
ent_int_onep(Lisp_Object l)
{
	return (ent_int(l) == 1);
}
static inline int
ent_int_unitp(Lisp_Object l)
{
	EMACS_INT rl = ent_int(l);
	return (rl == 1 || rl == -1);
}

static inline Lisp_Object
ent_sum_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_integer(ent_int(l) + ent_int(r));
}
static inline Lisp_Object
ent_diff_INT_T(Lisp_Object l, Lisp_Object r)
{
	return make_integer(ent_int(l) - ent_int(r));
}
static inline Lisp_Object
ent_neg_INT_T(Lisp_Object l)
{
	return make_integer(-ent_int(l));
}
static inline Lisp_Object
ent_prod_INT_T(Lisp_Object l, Lisp_Object r)
{
/* Due to potential overflow, we compute using MP */
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	bigz bz;
	Lisp_Object result;

	bigz_init(bz);

	bigz_set_long(ent_scratch_bigz, ent_int(l));
	bigz_set_long(bz, ent_int(r));
	bigz_mul(ent_scratch_bigz, ent_scratch_bigz, bz);
	result = ent_mpz_downgrade_maybe(ent_scratch_bigz);

	bigz_fini(bz);
	return result;
#else
	return make_integer(ent_int(l) * ent_int(r));
#endif
}
static inline Lisp_Object
ent_div_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_int(r) == 0) {
		if (ent_int(l) > 0)
			return make_indef(POS_INFINITY);
		else if (ent_int(l) < 0)
			return make_indef(NEG_INFINITY);
		else
			return make_indef(NOT_A_NUMBER);
	}
	return make_integer(ent_int(l)/ent_int(r));
}

static inline Lisp_Object
ent_inv_INT_T(Lisp_Object l)
{
	if (ent_int(l) == 0) {
		return make_indef(POS_INFINITY);
	}
	return make_integer(1L/ent_int(l));
}

static inline Lisp_Object
ent_rem_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rem;

	if (ent_int(r) == 0) {
		return Qzero;
	}
	rem = ent_int(l) % ent_int(r);

	return make_int(rem);
}
static inline Lisp_Object
ent_mod_INT_T(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT rem;

	if (ent_int(r) == 0) {
		return Qzero;
	}
	rem = ent_int(l) % ent_int(r);

	/* If the "remainder" comes out with the wrong sign, fix it.  */
	if (ent_int(r) < 0 ? rem > 0 : rem < 0)
		rem += ent_int(r);

	return make_int(rem);
}

static inline Lisp_Object
ent_pow_INT_T_integer(Lisp_Object l, Lisp_Object r)
{
	EMACS_INT retval;
	EMACS_INT x = ent_int(l);
	EMACS_INT y = ent_int(r);

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
}

/* comparison relations */
static inline int
ent_lt_int(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) < ent_int(r));
}
static inline int
ent_gt_int(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) > ent_int(r));
}
static inline int
ent_eq_int(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) == ent_int(r));
}
static inline int
ent_ne_int(Lisp_Object l, Lisp_Object r)
{
	return (ent_int(l) != ent_int(r));
}


static inline Lisp_Object
ent_lift_INT_T_INT_T(Lisp_Object number, unsigned long SXE_UNUSED(precision))
{
	return make_int(ent_int(number));
}

static inline Lisp_Object
ent_lift_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	return make_int(ent_int(number));
}


static inline void
ent_int_nullary_optable_init(void)
{
	Qzero = make_int(0);
	Qone = make_int(1);
	ent_nullop_register(ASE_NULLARY_OP_ZERO, INT_T, Qzero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, INT_T, Qone);
}

static inline void
ent_int_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, INT_T, ent_neg_INT_T);
	ent_unop_register(ASE_UNARY_OP_INV, INT_T, ent_inv_INT_T);
}

static inline void
ent_int_binary_optable_init(void)
{
	ent_binop_register(ASE_BINARY_OP_SUM, INT_T, INT_T, ent_sum_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF, INT_T, INT_T, ent_diff_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD, INT_T, INT_T, ent_prod_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV, INT_T, INT_T, ent_div_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO, INT_T, INT_T, ent_div_INT_T);
	ent_binop_register(ASE_BINARY_OP_REM, INT_T, INT_T, ent_rem_INT_T);
	ent_binop_register(ASE_BINARY_OP_MOD, INT_T, INT_T, ent_mod_INT_T);
	ent_binop_register(ASE_BINARY_OP_POW, INT_T, INT_T,
			   ent_pow_INT_T_integer);
}

static inline void
ent_int_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, INT_T, ent_int_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, INT_T, ent_int_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, INT_T, ent_int_unitp);
}

static inline void
ent_int_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_LESSP,
			    INT_T, INT_T, ent_lt_int);
	ent_binrel_register(ASE_BINARY_REL_GREATERP,
			    INT_T, INT_T, ent_gt_int);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    INT_T, INT_T, ent_eq_int);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    INT_T, INT_T, ent_ne_int);
}

static inline void
ent_int_lifttable_init(void)
{
	ent_lift_register(INT_T, INT_T, ent_lift_INT_T);
	ent_lift_register(INDEF_T, INT_T, ent_lift_INDEF_T_COMPARABLE);
}

void init_optables_INT_T(void)
{
	ent_int_nullary_optable_init();
	ent_int_unary_optable_init();
	ent_int_binary_optable_init();
	ent_int_unary_reltable_init();
	ent_int_binary_reltable_init();
	ent_int_lifttable_init();
}

void init_ent_int(void)
{
}

void syms_of_ent_int(void)
{
}

void vars_of_ent_int(void)
{
	Vmost_negative_int = EMACS_INT_MIN;
	DEFVAR_CONST_INT("most-negative-fixnum", &Vmost_negative_int /*
The (ordinary) integer closest in value to negative infinity.
								   */);

	Vmost_positive_int = EMACS_INT_MAX;
	DEFVAR_CONST_INT("most-positive-fixnum", &Vmost_positive_int /*
The (ordinary) integer closest in value to positive infinity.
								  */);
}

/* ent-int.c ends here */
