/*
  ent-binary-op.c -- Global Binary Operations
  Copyright (C) 2006-2012 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  *
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in the
  *    documentation and/or other materials provided with the distribution.
  *
  * 3. Neither the name of the author nor the names of any contributors
  *    may be used to endorse or promote products derived from this
  *    software without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
  * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
  * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "ent-optable.h"
#include "ent-binary-op.h"
#include "ent-unary-op.h"

extern Lisp_Object Qoperation_error;

int common_lisp_slash;
ase_binary_operation_f ase_binary_optable
[N_ASE_BINARY_OPS][ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];


Lisp_Object
ase_binary_operation_undefined(Lisp_Object l, Lisp_Object r)
{
	fprintf(stderr, "\n");
	print_internal(l, Qexternal_debugging_output, 0);
	fprintf(stderr, "  type:%d\n", ase_optable_index(l));
	print_internal(r, Qexternal_debugging_output, 0);
	fprintf(stderr, "  type:%d\n", ase_optable_index(r));

	Fsignal(Qoperation_error, list2(l, r));
	return Qnil;
}

static inline void
_ase_binary_optable_init(ase_binary_operation_t op)
{
	int i, j;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		for (j = 0; j < ASE_OPTABLE_SIZE; j++) {
			ent_binop_unregister(op, i, j);
		}
	}
}

void
ase_binary_optable_init(void)
{
	ase_binary_operation_t op;
	for (op = ASE_BINARY_FIRST_OP; op < N_ASE_BINARY_OPS; op++) {
		_ase_binary_optable_init(op);
	}
}


DEFUN("+", Fent_binop_sum, 0, MANY, 0, /*
Return sum of any number of arguments.
The arguments should all be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 0)
		return Qzero;

	return ent_binop_many(ASE_BINARY_OP_SUM, nargs, args);
}

DEFUN("1+", Fadd1, 1, 1, 0,	/*
Return NUMBER plus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
*/
      (number))
{
	return ent_binop(ASE_BINARY_OP_SUM, number, Qone);
}

DEFUN("-", Fent_binop_diff, 1, MANY, 0, /*
Negate number or subtract numbers, characters or markers.
With one arg, negates it.  With more than one arg,
subtracts all args left-associatively.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return ent_unop(ASE_UNARY_OP_NEG, args[0]);

	return ent_binop_many(ASE_BINARY_OP_DIFF, nargs, args);
}

DEFUN("1-", Fsub1, 1, 1, 0,	/*
Return NUMBER minus one.  NUMBER may be a number, character or marker.
Markers and characters are converted to integers.
*/
      (number))
{
	return ent_binop(ASE_BINARY_OP_DIFF, number, Qone);
}

DEFUN("*", Fent_binop_prod, 0, MANY, 0, /*
Return product of any number of arguments.
The arguments should all be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 0)
		return Qone;

	return ent_binop_many(ASE_BINARY_OP_PROD, nargs, args);
}

DEFUN("/", Fent_binop_divquo, 1, MANY, 0, /*
Return first argument divided by all the remaining arguments.
The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.
*/
      (int nargs, Lisp_Object *args))
{
	if (!common_lisp_slash)
		return Fent_binop_div(nargs, args);
	else
		return Fent_binop_quo(nargs, args);
}

DEFUN("div", Fent_binop_div, 1, MANY, 0, /*
Return the division of the first argument by all remaining
arguments, possibly leaving a rest.

The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.

The division of `a' and `b' is defined as the largest number `c'
such that \(* b c\) is less or equal `a'.
Hereby, `c' lies in the larger category of `a' and `b'.

The rest can be queried by `mod'.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return ent_binop(ASE_BINARY_OP_DIV, Qone, args[0]);

	return ent_binop_many(ASE_BINARY_OP_DIV, nargs, args);
}

DEFUN("//", Fent_binop_quo, 1, MANY, 0,	/*
Return first argument divided by all the remaining arguments.
If a rest occurred, the category is enlarged, such that
the division can be performed without leaving a rest.

The arguments must be numbers, characters or markers.
With one argument, reciprocates the argument.
*/
      (int nargs, Lisp_Object * args))
{
	if (nargs == 1)
		return ent_binop(ASE_BINARY_OP_QUO, Qone, args[0]);

	return ent_binop_many(ASE_BINARY_OP_QUO, nargs, args);
}

DEFUN("%", Fent_binop_rem, 2, 2, 0,	/*
Return remainder of first arg divided by second.
Both must be integers, characters or markers.
*/
      (number1, number2))
{
	return ent_binop(ASE_BINARY_OP_REM, number1, number2);
}

DEFUN("mod", Fent_binop_mod, 2, 2, 0,	/*
Return NUMBER modulo MODULUS.
The result falls in [0, MODULUS)
NUMBER must be a number and MODULUS must be a comparable,
a character or marker.

The result value lies in the larger category of NUMBER
and MODULUS.
*/
      (number, modulus))
{
	return ent_binop(ASE_BINARY_OP_MOD, number, modulus);
}

DEFUN("^", Fent_binop_pow, 2, 2, 0,	/*
Return the power NUMBER1 ^ NUMBER2.
*/
      (number1, number2))
{
	return ent_binop(ASE_BINARY_OP_POW, number1, number2);
}


void
syms_of_ent_binary_op(void)
{
	DEFSUBR(Fent_binop_sum);
	DEFSUBR(Fadd1);
	DEFSUBR(Fent_binop_diff);
	DEFSUBR(Fsub1);
	DEFSUBR(Fent_binop_prod);
	DEFSUBR(Fent_binop_divquo);
	DEFSUBR(Fent_binop_div);
	DEFSUBR(Fent_binop_quo);

	DEFSUBR(Fent_binop_rem);
	DEFSUBR(Fent_binop_mod);

	DEFSUBR(Fent_binop_pow);
}

void
vars_of_ent_binary_op(void)
{
	common_lisp_slash = 0;
	DEFVAR_BOOL("common-lisp-slash", &common_lisp_slash	/*
If non-nil the function `/' behaves like the common lisp function,
that is returns a rational when the arguments are either rationals
or rational integers.
								*/ );
}

/* ent-binary-op.c ends here */
