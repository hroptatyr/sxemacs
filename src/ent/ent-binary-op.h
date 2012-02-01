/*
  ent-binary-op.h -- Global Binary Operations
  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

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

#ifndef INCLUDED_ent_binary_op_h_
#define INCLUDED_ent_binary_op_h_

#include "ent-lift.h"

#define ENT_DEBUG_BINOP(args...)	ENT_DEBUG("[BINOP]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the binary optable */
typedef enum ase_binary_operation_e ase_binary_operation_t;
typedef Lisp_Object(*ase_binary_operation_f)(Lisp_Object, Lisp_Object);

enum ase_binary_operation_e {
	ASE_BINARY_OP_SUM,
	ASE_BINARY_FIRST_OP = ASE_BINARY_OP_SUM,
	ASE_BINARY_OP_DIFF,
	ASE_BINARY_OP_PROD,
	ASE_BINARY_OP_DIV,
	ASE_BINARY_OP_QUO,
	ASE_BINARY_OP_REM,
	ASE_BINARY_OP_MOD,
	ASE_BINARY_OP_POW,
	ASE_BINARY_LAST_OP = ASE_BINARY_OP_POW,
	N_ASE_BINARY_OPS
};

extern int common_lisp_slash;
extern ase_binary_operation_f
ase_binary_optable[N_ASE_BINARY_OPS][ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];

extern Lisp_Object ase_binary_operation_undefined(Lisp_Object, Lisp_Object);
extern_inline void
ent_binop_register(
	ase_binary_operation_t op,
	ase_object_type_t t1, ase_object_type_t t2,
	ase_binary_operation_f opf);
extern_inline void
ent_binop_unregister(
	ase_binary_operation_t op,
	ase_object_type_t t1, ase_object_type_t t2);
extern_inline Lisp_Object
_ent_binop(
	ase_binary_operation_t op,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2);
extern_inline Lisp_Object
ent_binop(ase_binary_operation_t op, Lisp_Object l1, Lisp_Object l2);
extern_inline Lisp_Object
ent_binop_many(ase_binary_operation_t op, int nargs, Lisp_Object *args);


extern_inline void
ent_binop_register(ase_binary_operation_t op,
		   ase_object_type_t t1, ase_object_type_t t2,
		   ase_binary_operation_f opf)
{
	ase_binary_optable[op][t1][t2] = opf;
	return;
}
extern_inline void
ent_binop_unregister(ase_binary_operation_t op,
		     ase_object_type_t t1, ase_object_type_t t2)
{
	ase_binary_optable[op][t1][t2] = ase_binary_operation_undefined;
	return;
}

extern void ase_binary_optable_init(void);

extern_inline Lisp_Object
_ent_binop(ase_binary_operation_t op,
	   ase_object_type_t l1t, Lisp_Object l1,
	   ase_object_type_t l2t, Lisp_Object l2)
{
	ase_binary_operation_f opf =
		ase_binary_optable[op][l1t][l2t];

	return opf(l1, l2);
}

extern_inline Lisp_Object
ent_binop(ase_binary_operation_t op, Lisp_Object l1, Lisp_Object l2)
{
	ase_object_type_t l1t = ase_optable_index(l1);
	ase_object_type_t l2t = ase_optable_index(l2);

	return _ent_binop(op, l1t, l1, l2t, l2);
}

extern_inline Lisp_Object
ent_binop_many(ase_binary_operation_t op, int nargs, Lisp_Object *args)
{
	REGISTER int i;
	Lisp_Object accum;
	ase_object_type_t _acct, addt;

	for (accum = args[0], i = 1; i < nargs; i++) {
		_acct = ase_optable_index(accum);
		addt = ase_optable_index(args[i]);
		accum = _ent_binop(op, _acct, accum, addt, args[i]);
	}

	return accum;
}

/* convenience funs (implement as macroes maybe?) */
#if 0
extern_inline Lisp_Object
_ent_binop_sum(ase_object_type_t l1t, Lisp_Object l1,
	       ase_object_type_t l2t, Lisp_Object l2);
extern_inline Lisp_Object
ent_binop_sum(Lisp_Object l1, Lisp_Object l2);

extern_inline void
ent_binop_register_sum(ase_object_type_t t1, ase_object_type_t t2,
		       ase_binary_operation_f opf);
extern_inline void
ent_binop_unregister_sum(ase_object_type_t t1, ase_object_type_t t2,
			 ase_binary_operation_f opf);
#endif

#if 0
#define Fplus	Fent_binop_sum
#define Fminus	Fent_binop_diff
#define Ftimes	Fent_binop_prod
#define Fdiv	Fent_binop_divX
#define Fquo	Fent_binop_div
#define Fquo2	Fent_binop_quo
#define Frem	Fent_binop_rem
#define Fmod	Fent_binop_mod
#define Fpow	Fent_binop_pow
#endif
EXFUN(Fent_binop_sum, MANY);
EXFUN(Fent_binop_diff, MANY);
EXFUN(Fent_binop_prod, MANY);
EXFUN(Fent_binop_div, MANY);
EXFUN(Fent_binop_divX, MANY);
EXFUN(Fent_binop_quo, MANY);
EXFUN(Fent_binop_rem, 2);
EXFUN(Fent_binop_mod, 2);
EXFUN(Fent_binop_pow, 2);

extern void syms_of_ent_binary_op(void);
extern void vars_of_ent_binary_op(void);

extern_inline Lisp_Object
__ent_binop_lift_1(
	ase_binary_operation_t op,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
_ent_binop_lift_1(
	ase_binary_operation_t op,
	Lisp_Object l1, ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
ent_binop_lift_1(
	ase_binary_operation_t op,
	Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
__ent_binop_lift_2(
	ase_binary_operation_t op,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
_ent_binop_lift_2(
	ase_binary_operation_t op,
	ase_object_type_t l1t, Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
ent_binop_lift_2(
	ase_binary_operation_t op,
	Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);


/* lift to first or second arg and perform an operation */
extern_inline Lisp_Object
__ent_binop_lift_1(ase_binary_operation_t op,
		   ase_object_type_t l1t, Lisp_Object l1,
		   ase_object_type_t l2t, Lisp_Object l2,
		   ent_lift_args_t la)
{
	Lisp_Object l1n = _ent_lift(l1t, l1, l2t, la);
	return _ent_binop(op, l2t, l1n, l2t, l2);
}

extern_inline Lisp_Object
_ent_binop_lift_1(ase_binary_operation_t op,
		  Lisp_Object l1, ase_object_type_t l2t, Lisp_Object l2,
		  ent_lift_args_t la)
{
	/* lifts l1 to l2t and calls the native op */
	ase_object_type_t l1t = ase_optable_index(l1);
	return __ent_binop_lift_1(op, l1t, l1, l2t, l2, la);
}

extern_inline Lisp_Object
ent_binop_lift_1(ase_binary_operation_t op,
		 Lisp_Object l1, Lisp_Object l2,
		 ent_lift_args_t la)
{
	/* lifts arg l1 to the world of l2 and calls the native op */
	ase_object_type_t l2t = ase_optable_index(l2);
	return _ent_binop_lift_1(op, l1, l2t, l2, la);
}

extern_inline Lisp_Object
__ent_binop_lift_2(ase_binary_operation_t op,
		   ase_object_type_t l1t, Lisp_Object l1,
		   ase_object_type_t l2t, Lisp_Object l2,
		   ent_lift_args_t la)
{
	Lisp_Object l2n = _ent_lift(l2t, l2, l1t, la);
	return _ent_binop(op, l1t, l1, l1t, l2n);
}

extern_inline Lisp_Object
_ent_binop_lift_2(ase_binary_operation_t op,
		  ase_object_type_t l1t, Lisp_Object l1, Lisp_Object l2,
		  ent_lift_args_t la)
{
	/* lifts l2 to l1t and calls the native op */
	ase_object_type_t l2t = ase_optable_index(l2);
	return __ent_binop_lift_2(op, l1t, l1, l2t, l2, la);
}

extern_inline Lisp_Object
ent_binop_lift_2(ase_binary_operation_t op,
		 Lisp_Object l1, Lisp_Object l2,
		 ent_lift_args_t la)
{
	/* lifts arg l2 to the world of l1 and calls the native op */
	ase_object_type_t l1t = ase_optable_index(l1);
	return _ent_binop_lift_2(op, l1t, l1, l2, la);
}

#endif	/* INCLUDED_ent_binary_op_h_ */
