/*
  ent-unary-op.h -- Global Unary Operations
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

#ifndef INCLUDED_ent_unary_op_h_
#define INCLUDED_ent_unary_op_h_

#define ENT_DEBUG_UNOP(args...)		ENT_DEBUG("[UNOP]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the unary optable */
typedef enum ase_unary_operation_e ase_unary_operation_t;
typedef Lisp_Object(*ase_unary_operation_f)(Lisp_Object);

enum ase_unary_operation_e {
	ASE_UNARY_OP_NEG,
	ASE_UNARY_FIRST_OP = ASE_UNARY_OP_NEG,
	ASE_UNARY_OP_INV,
	ASE_UNARY_LAST_OP = ASE_UNARY_OP_INV,
	N_ASE_UNARY_OPS
};

extern ase_unary_operation_f ase_unary_optable
[N_ASE_UNARY_OPS][ASE_OPTABLE_SIZE];

extern Lisp_Object ase_unary_operation_undefined(Lisp_Object l);
extern_inline void
ent_unop_register(
	ase_unary_operation_t op,
	ase_object_type_t t,
	ase_unary_operation_f opf);
extern_inline void
ent_unop_unregister(ase_unary_operation_t op, ase_object_type_t t);
extern_inline Lisp_Object
_ent_unop(ase_unary_operation_t op, ase_object_type_t lt, Lisp_Object l);
extern_inline Lisp_Object
ent_unop(ase_unary_operation_t op, Lisp_Object l);


extern_inline void
ent_unop_register(ase_unary_operation_t op,
		  ase_object_type_t t, ase_unary_operation_f opf)
{
	ase_unary_optable[op][t] = opf;
	return;
}
extern_inline void
ent_unop_unregister(ase_unary_operation_t op, ase_object_type_t t)
{
	ase_unary_optable[op][t] = ase_unary_operation_undefined;
	return;
}

extern void ase_unary_optable_init(void);

extern_inline Lisp_Object
_ent_unop(ase_unary_operation_t op, ase_object_type_t lt, Lisp_Object l)
{
	ase_unary_operation_f opf =
		ase_unary_optable[op][lt];

	return opf(l);
}
extern_inline Lisp_Object
ent_unop(ase_unary_operation_t op, Lisp_Object l)
{
	ase_object_type_t lt = ase_optable_index(l);

	return _ent_unop(op, lt, l);
}

#define _ent_unop_neg(_t, _o)	_ent_unop(ASE_UNARY_OP_NEG, _t, _o)
#define ent_unop_neg(_o)	ent_unop(ASE_UNARY_OP_NEG, _o)
#define _ent_unop_inv(_t, _o)	_ent_unop(ASE_UNARY_OP_INV, _t, _o)
#define ent_unop_inv(_o)	ent_unop(ASE_UNARY_OP_INV, _o)


EXFUN(Fent_unop_neg, MANY);
EXFUN(Fent_unop_inv, MANY);

extern void syms_of_ent_unary_op(void);
extern void vars_of_ent_unary_op(void);

#endif	/* INCLUDED_ent_unary_op_h_ */
