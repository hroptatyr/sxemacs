/*
  ent-nullary-op.h -- Global Nullary Operations
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

#ifndef INCLUDED_ent_nullary_op_h_
#define INCLUDED_ent_nullary_op_h_

#define ENT_DEBUG_NULLOP(args...)	ENT_DEBUG("[NULLOP]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the nullary optable */
typedef enum ase_nullary_operation_e ase_nullary_operation_t;
typedef Lisp_Object ase_nullary_operation_f;

enum ase_nullary_operation_e {
	ASE_NULLARY_OP_ZERO,
	ASE_NULLARY_FIRST_OP = ASE_NULLARY_OP_ZERO,
	ASE_NULLARY_OP_ONE,
	ASE_NULLARY_LAST_OP = ASE_NULLARY_OP_ONE,
	N_ASE_NULLARY_OPS
};

extern ase_nullary_operation_f
ase_nullary_optable[N_ASE_NULLARY_OPS][ASE_OPTABLE_SIZE];

extern ase_nullary_operation_f ase_nullary_operation_undefined;

extern_inline void
ent_nullop_register(
	ase_nullary_operation_t op,
	ase_object_type_t t, ase_nullary_operation_f opf);
extern_inline void
ent_nullop_unregister(ase_nullary_operation_t op, ase_object_type_t t);
extern_inline Lisp_Object
_ent_nullop(ase_nullary_operation_t op, ase_object_type_t lt);
extern_inline Lisp_Object
ent_nullop(ase_nullary_operation_t op, Lisp_Object l);


extern_inline void
ent_nullop_register(ase_nullary_operation_t op,
		    ase_object_type_t t, ase_nullary_operation_f opf)
{
	ase_nullary_optable[op][t] = opf;
	return;
}
extern_inline void
ent_nullop_unregister(ase_nullary_operation_t op, ase_object_type_t t)
{
	ase_nullary_optable[op][t] = ase_nullary_operation_undefined;
	return;
}

extern void ase_nullary_optable_init(void);

extern_inline Lisp_Object
_ent_nullop(ase_nullary_operation_t op, ase_object_type_t lt)
{
	ase_nullary_operation_f opf =
		ase_nullary_optable[op][lt];

	return opf;
}
extern_inline Lisp_Object
ent_nullop(ase_nullary_operation_t op, Lisp_Object l)
{
	ase_object_type_t lt = ase_optable_index(l);

	return _ent_nullop(op, lt);
}

#define _ent_nullop_zero(_t)	_ent_nullop(ASE_NULLARY_OP_ZERO, _t)
#define ent_nullop_zero(_o)	ent_nullop(ASE_NULLARY_OP_ZERO, _o)
#define _ent_nullop_one(_t)	_ent_nullop(ASE_NULLARY_OP_ONE, _t)
#define ent_nullop_one(_o)	ent_nullop(ASE_NULLARY_OP_ONE, _o)

EXFUN(Fzero, 1);
EXFUN(Fone, 1);

extern void syms_of_ent_nullary_op(void);
extern void vars_of_ent_nullary_op(void);

#endif	/* INCLUDED_ent_nullary_op_h_ */
