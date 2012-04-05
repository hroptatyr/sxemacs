/*
  ent-nullary-op.c -- Global Nullary Operations
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
#include "ent-nullary-op.h"

ase_nullary_operation_f ase_nullary_optable
[N_ASE_NULLARY_OPS][ASE_OPTABLE_SIZE];


ase_nullary_operation_f ase_nullary_operation_undefined;

static inline void
_ase_nullary_optable_init(ase_nullary_operation_t op)
{
	ase_object_type_t i;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		ent_nullop_unregister(op, i);
	}
}

void
ase_nullary_optable_init(void)
{
	ase_nullary_operation_t op;
	ase_nullary_operation_undefined = Qunbound;
	for (op = ASE_NULLARY_FIRST_OP; op < N_ASE_NULLARY_OPS; op++) {
		_ase_nullary_optable_init(op);
	}
}


DEFUN("zero", Fzero, 1, 1, 0, /*
Return the zero of the world NUMBER lives in.
*/
       (number))
{
	return ent_nullop(ASE_NULLARY_OP_ZERO, number);
}

DEFUN("one", Fone, 1, 1, 0, /*
Return the one of the world NUMBER lives in.
*/
       (number))
{
	return ent_nullop(ASE_NULLARY_OP_ONE, number);
}


void
syms_of_ent_nullary_op(void)
{
	DEFSUBR(Fzero);
	DEFSUBR(Fone);
}

void
vars_of_ent_nullary_op(void)
{
}

/* ent-nullary-op.c ends here */
