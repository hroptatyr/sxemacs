/*
  ent-unary-rel.c -- Global Unary Relations
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
#include "ent-unary-rel.h"

extern Lisp_Object Qrelation_error;

ase_unary_relation_f ase_unary_reltable
[N_ASE_UNARY_RELS][ASE_OPTABLE_SIZE];


int
ase_unary_relation_undefined(Lisp_Object l)
{
	Fsignal(Qrelation_error, list1(l));
	return 0;
}

static inline void
_ase_unary_reltable_init(ase_unary_relation_t rel)
{
	ase_object_type_t i;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		ent_unrel_unregister(rel, i);
	}
}

void
ase_unary_reltable_init(void)
{
	ase_unary_relation_t rel;
	for (rel = ASE_UNARY_FIRST_REL; rel < N_ASE_UNARY_RELS; rel++) {
		_ase_unary_reltable_init(rel);
	}
}


DEFUN("zerop", Fzerop, 1, 1, 0,	/*
Return t if NUMBER is a zero.
*/
      (number))
{
	if (!ent_unrel(ASE_UNARY_REL_ZEROP, number))
		return Qnil;
	else
		return Qt;
}

DEFUN("onep", Fonep, 1, 1, 0,	/*
Return t if NUMBER is a one.
*/
      (number))
{
	if (!ent_unrel(ASE_UNARY_REL_ONEP, number))
		return Qnil;
	else
		return Qt;
}

DEFUN("unitp", Funitp, 1, 1, 0,	/*
Return t if NUMBER is a unit, nil otherwise.
That is, if there exists another number B, such that
  NUMBER * B = 1
*/
      (number))
{
	if (!ent_unrel(ASE_UNARY_REL_UNITP, number))
		return Qnil;
	else
		return Qt;
}


void
syms_of_ent_unary_rel(void)
{
	DEFSUBR(Fzerop);
	DEFSUBR(Fonep);
	DEFSUBR(Funitp);
}

void
vars_of_ent_unary_rel(void)
{
}

/* ent-unary-rel.c ends here */
