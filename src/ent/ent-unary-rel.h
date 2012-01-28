/*
  ent-unary-rel.h -- Global Unary Relations
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

#ifndef INCLUDED_ent_unary_rel_h_
#define INCLUDED_ent_unary_rel_h_

#define ENT_DEBUG_UNREL(args...)	ENT_DEBUG("[UNREL]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the unary reltable */
typedef enum ase_unary_relation_e ase_unary_relation_t;
typedef int(*ase_unary_relation_f)(Lisp_Object);

enum ase_unary_relation_e {
	ASE_UNARY_REL_ZEROP,
	ASE_UNARY_FIRST_REL = ASE_UNARY_REL_ZEROP,
	ASE_UNARY_REL_ONEP,
	ASE_UNARY_REL_UNITP,
	ASE_UNARY_LAST_REL = ASE_UNARY_REL_UNITP,
	N_ASE_UNARY_RELS
};

extern ase_unary_relation_f
ase_unary_reltable[N_ASE_UNARY_RELS][ASE_OPTABLE_SIZE];

extern int ase_unary_relation_undefined(Lisp_Object l);

extern_inline void
ent_unrel_register(
	ase_unary_relation_t rel,
	ase_object_type_t t, ase_unary_relation_f relf);
extern_inline void
ent_unrel_unregister(ase_unary_relation_t rel, ase_object_type_t t);
extern_inline int
_ent_unrel(ase_unary_relation_t rel, ase_object_type_t lt, Lisp_Object l);
extern_inline int
ent_unrel(ase_unary_relation_t rel, Lisp_Object l);


extern_inline void
ent_unrel_register(ase_unary_relation_t rel,
		   ase_object_type_t t, ase_unary_relation_f relf)
{
	ase_unary_reltable[rel][t] = relf;
	return;
}
extern_inline void
ent_unrel_unregister(ase_unary_relation_t rel, ase_object_type_t t)
{
	ase_unary_reltable[rel][t] = ase_unary_relation_undefined;
	return;
}

extern void ase_unary_reltable_init(void);

extern_inline int
_ent_unrel(ase_unary_relation_t rel, ase_object_type_t lt, Lisp_Object l)
{
	ase_unary_relation_f relf =
		ase_unary_reltable[rel][lt];

	return relf(l);
}
extern_inline int
ent_unrel(ase_unary_relation_t rel, Lisp_Object l)
{
	ase_object_type_t lt = ase_optable_index(l);

	return _ent_unrel(rel, lt, l);
}

#define _ent_unrel_zerop(_t, _o)	_ent_unrel(ASE_UNARY_REL_ZEROP, _t, _o)
#define ent_unrel_zerop(_o)		ent_unrel(ASE_UNARY_REL_ZEROP, _o)
#define _ent_unrel_onep(_t, _o)		_ent_unrel(ASE_UNARY_REL_ONEP, _t, _o)
#define ent_unrel_onep(_o)		ent_unrel(ASE_UNARY_REL_ONEP, _o)
#define _ent_unrel_unitp(_t, _o)	_ent_unrel(ASE_UNARY_REL_UNITP, _t, _o)
#define ent_unrel_unitp(_o)		ent_unrel(ASE_UNARY_REL_UNITP, _o)

EXFUN(Fzerop, 1);
EXFUN(Fonep, 1);
EXFUN(Funitp, 1);

extern void syms_of_ent_unary_rel(void);
extern void vars_of_ent_unary_rel(void);

#endif	/* INCLUDED_ent_unary_rel_h_ */
