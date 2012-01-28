/*
  ent-binary-rel.h -- Global Binary Relations
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

#ifndef INCLUDED_ent_binary_rel_h_
#define INCLUDED_ent_binary_rel_h_

#define ENT_DEBUG_BINREL(args...)	ENT_DEBUG("[BINREL]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the binary reltable */
typedef enum ase_binary_relation_e ase_binary_relation_t;
typedef int(*ase_binary_relation_f)(Lisp_Object, Lisp_Object);

enum ase_binary_relation_e {
	ASE_BINARY_REL_LESSP,
	ASE_BINARY_FIRST_REL = ASE_BINARY_REL_LESSP,
	ASE_BINARY_REL_GREATERP,
	ASE_BINARY_REL_EQUALP,
	ASE_BINARY_REL_NEQP,
	ASE_BINARY_REL_SUBSETP,
	ASE_BINARY_REL_SUPERSETP,
	ASE_BINARY_REL_CONTAINSP,
	ASE_BINARY_REL_INP,
	ASE_BINARY_LAST_REL = ASE_BINARY_REL_INP,
	N_ASE_BINARY_RELS
};

extern ase_binary_relation_f
ase_binary_reltable[N_ASE_BINARY_RELS][ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];

extern int ase_binary_relation_undefined(Lisp_Object l, Lisp_Object r);
extern void ase_binary_reltable_init(void);

extern_inline void
ent_binrel_register(
	ase_binary_relation_t rel,
	ase_object_type_t t1, ase_object_type_t t2,
	ase_binary_relation_f relf);
extern_inline void
ent_binrel_unregister(
	ase_binary_relation_t rel,
	ase_object_type_t t1, ase_object_type_t t2);
extern_inline int
_ent_binrel(
	ase_binary_relation_t rel,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2);
extern_inline int
_ent_binrel2(
	ase_binary_relation_t rel1, ase_binary_relation_t rel2,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2);
extern_inline int
ent_binrel(ase_binary_relation_t rel, Lisp_Object l1, Lisp_Object l2);
extern_inline int
ent_binrel2(
	ase_binary_relation_t rel1, ase_binary_relation_t rel2,
	Lisp_Object l1, Lisp_Object l2);
extern_inline int
ent_binrel_transitive_many(
	ase_binary_relation_t rel,
	int nargs, Lisp_Object *args);
extern_inline int
ent_binrel2_transitive_many(
	ase_binary_relation_t rel1,
	ase_binary_relation_t rel2,
	int nargs, Lisp_Object *args);
extern_inline int
ent_binrel_intransitive_many(
	ase_binary_relation_t rel,
	int nargs, Lisp_Object *args);


extern_inline void
ent_binrel_register(ase_binary_relation_t rel,
		    ase_object_type_t t1, ase_object_type_t t2,
		    ase_binary_relation_f relf)
{
	ase_binary_reltable[rel][t1][t2] = relf;
	return;
}
extern_inline void
ent_binrel_unregister(ase_binary_relation_t rel,
		      ase_object_type_t t1, ase_object_type_t t2)
{
	ase_binary_reltable[rel][t1][t2] = ase_binary_relation_undefined;
	return;
}

extern_inline int
_ent_binrel(ase_binary_relation_t rel,
	    ase_object_type_t l1t, Lisp_Object l1,
	    ase_object_type_t l2t, Lisp_Object l2)
{
	ase_binary_relation_f relf =
		ase_binary_reltable[rel][l1t][l2t];

	return relf(l1, l2);
}

extern_inline int
_ent_binrel2(ase_binary_relation_t rel1, ase_binary_relation_t rel2,
	     ase_object_type_t l1t, Lisp_Object l1,
	     ase_object_type_t l2t, Lisp_Object l2)
{
	ase_binary_relation_f relf1 =
		ase_binary_reltable[rel1][l1t][l2t];
	ase_binary_relation_f relf2 =
		ase_binary_reltable[rel2][l1t][l2t];

	return (relf1(l1, l2) || relf2(l1, l2));
}

extern_inline int
ent_binrel(ase_binary_relation_t rel, Lisp_Object l1, Lisp_Object l2)
{
	ase_object_type_t l1t = ase_optable_index(l1);
	ase_object_type_t l2t = ase_optable_index(l2);

	return _ent_binrel(rel, l1t, l1, l2t, l2);
}

extern_inline int
ent_binrel2(ase_binary_relation_t rel1, ase_binary_relation_t rel2,
	    Lisp_Object l1, Lisp_Object l2)
{
	ase_object_type_t l1t = ase_optable_index(l1);
	ase_object_type_t l2t = ase_optable_index(l2);

	return _ent_binrel2(rel1, rel2, l1t, l1, l2t, l2);
}

extern_inline int
ent_binrel_transitive_many(ase_binary_relation_t rel,
			   int nargs, Lisp_Object *args)
{
	REGISTER int i;
	Lisp_Object accum;
	ase_object_type_t _acct, addt;

	accum = args[0];
	_acct = ase_optable_index(accum);
	for (i = 1; i < nargs; i++) {
		addt = ase_optable_index(args[i]);
		if (!_ent_binrel(rel, _acct, accum, addt, args[i]))
			return 0;
		accum = args[i];
		_acct = addt;
	}

	return 1;
}

extern_inline int
ent_binrel2_transitive_many(ase_binary_relation_t rel1,
			    ase_binary_relation_t rel2,
			    int nargs, Lisp_Object *args)
{
	REGISTER int i;
	Lisp_Object accum;
	ase_object_type_t _acct, addt;

	accum = args[0];
	_acct = ase_optable_index(accum);
	for (i = 1; i < nargs; i++) {
		addt = ase_optable_index(args[i]);
		if (!_ent_binrel2(rel1, rel2, _acct, accum, addt, args[i]))
			return 0;
		accum = args[i];
		_acct = addt;
	}

	return 1;
}

extern_inline int
ent_binrel_intransitive_many(ase_binary_relation_t rel,
			     int nargs, Lisp_Object *args)
{
	REGISTER int i, j;

	for (i = 0; i < nargs; i++) {
		for (j = i+1; j < nargs; j++) {
			Lisp_Object o1 = args[i], o2 = args[j];
			ase_object_type_t o1t, o2t;
			o1t = ase_optable_index(o1);
			o2t = ase_optable_index(o2);
			if (!_ent_binrel(rel, o1t, o1, o2t, o2))
				return 0;
		}
	}

	return 1;
}

#if 0
#define Flss		Fent_binrel_lessp
#define Fgtr		Fent_binrel_greaterp
#define Fleq		Fent_binrel_lessequalp
#define Fgeq		Fent_binrel_greaterequalp
#define Feqlsign	Fent_binrel_equalp
#define Fneq		Fent_binrel_neqp
#endif
EXFUN(Fent_binrel_lessp, MANY);
EXFUN(Fent_binrel_greaterp, MANY);
EXFUN(Fent_binrel_lessequalp, MANY);
EXFUN(Fent_binrel_greaterequalp, MANY);
EXFUN(Fent_binrel_equalp, MANY);
EXFUN(Fent_binrel_neqp, MANY);
EXFUN(Fmin, MANY);
EXFUN(Fmax, MANY);

extern void syms_of_ent_binary_rel(void);
extern void vars_of_ent_binary_rel(void);

extern_inline Lisp_Object
__ent_binrel_lift_1(
	ase_binary_relation_t rel,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
__ent_binrel_lift_1(
	ase_binary_relation_t rel,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
_ent_binrel_lift_1(
	ase_binary_relation_t rel,
	Lisp_Object l1, ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
ent_binrel_lift_1(
	ase_binary_relation_t op,
	Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
__ent_binrel_lift_2(
	ase_binary_relation_t rel,
	ase_object_type_t l1t, Lisp_Object l1,
	ase_object_type_t l2t, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
_ent_binrel_lift_2(
	ase_binary_relation_t rel,
	ase_object_type_t l1t, Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);
extern_inline Lisp_Object
ent_binrel_lift_2(
	ase_binary_relation_t rel,
	Lisp_Object l1, Lisp_Object l2,
	ent_lift_args_t la);


/* lift to first or second arg and perform an operation */
extern_inline Lisp_Object
__ent_binrel_lift_1(ase_binary_relation_t rel,
		    ase_object_type_t l1t, Lisp_Object l1,
		    ase_object_type_t l2t, Lisp_Object l2,
		    ent_lift_args_t la)
{
	/* lifts arg l1 to the world of l2 and calls the native op */
	Lisp_Object l1n = _ent_lift(l1t, l1, l2t, la);
	return ent_binrel(rel, l1n, l2);
}

extern_inline Lisp_Object
_ent_binrel_lift_1(ase_binary_relation_t rel,
		   Lisp_Object l1, ase_object_type_t l2t, Lisp_Object l2,
		   ent_lift_args_t la)
{
	/* lifts l1 to l2t and calls the native op */
	ase_object_type_t l1t = ase_optable_index(l1);
	return __ent_binrel_lift_1(rel, l1t, l1, l2t, l2, la);
}

extern_inline Lisp_Object
ent_binrel_lift_1(ase_binary_relation_t op,
		  Lisp_Object l1, Lisp_Object l2,
		  ent_lift_args_t la)
{
	/* lifts arg l1 to the world of l2 and calls the native op */
	ase_object_type_t l2t = ase_optable_index(l2);
	return _ent_binrel_lift_1(op, l1, l2t, l2, la);
}

extern_inline Lisp_Object
__ent_binrel_lift_2(ase_binary_relation_t rel,
		    ase_object_type_t l1t, Lisp_Object l1,
		    ase_object_type_t l2t, Lisp_Object l2,
		    ent_lift_args_t la)
{
	/* lifts arg l2 to the world of l1 and calls the native op */
	Lisp_Object l2n = _ent_lift(l2t, l2, l1t, la);
	return ent_binrel(rel, l1, l2n);
}

extern_inline Lisp_Object
_ent_binrel_lift_2(ase_binary_relation_t rel,
		   ase_object_type_t l1t, Lisp_Object l1, Lisp_Object l2,
		   ent_lift_args_t la)
{
	/* lifts l2 to l1t and calls the native op */
	ase_object_type_t l2t = ase_optable_index(l2);
	return __ent_binrel_lift_2(rel, l1t, l1, l2t, l2, la);
}

extern_inline Lisp_Object
ent_binrel_lift_2(ase_binary_relation_t rel,
		  Lisp_Object l1, Lisp_Object l2,
		  ent_lift_args_t la)
{
	/* lifts arg l2 to the world of l1 and calls the native op */
	ase_object_type_t l1t = ase_optable_index(l1);
	return _ent_binrel_lift_2(rel, l1t, l1, l2, la);
}

#endif	/* INCLUDED_ent_binary_rel_h_ */
