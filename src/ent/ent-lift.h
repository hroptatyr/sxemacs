/*
  ent-lift.h -- Global Lifting
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

#ifndef INCLUDED_ent_lift_h_
#define INCLUDED_ent_lift_h_

#define ENT_DEBUG_LIFT(args...)		ENT_DEBUG("[LIFT]: " args)

/*************************/
/* new ASE optable magic */
/*************************/
/* the lift table */
typedef struct ent_lift_args_s *ent_lift_args_t;
typedef Lisp_Object(*ase_lift_f)(Lisp_Object, ent_lift_args_t);

struct ent_lift_args_s {
	unsigned long precision;
};

extern ase_lift_f ase_lifttable[ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];

extern Lisp_Object
ase_lift_undefined(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused));
extern Lisp_Object
ase_lift_trivial(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused));

extern_inline void
ent_lift_register(ase_object_type_t t1, ase_object_type_t t2, ase_lift_f lf);
extern_inline void
ent_lift_unregister(ase_object_type_t t1, ase_object_type_t t2);
extern_inline Lisp_Object
_ent_lift(
	ase_object_type_t l1t, Lisp_Object l, ase_object_type_t l2t,
	ent_lift_args_t la);
extern_inline Lisp_Object
ent_lift(Lisp_Object l, ase_object_type_t lt, ent_lift_args_t la);


extern_inline void
ent_lift_register(ase_object_type_t t1, ase_object_type_t t2, ase_lift_f lf)
{
	ase_lifttable[t1][t2] = lf;
	return;
}

extern_inline void
ent_lift_unregister(ase_object_type_t t1, ase_object_type_t t2)
{
	if (t1 != t2)
		ase_lifttable[t1][t2] = ase_lift_undefined;
	else
		ase_lifttable[t1][t2] = ase_lift_trivial;
	return;
}

extern void ase_lifttable_init(void);

extern_inline Lisp_Object
_ent_lift(ase_object_type_t l1t, Lisp_Object l, ase_object_type_t l2t,
	  ent_lift_args_t la)
{
	ase_lift_f liftf = ase_lifttable[l1t][l2t];

	return liftf(l, la);
}

extern_inline Lisp_Object
ent_lift(Lisp_Object l, ase_object_type_t lt, ent_lift_args_t la)
{
	ase_object_type_t l1t = ase_optable_index(l);

	return _ent_lift(l1t, l, lt, la);
}

#if 0
EXFUN(Fent_binrel_lessp, MANY);
EXFUN(Fent_binrel_greaterp, MANY);
EXFUN(Fent_binrel_equalp, MANY);
EXFUN(Fent_binrel_neqp, MANY);
#endif
EXFUN(Fent_lift, MANY);

extern void syms_of_ent_lift(void);
extern void vars_of_ent_lift(void);

#endif	/* INCLUDED_ent_lift_h_ */
