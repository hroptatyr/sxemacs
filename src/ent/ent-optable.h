/*
  ent-optable.h -- Optable magic
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

#ifndef INCLUDED_ent_optable_h_
#define INCLUDED_ent_optable_h_

/*************************/
/* new ASE optable magic */
/*************************/
/* the number types */
typedef int ase_object_type_t;
#define ASE_OPTABLE_SIZE	lrecord_type_last_built_in_type
extern Lisp_Object Qoptable_index;

#define INT_T		(ase_object_type_t)lrecord_first_ent_type
#define BIGZ_T		(ase_object_type_t)lrecord_type_bigz
#define BIGQ_T		(ase_object_type_t)lrecord_type_bigq
#define BIGF_T		(ase_object_type_t)lrecord_type_bigf
#define BIGFR_T		(ase_object_type_t)lrecord_type_bigfr
#define FLOAT_T		(ase_object_type_t)lrecord_type_float
#define BIGG_T		(ase_object_type_t)lrecord_type_bigg
#define BIGC_T		(ase_object_type_t)lrecord_type_bigc
#define QUATERN_T	(ase_object_type_t)lrecord_type_quatern
#define OCTON_T		(ase_object_type_t)-1
#define INDEF_T		(ase_object_type_t)lrecord_type_indef


EXFUN(Ftype_of, 1);
extern int ase_optable_index(Lisp_Object arg);
extern int ase_optable_index_typesym(Lisp_Object typesym);

extern int ase_optable_add(Lisp_Object typesym);
extern void ase_optable_del(Lisp_Object typesym);

extern void init_ent_optables(void);

/* left-overs */
/* functions */
typedef Lisp_Object(*ase_ternary_operation_f)(
	Lisp_Object, Lisp_Object, Lisp_Object);
typedef enum ase_ternary_operation_e ase_ternary_operation_t;

enum ase_ternary_operation_e {
	ASE_TERNARY_FIRST_OP,
	ASE_TERNARY_LAST_OP = ASE_TERNARY_FIRST_OP,
	N_ASE_TERNARY_OPS
};


#endif	/* INCLUDED_ent_optable_h_ */
