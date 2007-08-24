/*
  ase.c -- Fancifying ENT a little
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

#include <config.h>
#include <emodules.h>
#include "ent.h"
#include "ase.h"
#ifdef EMOD_ASE_MONOMOD
#include "ase-cartesian.h"
#include "ase-interval.h"
#include "ase-neighbourhood.h"
#include "ase-metric.h"
#endif

EMOD_PROVIDE(ase);

/* until we have a set theory module */
dllist_t ase_empty_sets;
Lisp_Object Qase_empty_sets;


int
_ase_less_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_LESSP, a, b);
}
int
_ase_greater_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_GREATERP, a, b);
}
int
_ase_equal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel(ASE_BINARY_REL_EQUALP, a, b);
}
int
_ase_lessequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_LESSP,
			   ASE_BINARY_REL_EQUALP, a, b);
}
int
_ase_greaterequal_p(Lisp_Object a, Lisp_Object b)
{
	return ent_binrel2(ASE_BINARY_REL_GREATERP,
			   ASE_BINARY_REL_EQUALP, a, b);
}


#ifdef EMOD_ASE_MONOMOD
void syms_of_ase_mono(void)
{
	syms_of_ase_interval();
	syms_of_ase_neighbourhood();
	syms_of_ase_cartesian();
	syms_of_ase_metric();
}
#endif

void syms_of_ase(void)
{
	EMOD_PROVIDE_SYMS(ase);
#ifdef EMOD_ASE_MONOMOD
	syms_of_ase_mono();
#endif
}

void reinit_vars_of_ase(void)
{
	ase_empty_sets = make_noseeum_dllist();
#ifdef EMOD_ASE_MONOMOD
	reinit_vars_of_ase_interval();
	reinit_vars_of_ase_neighbourhood();
	reinit_vars_of_ase_cartesian();
	reinit_vars_of_ase_metric();
#endif
}

#ifdef EMOD_ASE_MONOMOD
void vars_of_ase_mono(void)
{
	Fprovide(intern("ase-mono"));

	reinit_vars_of_ase();

	vars_of_ase_interval();
	vars_of_ase_neighbourhood();
	vars_of_ase_cartesian();
	vars_of_ase_metric();
}
#endif

void vars_of_ase(void)
{
	Fprovide(intern("ase"));
	EMOD_PROVIDE_VARS(ase);

	reinit_vars_of_ase();
#ifdef EMOD_ASE_MONOMOD
	vars_of_ase_mono();
#endif
}

