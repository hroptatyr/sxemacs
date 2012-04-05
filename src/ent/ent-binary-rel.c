/*
  ent-binary-rel.c -- Global Binary Relations
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
#include "ent-lift.h"
#include "ent-binary-op.h"
#include "ent-binary-rel.h"

extern Lisp_Object Qrelation_error;

ase_binary_relation_f ase_binary_reltable
[N_ASE_BINARY_OPS][ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];


int
ase_binary_relation_undefined(Lisp_Object l, Lisp_Object r)
{
	Fsignal(Qrelation_error, list2(l, r));
	return 0;
}

static inline void
_ase_binary_reltable_init(ase_binary_relation_t rel)
{
	int i, j;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		for (j = 0; j < ASE_OPTABLE_SIZE; j++) {
			ent_binrel_unregister(rel, i, j);
		}
	}
}

inline void
ase_binary_reltable_init(void)
{
	ase_binary_relation_t rel;
	for (rel = ASE_BINARY_FIRST_REL; rel < N_ASE_BINARY_RELS; rel++) {
		_ase_binary_reltable_init(rel);
	}
}


DEFUN("<", Fent_binrel_lessp, 1, MANY, 0, /*
Return t if the ARGUMENTS are strictly monotonically increasing.
Arguments: &rest arguments

If there is more than one argument, the second argument, must be
numerically greater than the first, and the third, must be numerically
greater than the second, and so on. At least one argument is required.

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel_transitive_many(ASE_BINARY_REL_LESSP, nargs, args))
		return Qt;
	else
		return Qnil;
}

DEFUN(">", Fent_binrel_greaterp, 1, MANY, 0, /*
Return t if the ARGUMENTS are strictly monotonically decreasing.
Arguments: &rest arguments

If there is more than one argument, the second argument, must be
numerically smaller than the first, and the third, must be numerically
smaller than the second, and so on. At least one argument is required.

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel_transitive_many(ASE_BINARY_REL_GREATERP, nargs, args))
		return Qt;
	else
		return Qnil;
}

DEFUN("<=", Fent_binrel_lessequalp, 1, MANY, 0,	/*
Return t if the ARGUMENTS are monotonically decreasing.
Arguments: &rest arguments

If there is more than one argument, the second argument, must be
numerically greater than or equal to the first, and the third, must
be numerically greater than or equal to the second, and so on. At
least one argument is required.

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object * args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel2_transitive_many(
		    ASE_BINARY_REL_LESSP, ASE_BINARY_REL_EQUALP,
		    nargs, args))
		return Qt;
	else
		return Qnil;
}

DEFUN(">=", Fent_binrel_greaterequalp, 1, MANY, 0, /*
Return t if the ARGUMENTS are monotonically increasing.
Arguments: &rest arguments

If there is more than one argument, the second argument, must be
numerically smaller than or equal to the first, and the third, must
be numerically smaller than or equal to the second, and so on. At
least one argument is required.

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel2_transitive_many(
		    ASE_BINARY_REL_GREATERP, ASE_BINARY_REL_EQUALP,
		    nargs, args))
		return Qt;
	else
		return Qnil;
}

DEFUN("=", Fent_binrel_equalp, 1, MANY, 0, /*
Return t if all the arguments are numerically equal.
Arguments: &rest arguments

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel_transitive_many(ASE_BINARY_REL_EQUALP, nargs, args))
		return Qt;
	else
		return Qnil;
}

DEFUN("/=", Fent_binrel_neqp, 1, MANY, 0, /*
Return t if no two arguments are numerically equal.
Arguments: &rest arguments

The arguments may be numbers, characters or markers.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return Qt;

	if (ent_binrel_intransitive_many(ASE_BINARY_REL_NEQP, nargs, args))
		return Qt;
	else
		return Qnil;
}


DEFUN("min", Fmin, 1, MANY, 0, /*
Return smallest of all the arguments.
All arguments must be numbers, characters or markers.
The value is always a number; markers and characters are converted
to numbers.
*/
      (int nargs, Lisp_Object *args))
{
	REGISTER int i, minindex;
	Lisp_Object compmin, compi;
	ase_object_type_t nti, ntmin;

	minindex = 0;
	compmin = args[minindex];
	ntmin = ase_optable_index(compmin);

	for (i = 1; i < nargs; i++) {
		compi = args[i];
		nti = ase_optable_index(compi);

		if (_ent_binrel(ASE_BINARY_REL_LESSP,
				nti, compi, ntmin, compmin)) {
			 minindex = i;
			 ntmin = nti;
			 compmin = compi;
		}
	}
	return args[minindex];
}

DEFUN("max", Fmax, 1, MANY, 0, /*
Return largest of all the arguments.
All arguments must be numbers, characters or markers.
The value is always a number; markers and characters are converted
to numbers.
*/
      (int nargs, Lisp_Object *args))
{
	REGISTER int i, maxindex;
	Lisp_Object compmax, compi;
	ase_object_type_t nti, ntmax;

	maxindex = 0;
	compmax = args[maxindex];
	ntmax = ase_optable_index(compmax);

	for (i = 1; i < nargs; i++) {
		compi = args[i];
		nti = ase_optable_index(compi);

		if (_ent_binrel(ASE_BINARY_REL_GREATERP,
				nti, compi, ntmax, compmax)) {
			 maxindex = i;
			 ntmax = nti;
			 compmax = compi;
		}
	}
	return args[maxindex];
}


/* convenience functions */


void
syms_of_ent_binary_rel(void)
{
	DEFSUBR(Fent_binrel_lessp);
	DEFSUBR(Fent_binrel_greaterp);
	DEFSUBR(Fent_binrel_lessequalp);
	DEFSUBR(Fent_binrel_greaterequalp);
	DEFSUBR(Fent_binrel_equalp);
	DEFSUBR(Fent_binrel_neqp);
	DEFSUBR(Fmax);
	DEFSUBR(Fmin);
}

void
vars_of_ent_binary_rel(void)
{
}

/* ent-binary-rel.c ends here */
