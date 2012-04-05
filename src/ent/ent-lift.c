/*
  ent-lift.c -- Global Lifting
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

ase_lift_f ase_lifttable[ASE_OPTABLE_SIZE][ASE_OPTABLE_SIZE];


Lisp_Object
ase_lift_undefined(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	signal_error(Qdomain_error, list1(number));
	return Qnil;
}

Lisp_Object
ase_lift_trivial(Lisp_Object number, ent_lift_args_t SXE_UNUSED(unused))
{
	return number;
}

static inline void
_ase_lifttable_init(void)
{
	int i, j;
	for (i = 0; i < ASE_OPTABLE_SIZE; i++) {
		for (j = 0; j < ASE_OPTABLE_SIZE; j++) {
			ent_lift_unregister(i, j);
		}
	}
}

void
ase_lifttable_init(void)
{
	_ase_lifttable_init();
}


DEFUN("lift", Fent_lift, 0, MANY, 0, /*
*/
      (int nargs, Lisp_Object *args))
{
	return Qt;
}


/* convenience functions */


void
syms_of_ent_lift(void)
{
	DEFSUBR(Fent_lift);
}

void
vars_of_ent_lift(void)
{
}

/* ent-lift.c ends here */
