/*** ase-set.c -- Set defining types
 *
 * Copyright (C) 2006, 2007, 2008 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <hroptatyr@sxemacs.org>
 *
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
 *
 ***/

/* Synched up with: Not in FSF. */

#include "config.h"
#include "sxemacs.h"
<<<<<<< HEAD
#include "ent.h"
=======
#include "ent/ent.h"
>>>>>>> origin/master
#include "ase.h"
#include "ase-set.h"

#define EMODNAME	ase_set
PROVIDE(ase_set);

Lisp_Object Qase_set;


void
EMOD_PUBINIT(void)
{
	LTX_PUBFUN(ase_interval, init)();
	LTX_PUBFUN(ase_neighbourhood, init)();
	LTX_PUBFUN(ase_cartesian, init)();
	LTX_PUBFUN(ase_metric, init)();

	DEFSYMBOL(Qase_set);
	Fprovide(Qase_set);
}

void
EMOD_PUBREINIT(void)
{
	LTX_PUBFUN(ase_interval, reinit)();
	LTX_PUBFUN(ase_neighbourhood, reinit)();
	LTX_PUBFUN(ase_cartesian, reinit)();
	LTX_PUBFUN(ase_metric, reinit)();
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(Qase_set);
}

/* ase-set ends here */
