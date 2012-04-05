/*** ase.c -- Fancifying ENT a little
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
#include "lisp.h"
#include "ent/ent.h"
#include "ase.h"
#ifdef EMOD_ASE_MONOMOD
#include "ase-cartesian.h"
#include "ase-interval.h"
#include "ase-neighbourhood.h"
#include "ase-metric.h"
#endif

#define EMODNAME	ase
PROVIDE(ase);

Lisp_Object Qase;


#ifdef EMOD_ASE_MONOMOD
void
LTX_PUBFUN(ase_mono, init)(void)
{
	LTX_PUBFUN(ase_interval, init)();
	LTX_PUBFUN(ase_neighbourhood, init)();
	LTX_PUBFUN(ase_cartesian, init)();
	LTX_PUBFUN(ase_metric, init)();

	Fprovide(intern("ase-mono"));
}
#endif

void
EMOD_PUBINIT(void)
{
#ifdef EMOD_ASE_MONOMOD
	LTX_PUBFUN(ase_mono, init)();
#endif
	DEFSYMBOL(Qase);

	Fprovide(Qase);
}

void
EMOD_PUBREINIT(void)
{
#ifdef EMOD_ASE_MONOMOD
	LTX_PUBFUN(ase_interval, reinit)();
	LTX_PUBFUN(ase_neighbourhood, reinit)();
	LTX_PUBFUN(ase_cartesian, reinit)();
	LTX_PUBFUN(ase_metric, reinit)();
#endif
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(Qase);
}

/* ase ends here */
