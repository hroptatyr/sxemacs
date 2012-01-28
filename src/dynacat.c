/*** dynacat.c -- dynamic categories (`types' on top of types) for SXEmacs
 *
 * Copyright (C) 2005-2008 Sebastian Freundt <hroptatyr@sxemacs.org>
 *
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
 * Alternatively, for testing purposes, this file can be redistributed under
 * the following licence:
 * You can do whatever the fuck you want with this, except stop anyone else
 * doing whatever the fuck they want.
 *
 **/

#include "config.h"
#include "lisp.h"
#include "dynacat.h"

#define __DYNACAT_DEBUG__(args...)	fprintf(stderr, "DYNACAT " args)
#ifndef DYNACAT_DEBUG_FLAG
#define DYNACAT_DEBUG(args...)
#else
#define DYNACAT_DEBUG(args...)		__DYNACAT_DEBUG__(args)
#endif

Lisp_Object Qdynacatp, Qdynacat;

static Lisp_Object
mark_dynacat(Lisp_Object obj)
{
	dynacat_t emp = XDYNACAT(obj);
	mark_object(emp->type);
	mark_object(emp->plist);

	if (emp->mrkfun)
		emp->mrkfun(obj);

	return (emp->plist);
}

static void
print_dynacat(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	/* This function can GC */
	dynacat_t emp = XDYNACAT(obj);

	if (emp->prfun) {
		emp->prfun(obj, printcharfun, escapeflag);
		return;
	}

	write_c_string("#<dynacat object", printcharfun);
	if (!NILP(emp->type)) {
		write_c_string(" :type ", printcharfun);
		print_internal(emp->type, printcharfun, escapeflag);
	}
	write_c_string(">", printcharfun);
}

static void
finalise_dynacat(void *unused, int for_disksave)
{
	if (for_disksave) {
		signal_simple_error(
			"Can't dump an emacs containing "
			"dynacat objects", Qt);
	}
}

void
dynacat_fini(dynacat_t emp)
{
	DYNACAT_DEBUG(stderr, "#emdptr:0x%016lx@0x%016lx will pass away\n",
		      (long unsigned int)emp->ptr, (long unsigned int)emp);
	if (emp->finfun)
		emp->finfun(wrap_object(emp), 0);
	else if (emp->ptr)
		xfree(emp->ptr);

	emp->prfun = NULL;
	emp->finfun = NULL;
	emp->mrkfun = NULL;
	emp->ptr = NULL;
	emp->plist = Qnil;
	emp->type = Qnil;
}

static Lisp_Object
dynacat_getprop(Lisp_Object obj, Lisp_Object property)
{
	return external_plist_get(&XDYNACAT_PLIST(obj), property, 0, ERROR_ME);
}

static int
dynacat_putprop(Lisp_Object obj, Lisp_Object property, Lisp_Object value)
{
	external_plist_put(&XDYNACAT_PLIST(obj), property, value, 0, ERROR_ME);
	return 1;
}

static int
dynacat_remprop(Lisp_Object obj, Lisp_Object property)
{
	return external_remprop(&XDYNACAT_PLIST(obj), property, 0, ERROR_ME);
}

DEFUN("dynacat-plist", Fdynacat_plist, 1, 1, 0, /*
Return the property list of DYNACAT.
*/
      (dynacat))
{
	CHECK_DYNACAT(dynacat);
	return XDYNACAT_PLIST(dynacat);
}

static const struct lrecord_description dynacat_description[] = {
	{XD_LISP_OBJECT, offsetof(struct dynacat_s, type)},
	{XD_LISP_OBJECT, offsetof(struct dynacat_s, plist)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct dynacat_s, ptr)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct dynacat_s, mrkfun)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct dynacat_s, finfun)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct dynacat_s, prfun)},
	{XD_OPAQUE_DATA_PTR, offsetof(struct dynacat_s, intprfun)},
	{XD_END}
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS(
	"dynacat", dynacat,
	mark_dynacat, print_dynacat, finalise_dynacat,
	NULL, NULL,
	dynacat_description,
	dynacat_getprop, dynacat_putprop, dynacat_remprop, Fdynacat_plist,
	struct dynacat_s);

#if 0
inline static dynacat_t
allocate_dynacat(void)
{
	dynacat_t emp = alloc_lcrecord_type(struct dynacat_s, &lrecord_dynacat);
	return emp;
}

Lisp_Object
make_dynacat(void *ptr)
{
	dynacat_t emp = allocate_dynacat();
	Lisp_Object result;

	emp->prfun = NULL;
	emp->intprfun = NULL;
	emp->finfun = NULL;
	emp->mrkfun = NULL;
	emp->ptr = ptr;
	emp->type = Qnil;
	emp->plist = Qnil;

	XSETDYNACAT(result, emp);
	return result;
}
#endif


DEFUN("dynacatp", Fdynacatp, 1, 1, 0, /*
Return non-nil if OBJECT is an opaque emodule pointer.
*/
      (object))
{
	if (DYNACATP(object))
		return Qt;
	else
		return Qnil;
}


void syms_of_dynacat(void)
{
	INIT_LRECORD_IMPLEMENTATION(dynacat);

	DEFSYMBOL(Qdynacatp);
	DEFSYMBOL(Qdynacat);

	DEFSUBR(Fdynacatp);
	DEFSUBR(Fdynacat_plist);
}

void reinit_vars_of_dynacat(void)
{
}

void vars_of_dynacat(void)
{
	reinit_vars_of_dynacat();
	Fprovide(intern("dynacat"));
}

/* dynacat.c ends here */
