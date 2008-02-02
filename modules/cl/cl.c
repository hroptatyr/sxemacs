/*
  cl.c -- Common Lisp Goodness, the fast version
  Copyright (C) 2006, 2007 Sebastian Freundt

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
#include <sxemacs.h>
#include "cl.h"
#ifdef EMOD_CL_MONOMOD
#include "cl-loop.h"
#endif

PROVIDE(cl);
#if !defined EMOD_CL_MONOMOD
#define INIT	cl_LTX_init
#define REINIT	cl_LTX_reinit
#define DEINIT	cl_LTX_deinit
#else
#define INIT	cl_mono_LTX_init
#define REINIT	cl_mono_LTX_reinit
#define DEINIT	cl_mono_LTX_deinit
#endif


/* ###autoload */
DEFUN("cl:pop", Fcl_pop, 1, UNEVALLED, 0, /*
						      */
      (args))
{
	/* This function can GC */
	Lisp_Object place = XCAR(args);
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(result, place);

	if (SYMBOLP(place)) {
		Lisp_Object ls;
		ls = Fsymbol_value(place);
		if (CONSP(ls)) {
			result = XCAR(ls);
			Fset(place, XCDR(ls));
		}
	}

	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:push", Fcl_push, 2, UNEVALLED, 0, /*
							*/
      (args))
{
	/* This function can GC */
	Lisp_Object x = XCAR(args);
	Lisp_Object place = XCAR(XCDR(args));
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(result, x, place);

	if (SYMBOLP(place)) {
		Lisp_Object ls;
		ls = Fsymbol_value(place);
		x = Feval(x);
		Fset(place, (result = Fcons(x, ls)));
	}

	UNGCPRO;
	return result;
}

/* ###autoload */
DEFUN("cl:pushnew", Fcl_pushnew, 2, UNEVALLED, 0, /*
							      */
      (args))
{
	/* This function can GC */
	Lisp_Object x = XCAR(args);
	Lisp_Object place = XCAR(XCDR(args));
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(result, x, place);

	if (SYMBOLP(place)) {
		Lisp_Object ls;
		result = ls = Fsymbol_value(place);
		if (0) {	/* !X_NOT_FOUND_IN_LS_P */
			x = Feval(x);
			Fset(place, (result = Fcons(x, ls)));
		}
	}

	UNGCPRO;
	return result;
}

#define EMOD_CL_EQL(_a, _b)						\
	((!FLOATP(_a)) ? (EQ((_a), (_b))) : (!NILP(Fequal((_a), (_b)))))
static inline
int emodcl_eql(Lisp_Object a, Lisp_Object b)
{
	return EMOD_CL_EQL(a, b);
}

/* ###autoload */
DEFUN("cl:eql", Fcl_eql, 2, 2, 0, /*
					      */
      (a, b))
{
	if (EMOD_CL_EQL(a, b))
		return Qt;
	else
		return Qnil;
}

/* ###autoload */
DEFUN("cl:list*", Fcl_list_, 1, MANY, 0, /*
						     */
      (int nargs, Lisp_Object *args))
{
	if (nargs == 1)
		return args[0];
	else {
		Lisp_Object result = args[--nargs];
		for (; nargs > 0; ) {
			result = Fcons(args[--nargs], result);
		}
		return result;
	}
}

/* ###autoload */
DEFUN("cl:tailp", Fcl_tailp, 2, 2, 0, /*
						  */
      (list, object))
{
	Lisp_Object trav1 = Qnil, trav2 = Qnil;

	CHECK_CONS(list);

	if (CONSP(object)) {
		trav2 = XCAR(object);
		for (trav1 = list; CONSP(trav1); trav1 = XCDR(trav1)) {
			if (EMOD_CL_EQL(XCAR(trav1), trav2))
				break;
		}
		if (!CONSP(trav1))
			return Qnil;

		for (trav1 = XCDR(trav1), trav2 = XCDR(object);
		     CONSP(trav1) && CONSP(trav2);
		     trav1 = XCDR(trav1), trav2 = XCDR(trav2)) {
			if (!EMOD_CL_EQL(XCAR(trav1), XCAR(trav2))) {
				return Qnil;
			}
		}
		if (EMOD_CL_EQL(trav1, trav2))
			return Qt;
		else
			return Qnil;

	} else {
		for (trav1 = list; CONSP(trav1); trav1 = XCDR(trav1));
		if (EMOD_CL_EQL(trav1, object))
			return Qt;
		else
			return Qnil;
	}
	return Qnil;
}

/* ###autoload */
DEFUN("cl:ldiff", Fcl_ldiff, 2, 2, 0, /*
						  */
      (list, object))
{
	Lisp_Object result = Qnil, tmp1 = Qnil, tmp2 = Qnil;
	Lisp_Object trav1 = Qnil, trav2 = Qnil;
	int yes;

	CHECK_CONS(list);

	if (CONSP(object)) {
		trav2 = XCAR(object);
		for (trav1 = list; CONSP(trav1); trav1 = XCDR(trav1)) {
			if (EMOD_CL_EQL(XCAR(trav1), trav2))
				break;
			tmp1 = Fcons(XCAR(trav1), tmp1);
		}
		/* we traversed list and haven't found a match yet */
		if (!CONSP(trav1)) {
			result = trav1;
			goto build_result;
		} else {
			tmp2 = trav1;
		}

		yes = 1;
		for (trav1 = XCDR(trav1), trav2 = XCDR(object);
		     CONSP(trav1) && CONSP(trav2);
		     trav1 = XCDR(trav1), trav2 = XCDR(trav2)) {
			if (yes && !EMOD_CL_EQL(XCAR(trav1), XCAR(trav2))) {
				yes = 0;
			}
		}
		if (!yes || !EMOD_CL_EQL(trav1, trav2)) {
			/* if not, just pump the rest */
			for (trav1 = tmp2; CONSP(trav1); trav1 = XCDR(trav1)) {
				tmp1 = Fcons(XCAR(trav1), tmp1);
			}
			result = trav1;
		}
	} else {
		for (trav1 = list; CONSP(trav1); trav1 = XCDR(trav1))
			tmp1 = Fcons(XCAR(trav1), tmp1);
		if (!EMOD_CL_EQL(trav1, object))
			result = trav1;
	}

build_result:
	/* push the head */
	for (trav1 = tmp1; CONSP(trav1); trav1 = XCDR(trav1)) {
		result = Fcons(XCAR(trav1), result);
	}

	return result;
}

#if 0
/* ###4utoload */
D3FUN("cl:adjoin", Fcl_adjoin, 2, MANY, 0, /*
					   */
      (int nargs, Lisp_Object *args))
{
	return Qnil;
}
#endif	/* 0 */


/* simplified initialiser */
void
INIT(void)
{
	DEFSUBR(Fcl_pop);
	DEFSUBR(Fcl_push);
	DEFSUBR(Fcl_pushnew);

	DEFSUBR(Fcl_list_);
	DEFSUBR(Fcl_tailp);
	DEFSUBR(Fcl_ldiff);

	DEFSUBR(Fcl_eql);

#if defined EMOD_CL_MONOMOD
	cl_loop_LTX_init();
#endif

	Fprovide(intern("cl"));
}

void
DEINIT(void)
{
}

void
REINIT(void)
{
	Frevoke(intern("cl"));
}

/* cl.c ends here */
