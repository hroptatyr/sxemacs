/*
  ase-cartesian.c -- Cartesian (exterior) product for ASE objects
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

#include "config.h"
#include "sxemacs.h"
#include "ent/ent.h"
#include "ase.h"
#include "ase-cartesian.h"

PROVIDE(ase_cartesian);
REQUIRE(ase_cartesian, "ase");

Lisp_Object Qase_cartesian, Qase_cartesianp;
Lisp_Object Qase_cartesian_interior, Qase_cartesian_interior_p;
Lisp_Object Qembed_error, Qinterior_error;


/* stuff for the dynacat */
static int
_ase_cartesian_prnt_ase_object(Lisp_Object o, Lisp_Object pcf)
{
	dynacat_intprinter_f prfun = NULL;

	if (!DYNACATP(o))
		return 0;

	prfun = get_dynacat_intprinter(o);
	if (prfun == NULL)
		return 0;

	prfun(get_dynacat(o), pcf);
	return 1;
}

static void
_ase_cartesian_prnt(ase_cartesian_t n, Lisp_Object pcf)
{
	Lisp_Object *objs;
	int i;

	objs = n->objects;
	for (i = 0; i < n->dimension; i++) {
		Lisp_Object obji = objs[i];
		if (!_ase_cartesian_prnt_ase_object(obji, pcf)) {
			print_internal(objs[i], pcf, 0);
		}
		if (i+1 < n->dimension)
			write_c_string(" x ", pcf);
	}
}

static void
ase_cartesian_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_CART("n:0x%08x@0x%08x (rc:%d)\n",
			    (unsigned int)(XASE_CARTESIAN(obj)),
			    (unsigned int)obj, 1);
	write_c_string("#<", pcf);
	print_internal(XDYNACAT_TYPE(obj), pcf, unused);
	if (!NILP(XASE_CARTESIAN_INTERIOR_TYPE(obj))) {
		print_internal(XASE_CARTESIAN_INTERIOR_TYPE(obj), pcf, unused);
	}
	write_fmt_str(pcf, " of dimension %d, ",
		      XASE_CARTESIAN_DIMENSION(obj));
	_ase_cartesian_prnt(XASE_CARTESIAN(obj), pcf);

	write_c_string(">", pcf);
}

static void
ase_cartesian_fini(Lisp_Object obj, int unused)
{
	ase_cartesian_t n = XASE_CARTESIAN(obj);

	EMOD_ASE_DEBUG_GC("n:0x%08x@0x%08x (rc:%d) shall be freed...\n",
			  (unsigned int)(n), (unsigned int)obj, 1);

	if (ase_cartesian_decref(n) <= 0) {
		ase_cartesian_fini_refcnt(n);
		xfree(n->objects);
		xfree(n);
	} else {
		EMOD_ASE_DEBUG_GC("VETO! References exist\n");
	}
	return;
}

static inline void
_ase_cartesian_mark(ase_cartesian_t n)
{
	int i;

	if (n == NULL)
		return;

	for (i = 0; i < n->dimension; i++)
		mark_object(n->objects[i]);

	mark_object(n->lebesgue_measure);
	mark_object(n->rational_measure);
	mark_object(n->colour);
	mark_object(n->interior_type);
	return;
}

static void
ase_cartesian_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_CART("n:0x%08x@0x%08x (rc:%d) shall be marked...\n",
			    (unsigned int)(XASE_CARTESIAN(obj)),
			    (unsigned int)obj, 1);
	_ase_cartesian_mark(XASE_CARTESIAN(obj));
	return;
}


Lisp_Object
_ase_wrap_cartesian(ase_cartesian_t n)
{
	Lisp_Object result;

	result = make_dynacat(n);
	XDYNACAT_TYPE(result) = Qase_cartesian;

	if (n)
		ase_cartesian_incref(n);

	set_dynacat_printer(result, ase_cartesian_prnt);
	set_dynacat_marker(result, ase_cartesian_mark);
	set_dynacat_finaliser(result, ase_cartesian_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_cartesian_prnt);

	EMOD_ASE_DEBUG_CART("n:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			    (unsigned int)n, 1, (unsigned int)result);

	return result;
}

Lisp_Object
_ase_wrap_cartesian_interior(ase_cartesian_t n)
{
	Lisp_Object result;

	result = make_dynacat(n);
	XDYNACAT_TYPE(result) = Qase_cartesian_interior;

	if (n)
		ase_cartesian_incref(n);

	set_dynacat_printer(result, ase_cartesian_prnt);
	set_dynacat_marker(result, ase_cartesian_mark);
	set_dynacat_finaliser(result, ase_cartesian_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_cartesian_prnt);

	EMOD_ASE_DEBUG_CART("n:0x%08x (rc:%d) shall be wrapped to 0x%08x...\n",
			    (unsigned int)n, 1, (unsigned int)result);

	return result;
}

Lisp_Object
ase_make_cartesian(int nargs, Lisp_Object *args, int interiorp)
{
	ase_cartesian_t c = NULL;
	Lisp_Object result = Qnil;
	int i;

	/* We're in ZFC! So refuse to generate cartesian sets with
	 * empty set consituents, in terms: A x ( ) -> ( ) */
#if 1
	for (i = 0; i < nargs; i++) {
		WITH_DLLIST_TRAVERSE(
			ase_empty_sets,
			if ((void*)args[i] == dllist_item) {
				RETURN_FROM_DLLIST_TRAVERSE(
					ase_empty_sets, args[i]);
			}
			);
	}
#else
	for (i = 0; i < nargs; i++) {
		if (DYNACATP(args[i])) {
			ase_object_t obj = get_dynacat(args[i]);
			EMOD_ASE_CRITICAL("obj cat 0x%lx\n",
					  (long unsigned int)obj->category);
		}
	}
#endif	/* 0 */
	c = _ase_make_cartesian(nargs, args, interiorp);
	if (!interiorp)
		XSETASE_CARTESIAN(result, c);
	else
		XSETASE_CARTESIAN_INTERIOR(result, c);

	return result;
}

/* accessors */


DOESNT_RETURN ase_cartesian_embedding_error(Lisp_Object o1, Lisp_Object o2)
{
	signal_error(Qembed_error, list2(o1, o2));
	return;
}


/* lisp level */
DEFUN("ase-cartesianp", Fase_cartesianp, 1, 1, 0, /*
Return non-`nil' iff OBJECT is a cartesian product of objects.
*/
      (object))
{
	if (ASE_CARTESIANP(object))
		return Qt;

	return Qnil;
}

DEFUN("ase-cartesian*p", Fase_cartesianXp, 1, 1, 0, /*
Return non-`nil' iff OBJECT is an interior cartesian product of objects.
*/
      (object))
{
	if (ASE_CARTESIAN_INTERIOR_P(object))
		return Qt;

	return Qnil;
}

/* ###autoload */
DEFUN("ase-cartesian", Fase_cartesian, 0, MANY, 0, /*
Return a cartesian (exterior) product of OBJECTS.
*/
      (int nargs, Lisp_Object *args))
{
	if (nargs == 0)
		return Qnil;
	else if (nargs == 1)
		return args[0];

	return ase_make_cartesian(nargs, args, 0);
}

/* ###autoload */
DEFUN("ase-cartesian*", Fase_cartesianX, 0, MANY, 0, /*
Return an interior cartesian product of OBJECTS.
*/
      (int nargs, Lisp_Object *args))
{
	Lisp_Object tmp;
	int i;

	if (nargs == 0)
		return Qnil;
	else if (nargs == 1)
		return args[0];

	tmp = Ftype_of(args[0]);
	for (i = 1; i < nargs; i++) {
		Lisp_Object tmpi;
		if (!EQ(tmp, (tmpi = Ftype_of(args[i]))))
			signal_error(Qinterior_error, list2(tmp, tmpi));
	}

	return ase_make_cartesian(nargs, args, 1);
}

/* accessors */
DEFUN("ase-cartesian-ground-domain", Fase_cartesian_ground_domain, 1, 1, 0, /*
Return the ground domain (the type) of an interior product OBJECT.
*/
      (object))
{
	CHECK_ASE_CARTESIAN_INTERIOR(object);
	return XASE_CARTESIAN_INTERIOR_TYPE(object);
}

DEFUN("ase-cartesian-projection", Fase_cartesian_projection, 2, 2, 0, /*
Return the projection of CARTESIAN onto the DIMENSION-th component.
*/
      (cartesian, dimension))
{
	CHECK_ASE_UBERCARTESIAN(cartesian);
	CHECK_NATNUM(dimension);
	if (XINT(dimension) == 0 ||
	    XINT(dimension) > XASE_CARTESIAN_DIMENSION(cartesian)) {
		args_out_of_range(cartesian, dimension);
		return Qnil;
	}
	return XASE_CARTESIAN_OBJECTS(cartesian)[XINT(dimension)-1];
}

DEFUN("ase-cartesian-embed", Fase_cartesian_embed, 2, 2, 0, /*
Return the embedding of CARTESIAN according to the DIMENSION vector.
Use dimension indexes in DIMENSION vector to embed the specified
dimension, use 0 to denote a free subspace.
*/
      (cartesian, dimension))
{
	int i, dim, newdim;
	Lisp_Object *newos, *oldos, *vec;

	CHECK_ASE_UBERCARTESIAN(cartesian);
	CHECK_VECTOR(dimension);

	dim = XASE_CARTESIAN_DIMENSION(cartesian);
	newdim = XVECTOR_LENGTH(dimension);
	newos = alloca_array(Lisp_Object, newdim);
	oldos = XASE_CARTESIAN_OBJECTS(cartesian);
	vec = XVECTOR_DATA(dimension);

	for (i = 0; i < newdim; i++) {
		Lisp_Object c = vec[i];
		CHECK_NATNUM(c);

		if (XINT(c) > 0 && XINT(c) <= dim) {
			newos[i] = oldos[XINT(c)-1];
		} else {
			newos[i] = Qzero;
			/* should be Fzero(itype) */
		}
	}

	return ase_make_cartesian(newdim, newos, 0);
}


#define EMODNAME	ase_cartesian

void
EMOD_PUBINIT(void)
{
	/* constructors */
	DEFSUBR(Fase_cartesian);
	DEFSUBR(Fase_cartesianX);
	/* predicates */
	DEFSUBR(Fase_cartesianp);
	DEFSUBR(Fase_cartesianXp);
	/* accessors */
	DEFSUBR(Fase_cartesian_ground_domain);
	DEFSUBR(Fase_cartesian_projection);
	DEFSUBR(Fase_cartesian_embed);

	defsymbol(&Qase_cartesian, "ase:cartesian");
	defsymbol(&Qase_cartesianp, "ase:cartesianp");
	defsymbol(&Qase_cartesian_interior, "ase:cartesian/");
	defsymbol(&Qase_cartesian_interior_p, "ase:cartesian/...-p");

	DEFERROR(Qembed_error,
		 "Cannot embed domain unambiguously", Qdomain_error);
	DEFERROR(Qinterior_error,
		 "Cannot find an interior product, "
		 "types of objects must coincide", Qdomain_error);

	Fprovide(intern("ase-cartesian"));
	return;
}

void
EMOD_PUBREINIT(void)
{
	return;
}

void
EMOD_PUBDEINIT(void)
{
	/* constructors */
	UNDEFSUBR(Fase_cartesian);
	UNDEFSUBR(Fase_cartesianX);
	/* predicates */
	UNDEFSUBR(Fase_cartesianp);
	UNDEFSUBR(Fase_cartesianXp);
	/* accessors */
	UNDEFSUBR(Fase_cartesian_ground_domain);
	UNDEFSUBR(Fase_cartesian_projection);
	UNDEFSUBR(Fase_cartesian_embed);

	Frevoke(intern("ase-cartesian"));
	return;
}

/* ase-cartesian ends here */
