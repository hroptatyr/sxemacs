/* emodules.c - Support routines for dynamic module loading
(C) Copyright 1998, 1999 J. Kean Johnston. All rights reserved.

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "emodptr.h"

Lisp_Object Qemodptrp;

/* EmodPtr stuff */

static Lisp_Object
mark_emodptr(Lisp_Object obj)
{
	Lisp_EmodPtr *emp = XEMODPTR(obj);
	mark_object(emp->type);
	mark_object(emp->plist);

	if (emp->mrkfun)
		emp->mrkfun(obj);

	return (emp->plist);
}

static void
print_emodptr(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	/* This function can GC */
	Lisp_EmodPtr *emp = XEMODPTR(obj);

	if (emp->prfun)
		emp->prfun(obj, printcharfun, escapeflag);
	else
		write_c_string("#<emodptr object>", printcharfun);
}

static void
finalise_emodptr(void *header, int for_disksave)
{
	/* This function can GC */
	Lisp_EmodPtr *emp = (Lisp_EmodPtr*)header;

	if (emp->finfun)
		emp->finfun(wrap_object(emp), for_disksave);
	else if (emp->ptr)
		xfree(emp->ptr);

	emp->prfun = NULL;
	emp->finfun = NULL;
	emp->mrkfun = NULL;
	emp->ptr = NULL;
	emp->plist = Qnil;
	emp->type = Qnil;

	/* avoid some warning */
	if (for_disksave);
}

static Lisp_Object
emodptr_getprop(Lisp_Object obj, Lisp_Object property)
{
	return external_plist_get(&XEMODPTR_PLIST(obj), property, 0, ERROR_ME);
}

static int
emodptr_putprop(Lisp_Object obj, Lisp_Object property, Lisp_Object value)
{
	external_plist_put(&XEMODPTR_PLIST(obj), property, value, 0, ERROR_ME);
	return 1;
}

static int
emodptr_remprop(Lisp_Object obj, Lisp_Object property)
{
	return external_remprop(&XEMODPTR_PLIST(obj), property, 0, ERROR_ME);
}

DEFUN("emodptr-plist", Femodptr_plist, 1, 1, 0, /*
Return the property list of EMODPTR.
						*/
      (emodptr))
{
        CHECK_EMODPTR(emodptr);
        return XEMODPTR_PLIST(emodptr);
}

static const struct lrecord_description emodptr_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_EmodPtr, type)},
	{XD_LISP_OBJECT, offsetof(Lisp_EmodPtr, plist)},
        {XD_OPAQUE_DATA_PTR, offsetof(Lisp_EmodPtr, ptr)},
        {XD_OPAQUE_DATA_PTR, offsetof(Lisp_EmodPtr, mrkfun)},
        {XD_OPAQUE_DATA_PTR, offsetof(Lisp_EmodPtr, prfun)},
        {XD_OPAQUE_DATA_PTR, offsetof(Lisp_EmodPtr, finfun)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("emodptr", emodptr,
					 mark_emodptr, print_emodptr,
					 finalise_emodptr,
					 NULL, NULL,
					 emodptr_description,
					 emodptr_getprop,
					 emodptr_putprop,
					 emodptr_remprop,
					 Femodptr_plist,
					 Lisp_EmodPtr);


static Lisp_EmodPtr *
allocate_emodptr(void)
{
	Lisp_EmodPtr *emp =
		alloc_lcrecord_type(Lisp_EmodPtr, &lrecord_emodptr);
	return emp;
}

Lisp_Object make_emodptr(void *ptr)
{
	Lisp_EmodPtr *emp = allocate_emodptr();
	Lisp_Object result;

	emp->prfun = NULL;
	emp->finfun = NULL;
	emp->mrkfun = NULL;
	emp->ptr = ptr;
	emp->type = Qnil;
	emp->plist = Qnil;

	XSETEMODPTR(result, emp);
	return result;
}

void *get_emodptr(Lisp_Object ptr)
{
	CHECK_EMODPTR(ptr);
	return XEMODPTR(ptr)->ptr;
}

void set_emodptr(Lisp_Object ptr, void *p)
{
	CHECK_EMODPTR(ptr);
	XEMODPTR(ptr)->ptr = p;
	return;
}

void set_emodptr_marker(Lisp_Object ptr,
			void(*mrkfun)(Lisp_Object))
{
	CHECK_EMODPTR(ptr);
	XEMODPTR(ptr)->mrkfun = mrkfun;
}

void set_emodptr_printer(Lisp_Object ptr,
			 void(*prfun)(Lisp_Object, Lisp_Object, int))
{
	CHECK_EMODPTR(ptr);
	XEMODPTR(ptr)->prfun = prfun;
}

void set_emodptr_finaliser(Lisp_Object ptr,
			   void(*finfun)(Lisp_Object, int))
{
	CHECK_EMODPTR(ptr);
	XEMODPTR(ptr)->finfun = finfun;
}

DEFUN("emodptrp", Femodptrp, 1, 1, 0, /*
Return non-`nil' if OBJECT is an opaque emodule pointer,
`nil' otherwise.
				      */
      (object))
{
	if (EMODPTRP(object))
		return Qt;
	else
		return Qnil;
}


void syms_of_emodptr(void)
{
	INIT_LRECORD_IMPLEMENTATION(emodptr);

	defsymbol(&Qemodptrp, "emodptrp");

	DEFSUBR(Femodptrp);
	DEFSUBR(Femodptr_plist);
}

void reinit_vars_of_emodptr(void)
{
}

void vars_of_emodptr(void)
{
	reinit_vars_of_emodptr();
	Fprovide(intern("emodptr"));
}

