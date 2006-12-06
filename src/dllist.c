/*
  dllist.c -- Doubly Linked Lists
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

/* Synched up with: Not in FSF. */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"
#include "opaque.h"

#include "dllist.h"

Lisp_Object Qdllistp;
Lisp_Object Qdllist;

static Lisp_Object
mark_dllist(Lisp_Object obj)
{
	Lisp_Dllist *dllist = XDLLIST(obj);
	dllist_item_t *tmp;

	/* lock the entire dllist */
	DLL_LOCK_ALL(dllist);

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		mark_object((Lisp_Object)tmp->item);
		tmp = tmp->next;
	}

	/* unlock everything */
	DLL_UNLOCK_ALL(dllist);

        mark_object(XDLLIST_PLIST(obj));
	return XDLLIST_PLIST(obj);
}

static void
print_dllist(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Dllist *dllist = XDLLIST(obj);
	dllist_item_t *tmp;

	write_c_string("(dllist", printcharfun);

	/* lock the entire dllist */
	DLL_LOCK_ALL(dllist);

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		write_c_string(" ", printcharfun);
		print_internal((Lisp_Object)tmp->item,
			       printcharfun, escapeflag);
		tmp = tmp->next;
	}

	/* unlock the entire dllist */
	DLL_UNLOCK_ALL(dllist);

	write_c_string(")", printcharfun);
}

static void
finalise_dllist(void *header, int for_disksave)
{
	Lisp_Dllist *dllist = (Lisp_Dllist*)header;
	dllist_item_t *tmp, *tmp2;

	/* lock the entire dllist */
	DLL_LOCK_ALL(dllist);

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		tmp2 = tmp->next;
		free_dllist_item(tmp);
		tmp = tmp2;
	}

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;

	/* unlock and finish the mutices */
	DLL_UNLOCK_ALL(dllist);
	DLL_FINI_ALL(dllist);

	dllist->noseeum_data = NULL;

	/* avoid some warning */
	if (for_disksave);
}

static Lisp_Object
dllist_getprop(Lisp_Object obj, Lisp_Object property)
{
	return external_plist_get(&XDLLIST_PLIST(obj), property, 0, ERROR_ME);
}

static int
dllist_putprop(Lisp_Object obj, Lisp_Object property, Lisp_Object value)
{
	external_plist_put(&XDLLIST_PLIST(obj), property, value, 0, ERROR_ME);
	return 1;
}

static int
dllist_remprop(Lisp_Object obj, Lisp_Object property)
{
	return external_remprop(&XDLLIST_PLIST(obj), property, 0, ERROR_ME);
}

DEFUN("dllist-plist", Fdllist_plist, 1, 1, 0, /*
Return the property list of DLLIST.
					      */
      (dllist))
{
        CHECK_DLLIST(dllist);
        return XDLLIST_PLIST(dllist);
}

static const struct lrecord_description dllist_description[] = {
	{XD_OPAQUE_PTR, offsetof(Lisp_Dllist, first)},
	{XD_OPAQUE_PTR, offsetof(Lisp_Dllist, last)},
	{XD_INT, offsetof(Lisp_Dllist, size)},
	{XD_LISP_OBJECT, offsetof(Lisp_Dllist, plist)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("dllist", dllist,
					 mark_dllist, print_dllist,
					 finalise_dllist,
					 NULL, NULL,
					 dllist_description,
					 dllist_getprop,
					 dllist_putprop,
					 dllist_remprop,
					 Fdllist_plist,
					 Lisp_Dllist);

static Lisp_Dllist *
allocate_dllist(void)
{
	Lisp_Dllist *dllist =
		alloc_lcrecord_type(Lisp_Dllist, &lrecord_dllist);
	return dllist;
}

Lisp_Dllist *noseeum_make_dllist(void)
{
	Lisp_Dllist *dllist = xnew_and_zero(Lisp_Dllist);

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;
	dllist_plist(dllist) = Qnil;
	dllist->noseeum_data = NULL;

	/* init mutexes */
	DLL_INIT_ALL(dllist);

	return dllist;
}

void noseeum_free_dllist(Lisp_Dllist *dllist)
{
	if (dllist->noseeum_data)
		xfree(dllist->noseeum_data);

	finalise_dllist((void*)dllist, 0);

	xfree(dllist);
}

Lisp_Dllist *make_dllist(void)
{
	Lisp_Dllist *dllist = allocate_dllist();

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;
	dllist_plist(dllist) = Qnil;
	dllist->noseeum_data = NULL;

	/* init mutexes */
	DLL_INIT_ALL(dllist);

	return dllist;
}

/* constructor */
DEFUN("dllist", Fdllist, 0, MANY, 0, /*
Return a doubly-linked list.

Optionally passed arguments are filled into the resulting dllist.
				     */
      (int nargs, Lisp_Object *args))
{
	Lisp_Dllist *dllist = make_dllist();
	Lisp_Object result;
	int i;

	for (i = 0; i < nargs; i++)
		dllist_append(dllist, (void*)args[i]);

	XSETDLLIST(result, dllist);
	return result;
}

/* predicate */
DEFUN("dllistp", Fdllistp, 1, 1, 0, /*
Return non-`nil' if OBJECT is a dllist, `nil' otherwise.
				     */
      (object))
{
	if (DLLISTP(object))
		return Qt;
	else
		return Qnil;
}

DEFUN("dllist-empty-p", Fdllist_empty_p, 1, 1, 0, /*
Return non-`nil' if DLLIST is empty, `nil' otherwise.
						  */
      (dllist))
{
	CHECK_DLLIST(dllist);

	if (XDLLIST_SIZE(dllist) == 0)
		return Qt;
	else
		return Qnil;
}

/* modifiers and accessors */
void *dllist_car(Lisp_Dllist *dllist)
{
	void *result = NULL;

	WITH_DLL_FIRST(
		dllist,
		if (dllist_first(dllist))
			result = (void*)dllist_first(dllist)->item);

	return result;
}
DEFUN("dllist-car", Fdllist_car, 1, 1, 0, /*
Return the front element of DLLIST.
					      */
      (dllist))
{
	void *result;
	CHECK_DLLIST(dllist);
	if ((result = dllist_car(XDLLIST(dllist))) != NULL)
		return (Lisp_Object)result;
	else
		return Qnil;
}

void *dllist_rac(Lisp_Dllist *dllist)
{
	void *result = NULL;

	WITH_DLL_LAST(
		dllist,
		if (dllist_last(dllist))
			result = (void*)dllist_last(dllist)->item);

	return result;
}
DEFUN("dllist-rac", Fdllist_rac, 1, 1, 0, /*
Return the back element of DLLIST.
					      */
      (dllist))
{
	void *result;
	CHECK_DLLIST(dllist);
	if ((result = dllist_rac(XDLLIST(dllist))) != NULL)
		return (Lisp_Object)result;
	else
		return Qnil;
}

void dllist_prepend(Lisp_Dllist *dllist, void *element)
{
	dllist_item_t *new = new_dllist_item();
	dllist_item_t *old = dllist_first(dllist);

	WITH_DLL_FIRST(
		dllist,
		new->item = element;
		new->next = old;

		if (old) {
			old->prev = new;
		} else {
			/* fiddle with the tail as we are the only element */
			WITH_DLL_LAST(dllist, dllist_last(dllist) = new);
		}

		dllist_first(dllist) = new;
		WITH_DLL_SIZE(dllist, dllist_size(dllist)++));
	return;
}
DEFUN("dllist-prepend", Fdllist_prepend, 2, 2, 0, /*
Add ELEMENT to the front of DLLIST.
						  */
      (dllist, element))
{
	CHECK_DLLIST(dllist);

	dllist_prepend(XDLLIST(dllist), (void*)element);

	return dllist;
}

void dllist_append(Lisp_Dllist *dllist, void *element)
{
	dllist_item_t *new = new_dllist_item();
	dllist_item_t *old = dllist_last(dllist);

	WITH_DLL_LAST(
		dllist,
		new->item = element;
		new->prev = old;

		if (old) {
			old->next = new;
		} else {
			/* fiddle with the head as we are the only element */
			WITH_DLL_FIRST(dllist, dllist_first(dllist) = new);
		}

		dllist_last(dllist) = new;
		WITH_DLL_SIZE(dllist, dllist_size(dllist)++));
	return;
}
DEFUN("dllist-append", Fdllist_append, 2, 2, 0, /*
Add ELEMENT to the back of DLLIST.
						  */
      (dllist, element))
{
	CHECK_DLLIST(dllist);

	dllist_append(XDLLIST(dllist), (void*)element);

	return dllist;
}

void *dllist_pop_car(Lisp_Dllist *dllist)
{
	dllist_item_t *new;
	dllist_item_t *old;
	void *result = NULL;

	WITH_DLL_FIRST(
		dllist,
		old = dllist_first(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK_FIRST(dllist);
			return NULL;
		}

		new = old->next;
		dllist_first(dllist) = new;
		WITH_DLL_SIZE(dllist, dllist_size(dllist)--);

		if (new == NULL) {
			WITH_DLL_LAST(dllist, dllist_last(dllist) = new);
		} else
			new->prev = NULL;

		/* save the lisp data and free the item */
		result = old->item;
		free_dllist_item(old));

	return result;
}
DEFUN("dllist-pop-car", Fdllist_pop_car, 1, 1, 0, /*
Remove the front element of DLLIST and return it.
						      */
      (dllist))
{
	void *result;
	CHECK_DLLIST(dllist);
	if ((result = dllist_pop_car(XDLLIST(dllist))) != NULL)
		return (Lisp_Object)result;
	else
		return Qnil;
}

void *dllist_pop_rac(Lisp_Dllist *dllist)
{
	dllist_item_t *new;
	dllist_item_t *old;
	void *result = NULL;

	WITH_DLL_LAST(
		dllist,
		old = dllist_last(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK_LAST(dllist);
			return NULL;
		}

		new = old->prev;
		dllist_last(dllist) = new;
		WITH_DLL_SIZE(dllist, dllist_size(dllist)--);

		if (new == NULL) {
			WITH_DLL_FIRST(dllist, dllist_first(dllist) = new);
		} else
			new->next = NULL;

		/* save the lisp data and free the item */
		result = old->item;
		free_dllist_item(old));

	return result;
}
DEFUN("dllist-pop-rac", Fdllist_pop_rac, 1, 1, 0, /*
Remove the back element of DLLIST and return it.
						    */
      (dllist))
{
	void *result;
	CHECK_DLLIST(dllist);
	if ((result = dllist_pop_rac(XDLLIST(dllist))) != NULL)
		return (Lisp_Object)result;
	else
		return Qnil;
}

DEFUN("dllist-size", Fdllist_size, 1, 1, 0, /*
Return the size of DLLIST, that is the number of elements.
					    */
      (dllist))
{
	int result;
	CHECK_DLLIST(dllist);
	WITH_DLL_SIZE(XDLLIST(dllist), result = XDLLIST_SIZE(dllist));
	return make_int(result);
}


Lisp_Dllist *copy_dllist(Lisp_Dllist *dllist)
{
	Lisp_Dllist *dl_copy = make_dllist();
	dllist_item_t *elm;

	DLL_LOCK_ALL(dllist);
	elm = dllist_first(dllist);

	while (elm) {
		dllist_append(dl_copy, elm->item);
		elm = elm->next;
	}
	DLL_UNLOCK_ALL(dllist);

	return dl_copy;
}


DEFUN("copy-dllist", Fcopy_dllist, 1, 1, 0,	/*
Return a copy of dllist DLLIST.
The elements of DLLIST are not copied; they are shared
with the original.
						*/
      (dllist))
{

	CHECK_DLLIST(dllist);
	
	return wrap_dllist(copy_dllist(XDLLIST(dllist)));
}


/* converters */
DEFUN("dllist-to-list", Fdllist_to_list, 1, 1, 0, /*
Return the ordinary list induced by DLLIST, that is start with
the first element in DLLIST and traverse through the back.
						  */
      (dllist))
{
	/* this function can GC */
	dllist_item_t *tmp;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	CHECK_DLLIST(dllist);

	GCPRO2(dllist, result);

	DLL_LOCK_ALL(XDLLIST(dllist));
	/* traverse the list */
	tmp = XDLLIST_LAST(dllist);
	while (tmp) {
		result = Fcons((Lisp_Object)tmp->item, result);
		tmp = tmp->prev;
	}
	DLL_UNLOCK_ALL(XDLLIST(dllist));

	UNGCPRO;
	return result;
}

DEFUN("dllist-to-list-reversed", Fdllist_to_list_reversed, 1, 1, 0, /*
Return the ordinary list induced by DLLIST in reverse order,
that is start with the last element in DLLIST and traverse through
the front.
								    */
      (dllist))
{
	dllist_item_t *tmp;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	CHECK_DLLIST(dllist);

	GCPRO2(dllist, result);

	DLL_LOCK_ALL(XDLLIST(dllist));
	/* traverse the list */
	tmp = XDLLIST_FIRST(dllist);
	while (tmp) {
		result = Fcons((Lisp_Object)tmp->item, result);
		tmp = tmp->next;
	}
	DLL_UNLOCK_ALL(XDLLIST(dllist));

	UNGCPRO;
	return result;
}

void dllist_map_inplace(Lisp_Object function, Lisp_Object dllist)
{
	Lisp_Object args[2];
	struct gcpro gcpro1, gcpro2;

	GCPRO2(function, dllist);
	args[0] = function;
	WITH_DLLIST_TRAVERSE(
		XDLLIST(dllist),
		args[1] = (Lisp_Object)dllist_item;
		dllist_item = (void*)Ffuncall(2, args));
	UNGCPRO;
}

void dllist_map_inplace_C(void*(*fun)(void*), Lisp_Dllist *dllist)
{
	/* This cannot GC, it is intended for noseeum dllists anyway */
	WITH_DLLIST_TRAVERSE(dllist, dllist_item = fun(dllist_item));
}

void dllist_map_C(void(*fun)(void*), Lisp_Dllist *dllist)
{
	/* This cannot GC, it is intended for noseeum dllists anyway */
	WITH_DLLIST_TRAVERSE(dllist, fun(dllist_item));
}

/* testcase */
#if 0
(setq a (dllist))
(dllistp a)
(dllist-empty-p a)

(dllist-car a)
(dllist-rac a)

(dllist-prepend a 12)
(dllist-append a 21)
(dllist-prepend a 0)

(dllist-pop-car a)
(dllist-pop-rac a)

(dllist-size a)

(dllist-to-list a)
(dllist-to-list-reversed a)
#endif


/*
 * Initialisation stuff
 */
void syms_of_dllist(void)
{
	INIT_LRECORD_IMPLEMENTATION(dllist);

	defsymbol(&Qdllistp, "dllistp");
	defsymbol(&Qdllist, "dllist");

	DEFSUBR(Fdllist);
	DEFSUBR(Fdllist_plist);

	DEFSUBR(Fdllistp);
	DEFSUBR(Fdllist_empty_p);

	DEFSUBR(Fdllist_car);
	DEFSUBR(Fdllist_rac);
	DEFSUBR(Fdllist_prepend);
	DEFSUBR(Fdllist_append);
	DEFSUBR(Fdllist_pop_car);
	DEFSUBR(Fdllist_pop_rac);
	DEFSUBR(Fdllist_size);

	DEFSUBR(Fcopy_dllist);

	DEFSUBR(Fdllist_to_list);
	DEFSUBR(Fdllist_to_list_reversed);
}

void vars_of_dllist(void)
{
	Fprovide(Qdllist);
}

