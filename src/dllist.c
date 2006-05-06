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

static Lisp_Object
mark_dllist(Lisp_Object obj)
{
	dllist_item_t *tmp;

	/* traverse the list */
	tmp = XDLLIST_FIRST(obj);
	while (tmp) {
		mark_object(tmp->item);
		tmp = tmp->next;
	}

        mark_object(XDLLIST_PLIST(obj));
	return XDLLIST_PLIST(obj);
}

static void
print_dllist(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	dllist_item_t *tmp;

	write_c_string("(dllist", printcharfun);

	/* traverse the list */
	tmp = XDLLIST_FIRST(obj);
	while (tmp) {
		write_c_string(" ", printcharfun);
		print_internal(tmp->item, printcharfun, escapeflag);
		tmp = tmp->next;
	}

	write_c_string(")", printcharfun);
}

static void
finalise_dllist(void *header, int for_disksave)
{
	Lisp_Dllist *dllist = (Lisp_Dllist*)header;
	dllist_item_t *tmp, *tmp2;

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		tmp2 = tmp->next;
		xfree(tmp);
		tmp = tmp2;
	}

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;

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

Lisp_Object make_dllist(void)
{
	Lisp_Dllist *dllist = allocate_dllist();
	Lisp_Object result;

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;
	dllist_plist(dllist) = Qnil;

	XSETDLLIST(result, dllist);
	return result;
}

/* constructor */
DEFUN("dllist", Fdllist, 0, MANY, 0, /*
Return a doubly-linked list.

Optionally passed arguments are filled into the resulting dllist.
				     */
      (int nargs, Lisp_Object *args))
{
	Lisp_Object result;
	int i;

	result = make_dllist();

	for (i = 0; i < nargs; i++)
		dllist_append(XDLLIST(result), args[i]);

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
Lisp_Object dllist_car(Lisp_Dllist *dllist)
{
	if (dllist_first(dllist))
		return dllist_first(dllist)->item;
	else
		return Qnil;
}
DEFUN("dllist-car", Fdllist_car, 1, 1, 0, /*
Return the front element of DLLIST.
					      */
      (dllist))
{
	CHECK_DLLIST(dllist);
	return dllist_car(XDLLIST(dllist));
}

Lisp_Object dllist_rac(Lisp_Dllist *dllist)
{
	if (dllist_last(dllist))
		return dllist_last(dllist)->item;
	else
		return Qnil;
}
DEFUN("dllist-rac", Fdllist_rac, 1, 1, 0, /*
Return the back element of DLLIST.
					      */
      (dllist))
{
	CHECK_DLLIST(dllist);
	return dllist_rac(XDLLIST(dllist));
}

void dllist_prepend(Lisp_Dllist *dllist, Lisp_Object element)
{
	dllist_item_t *new = xnew_and_zero(dllist_item_t);
	dllist_item_t *old = dllist_first(dllist);

	new->item = element;
	new->next = old;

	if (old)
		old->prev = new;
	else
		dllist_last(dllist) = new;

	dllist_first(dllist) = new;
	dllist_size(dllist)++;
}
DEFUN("dllist-prepend", Fdllist_prepend, 2, 2, 0, /*
Add ELEMENT to the front of DLLIST.
						  */
      (dllist, element))
{
	CHECK_DLLIST(dllist);

	dllist_prepend(XDLLIST(dllist), element);

	return dllist;
}

void dllist_append(Lisp_Dllist *dllist, Lisp_Object element)
{
	dllist_item_t *new = xnew_and_zero(dllist_item_t);
	dllist_item_t *old = dllist_last(dllist);

	new->item = element;
	new->prev = old;

	if (old)
		old->next = new;
	else
		dllist_first(dllist) = new;

	dllist_last(dllist) = new;
	dllist_size(dllist)++;
}
DEFUN("dllist-append", Fdllist_append, 2, 2, 0, /*
Add ELEMENT to the back of DLLIST.
						  */
      (dllist, element))
{
	CHECK_DLLIST(dllist);

	dllist_append(XDLLIST(dllist), element);

	return dllist;
}

Lisp_Object dllist_pop_car(Lisp_Dllist *dllist)
{
	dllist_item_t *new;
	dllist_item_t *old = dllist_first(dllist);

	if (old == NULL)
		return Qnil;

	new = old->next;
	dllist_first(dllist) = new;
	dllist_size(dllist)--;

	if (new == NULL)
		dllist_last(dllist) = new;
	else
		new->prev = NULL;

	return old->item;
}
DEFUN("dllist-pop-car", Fdllist_pop_car, 1, 1, 0, /*
Remove the front element of DLLIST and return it.
						      */
      (dllist))
{
	CHECK_DLLIST(dllist);
	return dllist_pop_car(XDLLIST(dllist));
}

Lisp_Object dllist_pop_rac(Lisp_Dllist *dllist)
{
	dllist_item_t *new;
	dllist_item_t *old = dllist_last(dllist);

	if (old == NULL)
		return Qnil;

	new = old->prev;
	dllist_last(dllist) = new;
	dllist_size(dllist)--;

	if (new == NULL)
		dllist_first(dllist) = new;
	else
		new->next = NULL;

	return old->item;
}
DEFUN("dllist-pop-rac", Fdllist_pop_rac, 1, 1, 0, /*
Remove the back element of DLLIST and return it.
						    */
      (dllist))
{
	CHECK_DLLIST(dllist);
	return dllist_pop_rac(XDLLIST(dllist));
}

DEFUN("dllist-size", Fdllist_size, 1, 1, 0, /*
Return the size of DLLIST, that is the number of elements.
					    */
      (dllist))
{
	CHECK_DLLIST(dllist);
	return make_int((int32_t)XDLLIST_SIZE(dllist));
}


Lisp_Object copy_dllist(Lisp_Dllist *dllist)
{
	Lisp_Object dl_copy = make_dllist();
	dllist_item_t *elm = dllist_first(dllist);

	while (elm) {
		dllist_append(XDLLIST(dl_copy), elm->item);
		elm = elm->next;
	}

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
	
	return copy_dllist(XDLLIST(dllist));
}


/* converters */
DEFUN("dllist-to-list", Fdllist_to_list, 1, 1, 0, /*
Return the ordinary list induced by DLLIST, that is start with
the first element in DLLIST and traverse through the back.
						  */
      (dllist))
{
	dllist_item_t *tmp;
	Lisp_Object result = Qnil;

	CHECK_DLLIST(dllist);

	/* traverse the list */
	tmp = XDLLIST_LAST(dllist);
	while (tmp) {
		result = Fcons(tmp->item, result);
		tmp = tmp->prev;
	}

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

	CHECK_DLLIST(dllist);

	/* traverse the list */
	tmp = XDLLIST_FIRST(dllist);
	while (tmp) {
		result = Fcons(tmp->item, result);
		tmp = tmp->next;
	}

	return result;
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
	Fprovide(intern("dllist"));
}

