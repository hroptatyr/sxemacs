/*
  dllist.c -- Doubly Linked Lists
  Copyright (C) 2005, 2006, 2007 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not in FSF. */

#include <config.h>

#include "lisp.h"

#include "buffer.h"
#include "sysdep.h"
#include "lrecord.h"
#include "lstream.h"
#include "opaque.h"

#include "dllist.h"

/* for the category subsystem */
#include "category.h"
#include "seq.h"

Lisp_Object Qdllistp;
Lisp_Object Qdllist;

static Lisp_Object
mark_dllist(Lisp_Object obj)
{
	dllist_t dllist = XDLLIST(obj);
	dllist_item_t tmp;

	/* lock the entire dllist */
	DLL_LOCK(dllist);

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		mark_object((Lisp_Object)tmp->item);
		tmp = tmp->next;
	}

	/* unlock everything */
	DLL_UNLOCK(dllist);

	mark_object(XDLLIST_PLIST(obj));
	return XDLLIST_PLIST(obj);
}

static void
print_dllist(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	dllist_t dllist = XDLLIST(obj);
	dllist_item_t tmp;

	write_c_string("(dllist", printcharfun);

	/* traverse the list */
	tmp = dllist_first(dllist);
	while (tmp) {
		Lisp_Object ti;
		dllist_item_t nex;

		/* lock the entire dllist */
		DLL_LOCK(dllist);
		ti = (Lisp_Object)tmp->item;
		nex = tmp->next;
		/* unlock the entire dllist */
		DLL_UNLOCK(dllist);

		write_c_string(" ", printcharfun);
		print_internal(ti, printcharfun, escapeflag);
		tmp = nex;
	}

	write_c_string(")", printcharfun);
}

static int
dllist_equalp(Lisp_Object o1, Lisp_Object o2, int depth)
{
	dllist_t d1 = XDLLIST(o1);
	dllist_t d2 = XDLLIST(o2);
	dllist_item_t t1, t2;
	size_t s1, s2;

	/* before locking stuff, look if d1 and d2 coincide */
	if (d1 == d2)
		return 1;

	/* lock both lists */
	DLL_LOCK(d1);
	DLL_LOCK(d2);

	/* grab the sizes */
	s1 = dllist_size(d1);
	s2 = dllist_size(d2);

	/* compare sizes first */
	if (s1 != s2) {
		DLL_UNLOCK(d1);
		DLL_UNLOCK(d2);
		return 0;
	}

	/* traverse the list */
	t1 = dllist_first(d1);
	t2 = dllist_first(d2);
	while (t1 && t2) {
		Lisp_Object l1 = (Lisp_Object)t1->item;
		Lisp_Object l2 = (Lisp_Object)t2->item;
		if (!internal_equal(l1, l2, depth + 1)) {
			DLL_UNLOCK(d2);
			DLL_UNLOCK(d1);
			return 0;
		}
		t1 = t1->next;
		t2 = t2->next;
	}

	/* unlock them two */
	DLL_UNLOCK(d2);
	DLL_UNLOCK(d1);
	return 1;
}

static unsigned long
dllist_hash(Lisp_Object obj, int depth)
{
	dllist_t dll = XDLLIST(obj);
	unsigned int hash;
	dllist_item_t t;

	DLL_LOCK(dll);
	hash = dllist_size(dll);
	t = dllist_first(dll);
	while (t) {
		Lisp_Object i = (Lisp_Object)t->item;
		hash = HASH2(hash, internal_hash(i, depth+1));
		t = t->next;
	}
	DLL_UNLOCK(dll);
	return hash;
}

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
static void
finalise_dllist(void *header, int SXE_UNUSED(for_disksave))
{
	SXE_DEBUG_GC("finalising dllist %p\n", header);

	/* finish the mutex */
	DLL_MUTEX_FINI((dllist_t)header);
	/* clean sweep */
	memset(header, 0, sizeof(struct dllist_s));
	return;
}

#else  /* !BDWGC */

static void
finalise_dllist(void *header, int SXE_UNUSED(for_disksave))
{
	volatile dllist_t dllist = header;

	/* lock the entire dllist */
	DLL_LOCK(dllist);

	/* traverse the list */
	for (dllist_item_t tmp = dllist_first(dllist); tmp; ) {
		volatile dllist_item_t tmp2 = tmp->next;
		free_dllist_item(tmp);
		tmp = tmp2;
	}

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;

	/* unlock and finish the mutices */
	DLL_UNLOCK(dllist);
	DLL_MUTEX_FINI(dllist);

	dllist->noseeum_data = NULL;
	return;
}
#endif	/* BDWGC */

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
	{XD_OPAQUE_PTR, offsetof(struct dllist_s, first)},
	{XD_OPAQUE_PTR, offsetof(struct dllist_s, last)},
	{XD_INT, offsetof(struct dllist_s, size)},
	{XD_LISP_OBJECT, offsetof(struct dllist_s, plist)},
	{XD_OPAQUE_PTR, offsetof(struct dllist_s, noseeum_data)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("dllist", dllist,
					 mark_dllist, print_dllist,
					 finalise_dllist,
					 dllist_equalp, dllist_hash,
					 dllist_description,
					 dllist_getprop,
					 dllist_putprop,
					 dllist_remprop,
					 Fdllist_plist,
					 struct dllist_s);

/* the seq implementation */
static size_t
dll_length(const seq_t dll)
{
	return dllist_size((const dllist_t)dll);
}

static void
dll_iter_init(seq_t dll, seq_iter_t si)
{
	si->seq = dll;
	DLL_LOCK((dllist_t)dll);
	si->data = dllist_first((dllist_t)dll);
	DLL_UNLOCK((dllist_t)dll);
	return;
}

static void
dll_iter_reset(seq_iter_t si)
{
	DLL_LOCK((dllist_t)si->seq);
	si->data = dllist_first((dllist_t)si->seq);
	DLL_UNLOCK((dllist_t)si->seq);
	return;
}

static void
dll_iter_next(seq_iter_t si, void **elt)
{
	if (si->data != NULL) {
		DLL_LOCK((dllist_t)si->seq);
		*elt = (void*)((dllist_item_t)si->data)->item;
		si->data = (void*)((dllist_item_t)si->data)->next;
		DLL_UNLOCK((dllist_t)si->seq);
	} else {
		*elt = NULL;
	}
	return;
}

static void
dll_iter_fini(seq_iter_t si)
{
	si->data = si->seq = NULL;
	return;
}

static size_t
dll_explode(void *restrict tgt[], size_t ntgt, const seq_t s)
{
	volatile dllist_item_t di = NULL;
	volatile size_t i = 0;

	DLL_LOCK((const dllist_t)s);
	di = dllist_first((const dllist_t)s);
	while (di != NULL && i < ntgt) {
		tgt[i++] = di->item;
		di = di->next;
	}
	DLL_UNLOCK((const dllist_t)s);
	return i;
}

static struct seq_impl_s __sdll = {
	.length_f = dll_length,
	.iter_init_f = dll_iter_init,
	.iter_next_f = dll_iter_next,
	.iter_fini_f = dll_iter_fini,
	.iter_reset_f = dll_iter_reset,
	.explode_f = dll_explode,
};


static dllist_t
allocate_dllist(void)
{
	dllist_t dllist =
		alloc_lcrecord_type(struct dllist_s, &lrecord_dllist);
	return dllist;
}

dllist_t
make_noseeum_dllist(void)
{
	dllist_t dllist = xnew(struct dllist_s);
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	GC_finalization_proc *foo = NULL;
	void **bar = NULL;

	SXE_DEBUG_GC("created dllist %p\n", dllist);
	GC_REGISTER_FINALIZER(dllist, (GC_finalization_proc)finalise_dllist,
			      NULL, foo, bar);
#endif	/* BDWGC */

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;
	dllist_plist(dllist) = Qnil;
	dllist->noseeum_data = NULL;

	DLL_MUTEX_INIT(dllist);

	/* set the seq implementation */
	dllist->lheader.lheader.morphisms = (1<<cat_mk_lc);
	return dllist;
}

void free_noseeum_dllist(dllist_t dllist)
{
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	SXE_DEBUG_GC("freeing noseeum dllist %p (again)\n", dllist);
	finalise_dllist(dllist, 0);
	return;
#else  /* !BDWGC */
	WITH_DLL_LOCK(
		dllist,
		if (dllist->noseeum_data)
			xfree(dllist->noseeum_data);
		);

	finalise_dllist(dllist, 0);

	xfree(dllist);
#endif	/* BDWGC */
}

dllist_t
make_dllist(void)
{
	dllist_t dllist = allocate_dllist();

	dllist_first(dllist) = NULL;
	dllist_last(dllist) = NULL;
	dllist_size(dllist) = 0;
	dllist_plist(dllist) = Qnil;
	dllist->noseeum_data = NULL;

	DLL_MUTEX_INIT(dllist);

	/* set the seq implementation */
	dllist->lheader.lheader.morphisms = (1<<cat_mk_lc);
	return dllist;
}


/* constructor */
DEFUN("dllist", Fdllist, 0, MANY, 0, /*
Return a doubly-linked list.

Optionally passed arguments are filled into the resulting dllist.
*/
      (int nargs, Lisp_Object *args))
{
	dllist_t dllist = make_dllist();
	Lisp_Object result;
	int i;

	for (i = 0; i < nargs; i++)
		dllist_append(dllist, (void*)args[i]);

	XSETDLLIST(result, dllist);
	return result;
}

/* predicate */
DEFUN("dllistp", Fdllistp, 1, 1, 0, /*
Return non-nil if OBJECT is a dllist.
*/
      (object))
{
	if (DLLISTP(object))
		return Qt;
	else
		return Qnil;
}

DEFUN("dllist-empty-p", Fdllist_empty_p, 1, 1, 0, /*
Return non-nil if DLLIST is empty.
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

void
dllist_prepend(dllist_t dllist, void *element)
{
	dllist_item_t new = new_dllist_item();

	new->item = element;
	dllist_prepend_item(dllist, new);
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

DEFUN("dllist-transfer-car-car", Fdllist_transfer_car_car, 2, 2, 0, /*
Pop off SRC-DLL's car and prepend it to TGT-DLL.
*/
      (src_dll, tgt_dll))
{
	dllist_item_t item;

	CHECK_DLLIST(src_dll);
	CHECK_DLLIST(tgt_dll);

	item = dllist_transfer_car(XDLLIST(src_dll));
	if (item == NULL) {
		return tgt_dll;
	}
	dllist_prepend_item(XDLLIST(tgt_dll), item);

	return tgt_dll;
}

DEFUN("dllist-transfer-rac-car", Fdllist_transfer_rac_car, 2, 2, 0, /*
Pop off SRC-DLL's rac and prepend it to TGT-DLL.
*/
      (src_dll, tgt_dll))
{
	dllist_item_t item;

	CHECK_DLLIST(src_dll);
	CHECK_DLLIST(tgt_dll);

	item = dllist_transfer_rac(XDLLIST(src_dll));
	if (item == NULL) {
		return tgt_dll;
	}
	dllist_prepend_item(XDLLIST(tgt_dll), item);

	return tgt_dll;
}

void
dllist_append(dllist_t dllist, void *element)
{
	dllist_item_t new = new_dllist_item();

	new->item = element;
	dllist_append_item(dllist, new);
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


DEFUN("dllist-transfer-car-rac", Fdllist_transfer_car_rac, 2, 2, 0, /*
Pop off SRC-DLL's car and append it to TGT-DLL.
*/
      (src_dll, tgt_dll))
{
	dllist_item_t item;

	CHECK_DLLIST(src_dll);
	CHECK_DLLIST(tgt_dll);

	item = dllist_transfer_car(XDLLIST(src_dll));
	if (item == NULL) {
		return tgt_dll;
	}
	dllist_append_item(XDLLIST(tgt_dll), item);

	return tgt_dll;
}

DEFUN("dllist-transfer-rac-rac", Fdllist_transfer_rac_rac, 2, 2, 0, /*
Pop off SRC-DLL's rac and append it to TGT-DLL.
*/
      (src_dll, tgt_dll))
{
	dllist_item_t item;

	CHECK_DLLIST(src_dll);
	CHECK_DLLIST(tgt_dll);

	item = dllist_transfer_rac(XDLLIST(src_dll));
	if (item == NULL) {
		return tgt_dll;
	}
	dllist_append_item(XDLLIST(tgt_dll), item);

	return tgt_dll;
}

void *
dllist_pop_car(dllist_t dllist)
{
	dllist_item_t old;
	void *result = NULL;

	if ((old = dllist_transfer_car(dllist)) == NULL)
		return NULL;

	/* save the lisp data and free the item */
	result = old->item;
	free_dllist_item(old);

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

void*
dllist_pop_rac(dllist_t dllist)
{
	dllist_item_t old;
	void *result = NULL;

	if ((old = dllist_transfer_rac(dllist)) == NULL)
		return NULL;

	/* save the lisp data and free the item */
	result = old->item;
	free_dllist_item(old);

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

void*
dllist_rrotate(dllist_t dllist)
{
	dllist_item_t ofr;
	dllist_item_t oba;
	void *result = NULL;

	DLL_LOCK(dllist);
	ofr = dllist_first(dllist);
	oba = dllist_last(dllist);

	if (oba == ofr) {
		/* unlock the mutex due to non-local exit */
		DLL_UNLOCK(dllist);
		return NULL;
	}

	oba->prev->next = NULL;
	dllist_last(dllist) = oba->prev;
	oba->prev = NULL;
	oba->next = ofr;
	dllist_first(dllist) = oba;
	ofr->prev = oba;

	/* save the lisp data and free the item */
	result = oba->item;

	DLL_UNLOCK(dllist);
	return result;
}
DEFUN("dllist-rrotate", Fdllist_rrotate, 1, 1, 0, /*
Remove the back element of DLLIST and prepend it to the front.
*/
      (dllist))
{
	CHECK_DLLIST(dllist);
	dllist_rrotate(XDLLIST(dllist));
	return dllist;
}

void*
dllist_lrotate(dllist_t dllist)
{
	dllist_item_t ofr;
	dllist_item_t oba;
	void *result = NULL;

	DLL_LOCK(dllist);
	ofr = dllist_first(dllist);
	oba = dllist_last(dllist);

	if (oba == ofr) {
		/* unlock the mutex due to non-local exit */
		DLL_UNLOCK(dllist);
		return NULL;
	}

	ofr->next->prev = NULL;
	dllist_first(dllist) = ofr->next;
	ofr->next = NULL;
	oba->next = ofr;
	dllist_last(dllist) = ofr;
	ofr->prev = oba;

	/* save the lisp data and free the item */
	result = ofr->item;

	DLL_UNLOCK(dllist);
	return result;
}
DEFUN("dllist-lrotate", Fdllist_lrotate, 1, 1, 0, /*
Remove the head element of DLLIST and append it to the back.
*/
      (dllist))
{
	CHECK_DLLIST(dllist);
	dllist_lrotate(XDLLIST(dllist));
	return dllist;
}


DEFUN("dllist-size", Fdllist_size, 1, 1, 0, /*
Return the size of DLLIST, that is the number of elements.
*/
      (dllist))
{
	int result;
	CHECK_DLLIST(dllist);
	WITH_DLL_LOCK(XDLLIST(dllist), result = XDLLIST_SIZE(dllist));
	return make_int(result);
}


dllist_t
copy_dllist(dllist_t dllist)
{
	dllist_t dl_copy = make_dllist();
	dllist_item_t elm;

	DLL_LOCK(dllist);
	elm = dllist_first(dllist);

	while (elm) {
		dllist_append(dl_copy, elm->item);
		elm = elm->next;
	}
	DLL_UNLOCK(dllist);

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
	dllist_item_t tmp;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	CHECK_DLLIST(dllist);

	GCPRO2(dllist, result);

	DLL_LOCK(XDLLIST(dllist));
	/* traverse the list */
	tmp = XDLLIST_LAST(dllist);
	while (tmp) {
		result = Fcons((Lisp_Object)tmp->item, result);
		tmp = tmp->prev;
	}
	DLL_UNLOCK(XDLLIST(dllist));

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
	dllist_item_t tmp;
	Lisp_Object result = Qnil;
	struct gcpro gcpro1, gcpro2;

	CHECK_DLLIST(dllist);

	GCPRO2(dllist, result);

	DLL_LOCK(XDLLIST(dllist));
	/* traverse the list */
	tmp = XDLLIST_FIRST(dllist);
	while (tmp) {
		result = Fcons((Lisp_Object)tmp->item, result);
		tmp = tmp->next;
	}
	DLL_UNLOCK(XDLLIST(dllist));

	UNGCPRO;
	return result;
}

void
dllist_map_inplace(Lisp_Object function, Lisp_Object dllist)
{
	Lisp_Object arr[2];
	struct gcpro gcpro1, gcpro2;

	GCPRO2(function, dllist);
	arr[0] = function;
	WITH_DLLIST_TRAVERSE(
		XDLLIST(dllist),
		Lisp_Object tmp;
		arr[1] = (long int)dllist_item;
		tmp = Ffuncall(2, arr);
		dllist_item = (void*)tmp);
	UNGCPRO;
}

void
dllist_map_inplace_C(void*(*fun)(void*), dllist_t dllist)
{
	/* This cannot GC, it is intended for noseeum dllists anyway */
	WITH_DLLIST_TRAVERSE(dllist, dllist_item = fun(dllist_item));
}

void
dllist_map_C(void(*fun)(void*), dllist_t dllist)
{
	/* This cannot GC, it is intended for noseeum dllists anyway */
	WITH_DLLIST_TRAVERSE(dllist, fun(dllist_item));
}

/*
 * Initialisation stuff
 */
void
syms_of_dllist(void)
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
	DEFSUBR(Fdllist_transfer_car_car);
	DEFSUBR(Fdllist_transfer_rac_car);
	DEFSUBR(Fdllist_transfer_car_rac);
	DEFSUBR(Fdllist_transfer_rac_rac);
	DEFSUBR(Fdllist_pop_car);
	DEFSUBR(Fdllist_pop_rac);
	DEFSUBR(Fdllist_lrotate);
	DEFSUBR(Fdllist_rrotate);
	DEFSUBR(Fdllist_size);

	DEFSUBR(Fcopy_dllist);

	DEFSUBR(Fdllist_to_list);
	DEFSUBR(Fdllist_to_list_reversed);
}

void
dllist_reinit(void)
{
	/* the category subsystem */
	morphisms[lrecord_type_dllist].seq_impl = &__sdll;
	return;
}

void
vars_of_dllist(void)
{
	Fprovide(Qdllist);
}

/* dllist.c ends here */
