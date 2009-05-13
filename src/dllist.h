/*
  dllist.c -- Doubly Linked Lists
  Copyright (C) 2005, 2006 Sebastian Freundt

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

#ifndef INCLUDED_dllist_h_
#define INCLUDED_dllist_h_

#if defined(HAVE_THREADS)
#include "semaphore.h"
#endif

extern void dllist_reinit(void);

typedef struct dllist_item_s *dllist_item_t;
typedef struct dllist_s *dllist_t;


struct dllist_item_s {
	void *item;
	dllist_item_t next;
	dllist_item_t prev;
};

struct dllist_s {
	struct lcrecord_header lheader;

	/* the sequence category */
	void *si;

	dllist_item_t first;	/* pointer to first item */
	dllist_item_t last;	/* pointer to last item */
	size_t size;		/* number of elements */

	Lisp_Object plist;	/* property list */
	void *noseeum_data;	/* data for noseeum dllists */

#if !defined(EF_USE_POM) && defined(HAVE_THREADS)
	/* we want thread-safe dllists anyway, do we? */
	sxe_mutex_t mtx;
#endif
};

extern Lisp_Object Qdllistp;
extern Lisp_Object Qdllist;

DECLARE_LRECORD(dllist, struct dllist_s);
#define XDLLIST(x) XRECORD(x, dllist, struct dllist_s)
#define XSETDLLIST(x, p) XSETRECORD(x, p, dllist)
#define wrap_dllist(p) wrap_object(p)
#define DLLISTP(x) RECORDP(x, dllist)
#define CHECK_DLLIST(x) CHECK_RECORD(x, dllist)
#define CONCHECK_DLLIST(x) CONCHECK_RECORD(x, dllist)

#define dllist_first(ms) (ms)->first
#define dllist_last(ms) (ms)->last
#define dllist_size(ms) (ms)->size
#define dllist_plist(ms) (ms)->plist
#define XDLLIST_FIRST(x) dllist_first(XDLLIST(x))
#define XDLLIST_LAST(x) dllist_last(XDLLIST(x))
#define XDLLIST_SIZE(x) dllist_size(XDLLIST(x))
#define XDLLIST_PLIST(x) dllist_plist(XDLLIST(x))

#if defined(EF_USE_POM)
#define dllist_mtx(dllist)		XRECORD_MTX(dllist)
#define DLL_MUTEX_INIT(dllist)		SXE_MUTEX_INIT(&dllist_mtx(dllist));
#define DLL_MUTEX_FINI(dllist)		SXE_MUTEX_FINI(&dllist_mtx(dllist));
#define DLL_LOCK(dllist)		SXE_MUTEX_LOCK(&dllist_mtx(dllist))
#define DLL_UNLOCK(dllist)		SXE_MUTEX_UNLOCK(&dllist_mtx(dllist))
#elif defined(HAVE_THREADS)
#define dllist_mtx(dllist)		((dllist)->mtx)
#define DLL_MUTEX_INIT(dllist)		SXE_MUTEX_INIT(&dllist_mtx(dllist));
#define DLL_MUTEX_FINI(dllist)		SXE_MUTEX_FINI(&dllist_mtx(dllist));
#define DLL_LOCK(dllist)		SXE_MUTEX_LOCK(&dllist_mtx(dllist))
#define DLL_UNLOCK(dllist)		SXE_MUTEX_UNLOCK(&dllist_mtx(dllist))
#else  /* !EF_USE_POM && !HAVE_THREADS */
#define dllist_mtx(dllist)
#define DLL_MUTEX_INIT(dllist)
#define DLL_MUTEX_FINI(dllist)
#define DLL_LOCK(dllist)
#define DLL_UNLOCK(dllist)
#endif

#define dllist_item	_el->item
#define RETURN_FROM_DLLIST_TRAVERSE(_dl, _retval)	\
	{						\
		DLL_UNLOCK(_dl);			\
		return _retval;				\
	}
#define WITH_DLLIST_TRAVERSE(_dl, args...)			\
	do {							\
		dllist_item_t _el = NULL;			\
		DLL_LOCK(_dl);					\
		_el = dllist_first(_dl);			\
		while (_el) {					\
			dllist_item_t _eln = _el->next;		\
			args;					\
			/* if the idiots nuked all my elems */	\
			if (dllist_size(_dl) == 0)		\
				break;				\
			_el = _eln;				\
		}						\
		DLL_UNLOCK(_dl);				\
	} while (0)

#define new_dllist_item()	xnew_and_zero(struct dllist_item_s)
#define free_dllist_item(di)	xfree(di)

#define WITH_DLL_LOCK(dllist, args...)		\
	do {					\
		DLL_LOCK(dllist);		\
		args;				\
		DLL_UNLOCK(dllist);		\
	} while (0)


extern dllist_t make_dllist(void);
extern dllist_t make_noseeum_dllist(void);
extern void free_noseeum_dllist(dllist_t);
extern void dllist_prepend(dllist_t, void*);
extern void dllist_append(dllist_t, void*);
extern void *dllist_pop_car(dllist_t);
extern void *dllist_pop_rac(dllist_t);
extern dllist_t copy_dllist(dllist_t);
extern void dllist_map_inplace(Lisp_Object, Lisp_Object);
extern void dllist_map_inplace_C(void*(*)(void*), dllist_t);
extern void dllist_map_C(void(*)(void*), dllist_t);
extern void *dllist_rrotate(dllist_t);
extern void *dllist_lrotate(dllist_t);

/* for special purposes */
extern_inline void dllist_pop_and_pro_car(Lisp_Object*, dllist_t);
extern_inline void dllist_pop_and_pro_rac(Lisp_Object*, dllist_t);

EXFUN(Fdllist, MANY);
EXFUN(Fdllist_to_list, 1);
EXFUN(Fdllist_to_list_reversed, 1);
EXFUN(Fcopy_dllist, 1);
EXFUN(Fdllist_car, 1);
EXFUN(Fdllist_rac, 1);
EXFUN(Fdllist_prepend, 2);
EXFUN(Fdllist_append, 2);
EXFUN(Fdllist_pop_car, 1);
EXFUN(Fdllist_pop_rac, 1);

/* inlines */
extern_inline void *dllist_car(dllist_t);
extern_inline void *dllist_rac(dllist_t);
extern_inline size_t dllist_get_size(dllist_t);
extern_inline void dllist_prepend_item(dllist_t, dllist_item_t);
extern_inline void dllist_append_item(dllist_t, dllist_item_t);
extern_inline dllist_item_t dllist_transfer_car(dllist_t);
extern_inline dllist_item_t dllist_transfer_rac(dllist_t);
extern_inline dllist_item_t dllist_transfer_inner(dllist_t, dllist_item_t);
extern_inline void *dllist_pop_inner(dllist_t, dllist_item_t);


extern_inline void *
dllist_car(dllist_t dllist)
{
	void *result = NULL;

	WITH_DLL_LOCK(
		dllist,
		if (dllist_first(dllist))
			result = dllist_first(dllist)->item);

	return result;
}

extern_inline void *
dllist_rac(dllist_t dllist)
{
	void *result = NULL;

	WITH_DLL_LOCK(
		dllist,
		if (dllist_last(dllist))
			result = dllist_last(dllist)->item);

	return result;
}

extern_inline size_t
dllist_get_size(dllist_t dllist)
{
	size_t result = 0;

	WITH_DLL_LOCK(dllist, result = dllist_size(dllist));
	return result;
}

extern_inline void
dllist_prepend_item(dllist_t dllist, dllist_item_t item)
{
	dllist_item_t old;

	if (item == NULL)
		return;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_first(dllist);
		item->prev = NULL;
		item->next = old;

		if (old) {
			old->prev = item;
		} else {
			/* fiddle with the tail as we are the only element */
			dllist_last(dllist) = item;
		}

		dllist_first(dllist) = item;
		dllist_size(dllist)++);
	return;
}

extern_inline void
dllist_append_item(dllist_t dllist, dllist_item_t item)
{
	dllist_item_t old;

	if (item == NULL)
		return;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_last(dllist);
		item->prev = old;
		item->next = NULL;

		if (old) {
			old->next = item;
		} else {
			/* fiddle with the head as we are the only element */
			dllist_first(dllist) = item;
		}

		dllist_last(dllist) = item;
		dllist_size(dllist)++);
	return;
}

extern_inline dllist_item_t
dllist_transfer_car(dllist_t dllist)
{
	dllist_item_t new;
	dllist_item_t old;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_first(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK(dllist);
			return NULL;
		}

		new = old->next;
		dllist_first(dllist) = new;
		dllist_size(dllist)--;

		if (new == NULL) {
			dllist_last(dllist) = new;
		} else
			new->prev = NULL;
		);

	/* wipe out navigation information */
	old->next = old->prev = NULL;
	return old;
}

extern_inline void
dllist_pop_and_pro_car(Lisp_Object *result, dllist_t dllist)
{
	dllist_item_t new;
	dllist_item_t old;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_first(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK(dllist);
			return;
		}

		*result = (Lisp_Object)old->item;
		new = old->next;
		dllist_first(dllist) = new;
		dllist_size(dllist)--;

		if (new == NULL) {
			dllist_last(dllist) = new;
		} else
			new->prev = NULL;
		);

	/* wipe out navigation information */
	old->next = old->prev = NULL;
	return;
}

extern_inline dllist_item_t
dllist_transfer_rac(dllist_t dllist)
{
	dllist_item_t new;
	dllist_item_t old;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_last(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK(dllist);
			return NULL;
		}

		new = old->prev;
		dllist_last(dllist) = new;
		dllist_size(dllist)--;

		if (new == NULL) {
			dllist_first(dllist) = new;
		} else
			new->next = NULL;
		);

	/* wipe out navigation information */
	old->next = old->prev = NULL;
	return old;
}

extern_inline void
dllist_pop_and_pro_rac(Lisp_Object *result, dllist_t dllist)
{
	dllist_item_t new;
	dllist_item_t old;

	WITH_DLL_LOCK(
		dllist,
		old = dllist_last(dllist);

		if (old == NULL) {
			/* unlock the mutex due to non-local exit */
			DLL_UNLOCK(dllist);
			return;
		}

		*result = (Lisp_Object)old->item;
		new = old->prev;
		dllist_last(dllist) = new;
		dllist_size(dllist)--;

		if (new == NULL) {
			dllist_first(dllist) = new;
		} else
			new->next = NULL;
		);

	/* wipe out navigation information */
	old->next = old->prev = NULL;
	return;
}

extern_inline dllist_item_t
dllist_transfer_inner(dllist_t dll, dllist_item_t dlli)
{
	/* assumes lock has been set already,
	 * also see dllist_transfer_inner_with_lock */
	if (dlli == NULL || dll == NULL)
		return NULL;

	if (dlli->prev != NULL && dlli->next != NULL) {
		/* we're truly an inner node */
		dllist_size(dll)--;
		dlli->prev->next = dlli->next;
		dlli->next->prev = dlli->prev;
	} else if (dlli->prev == NULL && dlli->next == NULL) {
		/* we're the only node */
		dllist_size(dll) = 0;
		dllist_first(dll) = dllist_last(dll) = NULL;
	} else if (dlli->prev == NULL) {
		/* we're the head-node */
		dllist_size(dll)--;
		dlli->next->prev = NULL;
		dllist_first(dll) = dlli->next;
	} else if (dlli->next == NULL) {
		/* we're the tail-node */
		dllist_size(dll)--;
		dlli->prev->next = NULL;
		dllist_last(dll) = dlli->prev;
	}

	/* wipe out navigation info */
	dlli->next = dlli->prev = NULL;
	return dlli;
}

extern_inline void*
dllist_pop_inner(dllist_t dll, dllist_item_t dlli)
{
	/* transfers an inner item and frees it afterwards */
	void *result = dllist_transfer_inner(dll, dlli)->item;
	free_dllist_item(dlli);
	return result;
}

#endif	/* INCLUDED_dllist_h_ */
