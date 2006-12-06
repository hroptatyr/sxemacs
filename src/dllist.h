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

#ifndef INCLUDED_dllist_h_
#define INCLUDED_dllist_h_


typedef struct dllist_item_s dllist_item_t;
typedef struct Lisp_Dllist Lisp_Dllist;


struct dllist_item_s {
	void *item;
	dllist_item_t *next;
	dllist_item_t *prev;
};

struct Lisp_Dllist {
	struct lcrecord_header lheader;

	dllist_item_t *first;	/* pointer to first item */
	sxe_mutex_t first_m;	/* mutex for first item */
	dllist_item_t *last;	/* pointer to last item */
	sxe_mutex_t last_m;	/* mutex for last item */
	size_t size;		/* number of elements */
	sxe_mutex_t size_m;	/* elements mutex */

	Lisp_Object plist;	/* property list */
	void *noseeum_data;	/* data for noseeum dllists */
};

extern Lisp_Object Qdllistp;
extern Lisp_Object Qdllist;

DECLARE_LRECORD(dllist, Lisp_Dllist);
#define XDLLIST(x) XRECORD(x, dllist, Lisp_Dllist)
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

#define dllist_item	_el->item
#define WITH_DLLIST_TRAVERSE(_dl, args...)		\
	do {						\
		dllist_item_t *_el = NULL;		\
		DLL_LOCK_ALL(_dl);			\
		_el = dllist_first(_dl);		\
		while (_el) {				\
			args;				\
			_el = _el->next;		\
		}					\
		DLL_UNLOCK_ALL(_dl);			\
	} while (0)

#define new_dllist_item()	xnew_and_zero(dllist_item_t)
#define free_dllist_item(di)	xfree(di)

#define DLL_INIT_FIRST(dllist)		SXE_MUTEX_INIT(&(dllist)->first_m)
#define DLL_INIT_LAST(dllist)		SXE_MUTEX_INIT(&(dllist)->last_m)
#define DLL_INIT_SIZE(dllist)		SXE_MUTEX_INIT(&(dllist)->size_m)
#define DLL_FINI_FIRST(dllist)		SXE_MUTEX_FINI(&(dllist)->first_m)
#define DLL_FINI_LAST(dllist)		SXE_MUTEX_FINI(&(dllist)->last_m)
#define DLL_FINI_SIZE(dllist)		SXE_MUTEX_FINI(&(dllist)->size_m)

#define DLL_INIT_ALL(dllist)			\
	DLL_INIT_FIRST(dllist);			\
	DLL_INIT_LAST(dllist);			\
	DLL_INIT_SIZE(dllist)
#define DLL_FINI_ALL(dllist)			\
	DLL_FINI_SIZE(dllist);			\
	DLL_FINI_LAST(dllist);			\
	DLL_FINI_FIRST(dllist)

#define DLL_LOCK_FIRST(dllist)		SXE_MUTEX_LOCK(&(dllist)->first_m)
#define DLL_UNLOCK_FIRST(dllist)	SXE_MUTEX_UNLOCK(&(dllist)->first_m)
#define DLL_LOCK_LAST(dllist)		SXE_MUTEX_LOCK(&(dllist)->last_m)
#define DLL_UNLOCK_LAST(dllist)		SXE_MUTEX_UNLOCK(&(dllist)->last_m)
#define DLL_LOCK_SIZE(dllist)		SXE_MUTEX_LOCK(&(dllist)->size_m)
#define DLL_UNLOCK_SIZE(dllist)		SXE_MUTEX_UNLOCK(&(dllist)->size_m)

#define DLL_LOCK_ALL(dllist)			\
	DLL_LOCK_FIRST(dllist);			\
	DLL_LOCK_LAST(dllist);			\
	DLL_LOCK_SIZE(dllist)
#define DLL_UNLOCK_ALL(dllist)			\
	DLL_UNLOCK_SIZE(dllist);		\
	DLL_UNLOCK_LAST(dllist);		\
	DLL_UNLOCK_FIRST(dllist)

#define WITH_DLL_FIRST(dllist, args...)		\
	WITH_SXE_MUTEX(&(dllist)->first_m, args)
#define WITH_DLL_LAST(dllist, args...)		\
	WITH_SXE_MUTEX(&(dllist)->last_m, args)
#define WITH_DLL_SIZE(dllist, args...)		\
	WITH_SXE_MUTEX(&(dllist)->size_m, args)


extern Lisp_Dllist *make_dllist(void);
extern Lisp_Dllist *noseeum_make_dllist(void);
extern void noseeum_free_dllist(Lisp_Dllist*);
extern void dllist_prepend(Lisp_Dllist*, void*);
extern void dllist_append(Lisp_Dllist*, void*);
extern void *dllist_car(Lisp_Dllist*);
extern void *dllist_rac(Lisp_Dllist*);
extern void *dllist_pop_car(Lisp_Dllist*);
extern void *dllist_pop_rac(Lisp_Dllist*);
extern Lisp_Dllist *copy_dllist(Lisp_Dllist*);
extern void dllist_map_inplace(Lisp_Object, Lisp_Object);
extern void dllist_map_inplace_C(void*(*)(void*), Lisp_Dllist*);
extern void dllist_map_C(void(*)(void*), Lisp_Dllist*);

EXFUN(Fdllist, MANY);
EXFUN(Fdllist_to_list, 1);
EXFUN(Fdllist_to_list_reversed, 1);
EXFUN(Fcopy_dllist, 1);
EXFUN(Fdllist_car, 1);
EXFUN(Fdllist_rac, 1);

#endif	/* INCLUDED_dllist_h_ */
