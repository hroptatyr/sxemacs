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
struct dllist_item_s {
	Lisp_Object item;
	dllist_item_t *next;
	dllist_item_t *prev;
};

struct Lisp_Dllist {
	struct lcrecord_header lheader;

	dllist_item_t *first;	/* pointer to first item */
	dllist_item_t *last;	/* pointer to last item */
	uint32_t size;

	Lisp_Object plist;	/* property list */
};
typedef struct Lisp_Dllist Lisp_Dllist;

extern Lisp_Object Qdllistp;

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


extern Lisp_Object make_dllist(void);
extern void dllist_prepend(Lisp_Dllist*, Lisp_Object);
extern void dllist_append(Lisp_Dllist*, Lisp_Object);
extern Lisp_Object dllist_car(Lisp_Dllist*);
extern Lisp_Object dllist_rac(Lisp_Dllist*);
extern Lisp_Object dllist_pop_car(Lisp_Dllist*);
extern Lisp_Object dllist_pop_rac(Lisp_Dllist*);
extern Lisp_Object copy_dllist(Lisp_Dllist*);

EXFUN(Fdllist, MANY);

#endif	/* INCLUDED_dllist_h_ */
