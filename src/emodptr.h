/* emodptr.h - Declarations and definitions for XEmacs loadable modules.
(C) Copyright 1998, 1999 J. Kean Johnston. All rights reserved.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef EMODPTR_HDR
#define EMODPTR_HDR

#include <config.h>
#include "lisp.h"
#include "sysdll.h"

typedef struct Lisp_EmodPtr Lisp_EmodPtr;
struct Lisp_EmodPtr {
        struct lcrecord_header header;

        Lisp_Object type;               /* type of emod object */
	Lisp_Object plist;              /* properties list */

	void *ptr;			/* pointer to foreign data */
	void (*mrkfun)(Lisp_Object);	/* marker function */
	void (*prfun)(Lisp_Object, Lisp_Object, int); /* print function */
	void (*finfun)(Lisp_Object, int); /* finaliser function */
};

DECLARE_LRECORD(emodptr, Lisp_EmodPtr);
#define XEMODPTR(x) XRECORD (x, emodptr, Lisp_EmodPtr)
#define XSETEMODPTR(x, p) XSETRECORD (x, p, emodptr)
#define EMODPTRP(x) RECORDP (x, emodptr)
#define CHECK_EMODPTR(x) CHECK_RECORD (x, emodptr)
#define CONCHECK_EMODPTR(x) CONCHECK_RECORD (x, emodptr)
#define XEMODPTR_TYPE(x) (XEMODPTR(x)->type)
#define XEMODPTR_PLIST(x) (XEMODPTR(x)->plist)

extern Lisp_Object make_emodptr(void*);
extern void *get_emodptr(Lisp_Object);
extern void set_emodptr(Lisp_Object, void*);
extern void set_emodptr_marker(Lisp_Object,
			       void(*)(Lisp_Object));
extern void set_emodptr_printer(Lisp_Object,
				void(*)(Lisp_Object, Lisp_Object, int));
extern void set_emodptr_finaliser(Lisp_Object,
				  void(*)(Lisp_Object, int));

#endif
